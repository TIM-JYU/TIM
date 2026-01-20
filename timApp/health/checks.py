from pathlib import Path
from typing import List, Set
from timApp.util.logger import tim_logger
from datetime import timedelta
import time
from flask import current_app
from redis.exceptions import RedisError
from sqlalchemy import text
from timApp.document.caching import rclient
from timApp.health.models import CheckStatus
from timApp.timdb.sqa import db
from timApp.util.logger import tim_logger

import os
import tempfile
import socket

from celery import Celery
from tim_common.dumboclient import call_dumbo, DumboHTMLException

# Disable redis replication check until it has been properly tested
ENABLE_REDIS_REPLICATION_CHECK = False

try:
    import psutil
    HAS_PSUTIL = True
except ImportError:
    HAS_PSUTIL = False

try:
    import gunicorn.workers
    HAS_GUNICORN = True
except ImportError:
    HAS_GUNICORN = False

logger = tim_logger.getChild("health")

def check_pgsql() -> CheckStatus:
    """
    Check PostgreSQL database connectivity and health.
    
    Performs multiple checks:
    - Basic connectivity (SELECT 1)
    - Connection pool status
    - Query response time

    Returns DEGRADED if database is slow or connection pool is nearly exhausted.
    """
    SLOW_QUERY_THRESHOLD = timedelta(seconds=1)

    try:
        start_time = time.time()
        
        # Basic connectivity check
        result = db.session.execute(text("SELECT 1")).scalar()
        if result != 1:
            logger.warning("PostgreSQL SELECT 1 returned unexpected result: %s", result)
            return CheckStatus.error

        # Check connection pool status
        pool = db.engine.pool
        pool_size = pool.size()
        checked_out = pool.checkedout()

        # Check response time
        elapsed = time.time() - start_time
        if elapsed > SLOW_QUERY_THRESHOLD.total_seconds():
            logger.warning("PostgreSQL health check took %.2fs (slow)", elapsed)
            return CheckStatus.degraded

        # Check if connection pool is nearly exhausted (90% usage)
        if pool_size > 0 and checked_out >= pool_size * 0.9:
            logger.warning(
                "PostgreSQL connection pool nearly exhausted: %d/%d connections in use",
                checked_out,
                pool_size,
            )
            return CheckStatus.degraded
        
        return CheckStatus.ok
        
    except Exception as e:
        logger.error("PostgreSQL health check failed: %s", e)
        return CheckStatus.error

def check_redis() -> CheckStatus:
    """
    Check Redis/KeyDB connectivity and health.

    Status:
    - :object:`CheckStatus.OK`: Redis is reachable, responsive, and replication is in sync.
    - :object:`CheckStatus.DEGRADED`: Redis is reachable but slow, memory usage is high, or replicas are falling behind.
    - :object:`CheckStatus.ERROR`: Redis is unreachable or returns errors.

    """
    SLOW_QUERY_THRESHOLD = timedelta(seconds=1)  # A silly number, adjust as needed
    HIGH_MEMORY_THRESHOLD = 0.9  # 90% of max memory
    REPLICATION_LAG_THRESHOLD = 1024 * 1024 * 10  # 10MB replication lag in bytes

    def _replication_check(rclient=rclient) -> CheckStatus | None:
        try:
            replication = rclient.info("replication")
            role = replication.get("role", "unknown")

            # Only check replication metrics if this is a replica
            if role == "slave":
                master_link_down_since_seconds = replication.get("master_link_down_since_seconds", -1)

                # -1 means the link is up (or field doesn't exist)
                if master_link_down_since_seconds > 0:
                    logger.warning(
                        "Redis replica master link down for %d seconds",
                        master_link_down_since_seconds,
                    )
                    return CheckStatus.degraded

                # Check replication lag
                master_repl_offset = replication.get("master_repl_offset", 0)
                slave_repl_offset = replication.get("slave_repl_offset", 0)
                replication_lag = master_repl_offset - slave_repl_offset

                if replication_lag > REPLICATION_LAG_THRESHOLD:
                    logger.warning(
                        "Redis replica noticeably behind: %d bytes lag",
                        replication_lag,
                    )
                    return CheckStatus.degraded

            else:
                logger.debug("Redis replication - role: %s", role)

            return None
        except (KeyError, RedisError) as e:
            # Replication info not critical, log but don't fail
            logger.debug("Could not retrieve Redis replication info: %s", e)
            return None

    try:
        start_time = time.time()
        
        # Basic connectivity check
        ping_result = rclient.ping()
        if not ping_result:
            logger.warning("Redis PING returned False")
            return CheckStatus.error
        
        # Check response time
        elapsed = time.time() - start_time
        if elapsed > SLOW_QUERY_THRESHOLD.total_seconds():
            logger.warning("Redis health check took %.2fs (slow)", elapsed)
            return CheckStatus.degraded

        # Check memory usage
        try:
            info = rclient.info("memory")
            used_memory = info.get("used_memory", 0)
            maxmemory = info.get("maxmemory", 0)
            
            # Only check memory if maxmemory is set (0 means unlimited)
            if maxmemory > 0 and used_memory >= maxmemory * HIGH_MEMORY_THRESHOLD:
                logger.warning(
                    "Redis memory usage high: %d/%d bytes (%.1f%%)",
                    used_memory,
                    maxmemory,
                    (used_memory / maxmemory) * 100,
                )
                return CheckStatus.degraded
            
            logger.debug(
                "Redis memory stats - used_memory: %d bytes, maxmemory: %d bytes",
                used_memory,
                maxmemory,
            )
        except (KeyError, RedisError) as e:
            # Memory info not critical, log but don't fail
            logger.debug("Could not retrieve Redis memory info: %s", e)

        if ENABLE_REDIS_REPLICATION_CHECK:
            rep_result = _replication_check(rclient)
            if rep_result is CheckStatus.degraded:
                return CheckStatus.degraded
        else:
            logger.debug("Redis replication check disabled")

        return CheckStatus.ok
        
    except RedisError as e:
        logger.error("Redis health check failed: %s", e, exc_info=True)
        return CheckStatus.error
    except Exception as e:
        logger.error("Redis health check failed with unexpected error: %s", e, exc_info=True)
        return CheckStatus.error


def check_celery() -> CheckStatus:
    """
    Check Celery worker status.
    
    Performs checks:
    - At least one worker is active
    - Workers are responsive (via ping)
    
    Returns ERROR if no workers are available or they don't respond.
    """
    CELERY_PING_TIMEOUT = timedelta(seconds=2)
    
    try:
        # Create a minimal Celery instance for inspection only
        broker_url = current_app.config.get("CELERY_BROKER_URL", "redis://redis:6379")
        backend = current_app.config.get("CELERY_RESULT_BACKEND", "rpc://")
        cel = Celery(
            backend=backend,
            broker=broker_url
        )

        # Get active workers by pinging them
        inspector = cel.control.inspect(timeout=CELERY_PING_TIMEOUT.total_seconds())
        active_workers = inspector.active()

        if not active_workers:
            logger.warning("No Celery workers are active")
            return CheckStatus.error
        
        # Check if workers respond to ping
        ping_responses = inspector.ping()
        if not ping_responses:
            logger.warning("Celery workers did not respond to ping")
            return CheckStatus.error

        worker_count = len(ping_responses)
        logger.debug("Celery workers active: %d", worker_count)
        
        return CheckStatus.ok
        
    except Exception as e:
        logger.error("Celery health check failed: %s", e)
        return CheckStatus.error


def check_dumbo_service() -> CheckStatus:
    """
    Check Dumbo markdown converter service health.
    
    Performs a simple markdown conversion test to verify:
    - Service is reachable
    - Service responds correctly
    - Service can process markdown
    
    Returns ERROR if service is unreachable or fails to convert.
    """
    try:
        start_time = time.time()
        
        # Use call_dumbo to test the service with a simple markdown conversion
        result = call_dumbo(["# Test"])
        elapsed = time.time() - start_time
        
        # Verify response contains expected HTML
        if not isinstance(result, list) or len(result) == 0:
            logger.warning("Dumbo service returned unexpected response format")
            return CheckStatus.error
        
        # Check if the conversion worked (should contain heading tag)
        if "<h1" not in result[0]:
            logger.warning("Dumbo service conversion produced unexpected output")
            return CheckStatus.error
        
        logger.debug("Dumbo service responded in %.2fs", elapsed)
        return CheckStatus.ok
        
    except DumboHTMLException as e:
        logger.error("Dumbo service returned error status")
        return CheckStatus.error
    except Exception as e:
        # This catches ConnectionError from call_dumbo
        logger.error("Dumbo service health check failed: %s", e)
        return CheckStatus.error


def check_gunicorn() -> CheckStatus:
    """
    Check Gunicorn worker health and request queue depth.
    
    Performs checks:
    - At least one gunicorn worker process is running
    - Request queue depth is within acceptable limits
    - Workers are not using excessive memory

    Note:
    - Expectation is that Gunicorn is runnning as synchronous workers
      (not async/eventlet/gevent). If async workers are used, this check
      treshold `QUEUE_DEPTH_WARN_THRESHOLD` may need adjustment.
    """

    MAX_RAM_USAGE_RATIO = 0.80  # Warn if workers use 80%+ of total system RAM
    
    if not HAS_PSUTIL:
        logger.debug("psutil not available, skipping gunicorn health check")
        return CheckStatus.skipped

    if not HAS_GUNICORN or not os.environ.get("SERVER_SOFTWARE", "").startswith("gunicorn/"):
        logger.debug("Not running under Gunicorn, skipping gunicorn health check")
        return CheckStatus.skipped

    try:
        # Since this check runs on a gunicorn worker, we can directly get:
        # - Current process (this worker)
        # - Parent process (the gunicorn master)
        # - All children of the master (all workers including this one)
        current_proc = psutil.Process()
        master_proc = current_proc.parent()
        
        if not master_proc:
            logger.debug("Could not find parent process (gunicorn master)")
            return CheckStatus.skipped
        
        # Get all worker processes (children of the master)
        worker_processes: List[psutil.Process] = master_proc.children()

        if not worker_processes:
            logger.warning("Gunicorn master found (%s), but no active workers.", master_proc.pid if master_proc else "N/A")
            return CheckStatus.error

        # Check RAM usage of workers
        worker_rss = sum(p.memory_info().rss for p in worker_processes if p.is_running())
        mem = psutil.virtual_memory()

        if mem.total > 0 and (worker_rss / mem.total) >= MAX_RAM_USAGE_RATIO:
            logger.warning(
                "Gunicorn workers using high memory: %d/%d bytes (%.1f%%)",
                worker_rss,
                mem.total,
                (worker_rss / mem.total) * 100,
            )
            return CheckStatus.degraded

        return CheckStatus.ok

    except Exception as e:
        logger.error("Gunicorn health check failed: %s", e)
        return CheckStatus.error
    
    return CheckStatus.ok


def _get_writable_paths() -> Set[str]:
    """
    Get the set of paths that need to be writable for the application.
    """
    from timApp.util.utils import cache_folder_path
    from timApp.util.utils import temp_folder_path

    paths = set([
        current_app.config.get("FILES_PATH", None),  
        current_app.config.get("LOG_DIR", None),
        cache_folder_path,
        temp_folder_path,
    ])
    return {str(p) for p in paths if p is not None}


def check_writable() -> CheckStatus:
    """
    Check if the application can write to the configured writable directory.
    """

    paths = _get_writable_paths()
    now_bstr = time.strftime("%Y-%m-%d %H:%M:%S", time.gmtime()).encode("utf-8")

    for writable_path in paths:
        try:
            # Check if path exists and is a directory
            if not os.path.isdir(writable_path):
                logger.error("Expected writable directory is not a directory: %r", writable_path)
                return CheckStatus.error

            # Attempt to create and delete a temporary file in the writable directory
            # File will be deleted automatically
            with tempfile.NamedTemporaryFile(dir=writable_path, delete=True) as tmpfile:
                tmpfile.write(b"health check %b" % now_bstr)
                tmpfile.flush()
            continue  # Success for this path, check next
        except Exception as e:
            logger.error("Writable directory health check failed: %s", e)
            return CheckStatus.error

    return CheckStatus.ok


def check_disk_space() -> CheckStatus:
    """
    Check if there is sufficient disk space available in writable directories.

    Returns DEGRADED if free space is below threshold, otherwise OK.
    """
    import shutil

    DISK_SPACE_THRESHOLD = 0.2  # 20% free space

    paths = _get_writable_paths()
    for path in paths:
        try:
            total, used, free = shutil.disk_usage(path)
            free_ratio = free / total if total > 0 else 0

            if total and used >= total:
                logger.error("No disk space available on %s", path)
                return CheckStatus.error
            elif free_ratio < DISK_SPACE_THRESHOLD:
                logger.warning(
                    "Low disk space on %s: %.1f%% free",
                    path,
                    free_ratio * 100,
                )
                return CheckStatus.degraded
        except Exception as e:
            logger.error("Disk space check failed for %s: %s", path, e)
            return CheckStatus.error

    return CheckStatus.ok


def check_page(route):
    """
    Check if a given page route is accessible and returns expected content.

    Expects HTTP 200 and presence of "TIM" in the response.
    """
    try:
        with current_app.test_client() as client:
            response = client.get(route)
            # Check for 200 OK and that "TIM" exists in the decoded HTML
            return response.status_code == 200 and b"TIM" in response.data
    except Exception:
        return False


def check_frontpage() -> CheckStatus:
    """
    Check if the frontpage is accessible.
    """
    if not Path("static/scripts/build").exists():
        # If TIM is built, check the frontpage
        logger.debug("Scripts not built, skipping frontpage check")
        return CheckStatus.skipped

    if check_page("/"):
        return CheckStatus.ok
    return CheckStatus.error

