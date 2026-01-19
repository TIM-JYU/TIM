"""
TIM Health check
=================
Provides an endpoint for checking the health of various TIM components.
"""
from datetime import timedelta
import time
from concurrent.futures import ThreadPoolExecutor, as_completed, TimeoutError
from enum import Enum
from http import HTTPStatus
from typing import Callable, Literal, Protocol
from flask import Blueprint, current_app
from sqlalchemy import text, inspect
from timApp.health.checks import (
    check_celery,
    check_disk_space,
    check_dumbo_service,
    check_frontpage,
    check_gunicorn,
    check_pgsql,
    check_redis,
    check_writable,
)
from timApp.health.models import CheckStatus, HealthStatus
from timApp.timdb.sqa import db
from timApp.util.logger import tim_logger
from tim_common.marshmallow_dataclass import dataclass

health_blueprint = Blueprint("health", __name__, url_prefix="/health")

logger = tim_logger.getChild("health")

# Timeout for individual health checks in seconds
HEALTH_CHECK_TIMEOUT = 5.0

def run_check_safe(check_fn: Callable[[], CheckStatus]) -> CheckStatus:
    """
    Safely executes a health check function, catching any exceptions.
    
    :param check_fn: The check function to execute
    :return: CheckStatus.ERROR if an exception occurs, otherwise the check result
    """
    try:
        check = check_fn()
        logger.debug(f"Health check {check_fn.__name__} result: {check!s}")
        return check
    except Exception:
        logger.exception("Health check %s failed", check_fn.__name__, exc_info=True)
        return CheckStatus.ERROR


def run_health_checks(checks_registry: dict[str, Callable[[], CheckStatus]], timeout: float = HEALTH_CHECK_TIMEOUT) -> dict[str, CheckStatus]:
    """
    Runs all health checks concurrently using a thread pool.
    
    :param checks_registry: Dictionary mapping check names to check functions
    :param timeout: Timeout in seconds for each individual check before marking it as ERROR
    :return: Dictionary mapping check names to their results
    """
    checks: dict[str, CheckStatus] = {}
    
    with ThreadPoolExecutor(max_workers=len(checks_registry)) as executor:
        # Submit all check functions with exception handling wrapper
        future_to_check = {
            executor.submit(run_check_safe, check_fn): check_name
            for check_name, check_fn in checks_registry.items()
        }
        
        # Collect results as they complete
        for future in as_completed(future_to_check, timeout=timeout * len(checks_registry)):
            check_name = future_to_check[future]
            try:
                checks[check_name] = future.result(timeout=timeout)
            except TimeoutError:
                logger.warning("Health check %s timed out after %ss", check_name, timeout)
                checks[check_name] = CheckStatus.ERROR

    return checks


@health_blueprint.route("", methods=["GET"])
def health_check():
    """
    Health check endpoint

    Returns the overall health status and individual check results.
    """

    # Define the health checks to run
    checks_registry: dict[str, Callable[[], CheckStatus]] = {
        "pgsql": check_pgsql,
        "redis": check_redis,
        "celery": check_celery,
        "dumbo_service": check_dumbo_service,
        "wsgi": check_gunicorn,
        "writable": check_writable,
        "disk_space": check_disk_space,
        "frontpage": check_frontpage,
    }

    # Run all checks concurrently
    checks = run_health_checks(checks_registry)

    # Determine overall status
    response_code: HTTPStatus = HTTPStatus.SERVICE_UNAVAILABLE
    overall_status: CheckStatus = CheckStatus.ERROR

    if all(checks.values()):
        # a-ok
        overall_status = CheckStatus.OK
    elif CheckStatus.ERROR in checks.values():
        # some or all checks failed
        overall_status = CheckStatus.ERROR
    elif CheckStatus.DEGRADED in checks.values():
        # some issues detected
        # still OK, but degraded.
        overall_status = CheckStatus.DEGRADED
    else:
        logger.error("Inconsistent health check results: %s", checks)

    # Set appropriate HTTP response code
    if overall_status == CheckStatus.OK:
        response_code = HTTPStatus.OK
    elif overall_status == CheckStatus.DEGRADED:
        # Still OK, but degraded.
        # OK is returned so that load balancers etc. don't mark the service as down.
        response_code = HTTPStatus.OK
    else:
        response_code = HTTPStatus.SERVICE_UNAVAILABLE

    status = HealthStatus(status=overall_status, checks=checks)
    
    return status, response_code
