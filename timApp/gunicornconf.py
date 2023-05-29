import multiprocessing
import os
from typing import Any

from psycogreen.gevent import patch_psycopg

# This file MUST NOT contain any TIM imports! Otherwise Gunicorn restart signal will not work properly.

bind = "0.0.0.0:5000"
workers = multiprocessing.cpu_count()
worker_class = (
    "gevent"  # Possible values: sync, gevent, eventlet, tornado, gaiohttp, gthread
)
limit_request_line = 0
timeout = 600
max_requests = int(os.environ.get("GUNICORN_MAX_REQUESTS", 2000))
max_requests_jitter = int(os.environ.get("GUNICORN_MAX_REQUESTS_JITTER", 200))


def post_fork(server: Any, worker: Any) -> None:
    patch_psycopg()
