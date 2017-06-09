# workaround for https://github.com/requests/requests/issues/3752
import gevent.monkey
gevent.monkey.patch_ssl()

import multiprocessing

from timApp.timdb import timdb2

bind = "0.0.0.0:5000"
workers = multiprocessing.cpu_count() * 2 + 1  # Recommended value is cpu_count() * 2 + 1
worker_class = "gevent"  # Possible values: sync, gevent, eventlet, tornado, gaiohttp, gthread
limit_request_line = 0
timeout = 600


# noinspection PyUnusedLocal
def pre_request(worker, req):
    timdb2.worker_pid = worker.pid
