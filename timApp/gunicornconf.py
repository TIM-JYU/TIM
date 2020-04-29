import multiprocessing

# This file MUST NOT contain any TIM imports! Otherwise Gunicorn restart signal will not work properly.

bind = "0.0.0.0:5000"
workers = multiprocessing.cpu_count()
worker_class = "gevent"  # Possible values: sync, gevent, eventlet, tornado, gaiohttp, gthread
limit_request_line = 0
timeout = 600
