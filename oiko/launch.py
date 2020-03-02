import multiprocessing
import os
import signal
import subprocess
import sys


# noinspection PyUnusedLocal
def quit_fast(sig, frame):
    sys.exit(0)


if __name__ == '__main__':
    if len(sys.argv) <= 1:
        # log_info('Starting oiko without gunicorn.')
        p = subprocess.Popen(["flask", "run"])
        p.wait()
        pass
    elif sys.argv[1] == '--with-gunicorn':
        # log_info(f'Starting oiko with gunicorn. CPUs available: {multiprocessing.cpu_count()}')
        p = subprocess.Popen(["gunicorn", "-p", "/var/run/gunicorn.pid", "--config", "gunicornconf.py", "app:init_app()"])
        p.wait()
    else:
        raise Exception('Unknown command line argument: ' + sys.argv[1])
