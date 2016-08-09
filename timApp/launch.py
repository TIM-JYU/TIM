import multiprocessing
import os
import subprocess
import sys

import tim

import dumboclient
import initdb2
from bower_helper import scripts_path, copy_bower_libs_if_needed
from routes.logger import log_info

if __name__ == '__main__':
    if not os.path.exists(scripts_path):
        raise Exception('static/scripts directory does not seem to exist, '
                        'make sure the working directory is correct')
    copy_bower_libs_if_needed()
    dumbo_started = False
    try:
        if not os.environ.get("WERKZEUG_RUN_MAIN") == "true":
            d = dumboclient.launch_dumbo()
            dumbo_started = True
        initdb2.initialize_database()
        initdb2.initialize_temp_database()
        initdb2.update_database()
        if len(sys.argv) <= 1:
            log_info('Starting without gunicorn.')
            tim.start_app()
        elif sys.argv[1] == '--with-gunicorn':
            log_info('Starting with gunicorn. CPUs available: {}'.format(multiprocessing.cpu_count()))
            p = subprocess.Popen(["gunicorn", "--config", "gunicornconf.py", "tim:app"])
            p.wait()
        else:
            raise Exception('Unknown command line argument: ' + sys.argv[1])
        tim.start_app()
    finally:
        if dumbo_started:
            d.kill()
