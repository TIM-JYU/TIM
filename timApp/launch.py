import multiprocessing
import os
import shutil
import subprocess
import sys

# IMPORTANT: tim needs to be the first non-standard import because we want to disable contracts
# before any @contract decorator is encountered.
import tim

import dumboclient
import initdb2

from filemodehelper import change_permission_and_retry

scripts_path = os.path.join('static', 'scripts')
bower_path = os.path.join(scripts_path, 'bower_components')
bower_src = '/bower_components'


def copy_bower_libs():
    """Copies bower libraries from TIM's Docker image to static/scripts."""
    if os.path.exists(bower_path):
        shutil.rmtree(bower_path, onerror=change_permission_and_retry)
    print('Copying bower libs to static/scripts...', end="")
    sys.stdout.flush()
    shutil.copytree(bower_src, bower_path)
    print(' Done.')

if __name__ == '__main__':
    if not os.path.exists(scripts_path):
        raise Exception('static/scripts directory does not seem to exist, '
                        'make sure the working directory is correct')
    if not os.path.exists(bower_path):
        if os.path.exists(bower_src):
            copy_bower_libs()
        else:
            print('You may want to run inside docker first or copy bower libs to static/scripts if there are errors.')
    elif os.path.exists(bower_src):
        expected_files = set(os.listdir(bower_src))
        actual_files = set(os.listdir(bower_path))
        if actual_files != expected_files:
            print('actual bower files differ from expected ones, copying bower libs again')
            copy_bower_libs()
    else:
        print('Not running inside docker, skipping bower check.')
    ephemeral_started = False
    dumbo_started = False
    try:
        if not os.environ.get("WERKZEUG_RUN_MAIN") == "true":
            d = dumboclient.launch_dumbo()
            ephemeral_started = True
            dumbo_started = True
        initdb2.initialize_database()
        initdb2.initialize_temp_database()
        initdb2.update_database()
        if len(sys.argv) <= 1:
            print('Starting without gunicorn.')
            tim.start_app()
        elif sys.argv[1] == '--with-gunicorn':
            print('Starting with gunicorn. CPUs available: {}'.format(multiprocessing.cpu_count()))
            p = subprocess.Popen(["gunicorn", "--config", "gunicornconf.py", "tim:app"])
            p.wait()
        else:
            raise Exception('Unknown command line argument: ' + sys.argv[1])
        tim.start_app()
    finally:
        if ephemeral_started:
            p.kill()
        if dumbo_started:
            d.kill()
