import os
import shutil
import tim
import ephemeralclient
import sys
from filemodehelper import change_permission_and_retry

scripts_path = os.path.join('static', 'scripts')
bower_path = os.path.join(scripts_path, 'bower_components')


def copy_bower_libs():
    """Copies bower libraries from TIM's Docker image to static/scripts."""
    if os.path.exists(bower_path):
        shutil.rmtree(bower_path, onerror=change_permission_and_retry)
    print('Copying bower libs to static/scripts...', end="")
    sys.stdout.flush()
    shutil.copytree('/bower_components', bower_path)
    print(' Done.')

if __name__ == '__main__':
    if not os.path.exists(scripts_path):
        raise Exception('static/scripts directory does not seem to exist, '
                        'make sure the working directory is correct')
    if not os.path.exists(bower_path):
        copy_bower_libs()
    else:
        expected_files = set(os.listdir('/bower_components'))
        actual_files = set(os.listdir(bower_path))
        if actual_files != expected_files:
            print('actual bower files differ from expected ones, copying bower libs again')
            copy_bower_libs()
    ephemeral_started = False
    try:
        if not os.environ.get("WERKZEUG_RUN_MAIN") == "true":
            p = ephemeralclient.launch_ephemeral(ignore_signals='pudb' in sys.modules)
            ephemeral_started = True
        tim.startApp()
    finally:
        if ephemeral_started:
            p.kill()
