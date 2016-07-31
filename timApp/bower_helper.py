import os
import shutil
import sys

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


def copy_bower_libs_if_needed():
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


if __name__ == '__main__':
    copy_bower_libs_if_needed()
