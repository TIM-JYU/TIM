import os
import shutil
import sys

from filemodehelper import change_permission_and_retry

scripts_path = os.path.join('static', 'scripts')
bower_path = os.path.join(scripts_path, 'bower_components')
node_modules_path = os.path.join(scripts_path, 'node_modules')
bower_src = '/bower_components'
node_modules_src = '/tim_node_modules/node_modules'


def copy_dir(src, dst):
    """Copies a directory from TIM's Docker image to static/scripts."""
    if os.path.exists(dst):
        shutil.rmtree(dst, onerror=change_permission_and_retry)
    print('Copying {} to {}...'.format(src, dst), end="")
    sys.stdout.flush()
    shutil.copytree(src, dst)
    print(' Done.')


def copy_dir_if_needed(src, dst):
    if not os.path.exists(dst):
        if os.path.exists(src):
            copy_dir(src, dst)
        else:
            raise Exception('Source directory {} does not exist.'.format(src))
    elif os.path.exists(src):
        expected_files = set(os.listdir(src))
        actual_files = set(os.listdir(dst))
        if actual_files != expected_files:
            print('actual files ({}) differ from expected ones ({}), copying directory again'.format(src, dst))
            copy_dir(src, dst)
    else:
        raise Exception('Source directory {} does not exist.'.format(src))


def copy_dirs_if_needed():
    # copy_dir_if_needed(bower_src, bower_path)
    copy_dir_if_needed(node_modules_src, node_modules_path)


if __name__ == '__main__':
    copy_dirs_if_needed()
