import multiprocessing
import os
import signal
import subprocess
import sys
from pprint import pprint
from types import FrameType

import timApp.tim
from timApp.admin.routes import gunicorn_pid_path
from timApp.tim_app import app
from timApp.timdb.init import initialize_database
from timApp.util.logger import log_info
from timApp.util.utils import pycharm_running


def quit_fast(_sig: signal.Signals, _frame: FrameType) -> None:
    sys.exit(0)


def main() -> None:
    try:
        from timApp.rust_ext.tim_core import load_document
    except ModuleNotFoundError:
        print('Rust extension modules were not found. Run: ./build_rust.sh')
        sys.exit(1)
    # pprint(load_document(3))
    # quit faster when running in PyCharm
    if pycharm_running():
        signal.signal(signal.SIGINT, quit_fast)
    initialize_database()
    try:
        os.remove(app.config['GLOBAL_NOTIFICATION_FILE'])
    except FileNotFoundError:
        pass
    if len(sys.argv) <= 1:
        log_info('Starting without gunicorn.')
        timApp.tim.start_app()
    elif sys.argv[1] == '--with-gunicorn':
        log_info(f'Starting with gunicorn. CPUs available: {multiprocessing.cpu_count()}')
        p = subprocess.Popen(["gunicorn", "-p", gunicorn_pid_path, "--config", "gunicornconf.py", "tim:init_app()"])
        p.wait()
    else:
        raise Exception('Unknown command line argument: ' + sys.argv[1])


if __name__ == '__main__':
    main()
