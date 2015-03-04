import os
import shutil
import tim
import ephemeralclient
import sys

if __name__ == '__main__':
    bower_path = 'static/scripts/bower_components'
    if not os.path.exists(bower_path):
        print('Copying bower libs to static/scripts...', end="")
        sys.stdout.flush()
        shutil.copytree('/bower_components', bower_path)
        print(' Done.')
    ephemeral_started = False
    try:
        if not os.environ.get("WERKZEUG_RUN_MAIN") == "true":
            p = ephemeralclient.launch_ephemeral(ignore_signals='pudb' in sys.modules)
            ephemeral_started = True
        tim.startApp()
    finally:
        if ephemeral_started:
            p.kill()
