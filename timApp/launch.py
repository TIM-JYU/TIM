import os
import tim
import ephemeralclient
import sys

if __name__ == '__main__':
    try:
        ephemeral_started = False
        if not os.environ.get("WERKZEUG_RUN_MAIN") == "true":
            p = ephemeralclient.launch_ephemeral(ignore_signals='pudb' in sys.modules)
            ephemeral_started = True
        tim.startApp()
    finally:
        if ephemeral_started:
            p.kill()
