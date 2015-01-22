import tim
import ephemeralclient
import sys

if __name__ == '__main__':
    try:
        p = ephemeralclient.launch_ephemeral(ignore_signals='pudb' in sys.modules)
        tim.startApp()
    finally:
        p.kill()
