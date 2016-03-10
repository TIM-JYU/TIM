import logging
import os

from email.mime.text import MIMEText
from http.client import *
from logging.config import fileConfig
from smtplib import SMTP
from subprocess import call, TimeoutExpired
from sys import argv
from time import sleep

TIM_HOST = 'localhost'
USE_HTTPS = False
PLUGIN_HTTPS = False
TIM_NAME = 'tim'
RESTART_SCRIPT = './restart_local.sh'
MONITOR_INTERVAL = 120


def make_request(host, url, https):
    if https:
        conn = HTTPSConnection(host)
    else:
        conn = HTTPConnection(host)

    try:
        conn.request('GET', url, headers={'User-Agent': 'wuff'})
        resp = conn.getresponse()
        status, reason = resp.status, resp.reason
    except ConnectionError as e:
        status, reason = e.errno, e.strerror
    except Exception as e:
        status, reason = -1, str(e.__class__) + ': ' + str(e)
    finally:
        conn.close()

    return status, reason


def retry_request(host, url, https, times=3, delay=1):
    for i in range(0, times):
        status, reason = make_request(host, url, https)
        if status == 200:
            break
        sleep(delay)

    return status, reason


def get_crashed_message(args):
    return '{} down'.format(', '.join(args))


def log_crashed(args):
    if len(args) == 0:
        logging.getLogger().debug("Everything is up and running")
        return []

    logging.getLogger().critical(get_crashed_message(args))


def report_crashed(args):
    if len(args) == 0:
        return []

    log_crashed(args)
    msg = MIMEText(get_crashed_message(args))
    msg['Subject'] = get_crashed_message(args)
    msg['From'] = "wuff@tim.jyu.fi"
    msg['To'] = "tomi.karppinen@jyu.fi"

    s = SMTP('smtp.jyu.fi')
    s.send_message(msg)
    s.quit()


def poll(name, host, url, https):
    status, reason = retry_request(host, url, https)
    if status != 200:
        logging.getLogger().warn('{} returned status {} - {}'.format(name, status, reason))
        return status < 0 # ignore bad status lines and such for now

    return True


def poll_tim():
    return [] if poll('tim', TIM_HOST, '/', USE_HTTPS) else ['tim', 'postgre', 'nginx', 'funnel']


def poll_plugins():
    plugins = poll('csplugin', TIM_HOST, '/cs/reqs', USE_HTTPS) \
              and poll('haskellplugins', 'localhost:57000', '/reqs', False) \
              and poll('haskellplugins', 'localhost:58000', '/reqs', False) \
              and poll('haskellplugins', 'localhost:59000', '/reqs', False) \
              and poll('haskellplugins', 'localhost:60000', '/reqs', False) \

    return [] if plugins else ['plugins']


def restart(args, timeout=120, timeout_after=60):
    if len(args) == 0:
        return []

    try:
        logging.getLogger().info('Restarting {}'.format(', '.join(args)))
        call([RESTART_SCRIPT] + args, timeout=timeout)
        sleep(timeout_after)
        return get_crashed_programs()
    except TimeoutExpired:
        logging.getLogger().critical("{} {} didn't terminate".format(RESTART_SCRIPT, ' '.join(args)))
        return args


def get_crashed_programs():
    return poll_tim() + poll_plugins()


if __name__ == '__main__':
    fileConfig('wuff_logging.ini')

    run = True
    if len(argv) > 1:
        if argv[1] == 'tim':
            TIM_HOST = 'tim.jyu.fi'
        elif argv[1][:3] == 'tim':
            TIM_HOST = 'tim-{}.it.jyu.fi'.format(argv[1][3:])
        else:
            print('Usage:', argv[0], ' [tim|tim-beta|tim-dev]')
            run = False

        USE_HTTPS = True
        TIM_NAME = argv[1]
        RESTART_SCRIPT = './restart.sh'
        report_func = report_crashed
    else:
        report_func = log_crashed

    try:
        logging.info("Monitoring " + TIM_HOST)
        while True:
            if not os.path.isfile('restarting'):
                report_func(restart(get_crashed_programs()))
            else:
                logging.getLogger().debug('Restart script is running, wuff is paused')

            sleep(MONITOR_INTERVAL)

    except KeyboardInterrupt:
        logging.info("Shutting down")

