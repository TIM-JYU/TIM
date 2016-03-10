import logging

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
MONITOR_INTERVAL = 10


def make_request(host, url, https):
    if https:
        conn = HTTPSConnection(host)
    else:
        conn = HTTPConnection(host)

    try:
        req = conn.request('GET', url)
        resp = conn.getresponse()
        status, reason = resp.status, resp.reason
    except ConnectionError as e:
        status, reason = e.errno, e.strerror
    except Exception as e:
        status, reason = -1, str(e.__class__) + ': ' + str(e)
    finally:
        conn.close()

    return (status, reason)


def retry_request(host, url, https, times=3, delay=1):
    for i in range(0, times):
        status, reason = make_request(host, url, https)
        if status == 200:
            break
        sleep(delay)

    return status, reason


def log_status(name, status, reason, restarted):
    if status == 200:
        logging.getLogger().info(name + " is up")
        return True

    if restarted:
        logging.getLogger().warn("{} was down ({} {}) but was restarted".format(name, status, reason))
    else:
        logging.getLogger().error("{} is down ({} {}) and could not be restarted".format(name, status, reason))

    return True


def send_email(name, status, reason, restarted):
    log_status(name, status, reason, restarted)
    if status == 200 or restarted:
        return True
    msg = MIMEText("""
{0} is down and could not be restarted.
HTTP status is: {1} {2}
""".format(name, status, reason)
    )
    msg['Subject'] = "{} is down".format(name)
    msg['From'] = "wuff@tim.jyu.fi"
    msg['To'] = "tomi.karppinen@jyu.fi"

    s = SMTP('smtp.jyu.fi')
    s.send_message(msg)
    s.quit()
    return False


def poll_nginx():
    status, reason = retry_request(TIM_HOST, '/', USE_HTTPS)
    return (status, reason) if status == 111 else (200, 'OK')


def poll_tim():
    return retry_request(TIM_HOST, '/', USE_HTTPS)


def poll_csplugin():
    pluginhost = 'localhost:56000'
    status, reason = retry_request(pluginhost, '/reqs', PLUGIN_HTTPS)
    if status == -1 and 'BadStatusLine' in reason:
        # This is normal for csplugin, for now
        status = 200

    return (status, reason)


def poll_haskellplugins():
    pluginhost = 'localhost:60000'
    return retry_request(pluginhost, '/reqs', PLUGIN_HTTPS)


def restart(*args, timeout=60, timeout_after=5):
    try:
        call([RESTART_SCRIPT] + list(args), timeout=timeout)
        sleep(timeout_after)
    except TimeoutExpired:
        logging.getLogger().critical("{} {} didn't terminate".format(RESTART_SCRIPT, ' '.join(list(args))))


def restart_nginx():
    restart('nginx')


def restart_tim():
    restart('tim')


def restart_plugins():
    restart('plugins')


def watchdog(name, pollfunc, restartfunc, reportfunc, retries=1):
    restarted = False
    for i in range(0, retries):
        status, reason = pollfunc()
        if status == 200:
            return reportfunc(name, status, reason, restarted)
        restartfunc()
        restarted = True

    return reportfunc(name, status, reason, False)


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
        report_func = send_email
    else:
        report_func = log_status

    try:
        logging.critical("Monitoring " + TIM_HOST)
        while run and watchdog('nginx', poll_nginx, restart_nginx, report_func) \
                  and watchdog('tim', poll_tim, restart_tim, report_func) \
                  and watchdog('csplugin', poll_csplugin, restart_plugins, report_func) \
                  and watchdog('haskellplugins', poll_haskellplugins, restart_plugins, report_func):
            sleep(MONITOR_INTERVAL)

    except KeyboardInterrupt:
        logging.critical("Shutting down")

