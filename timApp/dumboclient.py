"""Defines a client interface for using Dumbo, the markdown converter."""
import json
import os
import subprocess
from subprocess import DEVNULL, STDOUT
from typing import List

import requests
import time

DUMBO_URL = 'http://127.0.0.1:8000'
DUMBO_PATH = os.path.join("..", "Ephemeral", "Dumbo", "dist", "build", "Dumbo")


def launch_dumbo():
    """
    Launches Dumbo.

    :return: The process object that represents the Dumbo process.
    :rtype: subprocess.Popen
    """
    path = DUMBO_PATH
    log_path = os.path.join(path, "log")
    if not os.path.exists(log_path):
        os.mkdir(log_path)

    old = os.getcwd()
    os.chdir(path)
    p = subprocess.Popen([os.path.join(".", "Dumbo"), "-p", "8000"], stdout=DEVNULL, stderr=STDOUT)
    os.chdir(old)
    time.sleep(0.1)
    return p


def call_dumbo(md_blocks: List[str]) -> List[str]:
    """
    Calls Dumbo for converting the given list of markdown texts to HTML.

    :type md_blocks: list[str]
    :rtype: list[str]
    :param md_blocks: A list of markdown blocks to be converted.
    """
    try:
        r = requests.post(url=DUMBO_URL, data=json.dumps(md_blocks))
    except requests.ConnectionError:
        raise Exception('Failed to connect to Dumbo')
    if r.status_code != 200:
        raise Exception('Failed to get HTML from Dumbo, status code={}'.format(r.status_code))
    return r.json()
