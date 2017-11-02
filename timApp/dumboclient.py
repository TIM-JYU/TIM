"""Defines a client interface for using Dumbo, the markdown converter."""
import json
from typing import List, Union, Dict

import requests

from timApp.documentmodel.timjsonencoder import TimJsonEncoder

DUMBO_URL = 'http://dumbo:5000'


def call_dumbo(data: Union[List[str], Dict, List[Dict]], path='') -> Union[List[str], Dict, List[Dict]]:
    """Calls Dumbo for converting the given markdown to HTML.

    :param data: The data to be converted.
    :param path: The path of the request. Valid paths are: '', '/', '/mdkeys' and '/markdown' (same as '/' and '').
     If path is '/mdkeys', data is expected to be a Dict or List[Dict]. Any dict value that begins with 'md:' is
     interpreted as Pandoc markdown and is converted to HTML. Otherwise the value is unchanged.
     The return value format will be the same as input.
     Otherwise, data is expected to be a List[str]. Each string is interpreted as Pandoc markdown and is converted to
     HTML. The return value format will be the same as input.

    """
    try:
        r = requests.post(url=DUMBO_URL + path, data=json.dumps(data, cls=TimJsonEncoder))
        r.encoding = 'utf-8'
    except requests.ConnectionError:
        raise Exception('Failed to connect to Dumbo')
    if r.status_code != 200:
        raise Exception(f'Failed to get HTML from Dumbo, status code={r.status_code}')
    return r.json()
