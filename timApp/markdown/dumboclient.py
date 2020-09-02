"""Defines a client interface for using Dumbo, the markdown converter."""
import json
from enum import Enum
from typing import List, Union, Dict, NamedTuple, Optional, overload

import requests

from timApp.document.timjsonencoder import TimJsonEncoder


class DumboHTMLException(Exception):
    code = 400
    description = ""


class MathType(Enum):
    SVG = 'svg'
    MathJax = 'mathjax'
    PNG = 'png'

    @staticmethod
    def from_string(s: str):
        try:
            return MathType(s)
        except ValueError:
            return MathType.MathJax


class InputFormat(Enum):
    CommonMark = 'commonmark'
    GitHubMarkdown = 'gfm'
    Markdown = 'markdown'
    MarkdownStrict = 'markdown_strict'
    MediaWiki = 'mediawiki'
    RST = 'rst'

    @staticmethod
    def from_string(s: str):
        try:
            return InputFormat(s)
        except ValueError:
            return InputFormat.Markdown


class DumboOptions(NamedTuple):
    math_type: MathType
    math_preamble: str
    input_format: InputFormat

    @staticmethod
    def default():
        return DumboOptions(
            math_type=MathType.MathJax,
            math_preamble='',
            input_format=InputFormat.Markdown,
        )

    def dict(self):
        return {
            'mathOption': self.math_type.value,
            'mathPreamble': self.math_preamble,
            'inputFormat': self.input_format.value,
        }


DUMBO_URL = 'http://dumbo:5000'

KEYS_PATHS = {'/mdkeys', '/latexkeys'}


@overload
def call_dumbo(data: List[str], path='',
               options: DumboOptions = DumboOptions.default(),
               data_opts: Optional[List[DumboOptions]]=None) -> List[str]: ...


@overload
def call_dumbo(data: Dict, path='',
               options: DumboOptions = DumboOptions.default(),
               data_opts: Optional[List[DumboOptions]]=None) -> Dict: ...


@overload
def call_dumbo(data: List[Dict], path='',
               options: DumboOptions = DumboOptions.default(),
               data_opts: Optional[List[DumboOptions]]=None) -> List[Dict]: ...


def call_dumbo(data: Union[List[str], Dict, List[Dict]], path='',
               options: DumboOptions = DumboOptions.default(),
               data_opts: Optional[List[DumboOptions]]=None) -> Union[
    List[str], Dict, List[Dict]]:
    """Calls Dumbo for converting the given markdown to HTML.

    :param options: Options for Dumbo.
    :param data: The data to be converted.
    :param path: The path of the request. Valid paths are: '', '/', '/mdkeys' and '/markdown' (same as '/' and '').
     If path is '/mdkeys', data is expected to be a Dict or List[Dict]. Any dict value that begins with 'md:' is
     interpreted as Pandoc markdown and is converted to HTML. Otherwise the value is unchanged.
     The return value format will be the same as input.
     Otherwise, data is expected to be a List[str]. Each string is interpreted as Pandoc markdown and is converted to
     HTML. The return value format will be the same as input.

    """
    is_dict = isinstance(data, dict)
    opts = options.dict()
    try:
        if path in KEYS_PATHS:
            if is_dict:
                data_to_send = {'content': [{'content': data}], **opts}
            else:
                if data_opts:
                    data_to_send = {'content': [{'content': d, **o.dict()} for d, o in zip(data, data_opts)], **opts}
                else:
                    data_to_send = {'content': [{'content': d} for d in data], **opts}
        else:
            data_to_send = {'content': data, **opts}
        r = requests.post(url=DUMBO_URL + path, data=json.dumps(data_to_send, cls=TimJsonEncoder))
        r.encoding = 'utf-8'
    except requests.ConnectionError:
        raise Exception('Failed to connect to Dumbo')
    if r.status_code != 200:
        raise DumboHTMLException()
    returned = r.json()
    if is_dict:
        return returned[0]
    else:
        return returned
