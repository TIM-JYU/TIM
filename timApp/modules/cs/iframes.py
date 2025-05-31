""""
Handles iframe processing for TIM.
Contains dataclasses and functions for reading and converting iframe data.

Author: vesal
License: MIT
"""

import os
import yaml
from dataclasses import dataclass, field, fields, asdict
from typing import Optional, List
from tim_common.fileParams import get_param, QueryClass


def remove_none(d):
    """
    Recursively remove None values from a dictionary or list.
    :param d: dictionary or list to process
    :return: dictionary or list with None values removed
    """
    if isinstance(d, dict):
        return {k: remove_none(v) for k, v in d.items() if v is not None}
    elif isinstance(d, list):
        return [remove_none(i) for i in d]
    else:
        return d


"""
There is iframe types for YAML written in document 
and for server send to csplugin.
YAML can contain recursive iframes but that is
flattened to IFramesType.files list.
"""


@dataclass
class IFrameYAMLType:
    filename: Optional[str] = None
    width: Optional[int] = None
    height: Optional[int] = None
    remove: Optional[bool] = True
    id: Optional[str] = None
    ignoreError: Optional[bool] = None
    sandbox: Optional[str] = None
    style: Optional[str] = None
    readIframes: Optional[str] = None


@dataclass
class IFrameType:
    filename: Optional[str] = None
    content: str = ""
    width: Optional[int] = None
    height: Optional[int] = None
    id: Optional[str] = None
    style: Optional[str] = None
    sandbox: Optional[str] = None


@dataclass
class IFramesDefaults:
    width: Optional[int] = None
    height: Optional[int] = None
    style: Optional[str] = None
    sandbox: Optional[str] = None


@dataclass
class IFramesType:
    files: List[IFrameType] = field(default_factory=list)
    style: Optional[str] = None
    defaults: Optional[IFramesDefaults] = None


def dict_to_iframeyamltype(d: dict) -> IFrameYAMLType:
    """Convert a dictionary to IFrameYAMLType.
    This function filters the dictionary to only include valid fields
    that match the IFrameYAMLType dataclass.
    :param d: dictionary to convert
    :return: IFrameYAMLType object
    """
    valid_keys = {f.name for f in fields(IFrameYAMLType)}
    filtered = {k: v for k, v in d.items() if k in valid_keys}
    return IFrameYAMLType(**filtered)


def get_files_list(jsfiles: List[dict]) -> List[IFrameYAMLType]:
    """
    Convert a list of dictionaries to a list of IFrameYAMLType objects.
    This is used to convert YAML data to the internal IFrameYAMLType.
    :param jsfiles: list of dictionaries representing iframes
    :return: list of IFrameYAMLType objects
    """
    return [dict_to_iframeyamltype(f) for f in jsfiles if isinstance(f, dict)]


def convert_iframeyaml(iframe: IFrameYAMLType, content: str) -> IFrameType:
    """
    Convert IFrameYAMLType to IFrameType.
    This is used to convert YAML data to the internal IFrameType.
    :param content: file content
    :param iframe: IFrameYAMLType object
    :return: IFrameType object
    """
    return IFrameType(
        filename=iframe.filename,
        width=iframe.width,
        height=iframe.height,
        id=iframe.id,
        style=iframe.style,
        sandbox=iframe.sandbox,
        content=content,
    )


def handle_iframes(
    files: List[IFrameYAMLType],
    web_iframes: IFramesType,
    out: str,
    err: str,
    prgpath: str,
    sandbox: str = None,
    recurse: bool = False,
) -> tuple[str, str]:
    """
    Handle list of iframes.  If íframe has readIframes attribute,
    read the iframes dict from file and continue by that list
    :param files: list of iframes to use
    :param web_iframes: result variable for iframes. This is returned flat,
           so possible recursive iframe files are added to this list.
    :param out: output string until now
    :param err: error string until now
    :param prgpath: program path directory
    :param sandbox: if recursice call, use this sandbox
    :param recurse: if recursive call, do not read iframes dict from file
    :return: possibly modified out and err
    """
    for iframe in files:
        content = None
        filename = ""
        fn = ""
        # noinspection PyBroadException
        try:
            # check if we should read iframes-dict from file
            fn = iframe.readIframes
            if fn:
                if recurse:
                    continue  # do not read iframes recursively
                # read iframes from file
                filename = prgpath + "/" + fn
                with open(filename, encoding="utf-8") as f:
                    content = f.read()
                # content_iframes = json.loads(content)
                content_iframes = yaml.safe_load(content)
                sbox = iframe.sandbox
                if isinstance(content_iframes, list):
                    content_iframes = {"files": content_iframes}
                else:
                    defaults = content_iframes.get("defaults", {})
                    defaults.pop("sandbox", None)
                    if defaults:
                        if web_iframes.defaults is None:
                            web_iframes.defaults = {}
                        web_iframes.defaults |= defaults
                if "style" in content_iframes:
                    if web_iframes.style is None:
                        web_iframes.style = ""
                    web_iframes.style += "; " + content_iframes.get("style", "")
                out, err = handle_iframes(
                    get_files_list(content_iframes.get("files", [])),
                    web_iframes,
                    out,
                    err,
                    prgpath,
                    sbox,
                    True,
                )
                continue

            # if readIframes was not set, read filename from iframe
            fn = iframe.filename
            if fn:
                if fn == "stdout":  # if fn is stdout, then use out and clear out
                    content = out
                    out = ""
                else:  # otherwise read the file normal way
                    filename = prgpath + "/" + fn
                    with open(filename, encoding="utf-8") as f:
                        content = f.read()
        except Exception as ex:  # pylint: disable=broad-except
            if not iframe.ignoreError:
                err += "\nError reading iframe file: " + fn + " : " + str(ex) + "\n"
        if content:  # if content was read, then put it to iframe
            if iframe.remove:
                try:
                    os.remove(filename)
                except FileNotFoundError:
                    pass
            if recurse:
                # if sandbox when read iframes from file, use top level sandbox
                # because we do not allow program made iframes-dict to

                iframe.sandbox = None
                if sandbox is not None:
                    iframe.sandbox = sandbox
            web_iframes.files.append(convert_iframeyaml(iframe, content))
    return out, err


def check_iframes(
    query: QueryClass, web: dict, out: str, err: str, prgpath: str
) -> tuple[str, str]:
    """
    Handle iframes in the query.
    This function will read the iframe files and put them to web-object.
    If the iframes is not defined, it will return out and err unchanged.
    If the iframes is defined, and it is a dict, it will read the files-list
    and copy other attributes to web["iframes"] .
    If iframes is a list, it will read the files as they are to web["iframes"]["files"].
    :param query: all query parameters
    :param web: result
    :param out: output string until now
    :param err: error string until now
    :param prgpath: path to the program directory
    :return possibly modified out and err, for example
            out may change to empty string if the filename is stdout
            and err may get new error messages
    """
    iframes = get_param(query, "iframes", None)
    if not iframes:
        return out, err
    web_iframes: IFramesType = IFramesType(files=[])
    if isinstance(iframes, dict):
        # iframes is a dict, convert to list
        files = get_files_list(iframes.get("files", []))
        if val := iframes.get("defaults", None):
            web_iframes.defaults = val
        if val := iframes.get("style", None):
            web_iframes.style = val
    elif isinstance(iframes, list):  # iframes is a list
        files = get_files_list(iframes)
    else:  # iframes is not a list or dict, return
        return out, err

    out, err = handle_iframes(files, web_iframes, out, err, prgpath)
    web["iframes"] = remove_none(asdict(web_iframes))
    return out, err
