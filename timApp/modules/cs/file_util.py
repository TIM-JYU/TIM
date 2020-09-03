import re
import os
from typing import Optional, List
from pathlib import Path
from fileParams import get_json_param, get_param, mkdirs
from marshmallow_dataclass import dataclass, NewType
from marshmallow import fields
from dataclasses import field
from shutil import rmtree, copy2, chown, copytree

from loadable import Loadable


def listify(item):
    return item if isinstance(item, list) else [item]


def default_filename(query):
    filename = get_param(query, "filename", None)
    if filename is None:
        try:
            tid = query.jso.get("taskID", "prg")
        except AttributeError:
            pass
        else:
            tid = tid.split(".", 1)[-1]
            asciified = re.sub(r"[^A-Za-z0-9_]", "", tid)
            # taskID variable may end with a dot (when the plugin doesn't have a task id),
            # so need to ensure asciified is not empty. Can also happen if task id has only non-ascii chars.
            if asciified:
                filename = asciified
    if filename is None:
        filename = "default"
    return filename


class Listify(fields.List):
    def _deserialize(self, value, attr, data, **kwargs):
        if value is not None:
            value = listify(value)

        return super()._deserialize(value, attr, data, **kwargs)


ListifiedStr = NewType("Listify", List[str], field=Listify, cls_or_instance=fields.String())


@dataclass
class FileSpecification(Loadable):
    path: Optional[str] = field(default=None)
    source: str = field(default="editor")
    paths: Optional[ListifiedStr] = field(default=None)
    canClose: Optional[bool] = field(default=False)
    canRename: Optional[bool] = field(default=False)
    maxSize: Optional[int] = field(default=None)
    maxTotalSize: Optional[int] = field(default=None)


@dataclass
class File(Loadable):
    path: str
    source: str = field(default="editor")
    content: Optional[str] = field(default=None)
    bcontent: Optional[bytes] = field(default=None)
    fileext: str = field(default="")
    filedext: str = field(default="")

    def size(self):
        return len(self.content or self.bcontent or [])

    @staticmethod
    def default(query, content: str = ""):
        return File(default_filename(query), "editor", content)


def rm(path: Path):
    if path.is_dir():
        rmtree(str(path))
    else:
        path.unlink()


def copy_files_regex(f: str, source: str, dest: str):
    if f is None or source is None or dest is None:
        return

    dest = Path(dest)

    regex = re.compile(f)

    for root, _, files in os.walk(source):
        rootp = Path(root)
        relpath = rootp.relative_to(source)
        for file in files:
            relfilepath = relpath / file
            if regex.fullmatch(str(relfilepath)) is not None:
                if dest.is_file():
                    dest.unlink()
                mkdirs(str(dest))
                filepath = dest / file
                copy2(str(rootp / file), str(filepath))
                chown(filepath, user="agent", group="agent")
                for f in filepath.parents:
                    chown(f, user="agent", group="agent")


def copy_files_glob(glob: str, source: str, dest: str):
    if glob is None or source is None or dest is None:
        return

    path = Path(source)
    if not path.exists():
        raise FileNotFoundError(f"{path} does not exist")
    elif not path.is_dir():
        raise NotADirectoryError(f"{path} is not directory")

    if dest.endswith("/"):
        dest_dir = True
        dest = dest[:-1]
        basepath = Path(dest)
    else:
        dest_dir = False
        basepath = Path(dest).parent

    if basepath.exists() and not basepath.is_dir():
        rm(basepath)

    mkdirs(basepath)

    if glob == "":
        matches = [Path(source)]
    else:
        matches = list(Path(source).glob(glob))

    if len(matches) == 1 and matches[0].is_dir():
        m = matches[0]
        destination = dest
        if dest_dir:
            destination = str(dest / Path(source).relative_to(m))

        copytree(str(m), destination, dirs_exist_ok=True)
        chown(destination, user="agent", group="agent")
        for root, dirs, files in os.walk(destination):
            for d in dirs:
                chown(os.path.join(root, d), user="agent", group="agent")
            for f in files:
                chown(os.path.join(root, f), user="agent", group="agent")
        return
    elif len(matches) > 1 and not dest_dir:
        raise Exception("Cannot copy multiple files to a single filename")

    if dest_dir:
        for m in matches:
            destination = dest / m.relative_to(source)
            if m.is_dir():
                if destination.exists() and not destination.is_dir():
                    rm(destination)
                mkdirs(destination)
            elif m.is_file():
                if destination.exists():
                    rm(destination)
                copy2(str(m), str(destination))
            else:
                raise FileExistsError(f"{str(m)} is of unknown type (not a file or directory)")
            chown(destination, user="agent", group="agent")
            for f in m.relative_to(source).parents:
                chown(dest / f, user="agent", group="agent")
    else:
        copy2(str(matches[0]), dest)
        chown(dest, user="agent", group="agent")


def is_parent_of(parent: str, child: str):
    """Returns whether a child is a subpath of parent"""
    parent = os.path.normpath(parent)
    child = os.path.normpath(child)
    return child.startswith(parent) or os.path.abspath(child).startswith(os.path.abspath(parent))


def is_relative_subpath(path: str):
    """Returns whether a path is a relative subpath"""
    path = os.path.normpath(path)
    return not path.startswith("/") and not path.startswith("..")
