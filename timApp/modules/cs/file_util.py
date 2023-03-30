import os
import re
from dataclasses import field
from pathlib import Path
from shutil import rmtree, copy2, chown, copytree
from typing import NewType

from marshmallow import fields

from cs_logging import log_warning
from loadable import Loadable
from tim_common.fileParams import get_param, mkdirs
from tim_common.marshmallow_dataclass import dataclass


def listify(item):
    return item if isinstance(item, list) else [item]


DENY_PATH_LIST = ["/logs", "/cs_data", "/var/run/docker.sock"]


def is_safe_path(path: str | Path):
    """Returns whether a path is safe to use"""
    p_abs = Path(path).resolve().as_posix()
    if any(p_abs.startswith(d) for d in DENY_PATH_LIST):
        return False
    return True


def write_safe(
    path: str,
    content: str,
    write_type: str = "w",
    others_permissions: str | None = None,
) -> bool:
    """
    Write contents to a file but only if the absolute path is not in DENY_PATH_LIST.

    :param path: Path to write the file to
    :param content: Contents to write
    :param write_type: File open mode.
    :param others_permissions: Permissions of non-root users.
    :return: True, if write was successful
    """
    p = Path(path)
    if not is_safe_path(p):
        return False
    encoding = "utf-8" if "b" not in write_type else None
    with p.open(write_type, encoding=encoding) as f:
        f.write(content)
    if others_permissions is not None:
        # Convert rwx permissions to octal
        other_perms = 0
        if "x" in others_permissions:
            other_perms |= 1
        if "w" in others_permissions:
            other_perms |= 2
        if "r" in others_permissions:
            other_perms |= 4
        current_perms = os.stat(p).st_mode
        new_perms = (current_perms & (~0o7)) | other_perms
        os.chmod(p, new_perms)
    return True


nonascii_pat = re.compile(r"[^A-Za-z0-9_]")


def default_filename(query):
    filename = get_param(query, "filename", None)
    if filename is None:
        try:
            tid = query.jso.get("taskID", "prg")
        except AttributeError:
            pass
        else:
            tid = tid.split(".", 1)[-1]
            asciified = re.sub(nonascii_pat, "", tid)
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


ListifiedStr = NewType("Listify", list[str])
ListifiedStr._marshmallow_field = Listify  # type: ignore
ListifiedStr._marshmallow_args = dict(cls_or_instance=fields.String())  # type: ignore


@dataclass
class FileSpecification(Loadable):
    path: str | None = field(default=None)
    source: str = field(default="editor")
    paths: ListifiedStr | None = field(default=None)
    canClose: bool | None = field(default=False)
    canRename: bool | None = field(default=False)
    maxSize: int | None = field(default=None)
    maxTotalSize: int | None = field(default=None)


@dataclass
class File(Loadable):
    path: str
    source: str = field(default="editor")
    content: str | None = field(default=None)
    bcontent: bytes | None = field(default=None)
    fileext: str = field(default="")
    filedext: str = field(default="")

    def size(self):
        return len(self.content or self.bcontent or [])

    @staticmethod
    def default(query, content: str = ""):
        return File(default_filename(query), "editor", content)


def rm(path: str | Path):
    if isinstance(path, str):
        path = Path(path)
    if not is_safe_path(path):
        log_warning(f"rm: refusing to remove unsafe path: {path}")
        return
    if path.is_dir():
        rmtree(str(path))
    else:
        path.unlink()


def rm_safe(path: str | Path):
    try:
        rm(path)
    except:
        pass


def copy_files_regex(f: str, source: str, dest: str):
    if f is None or source is None or dest is None:
        return 0
    if not is_safe_path(dest):
        return 0

    dest = Path(dest)

    regex = re.compile(f)

    count = 0
    for root, _, files in os.walk(source):
        rootp = Path(root)
        relpath = rootp.relative_to(source)
        for file in files:
            relfilepath = relpath / file
            if regex.fullmatch(str(relfilepath)) is not None:
                count += 1
                if dest.is_file():
                    dest.unlink()
                mkdirs(str(dest))
                filepath = dest / file
                copy2(str(rootp / file), str(filepath))
                chown(filepath, user="agent", group="agent")
                for f in filepath.parents:
                    chown(f, user="agent", group="agent")

    return count


def copy_files_glob(glob: str, source: str, dest: str):
    if glob is None or source is None or dest is None:
        return 0
    if not is_safe_path(dest):
        return 0

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
    elif len(matches) > 1 and not dest_dir:
        raise Exception("Cannot copy multiple files to a single filename")
    elif dest_dir:
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
                raise FileExistsError(
                    f"{str(m)} is of unknown type (not a file or directory)"
                )
            chown(destination, user="agent", group="agent")
            for f in m.relative_to(source).parents:
                chown(dest / f, user="agent", group="agent")
    elif len(matches) > 0:
        copy2(str(matches[0]), dest)
        chown(dest, user="agent", group="agent")

    return len(matches)


def is_parent_of(parent: str, child: str):
    """Returns whether a child is a subpath of parent"""
    parent = os.path.normpath(parent)
    child = os.path.normpath(child)
    return child.startswith(parent) or os.path.abspath(child).startswith(
        os.path.abspath(parent)
    )


def is_relative_subpath(path: str):
    """Returns whether a path is a relative subpath"""
    path = os.path.normpath(path)
    return not path.startswith("/") and not path.startswith("..")
