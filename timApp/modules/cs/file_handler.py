import re
import os
from typing import Optional, List
from pathlib import Path
from fileParams import get_json_param, get_param, mkdirs
from dataclasses import field
from marshmallow_dataclass import dataclass
from marshmallow import EXCLUDE
from shutil import rmtree, copy2, chown, copytree

from loadable import Loadable


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


@dataclass
class FileSpecification(Loadable):
    path: Optional[str]
    source: str = field(default="editor")
    paths: Optional[List[str]] = field(default=None)
    canClose: Optional[bool] = field(default=False)
    canRename: Optional[bool] = field(default=False)


@dataclass
class File(Loadable):
    path: str
    source: str = field(default="editor")
    content: Optional[str] = field(default=None)
    bcontent: Optional[bytes] = field(default=None)
    fileext: str = field(default="")
    filedext: str = field(default="")

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

    regex = re.compile(f)

    for root, dirs, files in os.walk(source):
        rootp = Path(root)
        relpath = rootp.relative_to(source)
        for file in files:
            relfilepath = relpath / file
            if regex.fullmatch(str(relfilepath)) is not None:
                if dest.is_file():
                    dest.unlink()
                mkdirs(str(dest))
                copy2(str(rootp / file), str(dest / file))


def copy_files(source: str, dest: str):
    if source is None or dest is None:
        return

    only_subfiles = False
    if source.endswith("/*"):
        only_subfiles = True
        source = source[:-1]

    path = Path(source)
    if not path.exists():
        raise FileNotFoundError(f"{path} does not exist")
    elif only_subfiles and not path.is_dir():
        raise NotADirectoryError(f"{path} is not directory")
    elif not path.is_dir() and not path.is_file():
        raise FileExistsError(f"{path} is of unknown type (not a file or directory)")


    if dest.endswith("/") or only_subfiles:
        dest_dir = True
        dest = dest[:-1]
        basepath = Path(dest)
    else:
        dest_dir = False
        basepath = Path(dest).parent

    if basepath.exists() and not basepath.is_dir():
        rm(basepath)

    mkdirs(basepath)

    destpath = Path(dest)
    if path.is_file():
        if dest_dir:
            destpath = destpath / os.path.basename(source)
        if destpath.exists():
            rm(destpath)

        copy2(source, dest)
    else:
        if destpath.exists() and not destpath.is_dir():
            rm(destpath)

        if only_subfiles:
            _, _, files = os.walk(source)
            for file in files:
                destp = destpath / file
                if destp.exists():
                    rm(destpath)
                copy2(os.path.join(source, file), str(destp))
        else:
            if dest.endswith("/"):
                copytree(source, os.path.join(dest, os.path.basename(source)), dirs_exist_ok=True)
            else:
                copytree(source, dest, dirs_exist_ok=True)


class FileHandler:
    def __init__(self, query, save):
        self.query = query
        self.save = save
        self.uploaded_non_text = []

        self.master_path = get_param(query, "masterPath", None)
        if self.master_path is not None:
            self.master_path = (Path("/cs/masters") / self.master_path).resolve()
            if Path("/cs/masters") not in self.master_path.parents:
                raise ValueError("Root path points outside of allowed directories (masterPath must be a relative subpath)")

        # Save names of uploaded files for access verification in load_uploaded.
        # If a file isn't listed in uploadedFiles it hasn't gone through
        # view access checks in TIM
        uploaded_files = get_json_param(self.query.jso, "input", "uploadedFiles", None)
        if uploaded_files is not None:
            save["uploadedFiles"] = uploaded_files
            self.uploaded_files = [file.path for file in uploaded_files]
        else:
            self.uploaded_files = None

        files = get_json_param(self.query.jso, "markup", "files", None)
        if files is None:
            files = [{"path": default_filename(self.query)}]
        if not isinstance(files, list):
            files = [files]
        files = FileSpecification.load(files, many=True, unknown=EXCLUDE)

        self.editor_files = [file for file in files if file.source == "editor"]
        self.editor_paths = None
        self.editor_must_paths = [file.path for file in self.editor_files if not file.canClose and not file.canRename]

        self.upload_files = [file.path for file in files if file.source == "upload"]
        self.upload_paths = [file.paths if isinstance(file["paths"], list) else [file.path] for file in self.upload_files]
        self.upload_paths = [path for paths in self.upload_paths for path in paths] # flatten

        self.uploadbycode_files = [file for file in files if file.source == "uploadByCode"]
        self.uploadbycode_paths = None
        self.uploadbycode_must_paths = [file.path for file in self.uploadbycode_files if not file.canClose and not file.canRename]

        allowed_paths = get_json_param(self.query.jso, "markup", "allowedPaths", None)
        if allowed_paths != "*":
            self.editor_paths = [file.path for file in self.editor_files]
            if allowed_paths is not None:
                self.editor_paths.extend(allowed_paths)

            self.uploadbycode_paths = [file.path for file in self.uploadbycode_files]
            if allowed_paths is not None:
                self.uploadbycode_paths.extend(allowed_paths)

        may_add_files = get_json_param(self.query.jso, "markup", "mayAddFiles", False)
        if may_add_files:
            self.max_editor_files = None
        else:
            self.max_editor_files = len(self.editor_files)

        self.external_files = [file for file in files if file.source not in ["upload", "editor", "uploadByCode"]]

    def parse_source(self, source: str) -> (str, str, bool):
        is_root = False
        parts = source.split(":", maxsplit=2)
        source = parts[0]
        path = None
        if len(parts) == 3 and (parts[1] == "root" or parts[1] == "task"):
            is_root = parts[1] == "root"
            path = parts[2]
        elif len(parts) < 2:
            path = None
        else:
            path = ":".join(parts[1:])

        return source, path, is_root

    def parse_destination(self, path: str, prgpath: str, rootpath: str) -> (str, bool):
        is_root = False
        if path.startswith("root:"):
            path = path[len("root:"):]
            is_root = True
        elif path.startswith("task:"):
            path = path[len("task:"):]

        if is_root and rootpath is None:
            raise ValueError("Cannot copy to root: root path not specified")

        return os.path.join((rootpath if is_root else prgpath), path), is_root

    def load_uploaded(self, file: File, source_path: str):
        if self.uploaded_files is None:
            raise ValueError("uploadedFiles not specified but upload source used")

        if source_path not in self.uploaded_files:
            raise ValueError("Upload source file not in uploadedFiles")

        path = Path(source_path)
        if not path.is_file():
            raise ValueError("Upload source file not found")

        try:
            file.bcontent = path.read_bytes()
        except Exception as e:
            raise Exception(f"Failed to read uploaded file {file.path}: {e}")

        try:
            file.content = file.bcontent.decode(encoding="utf-8")
        except Exception as e:
            file.content = None

    def load_file(self, file: File):
        source = file.source
        path = file.path

        source, source_path, is_root_source = self.parse_source(source)
        if is_root_source: # due to Language handling the destination paths
            raise ValueError("Submittable files cannot come from root path")

        if source == "editor":
            if path not in self.editor_paths:
                raise PermissionError(f"File {path} not recognized as a submittable file")
            return file

        elif source == "uploadByCode":
            if path not in self.uploadbycode_paths:
                raise PermissionError(f"File {path} not recognized as a submittable file")
            return file

        elif source == "upload":
            if path not in self.upload_paths:
                raise PermissionError(f"File {path} not recognized as a uploadable file")
            return self.load_uploaded(file, source_path)

        elif source == "master":
            raise PermissionError(f"Files from master path cannot be submitted")
        else: # TODO: add more source types e.g. http
            raise ValueError(f"Source {source} not recognized")

    def copy_master(self, path: str, source_path: str, destination_path: str):
        if path.startswith("r:"):
            path = path[2:]
            copy_files_regex(path, source_path, destination_path)
        else:
            copy_files(os.path.join(source_path, path), destination_path)

    def copy_file(self, file: File, prgpath: str, rootpath: str):
        """Copies/writes files instead of reading them to a string. Must only be
        used with trusted sources (i.e. students may not edit)."""

        source, source_path, is_root_source = self.parse_source(file.source)
        if is_root_source and rootpath is None:
            raise ValueError("Cannot copy from master task: cannot determine task path (root path not specified)")

        destination_path, _ = self.parse_destination(file.path, prgpath, rootpath)

        if source == "editor":
            raise PermissionError("Editor files may not be copied")
        elif source == "uploadByCode":
            raise PermissionError("UploadByCode files may not be copied")
        # we'll allow this so teachers can upload a file and include it in the task. Though, it is not easy
        elif source == "upload":
            return self.copy_uploaded(file, source_path, destination_path)
        elif source == "master":
            master_path = self.master_path / ("" if is_root_source else Path(prgpath).relative_to(rootpath))
            return self.copy_master(source_path, master_path, destination_path)
        else: # TODO: add more source types e.g. git
            raise ValueError(f"Source {file.source} not recognized")

    def get_files(self, s: str):
        submitted_files = get_json_param(self.query.jso, "input", "submittedFiles", [])
        external_files = [self.load_file(file) for file in self.external_files]
        submitted_files = submitted_files + external_files
        if len(submitted_files) != 0:
            self.save["submittedFiles"] = submitted_files
            for file in submitted_files:
                if file["path"] == "":
                    file["path"] = default_filename(self.query)

            submitted_files = File.load(submitted_files, many=True, unknown=EXCLUDE)
        else:
            submitted_files = [File.default(self.query, s)]

            usercode = get_json_param(self.query.jso, "input", "usercode", None)
            if isinstance(usercode, str):
                self.save["usercode"] = usercode


        editor_paths = [file.path for file in submitted_files if file.source == "editor"]
        if self.max_editor_files is not None and len(editor_paths) > self.max_editor_files:
            raise PermissionError(f"Too many files submitted. Maximum is {self.max_editor_files}")
        for path in self.editor_must_paths:
            if path not in editor_paths:
                raise ValueError(f"Couldn't find file {path} in submitted files")

        uploadbycode_paths = [file.path for file in submitted_files if file.source == "uploadByCode"]
        for path in self.uploadbycode_must_paths:
            if path not in uploadbycode_paths:
                raise ValueError(f"Couldn't find file {path} in submitted files")

        submitted_files = [self.load_file(file) for file in submitted_files]

        return submitted_files

    def save_file(self, file: File):
        path = Path(file.path)

        mkdirs(path.parent)

        if path.exists():
            if path.is_dir():
                rmtree(path) # TODO: is this safe?
            else:
                path.unlink()

        if file.content is not None:
            path.write_text(file.content, encoding="utf-8")
        elif file.bcontent is not None:
            path.write_bytes(file.bcontent)
        else:
            raise ValueError(f"Couldn't save file {path.name} because its content is None")

        chown(path, user="agent", group="agent")

    def save_files(self, submitted_files, prgpath, rootpath):
        external_files = get_json_param(self.query.jso, "markup", "externalFiles", [])
        external_files = File.load(external_files, many=True, unknown=EXCLUDE)
        for file in external_files:
            self.copy_file(file, prgpath, rootpath)

        for file in submitted_files:
            self.save_file(file)
