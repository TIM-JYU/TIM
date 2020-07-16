import os.path
from typing import Optional, List
from pathlib import Path
from fileParams import get_json_param, get_param, mkdirs
from marshmallow import EXCLUDE

from languages import Language
from file_util import *

def classinstancemethod(func):
    """Decorator for getting cls and self"""
    def wrapper(obj, *kargs, **kwargs):
        return func(type(obj), obj, *kargs, **kwargs)
    return wrapper


"""the class method create makes an encapsulated version of the class where
the class variables are bound to that version. The class method init is used to
initialize these variables"""
class FileSource:
    """Base class for file sources"""
    id = "_source"

    def __init__(self, file, source_path: str, language: Language):
        self.file = file
        self.language = language
        self.source_path = source_path

    def load(self) -> List[File]:
        raise NotImplementedError("Load not implemented")

    def copy(self, destination_path: str) -> None:
        raise NotImplementedError("Copy not implemented")

    @classmethod
    def init(cls, query, files) -> None:
        raise NotImplementedError("Init not implemented")

    @classmethod
    def create(cls, query, files):
        nclass = type(cls.__name__, (cls,), {})
        nclass.init(query, files)
        return nclass

    @classmethod
    def verify(cls, files):
        pass


class EditorSource(FileSource):
    id = "editor"

    @classinstancemethod
    def load(cls, self) -> List[File]:
        if self.paths is not None and self.file.path not in self.paths:
            raise PermissionError(f"File {self.file.path} not recognized as a submittable file")

        return [self.file]

    @classinstancemethod
    def copy(cls, self, destination_path: str) -> None:
        raise PermissionError("Editor files may not be copied")

    @classmethod
    def init(cls, query, files) -> None:

        cls.files = [file for file in files if file.source == "editor"]
        cls.paths = None
        cls.must_paths = [file.path for file in cls.files if not file.canClose and not file.canRename]

        allowed_paths = get_json_param(query.jso, "markup", "allowedPaths", None)
        if allowed_paths != "*":
            cls.paths = [file.path for file in cls.files]
            if allowed_paths is not None:
                cls.paths.extend(allowed_paths)

        cls.may_add_files = get_json_param(query.jso, "markup", "mayAddFiles", False)

    @classmethod
    def verify(cls, files):
        if cls.may_add_files:
            max_files = None
        else:
            max_files = len(cls.files)

        paths = [file.path for file in files if file.source == "editor"]
        if max_files is not None and len(paths) > max_files:
            raise PermissionError(f"Too many files submitted. Maximum is {max_files}")

        for path in cls.must_paths:
            if path not in paths:
                raise ValueError(f"Couldn't find file {path} in submitted files")


class UploadByCodeSource(FileSource):
    id = "uploadByCode"

    @classinstancemethod
    def load(cls, self) -> List[File]:
        if self.paths is not None and self.file.path not in self.paths:
            raise PermissionError(f"File {self.file.path} not recognized as a submittable file")

        return [self.file]

    @classinstancemethod
    def copy(cls, self, destination_path: str) -> None:
        raise PermissionError("UploadByCode files may not be copied")

    @classmethod
    def init(cls, query, files) -> None:
        cls.files = [file for file in files if file.source == "uploadByCode"]
        cls.paths = None
        cls.must_paths = [file.path for file in cls.files if not file.canClose and not file.canRename]

        allowed_paths = get_json_param(query.jso, "markup", "allowedPaths", None)
        if allowed_paths != "*":
            cls.paths = [file.path for file in cls.files]
            if allowed_paths is not None:
                cls.paths.extend(allowed_paths)

    @classmethod
    def verify(cls, files):
        paths = [file.path for file in files if file.source == "uploadByCode"]
        for path in cls.must_paths:
            if path not in paths:
                raise ValueError(f"Couldn't find file {path} in submitted files")


class UploadSource(FileSource):
    id = "upload"

    def __init__(self, file, source_path: str, language: Language):
        super().__init__(file, source_path, language)
        self.path = Path(source_path) if source_path is not None else None

        if not self.path.is_file():
            raise ValueError("Upload source file not found")

    @classinstancemethod
    def load(cls, self) -> List[File]:
        if self.file.path not in cls.paths:
            raise PermissionError(f"File {self.file.path} not recognized as a uploadable file")

        if cls.verified is None:
            raise ValueError("uploadedFiles not specified but upload source used")

        if self.source_path not in cls.verified:
            raise ValueError("Upload source file not in uploadedFiles")

        try:
            self.file.bcontent = self.path.read_bytes()
        except Exception as e:
            raise Exception(f"Failed to read uploaded file {self.file.path}: {e}")

        try:
            self.file.content = file.bcontent.decode(encoding="utf-8")
        except Exception as e:
            self.file.content = None

        return [self.file]

    def copy(self, destination_path: str) -> None:
        # we'll allow this so teachers can upload a file and include it in the task. Though, it is not easy
        dpath = Path(destination_path)
        if dpath.exists():
            rm(dpath)
        copy2(self.source_path, destination_path)

    @classmethod
    def init(cls, query, files) -> None:
        # Save names of uploaded files for access verification in load.
        # If a file isn't listed in uploadedFiles it hasn't gone through
        # view access checks in TIM
        uploaded_files = get_json_param(query.jso, "input", "uploadedFiles", None)
        if uploaded_files is not None:
            cls.verified = [file["path"] for file in uploaded_files]
        else:
            cls.verified = None

        cls.files = [file for file in files if file.source == "upload"]
        cls.paths = [file.paths if file.paths is not None else [file.path] for file in cls.files]
        cls.paths = [path for paths in cls.paths for path in paths] # flatten


class MasterSource(FileSource):
    id = "master"

    def __init__(self, file, source_path: str, language: Language):
        self.is_root = False
        if source_path.startswith("root:"):
            self.is_root = True
            source_path = source_path[5:]
        elif source_path.startswith("task:"):
            self.is_root = False
            source_path = source_path[5:]

        self.is_regex = False
        if source_path.startswith("r:"):
            self.is_regex = True
            source_path = source_path[2:]
        elif source_path.startswith("g:"):
            self.is_regex = False
            source_path = source_path[2:]

        if not is_relative_subpath(source_path):
            raise PermissionError(f"{source_path} directs outside the master directory")

        super().__init__(file, source_path, language)

    @classinstancemethod
    def load(cls, self) -> List[File]:
        raise PermissionError(f"Files from master path cannot be submitted")

    @classinstancemethod
    def copy(cls, self, destination_path: str) -> None:

        if self.is_root and self.language.rootpath is None:
            raise ValueError("Cannot copy from master task: cannot determine task path (root path not specified)")

        source_path = cls.master_path / ("" if self.is_root else Path(self.language.prgpath).relative_to(self.language.rootpath))

        if not source_path.exists():
            raise ValueError("Master source path not found")

        if self.is_regex:
            copy_files_regex(self.source_path, source_path, destination_path)
        else:
            copy_files_glob(self.source_path, source_path, destination_path)

    @classmethod
    def init(cls, query, files) -> None:
        cls.master_path = get_param(query, "masterPath", None)
        if cls.master_path is not None:
            cls.master_path = (Path("/cs/masters") / cls.master_path).resolve()


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

        uploaded_files = get_json_param(query.jso, "input", "uploadedFiles", None)
        if uploaded_files is not None:
            save["uploadedFiles"] = uploaded_files

        files = get_json_param(query.jso, "markup", "files", None)
        if files is None:
            files = [{"path": default_filename(query)}]
        files = FileSpecification.load(listify(files), many=True, unknown=EXCLUDE)

        self.sources = {cls.id: cls.create(query, files) for cls in FileSource.__subclasses__()}

        self.external_files = [file for file in files if file.source not in ["upload", "editor", "uploadByCode"]]

    def parse_source(self, source: str) -> (str, str, bool):
        parts = source.split(":", maxsplit=1)
        path = parts[1] if len(parts) == 2 else None

        return parts[0], path

    def parse_destination(self, path: str, prgpath: str, rootpath: str) -> str:
        is_root = False
        if path.startswith("root:"):
            path = path[len("root:"):]
            is_root = True
        elif path.startswith("task:"):
            path = path[len("task:"):]

        if is_root and rootpath is None:
            raise ValueError("Cannot copy to root: root path not specified")

        if path.endswith("/.") or path == ".":
            path = path[:-1]

        return os.path.join(rootpath if is_root else prgpath, path)

    def load_files(self, file: File):
        source, source_path = self.parse_source(file.source)

        try:
            return self.sources[source](file, source_path, None).load()
        except KeyError:
            raise ValueError(f"Source {source} (from {file.source}) not recognized")


    def get_files(self, s: str):
        submitted_files = get_json_param(self.query.jso, "input", "submittedFiles", [])

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

        submitted_files = [file for f in submitted_files for file in self.load_files(f)]

        external_files = [file for f in self.external_files for file in self.load_files(f)]
        external_files = [file for file in external_files if file.path not in used_paths]
        if len(external_files) != 0:
            self.save["externalFiles"] = external_files

        submitted_files = submitted_files + external_files

        for source in self.sources.values():
            source.verify(submitted_files)

        return submitted_files

    def copy_file(self, file: File, language: Language):
        """Copies/writes files instead of reading them to a string. Must only be
        used with trusted sources (i.e. students may not edit them)."""
        source, source_path = self.parse_source(file.source)
        destination_path = self.parse_destination(file.path, language.prgpath, language.rootpath)

        basepath = language.rootpath if language.rootpath is not None else language.prgpath
        if not is_parent_of(basepath, destination_path):
            raise PermissionError(f"{destination_path} directs outside the working directory")

        try:
            self.sources[source](file, source_path, language).copy(destination_path)
        except KeyError:
            raise ValueError(f"Source {source} (from {file.source}) not recognized")

    def save_file(self, file: File, language: Language):
        basepath = language.rootpath if language.rootpath is not None else language.prgpath
        if not is_parent_of(basepath, file.path):
            raise PermissionError(f"{file.path} directs outside the working directory")

        path = Path(file.path)

        mkdirs(path.parent)

        if path.exists():
            if path.is_dir():
                rmtree(path)
            else:
                path.unlink()

        if file.content is not None:
            path.write_text(file.content, encoding="utf-8")
        elif file.bcontent is not None:
            path.write_bytes(file.bcontent)
        else:
            raise ValueError(f"Couldn't save file {path.name} because its content is None")

        chown(path, user="agent", group="agent")

    def save_files(self, submitted_files, language):
        external_files = get_json_param(self.query.jso, "markup", "externalFiles", [])
        external_files = File.load(external_files, many=True, unknown=EXCLUDE)
        for file in external_files:
            self.copy_file(file, language)

        for file in submitted_files:
            self.save_file(file, language)
