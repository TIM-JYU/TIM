import os.path
from typing import Optional, List
from pathlib import Path
from tim_common.fileParams import get_json_param, get_param, mkdirs
from marshmallow import EXCLUDE
from shutil import chown, copy2

from git.gitlib import get_lib
from git.util import Settings as GitSettings, RemoteInfo

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

    def __init__(self, specification: FileSpecification, source_path: str, language: Language):
        self.specification = specification
        self.language = language
        self.source_path = source_path

    def matches(self, file: File) -> bool:
        raise NotImplementedError("Matches not implemented")

    def load(self, file: File) -> List[File]:
        raise NotImplementedError("Load not implemented")

    def copy(self, destination_path: str) -> None:
        raise NotImplementedError("Copy not implemented")

    @property
    def is_external(self) -> bool:
        return False

    @classmethod
    def init(cls, query, files) -> None:
        raise NotImplementedError("Init not implemented")

    @classmethod
    def create(cls, query, files):
        nclass = type(cls.__name__, (cls,), {})
        nclass.init(query, files)
        return nclass

    @staticmethod
    def source_classes(query, files):
        bases = [base for base in FileSource.__subclasses__() if base not in [ExternalFileSource]] + ExternalFileSource.__subclasses__()
        return {cls.id: cls.create(query, files) for cls in bases}

    @classmethod
    def verify(cls, files):
        pass


class ExternalFileSource(FileSource):
    """Base class for file sources"""
    id = "_esource"

    def load_external(self) -> List[File]:
        raise NotImplementedError("Load_external not implemented")

    def load(self, file: File) -> List[File]:
        return [file]

    @property
    def is_external(self) -> bool:
        return True


class EditorSource(FileSource):
    id = "editor"

    def matches(self, file: File) -> bool:
        return file.source == "editor" and file.path == self.specification.path

    @classinstancemethod
    def load(cls, self, file: File) -> List[File]:
        return [file]

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

    def matches(self, file: File) -> bool:
        if file.source != "uploadByCode":
            return False
        return file.path == self.specification.path or (self.specification.paths is not None and file.path in self.specification.paths)

    @classinstancemethod
    def load(cls, self, file: File) -> List[File]:
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

    def __init__(self, specification, source_path: str, language: Language):
        super().__init__(specification, source_path, language)

    def matches(self, file: File) -> bool:
        return file.source.startswith("upload") and (file.path == self.specification.path or file.path in self.specification.paths)

    @classinstancemethod
    def load(cls, self, file: File) -> List[File]:
        path = Path(file.source[len("upload:"):])

        if not path.is_file():
            raise ValueError("Upload source file not found")

        if cls.verified is None or str(path) not in cls.verified:
            raise PermissionError(f"File {file.path} didn't go through access verification")

        try:
            file.bcontent = path.read_bytes()
        except Exception as e:
            raise Exception(f"Failed to read uploaded file {file.path}: {e}")

        try:
            file.content = file.bcontent.decode(encoding="utf-8")
        except Exception as e:
            file.content = None

        return [file]

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


class MasterSource(FileSource):
    id = "master"

    def __init__(self, specification: FileSpecification, source_path: str, language: Language):
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

        super().__init__(specification, source_path, language)

    def matches(self, file: File) -> bool:
        return file.source == self.specification.source

    @classinstancemethod
    def load(cls, self, file: File) -> List[File]:
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


class GitSource(ExternalFileSource):
    id = "git"

    @classinstancemethod
    def __init__(cls, self, specification, source_path: str, language: Language):
        """source_path: <url>[;<sub_path>[<glob>;[;<branch>[;<remote>]]]]"""
        self.specification = specification
        self.language = language

        parts = source_path.split(";")
        info = RemoteInfo.parse_url(parts[0], cls.git_settings)
        self.sub_path = ""
        self.glob = ""
        if len(parts) > 1:
            self.sub_path = parts[1]
        if len(parts) > 2:
            self.glob = parts[2]
        if len(parts) > 3:
            info.branch = parts[3]
        if len(parts) > 4:
            info.name = parts[4]

        if not is_relative_subpath(self.sub_path):
            raise PermissionError(f"{self.sub_path} directs outside the git directory")

        self.lib = get_lib(info, cls.git_settings)

    def matches(self, file: File) -> bool:
        return file.source == self.specification.source

    @classinstancemethod
    def load_external(cls, self) -> List[File]:
        max_size = self.specification.maxSize
        max_total_size = self.specification.maxTotalSize

        files = self.lib.get_files(self.sub_path, self.glob, True)

        for f in files:
            f.source = self.specification.source

        if len(files) == 1 and files[0].path == "":
            files[0].path = self.specification.path
        else:
            for f in files:
                f.path = os.path.normpath(os.path.join(self.specification.path, f.path))

        if max_size is not None or max_total_size is not None:
            total = 0
            for f in files:
                if max_size is not None and f.size() > max_size:
                    raise ValueError(f"File {f.path} is too large (max {max_size})")
                total += f.size()
            if max_total_size is not None and total > max_total_size:
                raise ValueError(f"Files in {self.specification.path} are too large (max total {max_total_size})")

        return files

    @classinstancemethod
    def copy(cls, self, destination_path: str) -> None:
        return self.lib.copy_files(self.sub_path, destination_path, self.glob)

    @classmethod
    def init(cls, query, files) -> None:
        cls.git_settings = GitSettings.load(get_json_param(query.jso, "markup", "gitDefaults", {}))

class FileHandler:
    def __init__(self, query, save = None):
        self.query = query
        if save is not None:
            self.save = save
        else:
            self.save = {}
        self.uploaded_non_text = []

        self.master_path = get_param(query, "masterPath", None)
        if self.master_path is not None:
            self.master_path = (Path("/cs/masters") / self.master_path).resolve()
            if Path("/cs/masters") not in self.master_path.parents:
                raise ValueError("Root path points outside of allowed directories (masterPath must be a relative subpath)")

        uploaded_files = get_json_param(query.jso, "input", "uploadedFiles", None)
        if uploaded_files:
            save["uploadedFiles"] = uploaded_files

        files = get_json_param(query.jso, "markup", "files", None)
        if files is None:
            files = [{"path": default_filename(query)}]
        self.files = FileSpecification.load(listify(files), many=True, unknown=EXCLUDE)

        self.source_classes = FileSource.source_classes(query, self.files)
        self.sources = []
        for file in self.files:
            source, source_path = self.parse_source(file.source)

            try:
                source = self.source_classes[source]
            except KeyError:
                raise ValueError(f"Source {source} (from {file.source}) not recognized")

            self.sources.append(source(file, source_path, None))

        self.external_sources = [s for s in self.sources if s.is_external]

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

    def load_files(self, files: List[File]):
        """Loads files from markup sources and orders them in the same order as in markup"""
        mapping = []
        for source in self.sources:
            for file in files:
                if source.matches(file):
                    mapping.append((file, source))

        tmp = [file for file, _ in mapping]
        for file in files:
            if file not in tmp:
                raise PermissionError(f"File {file.path} was not recognized as a submittable file")

        return [f for file, source in mapping for f in source.load(file)]

    def get_external_files(self):
        external_files = [file for source in self.external_sources for file in source.load_external()]
        return File.dump(external_files, many=True, exclude=["bcontent"])

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

        submitted_files = self.load_files(submitted_files)

        file_mappings = {} # mapping of paths to matching files
        for file in submitted_files:
            tmp = file_mappings.setdefault(file.path, [])
            tmp.append(file)

        # if multiple files have the same path, take the first valid (non-empty) one
        files = []
        for file in submitted_files:
            if file_mappings[file.path][0] == file:
                if len(file_mappings[file.path]) > 1 and len(file.content) == 0:
                    file_mappings[file.path].pop(0)
                else:
                    files.append(file)

        for source in self.source_classes.values():
            source.verify(files)

        return files

    def copy_file(self, file: File, language: Language):
        """Copies/writes files instead of reading them to a string. Must only be
        used with trusted sources (i.e. students may not edit them)."""
        source, source_path = self.parse_source(file.source)
        destination_path = self.parse_destination(file.path, language.prgpath, language.rootpath)

        basepath = language.rootpath if language.rootpath is not None else language.prgpath
        if not is_parent_of(basepath, destination_path):
            raise PermissionError(f"{destination_path} directs outside the working directory")

        try:
            self.source_classes[source](file, source_path, language).copy(destination_path)
        except KeyError:
            raise ValueError(f"Source {source} (from {file.source}) not recognized")

    def save_file(self, file: File):
        path = Path(file.path)
        if file.content is not None:
            path.write_text(file.content, encoding="utf-8")
        elif file.bcontent is not None:
            path.write_bytes(file.bcontent)
        else:
            raise ValueError(f"Couldn't save file {path.name} because its content is None")

        chown(path, user="agent", group="agent")

    def save_files(self, submitted_files, language):
        basepath = language.rootpath if language.rootpath is not None else language.prgpath

        delete_files = get_json_param(self.query.jso, "markup", "deleteFiles", [])
        for path in delete_files:
            ppath = Path(self.parse_destination(path, language.prgpath, language.rootpath))
            if not is_parent_of(basepath, ppath):
                raise PermissionError(f"{path} (from deleteFiles) directs outside the working directory")
            if ppath.exists():
                rm(ppath)

        for file in submitted_files:
            if not is_parent_of(basepath, file.path):
                raise PermissionError(f"{file.path} directs outside the working directory")

            path = Path(file.path)
            mkdirs(path.parent)
            if path.exists():
                rm(path)

        for file in submitted_files:
            self.save_file(file)

        extra_files = get_json_param(self.query.jso, "markup", "moreFiles", [])
        extra_files = FileSpecification.load(extra_files, many=True, unknown=EXCLUDE)

        for file in extra_files:
            self.copy_file(file, language)
