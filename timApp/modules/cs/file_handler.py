import re
from typing import Optional, List
from pathlib import Path
from fileParams import get_json_param, get_param
from dataclasses import field
from marshmallow_dataclass import dataclass
from marshmallow import EXCLUDE

from loadable import Loadable

@dataclass
class File(Loadable):
    path: Optional[str]
    source: str = field(default="editor")
    paths: Optional[List[str]] = field(default=None)
    content: Optional[str] = field(default=None)
    bcontent: Optional[bytes] = field(default=None)
    canClose: Optional[bool] = field(default=False)
    canRename: Optional[bool] = field(default=False)

class FileHandler:
    def __init__(self, query, save):
        self.query = query
        self.save = save
        self.uploaded_non_text = []

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
            files = [{"path": self.default_filename()}]
        if not isinstance(files, list):
            files = [files]
        files = File.load(files, many=True, unknown=EXCLUDE)

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

    def default_filename(self):
        filename = get_param(self.query, "filename", None)
        if filename is None:
            try:
                tid = self.query.jso.get("taskID", "prg")
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

    def load_uploaded(self, file):
        if self.uploaded_files is None:
            raise ValueError("uploadedFiles not specified but upload source used")

        path = file.path
        if path not in self.uploaded_files:
            raise ValueError("Upload source file not in uploadedFiles")

        path = Path(path)
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

    def load_file(self, file):
        source = file.source
        path = file.path

        if source == "editor":
            if path not in self.editor_paths:
                raise ValueError(f"File {path} not recognized as a submittable file")
            return file

        elif source == "uploadByCode":
            if path not in self.uploadbycode_paths:
                raise ValueError(f"File {path} not recognized as a submittable file")
            return file

        elif source.startswith("upload:"):
            if path not in self.upload_paths:
                raise ValueError(f"File {path} not recognized as a uploadable file")
            return self.load_uploaded(self.query, source.replace("upload:", "", 1))

        # TODO: add master:
        else: # TODO: add more source types e.g. git
            raise ValueError(f"Source {source} not recognized")

    def get_files(self, s: str):
        submitted_files = get_json_param(self.query.jso, "input", "submittedFiles", None)
        if submitted_files is not None:
            self.save["submittedFiles"] = submitted_files
            for file in submitted_files:
                if file["path"] == "":
                    file["path"] = self.default_filename()
        else:
            submitted_files = [{"path": self.default_filename(), "content": s}]

            usercode = get_json_param(self.query.jso, "input", "usercode", None)
            if isinstance(usercode, str):
                self.save["usercode"] = usercode

        submitted_files = File.load(submitted_files, many=True, unknown=EXCLUDE)

        editor_paths = [file.path for file in submitted_files if file.source == "editor"]
        if self.max_editor_files is not None and len(editor_paths) > self.max_editor_files:
            raise ValueError(f"Too many files submitted. Maximum is {self.max_editor_files}")
        for path in self.editor_must_paths:
            if path not in editor_paths:
                raise ValueError(f"Couldn't find file {path} in submitted files")

        uploadbycode_paths = [file.path for file in submitted_files if file.source == "uploadByCode"]
        for path in self.uploadbycode_must_paths:
            if path not in uploadbycode_paths:
                raise ValueError(f"Couldn't find file {path} in submitted files")

        submitted_files = [self.load_file(file) for file in submitted_files]

        return File.dump(submitted_files, many=True)
