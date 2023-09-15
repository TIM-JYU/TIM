import json
import shutil
from pathlib import Path
from typing import Optional, Any

from timApp.auth.accesshelper import can_see_par_source
from timApp.document.docparagraph import DocParagraph, is_real_id
from timApp.document.document import Document
from timApp.document.documentparser import DocumentParser
from timApp.document.documentwriter import DocumentWriter
from timApp.document.randutils import random_id
from timApp.timdb.dbaccess import get_files_path
from timApp.timdb.exceptions import TimDbException
from timApp.user.user import User


class Clipboard:
    def get_path(self) -> Path:
        return get_files_path() / "clipboard"

    def get(self, user: User):
        return Clipboard.UserClipboard(self, user)

    def clear_all(self):
        path = self.get_path()
        if path.exists():
            shutil.rmtree(path)

    class UserClipboard:
        def __init__(self, parent: "Clipboard", user: User):
            self.user = user
            self.path = parent.get_path() / str(self.user.id)

        def get_metafilename(self) -> Path:
            return self.path / "metadata"

        def get_clipfilename(self) -> Path:
            return self.path / "content"

        def get_reffilename(self) -> Path:
            return self.path / "ref-content"

        def get_parreffilename(self) -> Path:
            return self.path / "ref-parcontent"

        def clear(self):
            for name in (
                self.get_clipfilename(),
                self.get_reffilename(),
                self.get_parreffilename(),
                self.get_metafilename(),
            ):
                if name.is_file():
                    name.unlink()

        def clear_refs(self):
            for name in (self.get_reffilename(), self.get_parreffilename()):
                if name.is_file():
                    name.unlink()

        def read_metadata(self) -> dict[str, Any]:
            try:
                with self.get_metafilename().open("rt", encoding="utf-8") as metafile:
                    metadata = json.loads(metafile.read())
                metadata["empty"] = False
                return metadata
            except FileNotFoundError:
                return {"empty": True}

        def read(
            self, as_ref: bool | None = False, force_parrefs: bool | None = False
        ) -> list[dict[str, str]] | None:
            if as_ref:
                clipfilename = (
                    self.get_parreffilename()
                    if force_parrefs
                    else self.get_reffilename()
                )
            else:
                clipfilename = self.get_clipfilename()

            if not clipfilename.is_file():
                return None
            with clipfilename.open("rt", encoding="utf-8") as clipfile:
                content = clipfile.read()
            dp = DocumentParser(content)
            dp.validate_structure().raise_if_has_critical_issues()
            return dp.get_blocks()

        def write_metadata(self, **kwargs):
            self.path.mkdir(exist_ok=True, parents=True)
            with self.get_metafilename().open("wt", encoding="utf-8") as metafile:
                metafile.write(json.dumps(kwargs))

        def update_metadata(self, **kwargs):
            metadata = self.read_metadata()
            metadata.update(kwargs)
            self.write_metadata(**metadata)

        def write(self, pars: list[dict[str, Any]]):
            self.path.mkdir(exist_ok=True, parents=True)
            text = DocumentWriter(pars).get_text()
            with self.get_clipfilename().open("wt", encoding="utf-8") as clipfile:
                clipfile.write(text)

        def write_refs(self, pars: list[DocParagraph], area_name: str | None):
            self.path.mkdir(exist_ok=True, parents=True)
            ref_pars = [p.create_reference(p.doc).dict() for p in pars]
            reftext = DocumentWriter(ref_pars).get_text()
            with self.get_parreffilename().open("wt", encoding="utf-8") as reffile:
                reffile.write(reftext)

            if area_name and len(pars) > 0:
                self.path.mkdir(exist_ok=True, parents=True)
                ref_pars = [
                    DocParagraph.create_area_reference(pars[0].doc, area_name).dict()
                ]
                reftext = DocumentWriter(ref_pars).get_text()
                with self.get_reffilename().open("wt", encoding="utf-8") as reffile:
                    reffile.write(reftext)
            else:
                shutil.copy(self.get_parreffilename(), self.get_reffilename())

        def cut_pars(
            self,
            doc: Document,
            par_start: str,
            par_end: str,
            area_name: str | None = None,
        ) -> list[DocParagraph]:
            pars = self.copy_pars(doc, par_start, par_end, area_name, disable_ref=True)
            doc.delete_section(par_start, par_end)
            self.update_metadata(last_action="cut")
            return pars

        def copy_pars(
            self,
            doc: Document,
            par_start: str,
            par_end: str,
            area_name: str | None = None,
            disable_ref: bool = False,
        ) -> list[DocParagraph]:
            par_objs = doc.get_section(par_start, par_end)
            pars = [p.dict() for p in par_objs]
            cannot_see_source = any(
                not can_see_par_source(self.user, p) for p in par_objs
            )

            self.write_metadata(
                area_name=area_name,
                disable_ref=disable_ref,
                disable_content=cannot_see_source,
                last_action="copy",
            )
            self.write(pars)
            self.write_refs(par_objs, area_name)

            return par_objs

        def paste_before(
            self, doc: Document, par_id: str, as_ref: bool = False
        ) -> list[DocParagraph]:
            pars = self.read(as_ref)
            if pars is None:
                raise TimDbException("There is nothing to paste.")

            metadata = self.read_metadata()
            if (
                not as_ref
                and metadata.get("area_name") is not None
                and doc.named_section_exists(metadata["area_name"])
            ):
                new_area_name = metadata["area_name"] + "_" + random_id()
                pars[0]["attrs"]["area"] = new_area_name
                pars[len(pars) - 1]["attrs"]["area_end"] = new_area_name

            doc_pars = []
            par_before = par_id
            if is_real_id(par_before):
                doc.raise_if_not_exist(par_before)
            for par in reversed(pars):
                # We need to reverse the sequence because we're inserting before, not after
                new_par_id = (
                    par["id"] if not doc.has_paragraph(par["id"]) else random_id()
                )
                new_par = doc.insert_paragraph(
                    par["md"],
                    insert_before_id=par_before,
                    par_id=new_par_id,
                    attrs=par.get("attrs"),
                )
                doc_pars.append(new_par)
                par_before = new_par.get_id()

            self.update_metadata(last_action="paste")
            doc_pars.reverse()
            return doc_pars

        def paste_after(
            self, doc: Document, par_id: str, as_ref: bool = False
        ) -> list[DocParagraph]:
            par_before = None

            if is_real_id(par_id):
                doc.raise_if_not_exist(par_id)
            # todo: make the iterator accept ranges
            i = doc.__iter__()
            try:
                while True:
                    if next(i).get_id() == par_id:
                        par_before = next(i).get_id()
                        raise StopIteration
            except StopIteration:
                pass
            finally:
                i.close()

            return self.paste_before(doc, par_before, as_ref)
