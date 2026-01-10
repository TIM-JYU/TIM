from dataclasses import dataclass, field
from typing import Optional

from timApp.auth.accesshelper import has_view_access
from timApp.document.docentry import DocEntry
from timApp.document.docparagraph import DocParagraph
from timApp.document.document import Document
from timApp.document.exceptions import ValidationException
from timApp.document.version import Version
from timApp.document.viewcontext import ViewRoute, viewroute_from_str
from timApp.util.flask.requesthelper import verify_json_params


@dataclass
class EditRequest:
    doc: Document
    area_start: str | None = None
    area_end: str | None = None
    par: str | None = None
    text: str | None = None
    next_par_id: str | None = None
    preview: bool = False
    forced_classes: list[str] = field(default_factory=list)
    mark_translated: bool | None = None
    viewname: ViewRoute | None = None
    old_doc_version: Version = field(init=False)
    editor_pars: list[DocParagraph] | None = field(init=False)
    original_par: DocParagraph | None = field(init=False)
    context_par: DocParagraph | None = field(init=False)

    def __post_init__(self):
        self.old_doc_version = self.doc.get_version()
        self.editor_pars = None
        self.original_par = (
            self.doc.get_paragraph(self.par)
            if not self.editing_area and self.par is not None and not self.is_adding
            else None
        )
        self.context_par = self.get_context_par()

    @property
    def is_adding(self):
        return self.par in ("HELP_PAR",) or self.par is None

    @property
    def editing_area(self) -> bool:
        return self.area_start is not None and self.area_end is not None

    def get_original_par(self) -> DocParagraph | None:
        return self.original_par

    def get_last_of_preamble(self) -> DocParagraph | None:
        preamble = self.doc.insert_preamble_pars()  # TODO could be optimized
        return preamble[-1] if preamble else None

    def get_context_par(self) -> DocParagraph:
        doc = self.doc
        if self.editing_area:
            context_par = doc.get_previous_par_by_id(self.area_start)
        elif not self.is_adding:
            context_par = doc.get_previous_par_by_id(self.par)
            if context_par is None:
                return self.get_last_of_preamble()
        elif self.next_par_id:
            context_par = doc.get_previous_par_by_id(self.next_par_id)
            if context_par is None:
                return self.get_last_of_preamble()
        else:
            context_par = doc.get_last_par()
        return context_par

    def get_pars(
        self, skip_access_check: bool = False, do_validation: bool = False
    ) -> list[DocParagraph]:
        if self.editor_pars is None:
            self.editor_pars = get_pars_from_editor_text(
                self.doc,
                self.text,
                break_on_elements=self.editing_area,
                skip_access_check=skip_access_check,
                do_validation=do_validation,
            )
            for c in self.forced_classes:
                for p in self.editor_pars:
                    p.add_class(c)
        return self.editor_pars

    @staticmethod
    def from_request(
        doc: Document, text: str | None = None, preview: bool = False
    ) -> "EditRequest":
        if text is None:
            (text,) = verify_json_params("text")
        (
            area_start,
            area_end,
            par,
            par_next,
            forced_classes,
            tags,
            view,
        ) = verify_json_params(
            "area_start",
            "area_end",
            "par",
            "par_next",
            "forced_classes",
            "tags",
            "view",
            require=False,
        )
        mark_translated = tags.get("marktranslated") if tags else None
        return EditRequest(
            doc=doc,
            text=text,
            area_start=area_start,
            area_end=area_end,
            par=par,
            next_par_id=par_next,
            preview=preview,
            forced_classes=forced_classes or [],
            mark_translated=mark_translated,
            viewname=viewroute_from_str(view) if view else None,
        )


def get_pars_from_editor_text(
    doc: Document,
    text: str,
    break_on_elements: bool = False,
    skip_access_check: bool = False,
    do_validation: bool = False,
) -> list[DocParagraph]:
    blocks, _ = doc.text_to_paragraphs(
        text, break_on_elements, do_validation=do_validation
    )
    for p in blocks:
        if p.is_reference():
            try:
                refdoc = int(p.get_attr("rd"))
            except (ValueError, TypeError):
                continue
            d = DocEntry.find_by_id(refdoc)
            if not skip_access_check and d and not has_view_access(d):
                raise ValidationException(
                    f"You don't have view access to document {refdoc}"
                )
    return blocks
