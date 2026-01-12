# python
#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""Module for renaming paragraph ids and checking duplicate plugin attributes.

This module deals with both DocParagraph objects and dict representations
of paragraphs (used when parsing JSON). Comments explain why we coerce
`attrs` with `or {}` and how lazy-loading of existing document names works.

TODO: In manage check names are leagal?
TODO: case in edit where new par added before existing with same name?

"""

from __future__ import annotations
from typing import TYPE_CHECKING, Callable, TypeAlias, Sequence, Any, Optional, TypeVar
import re
from collections import Counter

from timApp.document.randutils import random_id
from timApp.timdb.sqa import run_sql
from timApp.util.flask.requesthelper import RouteException
from timApp.util.utils import strip_not_allowed
from sqlalchemy import select
from timApp.answer.answer import Answer

if TYPE_CHECKING:
    from timApp.document.docparagraph import DocParagraph
    from timApp.document.document import Document

DocParOrDict: TypeAlias = "DocParagraph | dict[str, Any]"
DocParOrDictT = TypeVar("DocParOrDictT", "DocParagraph", dict[str, Any])

RenameCallback: TypeAlias = Callable[
    [
        Sequence[DocParOrDict],
        DocParOrDict,
        str,
        str,
    ],
    None,
]


def get_duplicate_id_msg(conflicting_ids: set[str]) -> str:
    return f'Duplicate paragraph id(s): {", ".join(conflicting_ids)}'


def split_tail_number(s: str) -> tuple[str, int]:
    """
    Returns (text_part, numeric_tail) where numeric_tail is an int if the string ends
    with digits, otherwise None.
    """
    m = re.search(r"(\d+)$", s)
    if m:
        text = s[: m.start()]
        num = int(m.group(1))
        return text, num
    return s, 1


def get_next_available_name(
    name: str, names_to_avoid: list[str], allow: str = ""
) -> str:
    """
    Returns next available name by appending/incrementing
    a trailing number.  If name is not in names_to_avoid,
    the original name is returned.
    :param name: current name
    :param names_to_avoid: names that cannot be used
    :param allow: name that is allowed even if in names_to_avoid
    :return: next available name
    """

    need_new_name = False
    # First try with original name
    for old_name in names_to_avoid:
        if old_name == name and old_name != allow:
            need_new_name = True
            break

    # If there was no previous par with the same task id keep it
    if not need_new_name:
        return name

    # Otherwise, determine a new one
    # Split the name into text and trailing number
    body, number = split_tail_number(name)
    new_name = body + str(number)

    j = 0
    while j < len(names_to_avoid):
        old_name = names_to_avoid[j]
        if old_name == new_name and old_name != allow:
            number += 1
            new_name = body + str(number)
            j = 0  # restart checking
        else:
            j += 1
    return new_name


def area_renamed(
    new_pars: Sequence[DocParagraph | dict[str, Any]],
    p: DocParagraph | dict[str, Any],
    _old_name: str,
    new_name: str,
) -> None:
    """
    If area start is renamed, also rename area end accordingly.
    Despite the oldname, this function finds the end on
    same level as the start paragraph.

    :param new_pars: list of new paragraphs
    :param p: starting paragraph
    :param _old_name: old area name
    :param new_name: new area name
    :return: None
    """
    try:
        p_index = new_pars.index(p)
    except ValueError:
        return

    area_level = 1
    # Try to find the corresponding area_end starting from p
    for i in range(p_index + 1, len(new_pars)):
        apar = new_pars[i]
        if isinstance(apar, dict):
            apar_attrs = apar.get("attrs", {})
        else:
            apar_attrs = apar.attrs

        if apar_attrs.get("area_end") is not None:
            if area_level == 1:
                apar_attrs["area_end"] = new_name
                return
            area_level -= 1
            if area_level < 0:  # end cannot be before start
                return

        elif apar_attrs.get("area") is not None:
            area_level += 1


def check_and_rename_new_name(attr_name: str, new_name: str, doc: Document) -> str:
    """
    Check if the new_name is already used in document for the given attribute.
    If so, generate a new name by appending/incrementing a trailing number.
    :param attr_name: attribute name to check
    :param new_name: proposed new name
    :param doc: document where the name is being compared to
    :return: modified new name
    """
    d = {"attrs": {attr_name: new_name}, "id": ""}
    renamed_pars = check_and_rename_attribute(attr_name, [d], doc)
    return renamed_pars[0]["attrs"][attr_name]


def check_and_rename_attribute(
    attr_name: str,
    new_pars: Sequence[DocParOrDictT],
    doc: Document,
    on_rename: Optional[RenameCallback] = None,
) -> Sequence[DocParOrDictT]:
    """
    Rename plugins selected attribute if duplicate found in document.
    :param attr_name:  attribute name to check and rename
    :param new_pars:  new blocks to check and rename
    :param doc: document where the blocks are being compared to
    :param on_rename: optional callback function called when a rename occurs
           with the new name as parameter
    :return: modified blocks with renamed task ids
    """
    names_to_check: list[str] | None = None  # lazy load for names_to_check
    names_to_check_map: dict[str, str] = {}
    name_counts: dict[str, int] = {}

    for p in new_pars:
        # go through all new pars if they need to be renamed
        p_id: str
        p_attrs: dict[str, str]
        if isinstance(p, dict):
            p_attrs = p.get("attrs") or {}
            p_id = str(p.get("id"))
        else:
            p_attrs = p.attrs or {}
            p_id = p.get_id()
        p_name = p_attrs.get(attr_name)
        if p_name is None:
            continue

        if names_to_check is None:  # now names_to_check is needed, load them once
            doc_pars = doc.get_paragraphs()
            names_to_check = []
            for paragraph in doc_pars:
                name = paragraph.get_attr(attr_name)
                if name:
                    did = paragraph.get_id()
                    names_to_check.append(name)
                    names_to_check_map[did] = name
            name_counts = Counter(n for n in names_to_check if n is not None)

        new_name = strip_not_allowed(p_name)
        if new_name == "" or new_name == "PLUGINNAMEHERE":
            # generate a new name that is not used in new_pars or in doc
            new_name = "a"
            if p_name == "PLUGINNAMEHERE":  # does not matter for area
                new_name = "Plugin1"
            new_names = list(names_to_check or [])
            for pn in new_pars:  # collect names from new_pars also
                pn_attrs: dict[str, str]
                if isinstance(pn, dict):
                    pn_attrs = pn.get("attrs") or {}
                else:
                    pn_attrs = pn.attrs or {}
                nn = pn_attrs.get(attr_name)
                if nn:
                    new_names.append(nn)
            new_name = get_next_available_name(new_name, new_names)
        else:
            # check if the name is already used in doc or renamed new_pars
            allow: str = names_to_check_map.get(p_id, "")
            if name_counts.get(allow, 0) > 1:
                allow = ""  # if original name is duplicate, do not allow it
            if new_name != allow:  # if not original for this par in doc
                new_name = get_next_available_name(
                    new_name, names_to_check or [], allow
                )
        if new_name != p_name:
            p_attrs[attr_name] = new_name
        if on_rename:  # for areas do the check even if name not changed
            on_rename(new_pars, p, p_name, new_name)
        names_to_check.append(new_name)
    # TODO: for area ends check that they are renamed also
    # TODO: if they have no starting par in new_pars
    return new_pars


def abort_if_duplicate_ids(
    doc: Document,
    pars_to_add: list[DocParagraph],
    auto_rename_ids: bool = False,
    no_other_checks: bool = False,
    existing_ids: set[str] | None = None,
    allow_id: str = "",
) -> None:
    """
    Aborts the request if any of the paragraphs
    to be added have IDs that conflict with existing
    paragraph IDs in the document.
    if rename_task_ids is True,
    ask automatically to rename conflicting task IDs.
    Also renames area IDs if needed.
    :param doc: The document to which paragraphs are being added.
    :param pars_to_add: The paragraphs to be added.
    :param auto_rename_ids: If True, automatically renames conflicting task IDs instead of aborting.
    :param no_other_checks: If True, skips other checks except for duplicate IDs.
    :param existing_ids: Optional set of existing paragraph IDs to check against.
                         If None, the IDs are fetched from the document.
    :param allow_id: for example first editing par id that is allowed even if duplicate
    :raises RouteException: If there are conflicting paragraph IDs and auto_rename_ids is False.
    """
    ids_set: set[str] = set()
    internal_duplicates: set[str] = set()
    dupls: dict[str, list[DocParagraph]] = {}
    for p in pars_to_add:
        pid = p.get_id()
        if pid in ids_set:
            internal_duplicates.add(pid)
            dupls.setdefault(pid, []).append(p)
        ids_set.add(pid)

    if existing_ids is None:
        existing_ids = set(doc.get_par_ids())
    conflicting_ids = ids_set & existing_ids - {allow_id}

    if (conflicting_ids or internal_duplicates) and not auto_rename_ids:
        raise RouteException(
            get_duplicate_id_msg(conflicting_ids | internal_duplicates)
        )

    if conflicting_ids:  # conflicting with existing IDs in the document
        for p in pars_to_add:
            if p.get_id() in conflicting_ids:
                p.set_id(random_id())
    for pid in internal_duplicates:  # internal duplicates
        cpars = dupls[pid]
        for p in cpars:
            p.set_id(random_id())

    if no_other_checks:
        return
    check_and_rename_attribute("taskId", pars_to_add, doc)
    check_and_rename_attribute("area", pars_to_add, doc, area_renamed)


def check_and_rename_pluginnamehere(
    blocks: list[DocParagraph], doc: Document
) -> list[DocParagraph]:
    # Automatically rename plugins with name pluginnamehere
    old_pars: list[DocParagraph] | None = None  # lazy load for old_pars
    i = 1
    j = 0
    # For all blocks check if taskId is pluginnamehere, if it is find next available name.
    for p in blocks:  # go through all new pars if they need to be renamed
        if p.is_task():
            task_id = p.get_attr("taskId")
            if task_id == "PLUGINNAMEHERE":
                if old_pars is None:  # now old_pars is needed, load them once
                    pars = doc.get_paragraphs()
                    old_pars = []
                    for paragraph in pars:
                        if not paragraph.is_task():
                            old_pars.append(paragraph)

                task_id = "Plugin" + str(i)
                while j < len(old_pars):
                    if task_id == old_pars[j].get_attr("taskId"):
                        i += 1
                        task_id = "Plugin" + str(i)
                        j = 0
                    else:
                        j += 1
                p.set_attr("taskId", task_id)
                old_pars.append(p)
                j = 0
    return blocks


# Check new paragraphs with plugins for duplicate task ids
def check_duplicates(pars: list[DocParagraph], doc: Document) -> list[list[str]]:
    duplicates = []
    all_pars = None  # cache all_pars
    for par in pars:
        task_id = par.get_attr("taskId")
        if task_id:
            if all_pars is None:  # now we need the pars
                doc.clear_mem_cache()
                docpars = doc.get_paragraphs()
                all_pars = []
                for paragraph in docpars:
                    d_task_id = paragraph.get_attr("taskId")
                    if d_task_id:
                        all_pars.append(paragraph)

            duplicate = []
            # task_id = par.get_attr("taskId")  # already got above
            par_id = par.get_id()
            count_of_same_task_ids = 0
            j = 0
            while j < len(all_pars):
                if (
                    all_pars[j].get_id() != par_id
                    and all_pars[j].get_attr("taskId") == task_id
                ):  # count not self
                    count_of_same_task_ids += 1
                    if count_of_same_task_ids > 0:
                        duplicate.append(task_id)
                        duplicate.append(par.get_id())
                        task_id_to_check = str(doc.doc_id) + "." + str(task_id)
                        if (
                            run_sql(
                                select(Answer)
                                .filter_by(task_id=task_id_to_check)
                                .limit(1)
                            )
                            .scalars()
                            .first()
                        ):
                            duplicate.append("hasAnswers")
                        duplicates.append(duplicate)
                        break
                j += 1
    return duplicates
