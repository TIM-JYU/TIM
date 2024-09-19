from __future__ import annotations

import re
from copy import deepcopy
from dataclasses import dataclass, field
from html import escape
from typing import TYPE_CHECKING, Any, Mapping

from flask import session, has_request_context, flash
from marshmallow import missing
from typing_extensions import Self

from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import ViewContext
from timApp.markdown.markdownconverter import (
    create_environment,
    TimSandboxedEnvironment,
)
from timApp.util.rndutils import get_rands_as_dict, SeedType
from timApp.util.utils import cached_property

if TYPE_CHECKING:
    from timApp.user.user import User
    from timApp.document.document import Document


@dataclass
class MacroInfo:
    """Represents information required for expanding macros in a DocParagraph."""

    view_ctx: ViewContext
    doc: Document | None = None
    macro_map: dict[str, object] = field(default_factory=dict)
    """The mapping of macro keys to their values."""
    macro_delimiter: str = "%%"
    """The delimiter used for macros in the markdown."""
    user_ctx: UserContext | None = None
    preserve_user_macros: bool = False
    """If True and user is not provided, get_macros() will preserve the user-specific-macros
     (instead of replacing them with empty values)."""

    def with_field_macros(self) -> Self:
        from timApp.auth.accesshelper import AccessDenied

        doc = self.doc
        if doc is not None:
            orig_doc = doc.get_source_document()
            # Lookup fields in relation to original doc, since original doc has the answers
            if orig_doc:
                doc = orig_doc
            docinfo = doc.get_docinfo()
            fieldmacros = doc.get_settings().fieldmacros()
            if fieldmacros and self.user_ctx:
                from timApp.util.get_fields import (
                    get_fields_and_users,
                    RequestedGroups,
                    GetFieldsAccess,
                )

                try:
                    field_data, _, _, _ = get_fields_and_users(
                        fieldmacros,
                        RequestedGroups(
                            groups=[self.user_ctx.user.get_personal_group()],
                            include_all_answered=False,
                        ),
                        docinfo,
                        self.user_ctx.logged_user,
                        self.view_ctx,
                        autoalias=True,
                        add_missing_fields=False,
                        access_option=GetFieldsAccess.RequireView,
                    )
                except AccessDenied as e:
                    if has_request_context():
                        flash(f"Could not fetch fieldmacros because: {e}", "error")
                    field_data = None

                if field_data:
                    data = field_data[0]
                    self.macro_map.update(data["fields"])

        return self

    def __post_init__(self) -> None:
        from timApp.tim_app import app

        doc = self.doc
        self.macro_map["host"] = app.config["TIM_HOST"]
        self.macro_map["last_referrers"] = (
            session.get("last_referrers", []) if session else []
        )
        if doc is not None:
            docinfo = doc.get_docinfo()
            self.macro_map.update(
                {
                    "docid": doc.doc_id,
                    "docpath": docinfo.path,
                    "docdir": docinfo.location,
                    "doclang": docinfo.lang_id,
                    "doctitle": docinfo.title,
                    "docname": docinfo.short_name,
                }
            )
            urlmacros = doc.get_settings().urlmacros()
            if urlmacros:
                self.macro_map.update(
                    get_url_macros(
                        self.macro_map,
                        urlmacros,
                        {key: val for (key, val) in self.view_ctx.urlmacros},
                    )
                )
            extramacros = self.view_ctx.extra_macros
            if extramacros:
                self.macro_map.update(extramacros)
            rndmacros = doc.get_settings().rndmacros()
            if rndmacros:
                self.macro_map.update(
                    get_rnd_macros(
                        rndmacros, self.user_ctx.user if self.user_ctx else None
                    )
                )

    def get_macros(self) -> dict[str, object]:
        user = self.user_ctx
        if user is None:
            if not self.preserve_user_macros:
                return self.macro_map
            else:
                return self.get_macros_preserving_user()
        else:
            return self.get_macros_with_user_specific(user)

    def get_macro_delimiter(self) -> str:
        return self.macro_delimiter

    @cached_property
    def jinja_env(self) -> TimSandboxedEnvironment:
        return create_environment(
            self.macro_delimiter, self.user_ctx, self.view_ctx, self.macro_map, self.doc
        )

    def get_macros_preserving_user(self) -> dict[str, object]:
        """Gets the macros and defines user-specific variables in such a way that the macro replacement for user
        variables does effectively nothing."""
        macros = deepcopy(self.macro_map)
        macros.update(
            {
                "userid": f"{self.macro_delimiter}userid{self.macro_delimiter}",
                "username": f"{self.macro_delimiter}username{self.macro_delimiter}",
                "realname": f"{self.macro_delimiter}realname{self.macro_delimiter}",
                "useremail": f"{self.macro_delimiter}useremail{self.macro_delimiter}",
                "loggedUsername": f"{self.macro_delimiter}loggedUsername{self.macro_delimiter}",
                "userfolder": f"{self.macro_delimiter}userfolder{self.macro_delimiter}",
                "profilepicture": f"{self.macro_delimiter}profilepicture{self.macro_delimiter}",
            }
        )
        return macros

    def get_macros_with_user_specific(
        self, user: UserContext | None = None
    ) -> dict[str, object]:
        if not user:
            return self.macro_map
        macros = deepcopy(self.macro_map)
        macros.update(get_user_specific_macros(user))
        return macros


def get_user_specific_macros(user_ctx: UserContext) -> dict[str, str | None]:
    """
    Gets the macros that are specific to the user.

    The user macros are defined as follows:
         - userid: The user id of the user.
         - username: The username of the user.
         - realname: The real name of the user.
         - useremail: The email address of the user.
         - loggedUsername: The username of the user that is logged in.
         - userfolder: The personal folder of the user.
         - profilepicture: The profile picture of the user.

    :param user_ctx: User context to get the macros for.
    :return: Dictionary of user macros.
    """
    user = user_ctx.user
    return {
        "userid": escape(str(user.id)),
        "username": escape(user.name),
        "realname": escape(user.real_name) if user.real_name else None,
        "useremail": escape(user.email) if user.email else None,
        "loggedUsername": escape(user_ctx.logged_user.name),
        "userfolder": escape(
            user.get_personal_folder().path
        ),  # personal folder object is cached and usually reused
        "profilepicture": escape(user.get_profile_picture()),
    }


def get_rnd_macros(
    rndmacros_setting: dict[str, str], user: User | None
) -> dict[str, str]:
    rnd_seed: SeedType | None = user.name if user else None
    state = None
    ret = {}
    rndm = rndmacros_setting
    if "rndnames" not in rndm:
        rndnames = []
        for rnd_name in rndm:
            if rnd_name not in ["seed"]:  # todo put other non rnd names here
                rndnames.append(rnd_name)
        rndm["rndnames"] = ",".join(rndnames)
    rands, rnd_seed, state = get_rands_as_dict(rndm, rnd_seed, state)
    if rands:
        for rnd_name, rnd in rands.items():
            ret[rnd_name] = rnd
    return ret


urlmacros_tester = re.compile(r"[^0-9A-Za-zÅÄÖåäöÜü.,_ \-/@+=]+")


def get_url_macros(
    docmacros: dict[str, Any],
    urlmacros: Mapping[str, int | float | str],
    urlargs: dict[str, str],
) -> dict[str, str]:
    ret = {}
    for um in urlmacros:
        if not um:
            continue

        # TODO: if already value and urlmacros.get(um) then old value wins
        urlmacro_value = urlmacros.get(um, missing)
        urlvalue = urlargs.get(um, urlmacro_value)
        if urlvalue is missing:
            continue
        try:
            uvalue = None
            try:
                uvalue = float(urlvalue)  # type: ignore
            except ValueError:
                pass
            if uvalue is not None:
                maxvalue = docmacros.get("MAX" + um, None)
                if maxvalue is not None:
                    if uvalue > maxvalue:
                        urlvalue = maxvalue
                minvalue = docmacros.get("MIN" + um, None)
                if minvalue is not None:
                    if uvalue < minvalue:
                        urlvalue = minvalue
            urlvalue = urlmacros_tester.sub("", str(urlvalue))
            ret[um] = urlvalue
        except TypeError:
            pass
    return ret
