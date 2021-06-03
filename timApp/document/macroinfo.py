from __future__ import annotations

import re
from copy import deepcopy
from dataclasses import dataclass, field
from html import escape
from typing import Dict, Optional, TYPE_CHECKING, Any, Union, Mapping

from flask import current_app
from jinja2.sandbox import SandboxedEnvironment

from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import ViewContext
from timApp.markdown.markdownconverter import create_environment
from timApp.util.rndutils import get_rands_as_dict
from timApp.util.utils import cached_property

if TYPE_CHECKING:
    from timApp.user.user import User
    from timApp.document.document import Document


@dataclass
class MacroInfo:
    """Represents information required for expanding macros in a DocParagraph.
    """
    view_ctx: ViewContext
    doc: Optional[Document] = None
    macro_map: Dict[str, object] = field(default_factory=dict)
    """The mapping of macro keys to their values."""
    macro_delimiter: str = '%%'
    """The delimiter used for macros in the markdown."""
    user_ctx: Optional[UserContext] = None
    preserve_user_macros: bool = False
    """If True and user is not provided, get_macros() will preserve the user-specific-macros
     (instead of replacing them with empty values)."""

    def __post_init__(self) -> None:
        doc = self.doc
        self.macro_map.update({'host': current_app.config["TIM_HOST"]})
        if doc is not None:
            self.macro_map.update({'docid': doc.doc_id})
            docinfo = doc.get_docinfo()
            self.macro_map.update({'docpath': docinfo.path})
            self.macro_map.update({'doctitle': docinfo.title})
            self.macro_map.update({'docname': docinfo.short_name})
            urlmacros = doc.get_settings().urlmacros()
            if urlmacros:
                self.macro_map.update(
                    get_url_macros(self.macro_map, urlmacros, {key: val for (key, val) in self.view_ctx.urlmacros}))
            rndmacros = doc.get_settings().rndmacros()
            if rndmacros:
                self.macro_map.update(get_rnd_macros(rndmacros, self.user_ctx.user if self.user_ctx else None))

    def get_macros(self) -> Dict[str, object]:
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
    def jinja_env(self) -> SandboxedEnvironment:
        return create_environment(self.macro_delimiter, self.user_ctx, self.view_ctx)

    def get_macros_preserving_user(self) -> Dict[str, object]:
        """Gets the macros and defines user-specific variables in such a way that the macro replacement for user
        variables does effectively nothing."""
        macros = deepcopy(self.macro_map)
        macros.update({
            'username': f'{self.macro_delimiter}username{self.macro_delimiter}',
            'realname': f'{self.macro_delimiter}realname{self.macro_delimiter}',
            'useremail': f'{self.macro_delimiter}useremail{self.macro_delimiter}',
            'loggedUsername': f'{self.macro_delimiter}loggedUsername{self.macro_delimiter}',
        })
        return macros

    def get_macros_with_user_specific(self, user: Optional[UserContext] = None) -> Dict[str, object]:
        if not user:
            return self.macro_map
        macros = deepcopy(self.macro_map)
        macros.update(get_user_specific_macros(user))
        return macros


def get_user_specific_macros(user_ctx: UserContext) -> Dict[str, Optional[str]]:
    user = user_ctx.user
    return {
        'username': escape(user.name),
        'realname': escape(user.real_name) if user.real_name else None,
        'useremail': escape(user.email) if user.email else None,
        'loggedUsername': escape(user_ctx.logged_user.name),
    }


def get_rnd_macros(rndmacros_setting: Dict[str, str], user: Optional['User']) -> Dict[str, str]:
    rnd_seed = user.name if user else None
    state = None
    ret = {}
    rndm = rndmacros_setting
    if 'rndnames' not in rndm:
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


urlmacros_tester = re.compile(r"[^0-9A-Za-zÅÄÖåäöÜü.,_ \-/@]+")


def get_url_macros(
        docmacros: Dict[str, Any],
        urlmacros: Mapping[str, Union[int, float, str]],
        urlargs: Dict[str, str],
) -> Dict[str, str]:
    ret = {}
    for um in urlmacros:
        if not um:
            continue

        # TODO: if already value and urlmacros.get(um) then old value wins
        urlvalue = urlargs.get(um, urlmacros.get(um))
        if not urlvalue:
            continue
        try:
            uvalue = None
            try:
                uvalue = float(urlvalue)
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
