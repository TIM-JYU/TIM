from copy import deepcopy
from html import escape
from typing import Dict, Optional

from timApp.timtypes import UserType, DocumentType


class MacroInfo:
    """Represents information required for expanding macros in a DocParagraph.
    
    :ivar macro_map: The mapping of macro keys to their values.
    :ivar macro_delimiter: The delimiter used for macros in the markdown.
    :ivar _user: The user to consider when processing macros.
    :ivar preserve_user_macros: If True and user is not provided, get_macros() will preserve the user-specific-macros
     (instead of replacing them with empty values).
    """

    def __init__(self, doc: Optional[DocumentType]=None,
                 macro_map: Optional[Dict[str, object]] = None,
                 macro_delimiter: Optional[str] = None,
                 user: Optional[UserType] = None,
                 nocache_user: Optional[UserType] = None):
        self.doc = doc
        self.macro_map: Dict[str, object] = macro_map or {}
        if not isinstance(self.macro_map, dict):
            self.macro_map = {}
        if doc is not None:
            self.macro_map.update({'docid': doc.doc_id})
            docinfo = doc.get_docinfo()
            self.macro_map.update({'docpath': docinfo.path})
            self.macro_map.update({'doctitle': docinfo.title})
            self.macro_map.update({'docname': docinfo.short_name})
        self.macro_delimiter = macro_delimiter or '%%'
        self._user = user
        self._nocache_user = nocache_user
        self.preserve_user_macros = False

    def get_macros(self, nocache:bool = False) -> Dict[str, object]:
        user = self._user
        if nocache and self._nocache_user:
            user = self._nocache_user
        if user is None:
            if not self.preserve_user_macros:
                return self.macro_map
            else:
                return self.get_macros_preserving_user()
        else:
            return self.get_macros_with_user_specific(user)

    def get_macro_delimiter(self) -> str:
        return self.macro_delimiter

    def get_macros_preserving_user(self) -> Dict[str, object]:
        """Gets the macros and defines user-specific variables in such a way that the macro replacement for user
        variables does effectively nothing."""
        macros = deepcopy(self.macro_map)
        macros.update({'username': f'{self.macro_delimiter}username{self.macro_delimiter}',
                       'realname': f'{self.macro_delimiter}realname{self.macro_delimiter}',
                       'useremail': f'{self.macro_delimiter}useremail{self.macro_delimiter}'})
        return macros

    def get_macros_with_user_specific(self, user: Optional[UserType] = None) -> Dict[str, object]:
        if not user:
            return self.macro_map
        macros = deepcopy(self.macro_map)
        macros.update(MacroInfo.get_user_specific_macros(user))
        return macros

    @staticmethod
    def get_user_specific_macros(user: Optional[UserType] = None) -> Dict[str, object]:
        if not user:
            return {}
        return {
            'username': escape(user.name),
            'realname': escape(user.real_name) if user.real_name else None,
            'useremail': escape(user.email) if user.email else None,
        }
