from typing import Dict, Optional

from copy import deepcopy

if False:
    from timApp.timdb.models.user import User


class MacroInfo:
    """Represents information required for expanding macros in a DocParagraph.
    
    :ivar macro_map: The mapping of macro keys to their values.
    :ivar macro_delimiter: The delimiter used for macros in the markdown.
    :ivar _user: The user to consider when processing macros.
    :ivar preserve_user_macros: If True and user is not provided, get_macros() will preserve the user-specific-macros
     (instead of replacing them with empty values).
    """

    def __init__(self,
                 macro_map: Optional[Dict[str, str]] = None,
                 macro_delimiter: Optional[str] = None,
                 user: Optional['User'] = None):
        self.macro_map = macro_map or {}  # type: Dict[str, str]
        self.macro_delimiter = macro_delimiter or '%%'
        self._user = user
        self.preserve_user_macros = False
        self.stringize_macros()

    def stringize_macros(self):
        """Converts macro keys and values to strings if they are not already."""
        if isinstance(self.macro_map, dict):
            self.macro_map = {str(k): str(self.macro_map[k]) for k in self.macro_map}
        else:
            self.macro_map = {}

    def get_macros(self) -> Dict[str, str]:
        if self._user is None:
            if not self.preserve_user_macros:
                return self.macro_map
            else:
                return self.get_macros_preserving_user()
        else:
            return self.get_macros_with_user_specific(self._user)

    def get_macro_delimiter(self) -> str:
        return self.macro_delimiter

    def get_macros_preserving_user(self) -> Dict[str, str]:
        """Gets the macros and defines user-specific variables in such a way that the macro replacement for user
        variables does effectively nothing."""
        macros = deepcopy(self.macro_map)
        macros.update({'username': '{0}username{0}'.format(self.macro_delimiter),
                       'realname': '{0}realname{0}'.format(self.macro_delimiter)})
        return macros

    def get_macros_with_user_specific(self, user: Optional['User'] = None) -> Dict[str, str]:
        if not user:
            return self.macro_map
        macros = deepcopy(self.macro_map)
        macros.update(MacroInfo.get_user_specific_macros(user))
        return macros

    @staticmethod
    def get_user_specific_macros(user: Optional['User'] = None) -> Dict[str, str]:
        if not user:
            return {}
        return {'username': user.name, 'realname': user.real_name}
