"""Sometimes, using type annotations require imports that would cause circular imports.

To work around it, this module defines aliases for the types that can be imported instead.

TODO: These are broken at the moment after timApp reorganization.
"""

from typing import Union

# noinspection PyUnresolvedReferences
import timApp.document

UserType = 'timApp.timdb.models.user.User'
UserGroupType = 'timApp.timdb.models.usergroup.UserGroup'
TranslationType = 'timApp.timdb.models.translation.Translation'
DocInfoType = 'timApp.timdb.docinfo.DocInfo'
DocumentType = 'timApp.documentmodel.document.Document'
UserOrGroup = Union[UserType, UserGroupType]
