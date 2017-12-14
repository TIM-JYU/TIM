from typing import Union

# noinspection PyUnresolvedReferences
import timApp.documentmodel
# noinspection PyUnresolvedReferences
import timApp.timdb.models

UserType = 'timApp.timdb.models.user.User'
UserGroupType = 'timApp.timdb.models.usergroup.UserGroup'
DocEntryType = 'timApp.timdb.models.docentry.DocEntry'
TranslationType = 'timApp.timdb.models.translation.Translation'
FolderType = 'timApp.timdb.models.folder.Folder'
DocInfoType = 'timApp.timdb.docinfo.DocInfo'
DocumentType = 'timApp.documentmodel.document.Document'
UserOrGroup = Union[UserType, UserGroupType]
