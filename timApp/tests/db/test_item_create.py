from timApp.document.docentry import DocEntry
from timApp.folder.folder import Folder
from timApp.tests.db.timdbtest import TimDbTest
from timApp.timdb.exceptions import TimDbException
from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup


class ItemCreateTest(TimDbTest):
    def test_no_document_subitem(self):
        """Tests that items cannot be created under documents and that an item
        cannot be created at the same path as some existing item."""
        anon_group = UserGroup.get_anonymous_group()
        Folder.create('path/to/folder', anon_group)
        DocEntry.create('path/to/document', anon_group)
        db.session.commit()

        with self.assertRaises(TimDbException) as cm:
            Folder.create('path/to/document/folder', anon_group)
        self.assertEqual('A document already exists at path path/to/document', str(cm.exception))

        with self.assertRaises(TimDbException) as cm:
            DocEntry.create('path/to/document/document', anon_group)
        self.assertEqual('A document already exists at path path/to/document', str(cm.exception))

        with self.assertRaises(TimDbException) as cm:
            Folder.create('path/to/document/and/some/folder', anon_group)
        self.assertEqual('A document already exists at path path/to/document', str(cm.exception))

        with self.assertRaises(TimDbException) as cm:
            DocEntry.create('path/to/document/and/some/document', anon_group)
        self.assertEqual('A document already exists at path path/to/document', str(cm.exception))

        with self.assertRaises(TimDbException) as cm:
            DocEntry.create('path/to/folder', anon_group)
        self.assertEqual('A folder already exists at path path/to/folder', str(cm.exception))

        with self.assertRaises(TimDbException) as cm:
            Folder.create('path/to/document', anon_group)
        self.assertEqual('A document already exists at path path/to/document', str(cm.exception))
