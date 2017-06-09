from timApp.tests.db.timdbtest import TimDbTest
from timApp.timdb.models.docentry import DocEntry
from timApp.timdb.models.folder import Folder
from timApp.timdb.models.usergroup import UserGroup
from timApp.timdb.tim_models import db


class FolderRenameTest(TimDbTest):
    def test_folder_rename(self):
        anon_g_id = UserGroup.get_anonymous_group().id
        Folder.create('path/to/first', anon_g_id)
        Folder.create('path/to/second', anon_g_id)
        Folder.create('path/to/third/and/more', anon_g_id)
        Folder.create('path/to/path/to/path/to', anon_g_id)
        DocEntry.create('path/to/firstdoc', anon_g_id)
        DocEntry.create('path/to/seconddoc', anon_g_id)
        DocEntry.create('path/to/third/and/more/doc', anon_g_id)

        # these should not get renamed
        Folder.create('path/to3/first', anon_g_id)
        Folder.create('some/path/to/first', anon_g_id)

        f = Folder.find_by_path('path/to')
        f.rename('path/to2')
        db.session.commit()
        self.assertIsNone(Folder.find_by_path('path/to/first'))
        self.assertIsNone(Folder.find_by_path('path/to/second'))
        self.assertIsNone(Folder.find_by_path('path/to/third/and/more'))
        self.assertIsNone(Folder.find_by_path('path/to/path/to/path/to'))
        self.assertIsNone(DocEntry.find_by_path('path/to/firstdoc'))
        self.assertIsNone(DocEntry.find_by_path('path/to/seconddoc'))
        self.assertIsNone(DocEntry.find_by_path('path/to/third/and/more/doc'))

        self.assertIsNone(Folder.find_by_path('path/to23/first'))
        self.assertIsNone(Folder.find_by_path('some/path/to2/first'))
        self.assertIsNotNone(Folder.find_by_path('path/to3/first'))
        self.assertIsNotNone(Folder.find_by_path('some/path/to/first'))

        self.assertIsNotNone(Folder.find_by_path('path/to2/first'))
        self.assertIsNotNone(Folder.find_by_path('path/to2/second'))
        self.assertIsNotNone(Folder.find_by_path('path/to2/third/and/more'))
        self.assertIsNotNone(Folder.find_by_path('path/to2/path/to/path/to'))
        self.assertIsNotNone(DocEntry.find_by_path('path/to2/firstdoc'))
        self.assertIsNotNone(DocEntry.find_by_path('path/to2/seconddoc'))
        self.assertIsNotNone(DocEntry.find_by_path('path/to2/third/and/more/doc'))
