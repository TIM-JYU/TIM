from contracts import contract
from timdb.timdbbase import TimDbBase,TimDbException

class Labels(TimDbBase):
    @contract
    def __init__(self, db_path: 'Connection', files_root_path: 'str', type_name: 'str', current_user_name: 'str'):
        """Initializes TimDB with the specified database and root path.

        :param type_name: The type name.
        :param current_user_name: The name of the current user.
        :param db_path: The path of the database file.
        :param files_root_path: The root path where all the files will be stored.
        """
        TimDbBase.__init__(self, db_path, files_root_path, type_name, current_user_name)

    @contract
    def get_velp_labels(self,velp_id: 'int') -> 'list(str)':
        """
        :param self:
        :param velp_id:
        :return:
        """
        cursor=self.db.cursor()
        #todo get label content also. return something.
        cursor.execute('SELECT label_id FROM Label_in_velp WHERE velp_id=?',velp_id)