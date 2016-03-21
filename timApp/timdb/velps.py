from contracts import contract
from timdb.timdbbase import TimDbBase,TimDbException


class Velps(TimDbBase):

    """

    Näitä tuskin tarvitsee?

    @contract
    def __init__(self, db_path: 'Connection', files_root_path: 'str', type_name: 'str', current_user_name: 'str'):
        ""Initializes TimDB with the specified database and root path.

        :param type_name: The type name.
        :param current_user_name: The name of the current user.
        :param db_path: The path of the database file.
        :param files_root_path: The root path where all the files will be stored.
        ""
        TimDbBase.__init__(self, db_path, files_root_path, type_name, current_user_name)

    """


    @contract
    def create_velp(self, creator_id: 'int', creation_time: 'str', default_points: 'float', icon_id: 'int',
                    valid_until: 'str' ):
        """
        :param creator_id: User id of creator.
        :param creation_time: Time of creation.
        :param default_points: Default points for velp.
        :param icon_id: Icon id attached to velp. Can be null.
        :param valid_until: Time after velp becomes unusable.
        :return:
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      INSERT INTO
                      Velp(creator_id, creation_time, default_points, icon_id, valid_until)
                      VALUES(?, ?, ?, ?, ?)
                      """, [creator_id, creation_time, default_points, icon_id, valid_until]
        )
        velp_id = cursor.lastrowid
        return velp_id

    def create_velp_version(self):

        cursor = self.db.cursor()
        cursor.execute("""
                      INSERT INTO

                      """
        )

    @contract
    def get_document_velps(self,doc_id: 'int') -> 'list(dict)':
        """Gets phrases that are linked to the document.

        :param doc_id: The id of the document.
        :returns: a list of dictionaries, each describing a different velp.
        """

        #TODO use doc_id and not return the whole database.
        cursor=self.db.cursor()
        cursor.execute('SELECT label_id,velp_id FROM label_in_phrase WHERE velp_id IN (SELECT id FROM Velp)')

        results=self.resultAsDictionary(cursor)
        return results