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
        self.db.commit()
        velp_id = cursor.lastrowid
        return velp_id

    @contract
    def create_velp_version(self, velp_id: 'int'):
        """
        :param velp_id:
        :return:
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      INSERT INTO
                      VelpVersion(velp_id)
                      VALUES(?)
                      """, [velp_id]
        )
        self.db.commit()

    @contract
    def create_velp_content(self, version_id: 'int', language_id: 'str', content: 'str'):
        """
        :param version_id:
        :param language_id:
        :param content:
        :return:
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      INSERT INTO
                      VelpContent(version_id, language_id, content)
                      VALUES (?, ?, ?)
                      """, [version_id, language_id, content]
        )
        self.db.commit()

    @contract
    def update_velp(self, velp_id: 'int', new_content: 'str', languages: 'str'):
        cursor = self.db.cursor()
        cursor.execute("""
        """)
        cursor.execute("""
                      INSERT INTO
                      VelpVersion(velp_id)
                      VALUES (?)
                      """, [velp_id]
        )
        new_versionId = cursor.lastrowid
        cursor.execute("""
                      INSERT INTO
                      VelpContent(version_id, language_id, content)
                      VALUES (?, ?, ?)
                      """, [new_versionId, languages[0], new_content[0]]
        )

    @contract
    def get_document_velps(self, doc_id: 'int') -> 'list(dict)':
        """Gets phrases that are linked to the document.

        :param doc_id: The id of the document.
        :returns: a list of dictionaries, each describing a different velp.
        """

        cursor=self.db.cursor()
        cursor.execute("""
                      SELECT * FROM Velp WHERE id IN (
                        SELECT velp_id FROM VelpInGroup WHERE velp_group_id IN (
                          SELECT velp_group_id FROM VelpGroupInDocument WHERE document_id = ?
                        )
                      )
                      """, [doc_id]
                      )
        return self.resultAsDictionary(cursor)
