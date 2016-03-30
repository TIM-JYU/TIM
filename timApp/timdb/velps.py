from contracts import contract
from timdb.timdbbase import TimDbBase, TimDbException


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
    def create_velp(self, creator_id: 'int', default_points: 'float', icon_id: 'int | None' = None,
                    valid_until: 'str | None' = None):
        """
        :param creator_id: User id of creator.
        :param default_points: Default points for velp.
        :param icon_id: Icon id attached to velp. Can be null.
        :param valid_until: Time after velp becomes unusable.
        :return:
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      INSERT INTO
                      Velp(creator_id, default_points, icon_id, valid_until)
                      VALUES(?, ?, ?, ?)
                      """, [creator_id, default_points, icon_id, valid_until]
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
        version_id = cursor.lastrowid
        return version_id

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


        # Something is f...d up here
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
    def get_latest_velp_version(self, velp_id: 'int', language_id: 'str' = "FI"):
        cursor = self.db.cursor()
        cursor.execute("""
                      SELECT
                      MAX(id)
                      FROM
                      VelpInformation
                      WHERE velp_id = ? AND language_id = ?
                      """, [velp_id, language_id]
                       )
        velp_version = cursor.fetchone()[0]
        return velp_version

    @contract
    def get_document_velps(self, doc_id: 'int', language: 'str' = 'FI') -> 'list(dict)':
        """Gets velps that are linked to the document.

        :param doc_id: The id of the document.
        :return: a list of dictionaries, each describing a different velp.
        """

        cursor = self.db.cursor()
        cursor.execute("""
                       SELECT Velp.id AS id, Velp.default_points AS points, Velp.icon_id AS icon_id,
                       y.content AS content, y.language_id AS language_id
                       FROM Velp
                       INNER JOIN(
                         SELECT x.velp_id, VelpContent.content, VelpContent.language_id
                         FROM VelpContent
                         INNER JOIN (
                           SELECT VelpVersion.velp_id, max(VelpVersion.id) AS latest_version
                           FROM VelpVersion GROUP BY VelpVersion.velp_id
                           ) AS x ON VelpContent.version_id = x.latest_version
                       ) AS y ON y.velp_id = velp.id
                       WHERE y.language_id = ? AND velp_id IN (
                         SELECT VelpInGroup.velp_id
                         FROM VelpInGroup
                         WHERE VelpInGroup.velp_group_id IN (
                           SELECT velp_group_id
                           FROM VelpGroupInDocument
                           WHERE document_id = ?
                         )
                       )
                       """, [language, doc_id]
                       )
        results = self.resultAsDictionary(cursor)
        return results
