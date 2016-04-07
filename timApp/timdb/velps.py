from contracts import contract
from timdb.timdbbase import TimDbBase, TimDbException
from assesment_area import AssessmentArea, assessment_area_from_document


class Velps(TimDbBase):
    """
    Used as an interface to query the database about velps.
    """

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
    def create_velp(self, creator_id: 'int', default_points: 'float', icon_id: 'int | None' = None,
                    valid_until: 'str | None' = None):
        """Creates a new velp

        :param creator_id: User ID of creator.
        :param default_points: Default points for velp.
        :param icon_id: Icon ID attached to velp. Can be null.
        :param valid_until: Time after velp becomes unusable.
        :return: ID of velp that was just created
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
        """Creates a new version for a velp to use

        :param velp_id: ID of velp we're adding version for
        :return: ID of version that was just created
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
        """Method to create content (text) for velp

        :param version_id: Version ID where the content will be stored
        :param language_id: Language id
        :param content: Text of velp
        :return: -
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
        """Updates velp content

        :param velp_id: ID of velp that's being updated
        :param new_content: New velp text
        :param languages: Language used
        :return:
        """
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
    def check_velp_languages(self, velp_id: 'int'):
        """Fetches all languages used within one velp

        :param velp_id: Velp ID
        :return: Returns languages as JSON
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      SELECT
                      language_id
                      FROM
                      VelpInformation
                      WHERE id = ?
                      """, [velp_id]
        )
        results = self.resultAsDictionary(cursor) # e.g. [{'language_id': 'EN'}, {'language_id': 'FI'}]
        return results

    @contract
    def get_latest_velp_version(self, velp_id: 'int', language_id: 'str' = "FI"):
        """Method to fetch the latest version for velp in specific language

        :param velp_id: ID of velp we're checking
        :param language_id: ID of language
        :return: ID of version
        """
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
    def get_velps(self, assessment_area: 'AssessmentArea', language_id: 'str' = 'FI') -> 'list(dict)':
        """Get velps that are linked to an assessment area.

        :param assessment_area: the relevant assessment area
        :param language_id The id of the language. 'EN', for example.
        :return: A list of dictionaries, each describing a different velp.
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
                       """ +
                       assessment_area.get_sql_for_velp_ids()
                       + """
                       )
                       """, [language_id] + assessment_area.get_parameters_for_velp_ids()
                       )
        results = self.resultAsDictionary(cursor)
        return results

    @contract
    def get_document_velps(self, doc_id: 'int', language_id: 'str' = 'FI') -> 'list(dict)':
        """Gets velps that are linked to the document.

        :param doc_id: The id of the document.
        :param language_id The id of the language. 'EN', for example.
        :return: A list of dictionaries, each describing a different velp.
        """
        return self.get_velps(assessment_area_from_document(doc_id), language_id)
