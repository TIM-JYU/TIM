from contracts import contract
from timdb.timdbbase import TimDbBase, TimDbException


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
    def create_label(self, language_id: 'str', content: 'str'):
        """
        Creates a new label
        :param language_id: Language chosen
        :param content: Label content
        :return:
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      INSERT INTO
                      Label(language_id, content)
                      VALUES (?, ?)
                      """, [language_id, content]
                       )
        self.db.commit()

    @contract
    def add_translation(self, label_id: 'int', language_id: 'str', content: 'str'):
        """
        Adds new translation to an existing label
        :param label_id: Label id
        :param language_id: Language chosen
        :param content: New translation
        :return:
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      INSERT INTO
                      Label(id, language_id, content)
                      VALUES (?, ?, ?)
                      """, [label_id, language_id, content]
                       )
        self.db.commit()

    @contract
    def update_label(self, label_id: 'int', language_id: 'str', content: 'str'):
        """
        Updates content of label in specific language
        :param label_id: Label id
        :param language_id: Language chosen
        :param content: Updated content
        :return:
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      UPDATE Label
                      SET content = ?
                      WHERE id = ? AND language_id = ?
                      """, [content, label_id, language_id]
                       )
        self.db.commit()

    @contract
    def get_velp_label_ids(self, velp_id: 'int', language_id: 'str'):
        """
        Gets information of labels for one velp in specific language
        :param velp_id: ID of velp
        :param language_id: Language chosen
        :return: List of labels associated with velp as
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      SELECT *
                      FROM Label
                      WHERE language_id = ? AND (id IN
                      (SELECT velp_id FROM LabelInVelp WHERE velp_id = ?))
                      """, [language_id, velp_id]
                       )
        return self.resultAsDictionary(cursor)

    @contract
    def get_document_velp_label_ids(self, document_id: 'int') -> 'list(dict)':
        """
        Finds all velps in use in the document and lists the labels associated with each of them.
        :param document_id: Id of the document
        :return: list of velp ids and label ids.
        """
        cursor = self.db.cursor()
        cursor.execute("""
                       SELECT
                         LabelInVelp.velp_id,
                         LabelInVelp.label_id AS labels
                       FROM LabelInVelp
                       WHERE velp_id IN (
                         SELECT VelpInGroup.velp_id
                         FROM VelpInGroup
                         WHERE velp_group_id IN (
                           SELECT VelpInGroup.velp_group_id
                           FROM VelpInGroup
                           WHERE VelpInGroup.velp_group_id IN (
                             SELECT VelpGroupInDocument.velp_group_id
                             FROM VelpGroupInDocument
                             WHERE VelpGroupInDocument.document_id = ?
                           )
                         )
                       )
                       ORDER BY velp_id ASC
                       """, [document_id]
                       )
        results = self.resultAsDictionary(cursor)
        return results


    @contract
    def get_document_velp_label_content(self, document_id: 'int', language_id: 'str' = 'FI') -> 'list(dict)':
        """Finds the relevant labels in the document and returns their content in the language specified.

        :param document_id: ID for the document.
        :param language_id: Optional, Id of the requested language. Default is 'FI'.
        :return: List of labels, each label represented by a dictionary
        """
        cursor = self.db.cursor()
        cursor.execute("""
                       SELECT Label.id, Label.content
                       FROM Label
                       WHERE label.language_id = ? AND label.id IN (
                         SELECT DISTINCT LabelInVelp.label_id
                         FROM LabelInVelp
                         WHERE LabelInVelp.velp_id IN (
                           SELECT VelpInGroup.velp_id
                           FROM VelpInGroup
                           WHERE VelpInGroup.velp_group_id IN (
                             SELECT VelpGroupInDocument.velp_group_id
                             FROM VelpGroupInDocument
                             WHERE VelpGroupInDocument.document_id = ?
                           )
                         )
                       )
                       """, [language_id, document_id]
                       )
        results = self.resultAsDictionary(cursor)
        return results

    @contract
    def delete_label(self, label_id: 'int'):
        """
        Deletes label (use with extreme caution)
        :param label_id: Label ID
        :return:
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      DELETE
                      FROM Label
                      WHERE id = ?
                      """, [label_id]
                       )
