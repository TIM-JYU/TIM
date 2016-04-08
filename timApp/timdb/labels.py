from contracts import contract
from timdb.timdbbase import TimDbBase, TimDbException
from assesment_area import AssessmentArea, assessment_area_from_document


class Labels(TimDbBase):
    """
    Used as an interface to query the database about labels.
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
        label_id = cursor.lastrowid
        return label_id

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
    def get_velp_label_ids(self, velp_id: 'int'):
        """
        Gets information of labels for one velp in specific language
        :param velp_id: ID of velp
        :return: List of labels associated with the velp
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      SELECT LabelInVelp.label_id
                      FROM LabelInVelp
                      WHERE LabelInVelp.velp_id = ?
                      """, [velp_id]
                       )
        return self.resultAsDictionary(cursor)

    @contract
    def add_labels_to_velp(self, velp_id: int, labels: 'list(int)'):
        """Associates a set of labels to a velp. (Appends to existing labels)

        :param velp_id: id of the velp that
        :param labels: list of label ids.
        :return: None
        """
        cursor = self.db.cursor()
        for label_id in labels:
            cursor.execute("""
                           INSERT INTO LabelInVelp(label_id, velp_id)
                           VALUES (?, ?)
                           """, [label_id, velp_id]
                           )
        self.db.commit()

    @contract
    def get_velp_label_ids(self, assessment_area: 'AssessmentArea') -> 'list(dict)':
        """Get labels for velps that are linked to an assessment area.

        :param assessment_area: the relevant assessment area
        :return: A list of dictionaries, each representing a single label-velp link.
        """
        cursor = self.db.cursor()
        cursor.execute("""
                       SELECT
                         LabelInVelp.velp_id,
                         LabelInVelp.label_id AS labels
                       FROM LabelInVelp
                       WHERE velp_id IN (
                       """ +
                       assessment_area.get_sql_for_velp_ids()
                       + """
                       )
                       ORDER BY velp_id ASC
                       """, assessment_area.get_parameters_for_velp_ids()
                       )
        results = self.resultAsDictionary(cursor)
        return results

    @contract
    def get_document_velp_label_ids(self, document_id: 'int') -> 'list(dict)':
        """Get labels for velps that are linked to a document.

        :param document_id: Id of the document
        :return: A list of dictionaries, each representing a single label-velp link.
        """
        return self.get_velp_label_ids(assessment_area_from_document(document_id))

    @contract
    def get_velp_label_content(self, assessment_area: 'AssessmentArea', language_id: 'str' = 'FI') -> 'list(dict)':
        """Get label content for labels that are linked to an assessment area.

        :param assessment_area: the relevant assessment area
        :param language_id: Optional, Id of the requested language of the content. Default is 'FI'
        :return: A list of dictionaries with each dictionary representing a single label.
        """
        cursor = self.db.cursor()
        cursor.execute("""
                       SELECT Label.id, Label.content
                       FROM Label
                       WHERE label.language_id = ? AND label.id IN (
                         SELECT DISTINCT LabelInVelp.label_id
                         FROM LabelInVelp
                         WHERE LabelInVelp.velp_id IN (
                       """ +
                       assessment_area.get_sql_for_velp_ids()
                       + """
                         )
                       )
                       """, [language_id] + assessment_area.get_parameters_for_velp_ids()
                       )
        results = self.resultAsDictionary(cursor)
        return results

    @contract
    def get_document_velp_label_content(self, document_id: 'int', language_id: 'str' = 'FI') -> 'list(dict)':
        """Get label content for labels that are linked to a document.

        :param document_id: ID for the document.
        :param language_id: Optional, Id of the requested language. Default is 'FI'.
        :return: List of labels, each label represented by a dictionary
        """
        return self.get_velp_label_content(assessment_area_from_document(document_id), language_id)

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
        self.db.commit()
