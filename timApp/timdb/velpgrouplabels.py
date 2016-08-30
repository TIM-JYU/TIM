"""
The module contains the database functions related to labels that are used in velp groups.
This includes adding, modifying and deleting velp group labels. The module also retrieves
the velp group labels from the database. The module is not yet used in production.

:authors: Joonas Lattu, Petteri Palojärvi
:copyright: 2016 Timber project members
:version: 1.0.0

"""

from sqlite3 import Connection
from timdb.timdbbase import TimDbBase, TimDbException


class VelpGroupLabels(TimDbBase):
    def __init__(self, db_path: 'Connection', files_root_path: str, type_name: str, current_user_name: str):
        """Initializes TimDB with the specified database and root path.

        :param type_name: The type name.
        :param current_user_name: The name of the current user.
        :param db_path: The path of the database file.
        :param files_root_path: The root path where all the files will be stored.
        """
        TimDbBase.__init__(self, db_path, files_root_path, type_name, current_user_name)

    def create_velp_group_label(self, language_id: str, content: str) -> int:
        """
        Creates a new label

        :param language_id: Language chosen
        :param content: Label content
        :return: id of the new label.
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      INSERT INTO
                      VelpGroupLabel(language_id, content)
                      VALUES (?, ?)
                      """, [language_id, content]
                       )
        self.db.commit()
        return cursor.lastrowid

    def add_translation(self, label_id: int, language_id: str, content: str):
        """
        Adds new translation to an existing label

        :param label_id: Label id
        :param language_id: Language chosen
        :param content: New translation
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      INSERT INTO
                      VelpGroupLabel(id, language_id, content)
                      VALUES (?, ?, ?)
                      """, [label_id, language_id, content]
                       )
        self.db.commit()

    def update_velp_group_label(self, label_id: int, language_id: str, content: str):
        """
        Updates content of label in specific language

        :param label_id: Label id
        :param language_id: Language chosen
        :param content: Updated content
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      UPDATE VelpGroupLabel
                      SET content = ?
                      WHERE id = ? AND language_id = ?
                      """, [content, label_id, language_id]
                       )
        self.db.commit()

    def get_velp_group_labels(self, velp_id: int, language_id: str):
        """
        Gets information of labels for one velp in specific language

        :param velp_id: ID of velp
        :param language_id: Language chosen
        :return: List of labels associated with velp as a dictionary
        """
        cursor = self.db.cursor()
        # todo get label content also. return something.
        cursor.execute("""
                      SELECT *
                      FROM VelpGroupLabel
                      WHERE language_id = ? AND (id IN
                      (SELECT velp_id FROM LabelInVelpGroup WHERE velp_id = ?))
                      """, [language_id, velp_id]
                       )
        return self.resultAsDictionary(cursor)

    def delete_velp_group_label(self, label_id):
        """
        Deletes label (use with extreme caution)

        :param label_id: Label ID
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      DELETE
                      FROM VelpGroupLabel
                      WHERE id = ?
                      """, [label_id]
                       )
