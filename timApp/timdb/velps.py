import copy
from sqlite3 import Connection
from typing import Optional, List
from timdb.timdbbase import TimDbBase, TimDbException


class Velps(TimDbBase):
    """
    Used as an interface to query the database about velps.
    """

    def __init__(self, db_path: Connection, files_root_path: str, type_name: str, current_user_name: str):
        """Initializes TimDB with the specified database and root path.

        :param type_name: The type name.
        :param current_user_name: The name of the current user.
        :param db_path: The path of the database file.
        :param files_root_path: The root path where all the files will be stored.
        """
        TimDbBase.__init__(self, db_path, files_root_path, type_name, current_user_name)

    def create_new_velp(self, creator_id: int, content=str, default_points: Optional[float] = None,
                        icon_id: Optional[int] = None, valid_until: Optional[str] = None,
                        language_id: str = "FI") -> int:
        """Creates a new velp with all information.

        Creates a new velp with all necessary information in one function using three others.
        :param creator_id: User ID of creator.
        :param content: Text for velp.
        :param default_points: Default points for velp, None if not given.
        :param icon_id: Icon ID attached to velp. Can be null.
        :param valid_until: Time after velp becomes unusable.
        :param language_id: Language ID of velp.
        :return: ID of the new velp.
        """
        new_velp_id = self._create_velp(creator_id, default_points, icon_id, valid_until)
        new_version_id = self.create_velp_version(new_velp_id)
        self.create_velp_content(new_version_id, language_id, content)
        return new_velp_id

    def _create_velp(self, creator_id: int, default_points: Optional[float], icon_id: Optional[int] = None,
                     valid_until: Optional[str] = None) -> int:
        """Creates a new entry to the velp table.

        :param creator_id: User ID of creator.
        :param default_points: Default points for velp.
        :param icon_id: Icon ID attached to velp. Can be null.
        :param valid_until: Time after velp becomes unusable.
        :return: ID of velp that was just created.
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

    def create_velp_version(self, velp_id: int):
        """Creates a new version for a velp to use.

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

    def create_velp_content(self, version_id: int, language_id: str, content: str):
        """Method to create content (text) for velp.

        :param version_id: Version id where the content will be stored
        :param language_id: Language id
        :param content: Text of velp
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      INSERT INTO
                      VelpContent(version_id, language_id, content)
                      VALUES (?, ?, ?)
                      """, [version_id, language_id, content]
                       )
        self.db.commit()

    def update_velp(self, velp_id: int, default_points: str, icon_id: str):
        """Changes the non-versioned properties of a velp. Does not update labels.

        :param velp_id: ID of velp that's being updated
        :param default_points: New default points
        :param icon_id: ID of icon
        """
        cursor = self.db.cursor()
        cursor.execute("""
                       UPDATE Velp
                       SET icon_id = ?, default_points = ?
                       WHERE id = ?
                       """, [icon_id, default_points, velp_id]
                       )
        self.db.commit()

    def get_latest_velp_version(self, velp_id: int, language_id: str = "FI"):
        """Method to fetch the latest version for velp in specific language

        :param velp_id: ID of velp we're checking
        :param language_id: ID of language
        :return: ID of version
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      SELECT
                      MAX(id),content
                      FROM
                      VelpInformation
                      WHERE velp_id = ? AND language_id = ?
                      """, [velp_id, language_id]
                       )
        row = cursor.fetchone()
        return {"id":row[0], "content":row[1]}

    # Methods concerning velp labels

    def create_velp_label(self, language_id: str, content: str) -> int:
        """Creates a new label.

        :param language_id: Language chosen
        :param content: Label content
        :return: id of the new label
        """

        #coalesce so that the first insert doesn't fail.
        cursor = self.db.cursor()
        cursor.execute("""
                      INSERT INTO
                      VelpLabel(language_id, content, id)
                      VALUES (?, ?, (SELECT
                      coalesce(MAX(VelpLabel.id)+1,1)
                      FROM VelpLabel))
                      """, [language_id, content]
                       )
        self.db.commit()
        label_id = cursor.lastrowid
        return label_id

    def add_velp_label_translation(self, label_id: int, language_id: str, content: str):
        """Adds new translation to an existing label.

        :param label_id: Label id
        :param language_id: Language chosen
        :param content: New translation
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      INSERT INTO
                      VelpLabel(id, language_id, content)
                      VALUES (?, ?, ?)
                      """, [label_id, language_id, content]
                       )
        self.db.commit()

    def update_velp_label(self, label_id: int, language_id: str, content: str):
        """Updates content of label in specific language.

        :param label_id: Label id
        :param language_id: Language chosen
        :param content: Updated content
        :return:
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      UPDATE VelpLabel
                      SET content = ?
                      WHERE id = ? AND language_id = ?
                      """, [content, label_id, language_id]
                       )
        self.db.commit()

    def get_velp_label_ids_for_velp(self, velp_id: int) -> dict():
        """Gets labels for one velp.

        :param velp_id: ID of velp
        :return: List of labels represented by their ids associated with the velp.
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      SELECT LabelInVelp.label_id
                      FROM LabelInVelp
                      WHERE LabelInVelp.velp_id = ?
                      """, [velp_id]
                       )
        return self.resultAsDictionary(cursor)

    def add_labels_to_velp(self, velp_id: int, labels: List[int]):
        """Associates a set of labels to a velp. (Appends to existing labels)

        Do note that update_velp_labels depends on this method
        :param velp_id: id of the velp that
        :param labels: list of label ids
        :return: None
        """
        cursor = self.db.cursor()
        if labels:  # Labels list can theoretically be null at some situations
            for label_id in labels:
                cursor.execute("""
                               INSERT INTO LabelInVelp(label_id, velp_id)
                               VALUES (?, ?)
                               """, [label_id, velp_id]
                               )
            self.db.commit()

    def update_velp_labels(self, velp_id: int, labels: List[int]):
        """Replaces the labels of a velp with new ones.

        :param velp_id:
        :param labels: list of label ids.
        """
        cursor = self.db.cursor()
        # First nuke existing labels.
        cursor.execute("""
                       DELETE FROM LabelInVelp
                       WHERE velp_id=?
                       """, [velp_id]
                       )
        # self.db.commit() # changes will be commited in add_labels_to_velp
        # Then add the new ones.
        self.add_labels_to_velp(velp_id, labels)


    # Methods for getting information for document

    def get_velp_content_for_document(self, doc_id: int, user_id: int, language_id: str = 'FI') -> dict():
        """Gets velp content including labels and velp groups for document.

        Uses VelpGroupsInDocument table data to determine which velp groups are usable
        for specific user in specific document.
        :param doc_id: ID of document in question
        :param user_id: ID of current user
        :param language_id: ID of language used
        :return: List of velps as dictionaries
        """
        velp_data = self.get_velps_for_document(doc_id, user_id, language_id)
        label_data = self.get_velp_label_ids_for_document(doc_id, user_id)
        group_data = self.get_velp_group_ids_for_document(doc_id, user_id)

        # Sort velp label data as lists and link them to velp data
        if velp_data and label_data:
            velp_id = label_data[0]['velp_id']
            list_help = []
            label_dict = {}
            for i in range(len(label_data)):
                next_id = label_data[i]['velp_id']
                if next_id != velp_id:
                    label_dict[velp_id] = copy.deepcopy(list_help)
                    velp_id = next_id
                    del list_help[:]
                    list_help.append(label_data[i]['labels'])
                else:
                    list_help.append(label_data[i]['labels'])
                if i == len(label_data) - 1:
                    label_dict[velp_id] = copy.deepcopy(list_help)
            for i in range(len(velp_data)):
                search_id = velp_data[i]['id']
                if search_id in label_dict:
                    velp_data[i]['labels'] = label_dict[search_id]

        # Sort velp group data as lists and link them to velp data
        if velp_data and group_data:
            velp_id = group_data[0]['velp_id']
            list_help2 = []
            label_dict2 = {}
            for i in range(len(group_data)):
                next_id = group_data[i]['velp_id']
                if next_id != velp_id:
                    label_dict2[velp_id] = copy.deepcopy(list_help2)
                    velp_id = next_id
                    del list_help2[:]
                    list_help2.append(group_data[i]['velp_groups'])
                else:
                    list_help2.append(group_data[i]['velp_groups'])
                if i == len(group_data) - 1:
                    label_dict2[velp_id] = copy.deepcopy(list_help2)
            for i in range(len(velp_data)):
                search_id = velp_data[i]['id']
                if search_id in label_dict2:
                    velp_data[i]['velp_groups'] = label_dict2[search_id]

        return velp_data

    def get_velps_for_document(self, doc_id: int, user_id: int, language_id: str = 'FI') -> dict():
        """Gets velps for document.

        Uses VelpGroupsInDocument table data to determine which velp groups and via those which velps are usable
        for specific user in specific document.
        :param doc_id: ID of document in question
        :param user_id: ID of current user
        :param language_id: ID of language used
        :return: List of dicts containing velp ids, default points, content, icon ids and language ids
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
                        SELECT Velp.id
                        FROM Velp
                        WHERE (Velp.valid_until >= current_timestamp OR Velp.valid_until ISNULL) AND Velp.id IN (
                          SELECT VelpInGroup.velp_id
                          FROM VelpInGroup
                          WHERE VelpInGroup.velp_group_id IN (
                            SELECT velp_group_id
                            FROM VelpGroupsInDocument
                            WHERE user_id = ? AND doc_id = ?
                          )
                        )
                      )

                      """, [language_id, user_id, doc_id]
                      )
        results = self.resultAsDictionary(cursor)
        return results

    def get_velp_group_ids_for_document(self, doc_id: int, user_id: int) -> dict():
        """Gets velp group ids for document.

        Uses VelpGroupsInDocument table data to determine which velp groups are usable
        for specific user in specific document.
        :param doc_id: ID of document in question
        :param user_id: ID of current user
        :return: List of dicts containing velp ids and velp groups ids
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      SELECT
                        velp_id,
                        velp_group_id AS velp_groups
                      FROM VelpInGroup
                      WHERE velp_id IN (
                        SELECT Velp.id
                        FROM Velp
                        WHERE (Velp.valid_until >= current_timestamp OR Velp.valid_until ISNULL) AND Velp.id IN (
                          SELECT VelpInGroup.velp_id
                          FROM VelpInGroup
                          WHERE VelpInGroup.velp_group_id IN (
                            SELECT velp_group_id
                            FROM VelpGroupsInDocument
                            WHERE doc_id = ? AND user_id = ?
                          )
                        )
                      )
                      ORDER BY velp_id ASC
                      """, [doc_id, user_id]
                      )
        results = self.resultAsDictionary(cursor)
        return results

    def get_velp_label_ids_for_document(self, doc_id: int, user_id: int) -> dict():
        """Gets velp labels ids for document.

        Uses VelpGroupsInDocument table data to determine which velp groups and via those which velp labels are usable
        for specific user in specific document.
        :param doc_id: ID of document in question
        :param user_id: ID of current user
        :return: List of dicts containing velp ids and label ids
        """

        cursor = self.db.cursor()
        cursor.execute("""
                      SELECT
                        LabelInVelp.velp_id,
                        LabelInVelp.label_id AS labels
                      FROM LabelInVelp
                      WHERE velp_id IN (
                        SELECT Velp.id
                        FROM Velp
                        WHERE (Velp.valid_until >= current_timestamp OR Velp.valid_until ISNULL) AND Velp.id IN (
                          SELECT VelpInGroup.velp_id
                          FROM VelpInGroup
                          WHERE VelpInGroup.velp_group_id IN (
                            SELECT velp_group_id
                            FROM VelpGroupsInDocument
                            WHERE doc_id = ? AND user_id = ?
                          )
                        )
                      )
                      ORDER BY velp_id ASC
                      """, [doc_id, user_id]
                      )
        results = self.resultAsDictionary(cursor)
        return results

    def get_velp_label_content_for_document(self, doc_id: int, user_id: int, language_id: str = 'FI') -> dict():
        """Gets velp label content for document.

        Uses VelpGroupsInDocument table data to determine which velp groups and via those which velp labels are usable
        for specific user in specific document.
        :param doc_id: ID of document in question
        :param user_id: ID of current user
        :param language_id: ID of language used
        :return: List of dicts containing velp label ids and content
        """
        cursor = self.db.cursor()
        cursor.execute("""
                       SELECT VelpLabel.id, VelpLabel.content
                       FROM VelpLabel
                       WHERE VelpLabel.language_id = ? AND VelpLabel.id IN (
                         SELECT DISTINCT LabelInVelp.label_id
                         FROM LabelInVelp
                         WHERE LabelInVelp.velp_id IN (
                          SELECT Velp.id
                          FROM Velp
                            WHERE (Velp.valid_until >= current_timestamp OR Velp.valid_until ISNULL) AND Velp.id IN (
                              SELECT VelpInGroup.velp_id
                              FROM VelpInGroup
                              WHERE VelpInGroup.velp_group_id IN (
                                SELECT velp_group_id
                                FROM VelpGroupsInDocument
                                WHERE doc_id = ? AND user_id = ?
                              )
                            )
                          )
                       )
                       """, [language_id, doc_id, user_id]
                       )
        results = self.resultAsDictionary(cursor)
        return results
