import copy
from typing import Optional, List

from tim_app import db
from timdb.timdbbase import TimDbBase
from timdb.velp_models import Velp, VelpVersion, VelpLabel, VelpLabelContent


class Velps(TimDbBase):
    """
    Used as an interface to query the database about velps.
    """

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

        v = Velp(creator_id=creator_id,
                 default_points=default_points,
                 icon_id=icon_id,
                 valid_until=valid_until)
        self.session.add(v)
        self.session.commit()
        return v.id

    def create_velp_version(self, velp_id: int):
        """Creates a new version for a velp to use.

        :param velp_id: ID of velp we're adding version for
        :return: ID of version that was just created
        """

        vv = VelpVersion(velp_id=velp_id)
        self.session.add(vv)
        self.session.commit()
        return vv.id

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
                      VALUES (%s, %s, %s)
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
                       SET icon_id = %s, default_points = %s
                       WHERE id = %s
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
                      SELECT version_id as id, content
                      FROM VelpContent
                      WHERE version_id IN
                      (
                      SELECT
                      MAX(id)
                      FROM VelpVersion
                      JOIN VelpContent ON VelpVersion.id = VelpContent.version_id
                      WHERE velp_id = %s AND language_id = %s
                      )
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


        vl = VelpLabel()
        self.session.add(vl)
        self.session.flush()
        self.add_velp_label_translation(vl.id, language_id, content)
        return vl.id

    def add_velp_label_translation(self, label_id: int, language_id: str, content: str):
        """Adds new translation to an existing label.

        :param label_id: Label id
        :param language_id: Language chosen
        :param content: New translation
        """
        vlc = VelpLabelContent(velplabel_id=label_id, language_id=language_id, content=content)
        self.session.add(vlc)
        self.session.commit()

    def update_velp_label(self, label_id: int, language_id: str, content: str):
        """Updates content of label in specific language.

        :param label_id: Label id
        :param language_id: Language chosen
        :param content: Updated content
        :return:
        """
        vlc = self.session.query(VelpLabelContent).filter((VelpLabelContent.velplabel_id == label_id) & (VelpLabelContent.language_id == language_id)).one()
        vlc.content = content
        self.session.commit()

    def get_velp_label_ids_for_velp(self, velp_id: int) -> dict():
        """Gets labels for one velp.

        :param velp_id: ID of velp
        :return: List of labels represented by their ids associated with the velp.
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      SELECT LabelInVelp.label_id
                      FROM LabelInVelp
                      WHERE LabelInVelp.velp_id = %s
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
        for label_id in labels:
            cursor.execute("""
                           INSERT INTO LabelInVelp(label_id, velp_id)
                           VALUES (%s, %s)
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
                       WHERE velp_id=%s
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
                      WHERE y.language_id = %s AND velp_id IN (
                        SELECT Velp.id
                        FROM Velp
                        WHERE (Velp.valid_until >= current_timestamp OR Velp.valid_until ISNULL) AND Velp.id IN (
                          SELECT VelpInGroup.velp_id
                          FROM VelpInGroup
                          WHERE VelpInGroup.velp_group_id IN (
                            SELECT velp_group_id
                            FROM VelpGroupsInDocument
                            WHERE user_id = %s AND doc_id = %s
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
                            WHERE doc_id = %s AND user_id = %s
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
                            WHERE doc_id = %s AND user_id = %s
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
                       SELECT VelpLabelContent.velplabel_id as id, VelpLabelContent.content
                       FROM VelpLabelContent
                       WHERE VelpLabelContent.language_id = %s AND VelpLabelContent.velplabel_id IN (
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
                                WHERE doc_id = %s AND user_id = %s
                              )
                            )
                          )
                       )
                       """, [language_id, doc_id, user_id]
                       )
        results = self.resultAsDictionary(cursor)
        return results
