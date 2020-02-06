"""The module contains the database functions related to velps and velp labels. This includes adding and modifying velps
and their labels. The module also retrieves the data related to velps and their labels from the database.

:authors: Joonas Lattu, Petteri Palojärvi
:copyright: 2016 Timber project members
:version: 1.0.0

"""

import copy
from typing import Optional, List, Tuple, Dict

from timApp.timdb.sqa import db
from timApp.timdb.timdbbase import TimDbBase, result_as_dict_list
from timApp.velp.velp_models import Velp, VelpVersion, VelpLabel, VelpLabelContent, VelpContent, LabelInVelp


def _create_velp(creator_id: int, default_points: Optional[float], icon_id: Optional[int] = None,
                 valid_until: Optional[str] = None, visible_to: Optional[int] = None,
                 color: Optional[str] = None) -> int:
    """Creates a new entry to the velp table.

    :param creator_id: User ID of creator.
    :param default_points: Default points for velp.
    :param icon_id: Icon ID attached to velp. Can be null.
    :param valid_until: Time after velp becomes unusable.
    :param visible_to: Default visibility to annotation.
    :return: ID of velp that was just created.

    """
    if not visible_to:
        visible_to = 4
    v = Velp(creator_id=creator_id,
             default_points=default_points,
             icon_id=icon_id,
             color=color,
             valid_until=valid_until,
             visible_to=visible_to)
    db.session.add(v)
    db.session.commit()
    return v.id


def create_velp_version(velp_id: int):
    """Creates a new version for a velp to use.

    :param velp_id: ID of velp we're adding version for
    :return: ID of version that was just created

    """

    vv = VelpVersion(velp_id=velp_id)
    db.session.add(vv)
    db.session.commit()
    return vv.id


def add_velp_label_translation(label_id: int, language_id: str, content: str):
    """Adds new translation to an existing label.

    :param label_id: Label id
    :param language_id: Language chosen
    :param content: New translation

    """
    vlc = VelpLabelContent(velplabel_id=label_id, language_id=language_id, content=content)
    db.session.add(vlc)
    db.session.commit()


def update_velp_label(label_id: int, language_id: str, content: str):
    """Updates content of label in specific language.

    :param label_id: Label id
    :param language_id: Language chosen
    :param content: Updated content

    """
    vlc = VelpLabelContent.query.filter(
        (VelpLabelContent.velplabel_id == label_id) & (VelpLabelContent.language_id == language_id)
    ).one()
    vlc.content = content
    db.session.commit()


def create_velp_label(language_id: str, content: str) -> int:
    """Creates a new label.

    :param language_id: Language chosen
    :param content: Label content
    :return: id of the new label

    """

    vl = VelpLabel()
    db.session.add(vl)
    db.session.flush()
    add_velp_label_translation(vl.id, language_id, content)
    return vl.id


def create_velp_content(version_id: int, language_id: str, content: str, default_comment: str):
    """Method to create content (text) for velp.

    :param version_id: Version id where the content will be stored
    :param language_id: Language id
    :param content: Text of velp
    :param default_comment: Default comment for velp

    """
    vc = VelpContent(version_id=version_id, language_id=language_id, content=content, default_comment=default_comment)
    db.session.add(vc)
    db.session.commit()


def create_new_velp(creator_id: int, content: str, default_points: Optional[float] = None,
                    default_comment: Optional[str] = None, icon_id: Optional[int] = None,
                    valid_until: Optional[str] = None, language_id: str = "FI",
                    visible_to: Optional[int] = None, color: Optional[int] = None) -> Tuple[int, int]:
    """Creates a new velp with all information.

    Creates a new velp with all necessary information in one function using three others.

    :param default_comment: Default comment for velp
    :param creator_id: User ID of creator.
    :param content: Text for velp.
    :param default_points: Default points for velp, None if not given.
    :param icon_id: Icon ID attached to velp. Can be null.
    :param valid_until: Time after velp becomes unusable.
    :param language_id: Language ID of velp.
    :param visible_to: Default visibility to annotation.
    :param color: Velp color
    :return: A tuple of (velp id, velp version id).

    """
    new_velp_id = _create_velp(creator_id, default_points, icon_id, valid_until, visible_to, color)
    new_version_id = create_velp_version(new_velp_id)
    create_velp_content(new_version_id, language_id, content, default_comment)

    return new_velp_id, new_version_id


def update_velp(velp_id: int, default_points: str, icon_id: str, color: str, visible_to: int):
    """Changes the non-versioned properties of a velp. Does not update labels.

    :param velp_id: ID of velp that's being updated
    :param default_points: New default points
    :param icon_id: ID of icon
    :param color: Velp color
    :param visible_to: Velp visibility

    """
    if not visible_to:
        visible_to = 4
    v: Velp = Velp.query.get(velp_id)
    if v:
        v.default_points = default_points
        v.icon_id = icon_id
        v.color = color
        v.visible_to = visible_to
    db.session.commit()


def add_labels_to_velp(velp_id: int, labels: List[int]):
    """Associates a set of labels to a velp. (Appends to existing labels)

    Do note that update_velp_labels depends on this method

    :param velp_id: id of the velp that
    :param labels: list of label ids

    """

    if labels:  # Labels list can theoretically be null at some situations
        for label_id in labels:
            db.session.add(LabelInVelp(label_id=label_id, velp_id=velp_id))
    db.session.commit()


def update_velp_labels(velp_id: int, labels: List[int]):
    """Replaces the labels of a velp with new ones.

    :param velp_id: velp ID
    :param labels: list of label IDs.

    """
    # First nuke existing labels.
    LabelInVelp.query.filter_by(velp_id=velp_id).delete()
    # self.db.commit() # changes will be commited in add_labels_to_velp
    # Then add the new ones.
    add_labels_to_velp(velp_id, labels)


def get_latest_velp_version(velp_id: int, language_id: str = "FI") -> Optional[VelpContent]:
    """Method to fetch the latest version for velp in specific language.

    :param velp_id: ID of velp we're checking
    :param language_id: ID of language
    :return: Dictionary containing ID and content of velp version.

    """
    return (VelpContent.query
            .filter_by(language_id=language_id)
            .join(VelpVersion)
            .filter_by(velp_id=velp_id)
            .order_by(VelpVersion.id.desc())
            .with_entities(VelpContent)
            .first())


class Velps(TimDbBase):
    """Used as an interface to query the database about velps."""

    # Methods concerning velp labels

    def get_velp_label_ids_for_velp(self, velp_id: int) -> Dict:
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
        return result_as_dict_list(cursor)

    # Methods for getting information for document

    def get_velp_content_for_document(self, doc_id: int, user_id: int, language_id: str = 'FI') -> Dict:
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

    def get_velps_for_document(self, doc_id: int, user_id: int, language_id: str = 'FI') -> Dict:
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
                      SELECT Velp.id AS id, Velp.default_points AS points, Velp.icon_id AS icon_id, Velp.color AS color,
                      Velp.visible_to AS visible_to, y.content AS content, y.language_id AS language_id,
                      y.default_comment AS default_comment
                      FROM Velp
                      INNER JOIN(
                        SELECT x.velp_id, VelpContent.content, VelpContent.language_id, VelpContent.default_comment
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
        results = result_as_dict_list(cursor)
        return results

    def get_velp_group_ids_for_document(self, doc_id: int, user_id: int) -> Dict:
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
        results = result_as_dict_list(cursor)
        return results

    def get_velp_label_ids_for_document(self, doc_id: int, user_id: int) -> Dict:
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
        results = result_as_dict_list(cursor)
        return results

    def get_velp_label_content_for_document(self, doc_id: int, user_id: int, language_id: str = 'FI') -> Dict:
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
        results = result_as_dict_list(cursor)
        return results
