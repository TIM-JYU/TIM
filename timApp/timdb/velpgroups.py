from typing import Optional, List

from timdb.timdbbase import TimDbBase, TimDbException, blocktypes


class VelpGroups(TimDbBase):
    def create_default_velp_group(self, name: str, owner_group_id: int, valid_until: None):
        """Creates default velp group where all velps used in document are stored.

        :param name: Name of the new default velp group.
        :param owner_group_id: The id of the owner group.
        :param valid_until: Valid forever, thus None.
        :return:
        """
        cursor = self.db.cursor()
        velp_group_id = self.insertBlockToDb(name, owner_group_id, blocktypes.VELPGROUP)
        cursor.execute("""
                      INSERT INTO
                      VelpGroup(id, name, valid_until, document_def)
                      VALUES (?, ?, ?, ?)
                      """, [velp_group_id, name, valid_until, 1]
                       )
        self.db.commit()
        return velp_group_id

    def create_velp_group(self, name: str, owner_group_id: int, valid_until: Optional[str] = None):
        """Create a velp group

        :param name: Name of the created group.
        :param owner_group_id: The id of the owner group.
        :param valid_until: How long velp group is valid (None is forever).
        :return:
        """
        cursor = self.db.cursor()
        velp_group_id = self.insertBlockToDb(name, owner_group_id, blocktypes.VELPGROUP)
        cursor.execute("""
                      INSERT INTO
                      VelpGroup(id, name, valid_until)
                      VALUES (?, ?, ?)
                      """, [velp_group_id, name, valid_until]
                       )
        self.db.commit()
        return  velp_group_id

    def update_velp_group(self, velp_group_id: int, name: str, valid_until: Optional[str]):
        """
        Updates name and/or valid until time of velp group
        :param velp_group_id: Velp group id
        :param name: Name of velp group
        :param valid_until: How long velp group is valid
        :return:
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      UPDATE VelpGroup
                      SET name = ? AND  valid_until = ?
                      WHERE id = ?
                      """, [name, valid_until, velp_group_id]
                       )
        self.db.commit()

    def copy_velp_group(self, copied_group_id: int, new_group_id):
        cursor = self.db.cursor()
        cursor.execute("""
                      INSERT INTO
                      VelpInGroup(velp_group_id, velp_id, points)
                      SELECT
                      ?, velp_id, points
                      FROM
                      VelpInGroup
                      WHERE velp_group_id = ?
                      """, [new_group_id, copied_group_id]
                       )
        self.db.commit()

    def delete_velp_group(self, velp_group_id: int):
        """
        Deletes velp group

        Doesn't delete velps belonging to group, only their links to deleted group
        :param velp_group_id: Velp group id
        :return:
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      DELETE
                      FROM VelpGroup
                      WHERE  id = ?;
                      DELETE
                      FROM VelpInGroup
                      WHERE velp_group_id = ?
                      """, [velp_group_id, velp_group_id]
                       )

    def add_velp_to_group(self, velp_id: int, velp_group_id: int):
        """Adds a velp to a specific group

        :param velp_id: Velp if
        :param velp_group_id: Velp group id
        :return:
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      INSERT INTO
                      VelpInGroup(velp_group_id, velp_id)
                      VALUES (?, ?)
                      """, [velp_group_id, velp_id]
                       )
        self.db.commit()

    def remove_velp_from_group(self, velp_id: int, velp_group_id: int):
        """
        Removes a velp from a specific group
        :param velp_id: Velp id
        :param velp_group_id: Velp group id
        :return:
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      DELETE
                      FROM VelpInGroup
                      WHERE velp_id = ? AND velp_group_id = ?
                      """, [velp_id, velp_group_id]
                       )
        self.db.commit()

    def get_velps_from_group(self, velp_group_id: int):
        """
        Gets velps belonging to specific group
        :param velp_group_id: Velp group id
        :return: Returns velp ids as JSON
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      SELECT VelpInGroup.velp_id
                      FROM VelpInGroup
                      WHERE velp_group_id = ?
                      """, [velp_group_id]
                       )
        return self.resultAsDictionary(cursor)

    def check_default_group_exists(self, document_id):
        """Checks whether document has default velp group attached.

        :param document_id: The id of document.
        :return: 0 if doesn't exist, else ID as dictionary
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      SELECT DISTINCT
                      id
                      FROM VelpGroup
                      INNER JOIN
                        VelpGroupInDocument
                        WHERE document_def = 1 AND document_id = ?
                          AND id = velp_group_id;
                      """, [document_id]
                       )
        result = self.resultAsDictionary(cursor)
        if len(result) == 0:
            return 0
        return result


    def get_velp_groups_in_assessment_area(self, document_id: int = None, paragraph_id: str = None,
                                           area_id: str = None, folder_id: int = None) -> List[int]:
        """Get all velp groups linked to this assessment area. Any and even all of the parameters can be null.

        :param document_id: Id of the document.
        :param paragraph_id: Id of the paragraph.
        :param area_id: Id of the area.
        :param folder_id: Id of the folder.
        :return: List of velp group ids together with their location info.
        """
        cursor = self.db.cursor()
        cursor.execute("""
                       SELECT DISTINCT velp_group_id, document_id, paragraph_id, area_id, folder_id
                       FROM VelpGroupInAssessmentArea
                       WHERE document_id = ? OR (document_id = ? AND paragraph_id = ?) OR area_id = ? OR folder_id = ?
                       """, [document_id, document_id, paragraph_id, area_id, folder_id]
                       )
        results = self.resultAsDictionary(cursor)
        return results

    def insert_group_to_document(self, velp_group_id: int, document_id: int):
        cursor = self.db.cursor()
        cursor.execute("""
                       INSERT INTO
                       VelpGroupInDocument(velp_group_id, document_id)
                       VALUES (?, ?)
                       """, [velp_group_id, document_id]
                       )
        self.db.commit()
