from contracts import contract
from timdb.timdbbase import TimDbBase, TimDbException


class VelpGroups(TimDbBase):
    @contract
    def create_velp_group(self, name: 'str', valid_until: 'str'):
        """
        Create a velp group
        :param name: Name of the created group
        :param valid_until: How long velp group is valid (null is forever)
        :return:
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      INSERT INTO
                      VelpGroup(name, valid_until)
                      VALUES (?, ?)
                      """, [name, valid_until]
                       )
        self.db.commit()

    @contract
    def update_velp_group(self, velp_group_id: 'int', name: 'str', valid_until: 'str'):
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

    @contract
    def delete_velp_group(self, velp_group_id: 'int'):
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

    @contract
    def add_velp_to_group(self, velp_id: 'int', velp_group_id: 'int'):
        """
        Adds a velp to a specific group
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

    @contract
    def remove_velp_from_group(self, velp_id: 'int', velp_group_id: 'int'):
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

    @contract
    def get_velps_from_group(self, velp_group_id: 'int'):
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

    @contract
    def get_velp_groups_in_assessment_area(self, document_id: 'int' = None, paragraph_id: 'str' = None,
                                          area_id: 'str' = None, folder_id: 'int' = None) -> 'list(int)':
        """
        Get all velp groups linked to this assessment area. Any and even all of the parameters can be null.
        :param document_id: Id of the document.
        :param paragraph_id: Id of the paragraph
        :param area_id: Id of the area
        :param folder_id: Id of the folder.
        :return: List of velp group ids.
        """
        cursor = self.db.cursor()
        cursor.execute("""
                       SELECT DISTINCT velp_group_id
                       FROM VelpGroupInAssessmentArea
                       WHERE document_id = ? OR (document_id = ? AND paragraph_id = ?) OR area_id = ? OR folder_id = ?
                       """, [document_id, document_id, paragraph_id, area_id, folder_id]
                       )
        results=self.resultAsDictionary(cursor)
        return results
