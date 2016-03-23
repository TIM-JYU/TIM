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
