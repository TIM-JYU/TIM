from typing import Optional, List

from contracts import contract, new_contract
from typing import Optional

import hashlib
import sqlite3

new_contract('row', sqlite3.Row)

from timdb.timdbbase import TimDbBase, TimDbException, blocktypes
from timdb.documents import *

class VelpGroups(Documents):

    def create_default_velp_group(self, name: str, owner_group_id: int, default_group_path: str):
        """Creates default velp group for document.

        :param name: Name of the new default velp group.
        :param owner_group_id: The id of the owner group.
        :param velp_group_id: ID
        :return:
        """

        # Create new document and add its ID to VelpGroupTable
        new_group = self.create(default_group_path, owner_group_id)
        new_group_id = new_group.doc_id
        valid_until = None
        cursor = self.db.cursor()
        cursor.execute("""
                      INSERT INTO
                      VelpGroup(id, name, valid_until, document_def)
                      VALUES (?, ?, ?, ?)
                      """, [new_group_id, name, valid_until, 1]
                       )
        self.db.commit()
        return new_group_id

    def create_velp_group(self, name: str, owner_group_id: int, new_group_path: str, valid_until: Optional[str] = None):
        """Create a velp group

        :param name: Name of the created group.
        :param owner_group_id: The id of the owner group.
        :param valid_until: How long velp group is valid (None is forever).
        :return:
        """

        # Create new document and add its ID to VelpGroupTable
        new_group = self.create(new_group_path, owner_group_id)
        new_group_id = new_group.doc_id
        cursor = self.db.cursor()
        cursor.execute("""
                      INSERT INTO
                      VelpGroup(id, name, valid_until)
                      VALUES (?, ?, ?)
                      """, [new_group_id, name, valid_until]
                       )
        self.db.commit()
        return new_group_id

    def make_document_a_velp_group(self, name: str, velp_group_id: int, valid_until: Optional[str] = None):
        """Adds document to VelpGroup table

        :param name: Name of the created group.
        :param velp_group_id: ID of new velp group (and existing document)
        :param valid_until: How long velp group is valid (None is forever).
        :return:
        """
        cursor = self.db.cursor()

        cursor.execute("""
                      INSERT OR IGNORE INTO
                      VelpGroup(id, name, valid_until)
                      VALUES (?, ?, ?)
                      """, [velp_group_id, name, valid_until]
                       )
        self.db.commit()
        return velp_group_id

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

    def add_velp_to_groups(self, velp_id: int, velp_group_ids: [int]):
        """Adds a velp to specific groups

        :param velp_id: ID of velp
        :param velp_group_ids: List of velp group IDs
        :return:
        """
        cursor = self.db.cursor()
        for velp_group in velp_group_ids:
            cursor.execute("""
                          INSERT INTO
                          VelpInGroup(velp_group_id, velp_id)
                          VALUES (?, ?)
                          """, [velp_group, velp_id]
                          )
        self.db.commit()

    def remove_velp_from_group(self, velp_id: int, velp_group_id: int):
        """Removes a velp from a specific group

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

    def remove_velp_from_groups(self, velp_id: int, velp_group_ids: [int]):
        """Removes a velp from specific groups

        :param velp_id: ID of velp
        :param velp_group_ids: List of velp group IDs
        :return:
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      DELETE
                      FROM VelpInGroup
                      WHERE velp_id = ? AND velp_group_id = ({})
                      """.format(self.get_sql_template(velp_group_ids)), [velp_id] + velp_group_ids
                      )
        self.db.commit()

    def get_velp_group_name(self, velp_group_id: int) -> str:
        cursor = self.db.cursor()
        cursor.execute('SELECT name FROM VelpGroup WHERE id = ?', [velp_group_id])
        result = cursor.fetchone()
        return result[0] if result is not None else None

    def get_groups_for_velp(self, velp_id):
        cursor = self.db.cursor()
        cursor.execute('SELECT velp_group_id AS id FROM VelpInGroup WHERE velp_id = ?', [velp_id])
        result = cursor.fetchall()
        return result

    def is_id_velp_group(self, doc_id: int) -> bool:
        """ Checks whether given document id can also be found from VelpGroup table

        :param doc_id: ID of document
        :return: True if part of VelpGroup table, else False
        """
        cursor = self.db.cursor()
        cursor.execute('SELECT name FROM VelpGroup WHERE id = ?', [doc_id])
        result = cursor.fetchone()
        return True if result is not None else False


    def add_group_to_imported_table(self, user_group: int, doc_id: int, target_type: int, target_id: int,
                                    velp_group_id: int):
        """Adds velp groups to ImportedVelpGroups table for specific document / user group combo

        :param user_group: ID of user group
        :param doc_id: Id of document
        :param target_type: Which kind of area group targets to (0 doc, 1 paragraph, 2 area)
        :param target_id:  ID of target (0 for documents)
        :param velp_group_id: ID of velp group
        :return:
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      INSERT OR IGNORE INTO
                      ImportedVelpGroups(user_group, doc_id, target_type, target_id, velp_group_id)
                      VALUES (?, ?, ?, ?, ?)
                      """, [user_group, doc_id, target_type, target_id, velp_group_id]
                      )
        self.db.commit()

        return


    def get_groups_from_imported_table(self, user_groups: [int], doc_id: int):
        """Gets velp groups from ImportedVelpGroups table for specific document / user group IDs combo

        :param user_groups: List of user group IDs
        :param doc_id: ID of document
        :return:
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      SELECT
                      user_group, doc_id, target_type, target_id, velp_group_id as id
                      FROM ImportedVelpGroups
                      WHERE doc_id = ? AND user_group IN ({})
                      """.format(self.get_sql_template(user_groups)), [doc_id] + user_groups
                      )
        results = self.resultAsDictionary(cursor)
        return results

    def add_groups_to_selection_table(self, velp_groups: dict, doc_id: int, user_id: int):
        """Adds velp groups to VelpGroupSelection table

        :param velp_groups: Velp groups as dictionaries
        :param doc_id: ID of document
        :param user_id: ID of user
        :return:
        """
        cursor = self.db.cursor()
        for velp_group in velp_groups:
            target_type = velp_group['target_type']
            target_id = velp_group['target_id']
            selected = 1
            velp_group_id = velp_group['id']
            cursor.execute("""
                          INSERT OR IGNORE INTO
                          VelpGroupSelection(user_id, doc_id, target_type, target_id, selected, velp_group_id)
                          VALUES (?, ?, ?, ?, ?, ?)
                          """, [user_id, doc_id, target_type, target_id, selected, velp_group_id]
                           )
        self.db.commit()

    def get_groups_from_selection_table(self, doc_id: int, user_id: int):
        """Gets velp groups from VelpGroupSelection table of specific document / user combo

        :param doc_id: ID of document
        :param user_id: ID of user
        :return:
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      SELECT
                      target_type, target_id, velp_group_id as id,
                      VelpGroup.name, DocEntry.name AS location
                      FROM VelpGroupSelection
                      JOIN VelpGroup ON VelpGroup.id = VelpGroupSelection.velp_group_id
                      JOIN DocEntry ON DocEntry.id = VelpGroupSelection.velp_group_id
                      WHERE doc_id = ? AND user_id = ?
                      """, [doc_id, user_id]
                      )
        results = self.resultAsDictionary(cursor)
        return results

    def change_selection(self, doc_id: int, velp_group_id: int, user_id: int, selected: bool):
        """Changes selection for velp group in VelpGroupSelection for specific user / document combo

        :param doc_id: ID of document
        :param velp_group_id: ID of velp group
        :param user_id: ID of user
        :param selected: Boolean whether group is selected or not
        :return:
        """
        cursor = self.db.cursor()
        if selected is True:
            cursor.execute("""
                          UPDATE VelpGroupSelection
                            SET selected = ?
                            WHERE user_id = ? AND doc_id
                            """, [selected, user_id, doc_id]
                          )


    # Unused methods

    def update_velp_group(self, velp_group_id: int, name: str, valid_until: Optional[str]):
        """Updates name and/or valid until time of velp group

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




    # TODO Outdated methods?


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

    def get_velps_from_groups(self, velp_groups: [int]):
        results = []
        for group in velp_groups:
            results.append(self.get_velps_from_group(group))
        return results

    def check_default_group_exists(self, document_id) -> Optional[int]:
        """Checks whether document has default velp group attached.

        :param document_id: The id of document.
        :return: id of the default group in a dictionary, none if nothing found.
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
        if not result:
            return None
        else:
            return result

    def insert_group_to_document(self, velp_group_id: int, document_id: int):
        cursor = self.db.cursor()
        cursor.execute("""
                       INSERT INTO
                       VelpGroupInDocument(velp_group_id, document_id)
                       VALUES (?, ?)
                       """, [velp_group_id, document_id]
                       )
        self.db.commit()

    def create_default_velp_group_orig(self, name: str, owner_group_id: int, velp_group_id: Optional[int] = None):
        """Creates default velp group where all velps used in document are stored.

        :param name: Name of the new default velp group.
        :param owner_group_id: The id of the owner group.
        :param valid_until: Valid forever, thus None.
        :return:
        """
        cursor = self.db.cursor()
        # Velp groups are stored as documents as well, if this changes, change the block type
        velp_group_id = self.insertBlockToDb(name, owner_group_id, blocktypes.DOCUMENT)
        valid_until = None
        cursor.execute("""
                      INSERT INTO
                      VelpGroup(id, name, valid_until, document_def)
                      VALUES (?, ?, ?, ?)
                      """, [velp_group_id, name, valid_until, 1]
                       )
        self.db.commit()
        return velp_group_id

    def create_velp_group_orig(self, name: str, owner_group_id: int, valid_until: Optional[str] = None, velp_group_id: int = None):
        """Create a velp group

        :param name: Name of the created group.
        :param owner_group_id: The id of the owner group.
        :param valid_until: How long velp group is valid (None is forever).
        :return:
        """
        cursor = self.db.cursor()
        # Velp groups are stored as documents as well, if this changes, change the block type
        velp_group_id = self.insertBlockToDb(name, owner_group_id, blocktypes.DOCUMENT)
        cursor.execute("""
                      INSERT INTO
                      VelpGroup(id, name, valid_until)
                      VALUES (?, ?, ?)
                      """, [velp_group_id, name, valid_until]
                       )
        self.db.commit()
        return velp_group_id


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
