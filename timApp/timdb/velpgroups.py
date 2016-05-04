from typing import Optional, List

# from timdb.folders import *

from timdb.timdbbase import TimDbBase, TimDbException, blocktypes

class VelpGroups(TimDbBase):
    def create_default_velp_group(self, name: str, owner_group_id: int, velp_group_id: Optional[int] = None):
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

    def create_velp_group(self, name: str, owner_group_id: int, valid_until: Optional[str] = None, velp_group_id: int = None):
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

    def create_default_velp_group2(self, name: str, owner_group_id: int, velp_group_id: int):
        """Creates default velp group where all velps used in document are stored.

        Make sure you have made a new document (needed for rights management) and use its id as velp_group_id
        :param name: Name of the new default velp group.
        :param owner_group_id: The id of the owner group.
        :param velp_group_id: ID
        :return:
        """
        cursor = self.db.cursor()


        valid_until = None
        cursor.execute("""
                      INSERT INTO
                      VelpGroup(id, name, valid_until, document_def)
                      VALUES (?, ?, ?, ?)
                      """, [velp_group_id, name, valid_until, 1]
                       )
        self.db.commit()
        return velp_group_id

    def create_velp_group2(self, name: str, owner_group_id: int, velp_group_id: int, valid_until: Optional[str] = None):
        """Create a velp group

        Make sure you have made a new document (needed for rights management) and use its id as velp_group_id
        :param name: Name of the created group.
        :param owner_group_id: The id of the owner group.
        :param velp_group_id: ID
        :param valid_until: How long velp group is valid (None is forever).
        :return:
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      INSERT INTO
                      VelpGroup(id, name, valid_until)
                      VALUES (?, ?, ?)
                      """, [velp_group_id, name, valid_until]
                       )
        self.db.commit()
        return velp_group_id

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

    def get_velp_group_name(self, velp_group_id: int) -> str:
        cursor = self.db.cursor()
        cursor.execute('SELECT name FROM VelpGroup WHERE id = ?'), [velp_group_id]
        result = cursor.fetchone()
        return result[0] if result is not None else None

    # TODO: Delete of find out fix
    def check_velp_group_folder_path(self, root_path: str, owner_group_id: int):
        """ Checks if velp group folder path exists and if not, creates it

        :param root_path: Root path where method was called from
        :param owner_group_id: Owner group ID for the new folder if one is to be created
        :return: Path for velp group folder
        """
        group_folder_name = "velp groups"
        velps_folder_path = root_path + "/" + group_folder_name
        folders = Folders.get_folders(root_path)
        velps_folder = False
        # Check if velps folder exist
        for folder in folders:
            if folder['name'] == group_folder_name:
                print("ASD")
                velps_folder = True
        #
        if velps_folder is False:
            new_block = self.Folders.create(velps_folder_path, owner_group_id)
            print("Created new folder, id: " + str(new_block))

        return velps_folder_path

# TODO Delete or update down below, not functional

    def get_velp_groups_in_folders(self, root_path: str):
        # Check help from tim.py and timApp.js
        current_path = root_path
        return

    def create_velp_group_and_document(self, root_path: str, name: str, owner_group_id: int):
        current_path = root_path


        return

    def is_id_velp_group(self, doc_id: int) -> bool:
        cursor = self.db.cursor()
        cursor.execute('SELECT id FROM VelpGroup WHERE id = ?'), [doc_id]
        result = cursor.fetchone()
        return True if result is not None else False