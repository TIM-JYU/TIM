from contracts import contract
from timdb.timdbbase import TimDbBase
from assesment_area import AssessmentArea


class Annotations(TimDbBase):
    """
    Used as an interface to query the database about annotations.
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
    def get_annotations(self, document_id: int, paragraph_id: str) -> 'list(dict)':
        """
        Gets all annotations made in a given area.
        :param document_id: The relevant document.
        :param paragraph_id: The relevant paragraph in the document.
        :return: List of dictionaries, each dictionary representing a single annotation.
        """
        cursor = self.db.cursor()
        cursor.execute("""
                       SELECT
                         Annotation.id,
                         VelpVersion.velp_id,
                         Annotation.icon_id,
                         Annotation.points,
                         Annotation.creation_time,
                         Annotation.annotator_id,
                         Annotation.element_number,
                         Annotation.place_start,
                         Annotation.place_end
                       FROM Annotation
                         INNER JOIN VelpVersion ON VelpVersion.id = Annotation.version_id
                       WHERE (Annotation.valid_until ISNULL OR
                             Annotation.valid_until >= CURRENT_TIMESTAMP) AND
                             Annotation.document_id = ? AND
                             Annotation.paragraph_id = ?
                       """, [document_id, paragraph_id]
                       )
        return self.resultAsDictionary(cursor)

    @contract
    def create_annotation(self, version_id: 'int', points: 'float', place_start: 'int', place_end: 'int',
                          annotator_id: 'int', document_id: int, paragraph_id: 'str', element_number: 'int|None',
                          icon_id: 'int | None' = None,
                          answer_id: 'int | None' = None):
        """
        Create new annotation
        :param version_id: version of the velp that the annotation uses
        :param document_id: Document id     \
        :param paragraph_id: Paragraph id   - either bot document and paragraph or answer are null
        :param answer_id: Answer id         /
        :param element_number Number of the html element from which we start counting.
        :param place_start: start
        :param place_end: end
        :param annotator_id: id of user who left the annotation
        :param points: Points given, overrides velp's default and can be null
        :param icon_id: Icon id, can be null
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      INSERT INTO
                      Annotation(version_id, document_id, paragraph_id, answer_id, place_start,
                      place_end, element_number, annotator_id, points, icon_id)
                      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                      """, [version_id, document_id, paragraph_id, answer_id, place_start,
                            place_end, element_number, annotator_id, points, icon_id]
                       )
        self.db.commit()

    @contract
    def update_annotation(self, version_id: 'int', points: 'float', place_start: 'int', place_end: 'int',
                          annotator_id: 'int', icon_id: 'int | None' = None):
        """Changes an existing annotation.

        :param version_id: version of the velp that the annotation uses
        :param points:
        :param place_start:
        :param place_end:
        :param annotator_id:
        :param icon_id:
        :return:
        """
        cursor = self.db.cursor()
        cursor.execute("""

                      """
                       )
        return

    @contract
    def invalidate_annotation(self, annotation_id: 'int', valid_until: 'str | None' = None):
        cursor = self.db.cursor()

        if valid_until is None:  # None if we want to invalidate immediately
            cursor.execute("""
                          UPDATE
                          Annotation
                          SET
                          valid_until = current_timestamp
                          WHERE Annotation.id = ?
                          """, [annotation_id]
                           )
        else:  # Else invalidate at some specific time
            cursor.execute("""
                          UPDATE
                          Annotation
                          SET
                          valid_until = ?
                          WHERE Annotation.id = ?
                          """, [valid_until, annotation_id]
                           )
