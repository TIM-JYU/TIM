from contracts import contract
from timdb.timdbbase import TimDbBase

class Annotations(TimDbBase):
    @contract
    def create_annotation(self, version_id: 'int',  points: 'float', place_start: 'int', place_end: 'int',
                          annotator_id: 'int', icon_id: 'int | None' = None,
                          document_id: 'int | None' = None, paragraph_id: 'int | None' = None,
                          answer_id: 'int | None' = None):
        """
        Create new annotation
        :param version_id: Version of velp annotation uses
        :param document_id: Document id     \
        :param paragraph_id: Paragraph id   - either bot document and paragraph or answer are null
        :param answer_id: Answer id         /
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
                      place_end, annotator_id, points, icon_id)
                      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
                      """, [version_id, document_id, paragraph_id, answer_id, place_start,
                           place_end, annotator_id, points, icon_id]
        )
        self.db.commit()