from contracts import contract
from timdb.timdbbase import TimDbBase

class Annotation(TimDbBase):
    @contract
    def create_annotation(self, version_id: 'int', document_id: 'int', paragraph_id: 'int',
                          answer_id: 'int', place_start: 'int', place_end: 'int',
                          annotator_id: 'int', points: 'float', icon_id: 'int'):
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
        :return:
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      INSERT INTO
                      Annotation(version_id, document_id, paragraph_id, answer_id, place_start,
                      place_end, annotator_id, points, icon_id)
                      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
                      """ [version_id, document_id, paragraph_id, answer_id, place_start,
                       place_end, annotator_id, points, icon_id]
        )
        self.db.commit()