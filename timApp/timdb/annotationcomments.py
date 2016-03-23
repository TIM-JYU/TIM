from contracts import contract
from timdb.timdbbase import TimDbBase

class AnnotationComments(TimDbBase):
    @contract
    def add_comment(self, annotation_id: 'int', commenter_id: 'int', content: 'str'):
        """
        Adds new comment to annotation
        :param annotation_id:
        :param commenter_id:
        :param content:
        :return:
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      INSERT INTO
                      Comment(annotation_id, commenter_id, content)
                      VALUES (?, ?, ?)
                      """, [annotation_id, commenter_id, content]
        )