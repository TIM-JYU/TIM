from contracts import contract
from timdb.timdbbase import TimDbBase


class AnnotationComments(TimDbBase):
    """
    Used as an interface to query the database about comments related to an annotation..
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
    def add_comment(self, annotation_id: 'int', commenter_id: 'int', content: 'str'):
        """Adds new comment to an annotation

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
        self.db.commit()

    #Todo write support for answer_id.
    @contract
    def get_comments(self, document_id: int, paragraph_id: str) -> 'list(dict)':
        """Gets all the comments in annotations in this paragraph.

        :param document_id: Id of the document.
        :param paragraph_id: Id of the paragraph
        :return: a list of dictionaries, each dictionary representing a single comment
        """
        cursor = self.db.cursor()
        cursor.execute("""
                       SELECT
                         Comment.annotation_id,
                         Comment.comment_time,
                         Comment.commenter_id,
                         Comment.content
                       FROM Comment
                       WHERE Comment.annotation_id IN (
                         SELECT Annotation.id
                         FROM Annotation
                         WHERE Annotation.document_id = ? AND Annotation.paragraph_id = ?
                       ) ORDER BY Comment.annotation_id ASC;
                       """, [document_id, paragraph_id]
                       )
        return self.resultAsDictionary(cursor)
