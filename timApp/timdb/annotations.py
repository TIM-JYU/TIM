import copy
from enum import Enum, unique
from sqlite3 import Connection
from typing import Dict, List, Optional
from timdb.timdbbase import TimDbBase
from timdb.users import Users
from assessment_area import AssessmentArea
from utils import datestr_to_relative


class Annotations(TimDbBase):
    """
    Used as an interface to query the database about annotations.
    """

    @unique
    class AnnotationVisibility(Enum):
        """Enum for storing the visibility"""
        myself = 1
        owner = 2
        teacher = 3
        everyone = 4

    def __init__(self, db_path: Connection, files_root_path: str, type_name: str, current_user_name: str):
        """Initializes TimDB with the specified database and root path.

        :param type_name: The type name.
        :param current_user_name: The name of the current user.
        :param db_path: The path of the database file.
        :param files_root_path: The root path where all the files will be stored.
        """
        TimDbBase.__init__(self, db_path, files_root_path, type_name, current_user_name)

    def create_annotation(self, velp_version_id: int, visible_to: AnnotationVisibility, points: Optional[float],
                          annotator_id: int, document_id: int, paragraph_id_start: Optional[str],
                          paragraph_id_end: Optional[str], offset_start: int, node_start: int, depth_start: int,
                          offset_end: int, node_end: int, depth_end: int, hash_start: Optional[str],
                          hash_end: Optional[str], element_path_start: str, element_path_end: str,
                          valid_until: Optional[str] = None, icon_id: Optional[int] = None,
                          answer_id: Optional[int] = None) -> int:
        """Create a new annotation.

        :param version_id: Version of the velp that the annotation uses.
        :param visible_to: visibility of the annotation.
        :param points: Points given, overrides velp's default and can be null.
        :param annotator_id: ID of user who left the annotation.
        :param document_id: ID of document in which annotation is located in.
        :param paragraph_id_start: ID of paragraph where annotation starts.
        :param paragraph_id_end: ID of paragraph where annotation ends.
        :param offset_start: Character location where annotation starts.
        :param node_start:
        :param depth_start: depth of the element path
        :param offset_end: Character location where annotation ends.
        :param node_end:
        :param depth_end: depth of the element path
        :param hash_start: Hash code of paragraph where annotation starts.
        :param hash_end: Hash code of paragraph where annotation ends.
        :param element_path_start: List of elements as text (parsed in interface) connected to annotation start.
        :param element_path_end: List of elements as text (parsed in interface) connected to annotation end.
        :param valid_until: Datetime until which annotation is valid for, 'none' for forever.
        :param icon_id: ID of icon associated with annotation, can be 'none'.
        :param answer_id: ID of answer if annotation is located within one.
        :return:
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      INSERT INTO
                      Annotation(velp_version_id, visible_to, points, valid_until, icon_id, annotator_id,
                      document_id, answer_id, paragraph_id_start, paragraph_id_end,
                      offset_start, node_start, depth_start, offset_end, node_end, depth_end, hash_start, hash_end,
                      element_path_start, element_path_end)
                      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                      """, [velp_version_id, visible_to.value, points, valid_until, icon_id, annotator_id, document_id,
                            answer_id, paragraph_id_start, paragraph_id_end, offset_start, node_start, depth_start,
                            offset_end, node_end, depth_end, hash_start, hash_end, element_path_start, element_path_end]
                       )
        self.db.commit()
        return cursor.lastrowid

    def update_annotation(self, annotation_id: int, version_id: Optional[int], visible_to: AnnotationVisibility,
                          points: Optional[float], icon_id: Optional[int]):
        """Changes an existing annotation.

        :param annotation_id annotation to be changed.
        :param version_id: version of the velp that the annotation uses
        :param visible_to: visibility of the annotation
        :param points: Points given, overrides velp's default and can be null
        :param icon_id: Icon id, can be null
        :return:
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      UPDATE Annotation
                      SET
                        velp_version_id = ?,
                        visible_to      = ?,
                        points          = ?,
                        icon_id         = ?
                      WHERE id = ?
                      """, [version_id, visible_to.value, points, icon_id, annotation_id]
                      )
        self.db.commit()
        return

    def get_annotation(self, annotation_id: int) -> List[Dict]:
        """Gets an annotation.

        :param annotation_id: ID of annotation
        :return: A list of dictionaries with the fields of individual annotations. List is empty if nothing is found and has several rows if there are duplicate ids(not good).
        """
        cursor = self.db.cursor()
        cursor.execute("""
                       SELECT *
                       FROM Annotation
                       WHERE Annotation.id = ?
        """, [annotation_id])
        return self.resultAsDictionary(cursor)

    def invalidate_annotation(self, annotation_id: int, valid_until: Optional[str] = None):
        """Invalidates and thus hides annotation

        :param annotation_id: Id of annotation
        :param valid_until: Time when annotation will be invalidated
        :return:
        """
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
        self.db.commit()

    def add_comment(self, annotation_id: int, commenter_id: int, content: str) -> int:
        """Adds new comment to an annotation

        :param annotation_id:
        :param commenter_id:
        :param content:
        :return: id of the new comment.
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      INSERT INTO
                      AnnotationComment(annotation_id, commenter_id, content)
                      VALUES (?, ?, ?)
                      """, [annotation_id, commenter_id, content]
                       )
        self.db.commit()
        return cursor.lastrowid


    def get_annotations_with_comments_in_document(self, user_id: int, user_has_see_answers: bool, user_has_teacher: bool,
                                                  user_has_owner: bool, document_id: int):
        annotations = self.get_annotations_in_document(user_id, user_has_see_answers, user_has_teacher, user_has_owner,
                                                       document_id)
        annotation_ids = []
        for annotation in annotations:
            annotation_ids.append(annotation['id'])

        comment_data = self.get_comments_for_annotations(annotation_ids)

        comment_dict = {}

        if comment_data:
            user_dict = {}
            user_info = {}
            annotation_id = comment_data[0]['annotation_id']
            list_help = []
            dict_help = {}
            # comment_list = []
            for i in range(len(comment_data)):
                next_id = comment_data[i]['annotation_id']
                if next_id != annotation_id:
                    # comment_dict['id'] = annotation_id
                    comment_dict[annotation_id] = copy.deepcopy(list_help)
                    # comment_list.append(copy.deepcopy(comment_dict))
                    annotation_id = next_id
                    dict_help.clear()
                    del list_help[:]
                    dict_help['comment_relative_time'] = datestr_to_relative(comment_data[i]['comment_time'])
                    dict_help['comment_time'] = comment_data[i]['comment_time']
                    dict_help['commenter_id'] = comment_data[i]['commenter_id']
                    dict_help['content'] = comment_data[i]['content']
                    dict_help['commenter_username'] = comment_data[i]['username']
                    dict_help['commenter_real_name'] = comment_data[i]['real_name']
                    dict_help['commenter_email'] = comment_data[i]['user_email']
                    list_help.append(copy.deepcopy(dict_help))
                else:
                    dict_help['comment_relative_time'] = datestr_to_relative(comment_data[i]['comment_time'])
                    dict_help['comment_time'] = comment_data[i]['comment_time']
                    dict_help['commenter_id'] = comment_data[i]['commenter_id']
                    dict_help['content'] = comment_data[i]['content']
                    dict_help['commenter_username'] = comment_data[i]['username']
                    dict_help['commenter_real_name'] = comment_data[i]['real_name']
                    dict_help['commenter_email'] = comment_data[i]['user_email']
                    list_help.append(copy.deepcopy(dict_help))
                if i == len(comment_data) - 1:
                    # comment_dict['id'] = annotation_id
                    comment_dict[annotation_id] = copy.deepcopy(list_help)
                    # comment_list.append(copy.deepcopy(comment_dict))
                    annotation_id = next_id
                    dict_help.clear()

        for annotation in annotations:
            annotation_id = annotation['id']
            if annotation_id in comment_dict:
                annotation['comments'] = copy.deepcopy(comment_dict[annotation_id])
            else:
                annotation['comments'] = []
                print("No comments for annotation id: " + str(annotation_id))

        return annotations

    def get_annotations_in_document(self, user_id: int, user_has_see_answers, user_has_teacher: bool,
                                    user_has_owner: bool, document_id: int) -> List[Dict]:
        """Gets all annotations made in a document. Both in document and in answers.

        :param user_has_see_answers:
        :param user_id: user that is viewing annotations. Affects which annotations are returned.
        :param user_has_teacher:
        :param user_has_owner:
        :param document_id: The relevant document.
        :return: List of dictionaries, each dictionary representing a single annotation.
        """
        # Todo choose velp language. Have fun.
        language_id = 'FI'
        check_annotation_access_right_sql = "Annotation.visible_to = ?"
        annotation_access_levels = [Annotations.AnnotationVisibility.everyone.value]
        if user_has_teacher:
            check_annotation_access_right_sql += "OR\nAnnotation.visible_to = ?"
            annotation_access_levels = annotation_access_levels + [Annotations.AnnotationVisibility.teacher.value]
        if user_has_owner:
            check_annotation_access_right_sql += "OR\nAnnotation.visible_to = ?"
            annotation_access_levels = annotation_access_levels + [Annotations.AnnotationVisibility.owner.value]
        check_annotation_access_right_sql += "\n"

        cursor = self.db.cursor()
        # Distinct is necessary, because we get several identical rows if there are several users contributing in the
        # same answer.
        cursor.execute("""
                       select distinct
                         annotation.id,
                         velpversion.velp_id as velp,
                         velpcontent.content as content,
                         annotation.visible_to,
                         annotation.points,
                         annotation.creation_time,
                         annotation.valid_until,
                         annotation.icon_id,
                         annotation.annotator_id,
                         user.name as annotator_name,
                         user.real_name as annotator_real_name,
                         annotation.answer_id,
                         annotation.paragraph_id_start,
                         annotation.paragraph_id_end,
                         annotation.offset_start,
                         annotation.node_start,
                         annotation.depth_start,
                         annotation.offset_end,
                         annotation.node_end,
                         annotation.depth_end,
                         annotation.hash_start,
                         annotation.hash_end,
                         annotation.element_path_start,
                         annotation.element_path_end
                       from annotation
                         inner join velpversion on velpversion.id = annotation.velp_version_id
                         inner join velpcontent on velpcontent.version_id = annotation.velp_version_id
                         left join useranswer on useranswer.answer_id = annotation.answer_id
                         inner join user on user.id = annotation.annotator_id
                       where velpcontent.language_id = ? and
                             (annotation.valid_until isnull or
                                annotation.valid_until >= current_timestamp) and
                             annotation.document_id = ? and (
                                annotation.annotator_id = ? or (
                                """ + check_annotation_access_right_sql +
                                """ AND (? OR UserAnswer.user_id = ? OR UserAnswer.user_id ISNULL)
                             )
                          )
           ORDER BY depth_start DESC, node_start DESC, offset_start DESC""",
                       [language_id, document_id, user_id] + annotation_access_levels + [user_has_see_answers, user_id])
        results = self.resultAsDictionary(cursor)
        for result in results:
            start_path = [int(i) for i in result['element_path_start'][1:-1].split(',')]
            end_path = [int(i) for i in result['element_path_end'][1:-1].split(',')]

            start = {'par_id': result['paragraph_id_start'], 'offset': result['offset_start'],
                     'node': result['node_start'], 'depth': result['depth_start'], 't': result['hash_start'],
                     'el_path': start_path}
            end = {'par_id': result['paragraph_id_end'], 'offset': result['offset_end'],
                   'node': result['node_end'], 'depth': result['depth_end'], 't': result['hash_end'],
                   'el_path': end_path}
            coord = {'start': start, 'end': end}
            result['coord'] = coord
        return results

    def get_comments_for_annotations(self, annotation_ids: List[int]) -> List[dict]:
        """Gets comments for annotations given as a list

        :param annotation_ids: List of annotation IDs
        :return:
        """
        cursor = self.db.cursor()
        cursor.execute("""
                      SELECT
                        AnnotationComment.annotation_id,
                        AnnotationComment.comment_time,
                        AnnotationComment.commenter_id,
                        AnnotationComment.content,
                        User.name as username, User.real_name,
                        User.email as user_email
                      FROM AnnotationComment
                      JOIN User ON AnnotationComment.commenter_id = User.id
                      WHERE AnnotationComment.annotation_id IN ({})
                      ORDER BY AnnotationComment.annotation_id ASC;
                      """.format(self.get_sql_template(annotation_ids)), annotation_ids
                      )
        comment_data = self.resultAsDictionary(cursor)

        return comment_data
