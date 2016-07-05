from datetime import datetime
from typing import List, Tuple

from timdb.tim_models import Lecture

__author__ = 'localadmin'

from timdb.timdbbase import TimDbBase
import json


class Lectures(TimDbBase):
    def create_lecture(self, doc_id: int, lecturer: int, start_time: str, end_time: str,
                       lecture_code: str, password: str, options: str, commit: bool) -> int:
        l = Lecture(lecture_code=lecture_code, lecturer=lecturer, start_time=start_time, end_time=end_time,
                    password=password, options=options, doc_id=doc_id)
        self.session.add(l)
        self.session.flush()
        if commit:
            self.session.commit()
        return l.lecture_id


    def update_lecture(self, lecture_id: int, doc_id: int, lecturer: int, start_time: str, end_time: str,
                       lecture_code: str, password: str, options: str):

        cursor = self.db.cursor()

        cursor.execute("""
                        UPDATE Lecture
                        SET lecture_code = %s, doc_id = %s, lecturer = %s, start_time = %s, end_time = %s, password = %s,
                            options = %s
                        WHERE lecture_id = %s
                        """, [lecture_code, doc_id, lecturer, start_time, end_time, password, options, lecture_id])

        self.db.commit()
        return lecture_id

    def delete_lecture(self, lecture_id: int, commit: bool):
        cursor = self.db.cursor()

        cursor.execute(
            """
            DELETE
            FROM Lecture
            WHERE lecture_id = %s
            """, [lecture_id])

        if commit:
            self.db.commit()

    def delete_users_from_lecture(self, lecture_id: int, commit: bool=True):
        cursor = self.db.cursor()

        cursor.execute(
            """
            DELETE FROM LectureUsers
            WHERE lecture_id = %s
            """, [lecture_id]
        )

        if commit:
            self.db.commit()

    def get_lecture(self, lecture_id: int) -> List[dict]:
        cursor = self.db.cursor()

        cursor.execute(
            """
            SELECT *
            FROM Lecture
            WHERE lecture_id = %s
            """, [lecture_id]
        )

        return self.resultAsDictionary(cursor)

    def get_lecture_by_name(self, lecture_code: str, doc_id: int) -> List[dict]:
        cursor = self.db.cursor()

        cursor.execute(
            """
            SELECT *
            FROM Lecture
            WHERE lecture_code = %s AND doc_id = %s
            """, [lecture_code, doc_id]
        )

        return self.resultAsDictionary(cursor)

    def get_all_lectures_from_document(self, document_id:int) -> List[dict]:
        cursor = self.db.cursor()

        cursor.execute(
            """
            SELECT *
            FROM Lecture
            WHERE doc_id = %s
            """, [document_id]
        )

        return self.resultAsDictionary(cursor)

    def join_lecture(self, lecture_id: int, user_id: int, commit: bool=True):
        cursor = self.db.cursor()

        cursor.execute("""
                       INSERT INTO LectureUsers(lecture_id, user_id)
                       VALUES (%s,%s)
                       """, [lecture_id, user_id])
        if commit:
            self.db.commit()

    def leave_lecture(self, lecture_id: int, user_id: int, commit: bool=True):
        cursor = self.db.cursor()

        cursor.execute("""
                       DELETE
                       FROM LectureUsers
                       WHERE lecture_id = %s AND user_id = %s
                       """, [lecture_id, user_id])

        if commit:
            self.db.commit()

    def get_document_lectures(self, doc_id: int, time: datetime):
        cursor = self.db.cursor()

        cursor.execute("""
                        SELECT lecture_code, start_time,end_time, password
                        FROM Lecture
                        WHERE doc_id = %s AND end_time > %s
                        ORDER BY lecture_code
                        """, [doc_id, time])

        return self.resultAsDictionary(cursor)

    def get_all_lectures(self, time: str):
        cursor = self.db.cursor()

        cursor.execute("""
                        SELECT lecture_code, start_time,end_time, password, lecturer
                        FROM Lecture
                        WHERE end_time > %s
                        ORDER BY lecture_code
                        """, [time])

        return self.resultAsDictionary(cursor)

    def get_lecture_by_code(self, lecture_code: str, doc_id: int) -> int:
        cursor = self.db.cursor()

        cursor.execute("""
                        SELECT lecture_id, password
                        FROM Lecture
                        WHERE lecture_code = %s AND doc_id = %s
                        """, [lecture_code, doc_id])

        return cursor.fetchone()[0]

    def check_if_correct_name(self, doc_id: int, lecture_code: str, lecture_id : int) -> int:
        cursor = self.db.cursor()

        cursor.execute("""
                        SELECT lecture_id
                        FROM Lecture
                        WHERE lecture_code = %s AND doc_id = %s AND lecture_id != %s
                        """, [lecture_code, doc_id, lecture_id])

        answer = cursor.fetchall()

        if len(answer) >= 1:
            return False

        return True

    def set_end_for_lecture(self, lecture_id: int, end_time: datetime):
        cursor = self.db.cursor()

        cursor.execute(
            """
            UPDATE Lecture
            SET end_time = %s
            WHERE lecture_id = %s
            """, [end_time, lecture_id]
        )

        self.db.commit()

    def check_if_lecture_is_running(self, lecture_id: int, now: datetime) -> bool:
        cursor = self.db.cursor()

        cursor.execute(
            """
            SELECT lecture_id
            FROM Lecture
            WHERE lecture_id = %s AND end_time > %s
            """, [lecture_id, now]
        )

        lecture_id = cursor.fetchall()
        if len(lecture_id) <= 0:
            return False

        return True

    def check_if_lecture_is_full(self, lecture_id: int) -> bool:
        cursor = self.db.cursor()

        cursor.execute(
            """
            SELECT COUNT(*)
            FROM LectureUsers
            WHERE lecture_id == %s;
            """, [lecture_id]
        )

        students = int(cursor.fetchone()[0])

        cursor.execute(
            """
            SELECT options
            FROM Lecture
            WHERE Lecture_id == %s
            """, [lecture_id]
        )

        options = cursor.fetchone()
        options = json.loads(options[0])

        max = None
        if 'max_students' in options:
            max = int(options['max_students'])

        if max is None:
            return False
        else:
            return students + 1 >= max

    def check_if_in_lecture(self, doc_id: int, user_id: int) -> Tuple:
        """
        Check if user is in lecture from specific document
        :param doc_id: document id
        :param user_id: user id
        :return:
        """

        cursor = self.db.cursor()

        cursor.execute("""
                           SELECT lecture_id
                           FROM Lecture
                           WHERE doc_id = %s
                           """, [doc_id])

        lecture_ids = cursor.fetchall()
        if len(lecture_ids) <= 0:
            return False, 0

        string_of_lectures = ""
        comma = ""
        for lecture in lecture_ids:
            string_of_lectures += comma + str(lecture[0])
            comma = ","

        if len(string_of_lectures) <= 0:
            return False, -1

        cursor.execute("""
                           SELECT lecture_id, user_id
                           FROM LectureUsers
                           WHERE lecture_id IN """ + "(" + string_of_lectures + ")" + """ AND user_id = %s
                            """, [user_id])

        result = cursor.fetchall()
        if len(result) > 0:
            return True, result[0][0]
        else:
            return False, -1

    def check_if_in_any_lecture(self, user_id: int) -> Tuple:
        """
        Check if user is in lecture from specific document
        :param doc_id: document id
        :param user_id: user id
        :return:
        """

        cursor = self.db.cursor()

        cursor.execute("""
                           SELECT lecture_id, user_id
                           FROM LectureUsers
                           WHERE user_id = %s
                            """, [user_id])

        result = cursor.fetchall()
        if len(result) > 0:
            return True, result[0][0]
        else:
            return False, -1

    def get_users_from_leture(self, lecture_id: int) -> List[dict]:
        cursor = self.db.cursor()

        cursor.execute("""
                            SELECT user_id
                            FROM LectureUsers
                            WHERE lecture_id = %s
                      """, [lecture_id])

        return self.resultAsDictionary(cursor)

    def update_lecture_starting_time(self, lecture_id: int, start_time: datetime, commit: bool=True) -> dict:
        cursor = self.db.cursor()

        cursor.execute("""
                        UPDATE Lecture
                        SET start_time = %s
                        WHERE lecture_id = %s
        """, [start_time, lecture_id])

        if commit:
            self.db.commit()

        cursor.execute("""
                      SELECT *
                      FROM Lecture
                      WHERE lecture_id = %s
        """, [lecture_id])

        return self.resultAsDictionary(cursor)[0]

    def extend_lecture(self, lecture_id: int, new_end_time: str, commit: bool=True):
        cursor = self.db.cursor()

        cursor.execute("""
                        UPDATE Lecture
                        SET end_time = %s
                        WHERE lecture_id = %s
        """,[new_end_time, lecture_id])

        if commit:
            self.db.commit()

