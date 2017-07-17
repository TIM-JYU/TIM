from datetime import datetime
from typing import List, Tuple

from timApp.timdb.tim_models import Lecture, LectureUsers
from timApp.timdb.timdbbase import TimDbBase
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

    def get_lecture(self, lecture_id: int) -> Lecture:
        return Lecture.query.get(lecture_id)

    def get_lecture_by_name(self, lecture_code: str, doc_id: int) -> Lecture:
        return Lecture.query.filter_by(lecture_code=lecture_code, doc_id=doc_id).one()

    def get_all_lectures_from_document(self, document_id: int) -> List[Lecture]:
        return Lecture.query.filter_by(doc_id=document_id).all()

    def join_lecture(self, lecture_id: int, user_id: int, commit: bool=True):
        cursor = self.db.cursor()
        try:
            cursor.execute("""
                           INSERT INTO LectureUsers(lecture_id, user_id)
                           VALUES (%s,%s)
                           """, [lecture_id, user_id])
            if commit:
                self.db.commit()
        except:
            return  # was allready

    def leave_lecture(self, lecture_id: int, user_id: int, commit: bool=True):
        cursor = self.db.cursor()

        cursor.execute("""
                       DELETE
                       FROM LectureUsers
                       WHERE lecture_id = %s AND user_id = %s
                       """, [lecture_id, user_id])

        if commit:
            self.db.commit()

    def get_document_lectures(self, doc_id: int, time: datetime) -> List[Lecture]:
        return Lecture.query.filter_by(doc_id=doc_id).filter(Lecture.end_time > time).order_by(Lecture.lecture_code.asc()).all()

    def get_lecture_by_code(self, lecture_code: str, doc_id: int) -> Lecture:
        return Lecture.query.filter_by(lecture_code=lecture_code, doc_id=doc_id).one()

    def check_if_correct_name(self, doc_id: int, lecture_code: str, lecture_id: int) -> int:
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

    def check_if_in_any_lecture(self, user_id: int) -> List[Lecture]:
        """Checks if the user is in any lecture.

        :param user_id: user id
        :return:

        """
        return Lecture.query.join(LectureUsers, LectureUsers.lecture_id == Lecture.lecture_id).filter(LectureUsers.user_id == user_id).with_entities(Lecture).all()

    def extend_lecture(self, lecture_id: int, new_end_time: str, commit: bool=True):
        cursor = self.db.cursor()

        cursor.execute("""
                        UPDATE Lecture
                        SET end_time = %s
                        WHERE lecture_id = %s
        """, [new_end_time, lecture_id])

        if commit:
            self.db.commit()
