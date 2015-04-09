__author__ = 'localadmin'

from contracts import contract
from timdb.timdbbase import TimDbBase


class Lectures(TimDbBase):
    @contract
    def create_lecture(self, doc_id: "int", start_time: "string", end_time: "string", lecture_code: "string",
                       password: "string", commit: "bool") -> "int":
        cursor = self.db.cursor()

        cursor.execute("""
                          INSERT INTO Lecture(lecture_code, doc_id, start_time, end_time, password)
                          VALUES (?,?,?,?,?)
                          """, [lecture_code, doc_id, start_time, end_time, password])

        if commit:
            self.db.commit()
        lecture_id = cursor.lastrowid

        return lecture_id

    @contract
    def delete_lecture(self, lecture_id: 'int', commit: 'bool'):
        cursor = self.db.cursor()

        cursor.execute(
            """
                DELETE FROM Lecture
                WHERE lecture_id = ?
            """, [lecture_id])

        if commit:
            self.db.commit()

    @contract
    def add_user_to_lecture(self, lecture_id: "int", user_id: "int", commit: "bool"):
        cursor = self.db.cursor()

        cursor.execute("""
                          INSERT INTO LectureUsers(lecture_id, user_id)
                          VALUES (?,?)
                        """, [lecture_id, user_id])
        if commit:
            self.db.commit()


