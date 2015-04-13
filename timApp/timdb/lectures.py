__author__ = 'localadmin'

from contracts import contract
from timdb.timdbbase import TimDbBase


class Lectures(TimDbBase):
    @contract
    def create_lecture(self, doc_id: "int", lecturer: 'int', start_time: "string", end_time: "string",
                       lecture_code: "string",
                       password: "string", commit: "bool") -> "int":
        cursor = self.db.cursor()

        cursor.execute("""
                          INSERT INTO Lecture(lecture_code, doc_id,lecturer, start_time, end_time, password)
                          VALUES (?,?,?,?,?,?)
                          """, [lecture_code, doc_id, lecturer, start_time, end_time, password])

        if commit:
            self.db.commit()
        lecture_id = cursor.lastrowid

        return lecture_id

    @contract
    def delete_lecture(self, lecture_id: 'int', commit: 'bool'):
        cursor = self.db.cursor()

        cursor.execute(
            """
            DELETE
            FROM Lecture
            WHERE lecture_id = ?
            """, [lecture_id])

        if commit:
            self.db.commit()

    @contract
    def delete_users_from_lecture(self, lecture_id: 'int', commit: 'bool'):
        cursor = self.db.cursor()

        cursor.execute(
            """
            DELETE FROM LectureUsers
            WHERE lecture_id = ?
            """, [lecture_id]
        )

        if commit:
            self.db.commit()

    @contract
    def get_lecture(self, lecture_id: "int") -> 'list(dict)':
        cursor = self.db.cursor()

        cursor.execute(
            """
            SELECT *
            FROM Lecture
            WHERE lecture_id = ?
            """, [lecture_id]
        )

        return self.resultAsDictionary(cursor)

    @contract
    def join_lecture(self, lecture_id: "int", user_id: "int", commit: "bool"):
        cursor = self.db.cursor()

        cursor.execute("""
                       INSERT INTO LectureUsers(lecture_id, user_id)
                       VALUES (?,?)
                       """, [lecture_id, user_id])
        if commit:
            self.db.commit()

    @contract
    def leave_lecture(self, lecture_id: "int", user_id: "int", commit: "bool"):
        cursor = self.db.cursor()

        cursor.execute("""
                       DELETE
                       FROM LectureUsers
                       WHERE lecture_id = ? AND user_id = ?
                       """, [lecture_id, user_id])

        if commit:
            self.db.commit()

    @contract
    def get_document_lectures(self, doc_id: 'int', time: 'string'):
        cursor = self.db.cursor()

        cursor.execute("""
                        SELECT lecture_code
                        FROM Lecture
                        WHERE doc_id = ? AND start_time <= ? AND end_time >= ?
                        """, [doc_id, time, time])

        return self.resultAsDictionary(cursor)

    @contract
    def get_lecture_by_code(self, lecture_code: 'string') -> 'int':
        cursor = self.db.cursor()

        cursor.execute("""
                        SELECT lecture_id
                        FROM Lecture
                        WHERE lecture_code = ?
                        """, [lecture_code])

        return cursor.fetchone()[0]

    @contract
    def check_if_in_lecture(self, doc_id: "int", user_id: "int") -> "tuple":
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
                           WHERE doc_id = ?
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
                           WHERE lecture_id IN """ + "(" + string_of_lectures + ")" + """ AND user_id = ?
                            """, [user_id])

        result = cursor.fetchall()
        if len(result) > 0:
            return True, result[0][0]
        else:
            return False, -1



