"""
Used to handle temp data that is related to lecture, question and user
"""

from contracts import contract


class TempInfoUserQuestion:
    def __init__(self, db, cursor, table_name):
        self.db = db
        self.cursor = cursor
        self.table_name = table_name

    @contract
    def add_user_info(self, lecture_id: "int", asked_id: "int", user_id: "int", commit: 'bool'=True):

        cursor = self.db.cursor()

        cursor.execute("""
            SELECT *
            FROM {}
            WHERE lecture_id = %s AND asked_id = %s AND user_id = %s
        """.format(self.table_name), (lecture_id, asked_id, user_id))

        exists = cursor.fetchall()

        if not exists:
            cursor.execute("""
                INSERT INTO {} (lecture_id, asked_id, user_id)
                VALUES (%s,%s,%s)
            """.format(self.table_name), (lecture_id, asked_id, user_id))

            if commit:
                self.db.commit()

    @contract
    def delete_user_info(self, lecture_id: "int", asked_id: "int", user_id: "int", commit: 'bool'=True):
        self.cursor.execute("""
            DELETE FROM {}
            WHERE lecture_id = %s AND asked_id = %s AND user_id = %s
        """.format(self.table_name), (lecture_id, asked_id, user_id))

        if commit:
            self.db.commit()

    @contract
    def delete_all_from_question(self, asked_id: "int", commit: 'bool'=True):
        self.cursor.execute("""
            DELETE FROM {}
            WHERE asked_id = %s
        """.format(self.table_name), [asked_id])

        if commit:
            self.db.commit()

    @contract
    def delete_all_from_lecture(self, lecture_id: "int", commit: 'bool'=True):
        self.cursor.execute("""
            DELETE FROM {}
            WHERE lecture_id = %s
        """.format(self.table_name), [lecture_id])

        if commit:
            self.db.commit()

    @contract
    def has_user_info(self, asked_id: "int", user_id: "int", commit: "bool"=True):
        self.cursor.execute("""
            SELECT *
            FROM {}
            WHERE asked_id = %s AND user_id = %s
        """.format(self.table_name), (asked_id, user_id))

        questions = self.cursor.fetchall()
        return len(questions) > 0
