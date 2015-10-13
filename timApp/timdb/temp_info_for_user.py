"""
Used to handle temp data that is related to lecture, question and user
"""

from contracts import contract
from timdb.timdbbase import TimDbBase


class TempInfoUserQuestion(TimDbBase):
    def __init__(self, db: 'Connection', files_root_path: 'str', type_name: 'str', current_user_name: 'str',
                 table_name):
        TimDbBase.__init__(self, db, files_root_path, type_name, current_user_name)
        self.table_name = table_name

    @contract
    def add_user_info(self, lecture_id: "int", asked_id: "int", user_id: "int", commit: 'bool'=True):
        cursor = self.db.cursor()
        cursor.execute("""
            INSERT OR REPLACE INTO %s (lecture_id, asked_id, user_id)
            VALUES (?,?,?)
        """ % self.table_name, [lecture_id, asked_id, user_id])

        if commit:
            self.db.commit()

    @contract
    def delete_user_info(self, lecture_id: "int", asked_id: "int", user_id: "int", commit: 'bool'=True):
        cursor = self.db.cursor()
        cursor.execute("""
            DELETE FROM %s
            WHERE lecture_id = ? AND asked_id = ? AND user_id = ?
        """ % self.table_name, [lecture_id, asked_id, user_id])

        if commit:
            self.db.commit()

    @contract
    def delete_all_from_question(self, asked_id: "int", commit: 'bool'=True):
        cursor = self.db.cursor()
        cursor.execute("""
            DELETE FROM %s
            WHERE asked_id = ?
        """ % self.table_name, [asked_id])

        if commit:
            self.db.commit()

    @contract
    def delete_all_from_lecture(self, lecture_id: "int", commit: 'bool'=True):
        cursor = self.db.cursor()
        cursor.execute("""
            DELETE FROM %s
            WHERE lecture_id = ?
        """ % self.table_name, [lecture_id])

        if commit:
            self.db.commit()

    @contract
    def has_user_info(self, asked_id: "int", user_id: "int", commit: "bool"=True):
        cursor = self.db.cursor()
        cursor.execute("""
            SELECT *
            FROM %s
            WHERE asked_id = ? AND user_id = ?
        """ % self.table_name, [asked_id, user_id])

        questions = cursor.fetchall()
        return len(questions) > 0
