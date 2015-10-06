from contracts import contract
from timdb.timdbbase import TimDbBase


class RunningQuestions(TimDbBase):
    """
    LectureAnswer class to handle database for lecture answers
    """
    @contract
    def add_running_question(self, lecture_id: "int", asked_id: "int", ask_time: "int", end_time: "int"=None,
                             commit: 'bool'=True):
        """
        Adds a running question that is related to lecture
        :param lecture_id: lecture id
        :param asked_id: asked questions id
        :return:
        """
        cursor = self.db.cursor()

        cursor.execute("""
            INSERT INTO RunningQuestion(lecture_id, asked_id, ask_time, end_time)
            VALUES (?,?,?,?)
        """, [lecture_id, asked_id, ask_time, end_time])

        if commit:
            self.db.commit()

    @contract
    def delete_running_question(self, asked_id: "int", commit: 'bool'=True):
        """
        Remove a running question that is related to lecture
        :param asked_id: asked questions id
        :return:
        """
        cursor = self.db.cursor()

        cursor.execute("""
            DELETE FROM RunningQuestion
            WHERE  asked_id = ?
        """, [asked_id])

        if commit:
            self.db.commit()

    @contract
    def delete_lectures_running_questions(self, lecture_id: "int", commit: 'bool'=True):
        """
        Remove a running question that is related to lecture
        :param asked_id: asked questions id
        :return:
        """
        cursor = self.db.cursor()

        cursor.execute("""
            DELETE FROM RunningQuestion
            WHERE lecture_id = ?
        """, [lecture_id])

        if commit:
            self.db.commit()

    def get_lectures_running_questions(self, lecture_id: "int"):
        cursor = self.db.cursor()

        cursor.execute("""
            SELECT *
            FROM RunningQuestion
            WHERE lecture_id = ?
        """, [lecture_id])

        questions = cursor.fetchall()
        return questions

    def get_running_question_by_id(self, asked_id: "int"):
        cursor = self.db.cursor()

        cursor.execute("""
            SELECT *
            FROM RunningQuestion
            WHERE asked_id = ?
        """, [asked_id])

        question = cursor.fetchone()
        return question

    def extend_question(self, asked_id: "int", extend: "int", commit: "bool"=True):
        cursor = self.db.cursor()
        
        cursor.execute("""
            SELECT end_time
            FROM RunningQuestion
            WHERE asked_id = ?
        """, [asked_id])
        
        end_time = cursor.fetchone()[0]
        end_time += extend
        
        cursor.execute("""
            UPDATE RunningQuestion
            SET end_time = ?
            WHERE asked_id = ?
        """, [end_time, asked_id])

        if commit:
            self.db.commit()

    def get_end_time(self, asked_id: "int", commit: "bool"=True):
        cursor = self.db.cursor()

        cursor.execute("""
            SELECT end_time
            FROM RunningQuestion
            WHERE asked_id = ?
        """, [asked_id])

        return cursor.fetchone()
