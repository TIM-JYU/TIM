from contracts import contract
from timdb.tempdbbase import TempDbBase


class RunningQuestions(TempDbBase):
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
        self.cursor.execute("""
            INSERT INTO RunningQuestion(lecture_id, asked_id, ask_time, end_time)
            VALUES (%s,%s,%s,%s)
        """, (lecture_id, asked_id, ask_time, end_time))

        if commit:
            self.db.commit()

    @contract
    def delete_running_question(self, asked_id: "int", commit: 'bool'=True):
        """
        Remove a running question that is related to lecture
        :param asked_id: asked questions id
        :return:
        """
        self.cursor.execute("""
            DELETE FROM RunningQuestion
            WHERE  asked_id = %s
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
        self.cursor.execute("""
            DELETE FROM RunningQuestion
            WHERE lecture_id = %s
        """, [lecture_id])

        if commit:
            self.db.commit()

    def get_lectures_running_questions(self, lecture_id: "int"):
        self.cursor.execute("""
            SELECT *
            FROM RunningQuestion
            WHERE lecture_id = %s
        """, [lecture_id])

        questions = self.cursor.fetchall()
        return questions

    def get_running_question_by_id(self, asked_id: "int"):
        self.cursor.execute("""
            SELECT *
            FROM RunningQuestion
            WHERE asked_id = %s
        """, [asked_id])

        question = self.cursor.fetchone()
        return question

    def extend_question(self, asked_id: "int", extend: "int", commit: "bool"=True):
        self.cursor.execute("""
            SELECT end_time
            FROM RunningQuestion
            WHERE asked_id = %s
        """, [asked_id])
        
        end_time = self.cursor.fetchone()[0]
        end_time += extend
        
        self.cursor.execute("""
            UPDATE RunningQuestion
            SET end_time = %s
            WHERE asked_id = %s
        """, (end_time, asked_id))

        if commit:
            self.db.commit()

    def get_end_time(self, asked_id: "int", commit: "bool"=True):
        self.cursor.execute("""
            SELECT end_time
            FROM RunningQuestion
            WHERE asked_id = %s
        """, [asked_id])

        return self.cursor.fetchone()
