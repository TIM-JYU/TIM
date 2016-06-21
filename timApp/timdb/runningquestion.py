from timdb.tempdbbase import TempDbBase


class RunningQuestions(TempDbBase):
    """
    LectureAnswer class to handle database for lecture answers
    """
    def add_running_question(self, lecture_id: int, asked_id: int, ask_time: int, end_time: int=None):
        running = self.table(asked_id, lecture_id, ask_time, end_time)
        self.session.merge(running)
        self.session.commit()

    def delete_running_question(self, asked_id: int):
        self.table.query.filter_by(asked_id=asked_id).delete()
        self.session.commit()

    def delete_lectures_running_questions(self, lecture_id: int):
        self.table.query.filter_by(lecture_id=lecture_id).delete()
        self.session.commit()

    def get_lectures_running_questions(self, lecture_id: int):
        questions = self.table.query.filter_by(lecture_id=lecture_id)
        return questions.all()

    def get_running_question_by_id(self, asked_id: int):
        questions = self.table.query.filter_by(asked_id=asked_id)
        question = questions.first()
        return question

    def extend_question(self, asked_id: int, extend: int):
        questions = self.table.query.filter_by(asked_id=asked_id)
        question = questions.first()
        question.end_time += extend
        self.session.commit()

    def get_end_time(self, asked_id: int):
        questions = self.table.query.filter_by(asked_id=asked_id)
        question = questions.first()
        return question.end_time
