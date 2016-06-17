from contracts import contract
from timdb.tempdbbase import TempDbBase


class NewAnswers(TempDbBase):
    """
    LectureAnswer class to handle database for lecture answers
    """
    @contract
    def user_answered(self, lecture_id: "int", asked_id: "int", user_id: "int"):
        new_answer = self.table(lecture_id, asked_id, user_id)
        self.session.add(new_answer)
        self.session.commit()

    @contract
    def delete_question_answers(self, asked_id: "int"):
        self.table.query.filter_by(asked_id=asked_id).delete()
        self.session.commit()

    @contract
    def delete_lecture_answers(self, lecture_id: "int"):
        self.table.query.filter_by(lecture_id=lecture_id).delete()
        self.session.commit()

    @contract
    def get_new_answers(self, asked_id: "int"):
        answers = self.table.query.filter_by(asked_id=asked_id)
        new_answers = answers.all()

        new_user_ids = []

        if new_answers:
            for answer in new_answers:
                new_user_ids.append(answer.user_id)
                self.session.delete(answer)
            self.session.commit()

        return new_user_ids
