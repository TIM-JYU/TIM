from contracts import contract
from timdb.tempdbbase import TempDbBase


class ShowPoints(TempDbBase):
    """
    LectureAnswer class to handle database for lecture answers
    """
    @contract
    def add_show_points(self, lecture_id: "int", asked_id: "int"):
        points = self.table(lecture_id, asked_id)
        self.db.session.merge(points)
        self.db.session.commit()

    @contract
    def stop_showing_points(self, lecture_id: "int"):
        self.table.query.filter_by(lecture_id=lecture_id).delete()
        self.db.session.commit()

    @contract
    def get_currently_shown_points(self, lecture_id: "int"):
        points = self.table.query.filter_by(lecture_id=lecture_id)
        points = points.all()
        return points
