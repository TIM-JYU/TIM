from timdb.tempdbbase import TempDbBase


class ShowPoints(TempDbBase):
    """
    LectureAnswer class to handle database for lecture answers
    """
    def add_show_points(self, lecture_id: int, asked_id: int):
        points = self.table(lecture_id, asked_id)
        self.session.merge(points)
        self.session.commit()

    def stop_showing_points(self, lecture_id: int):
        self.table.query.filter_by(lecture_id=lecture_id).delete()
        self.session.commit()

    def get_currently_shown_points(self, lecture_id: int):
        points = self.table.query.filter_by(lecture_id=lecture_id)
        points = points.all()
        return points
