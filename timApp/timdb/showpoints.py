from contracts import contract
from timdb.timdbbase import TimDbBase


class ShowPoints(TimDbBase):
    """
    LectureAnswer class to handle database for lecture answers
    """
    @contract
    def add_show_points(self, lecture_id: "int", asked_id: "int", commit: 'bool'=True):
        """
        Adds a running question that is related to lecture
        :param lecture_id: lecture id
        :param asked_id: asked questions id
        :return:
        """
        cursor = self.db.cursor()

        cursor.execute("""
            INSERT INTO ShowPoints(lecture_id, asked_id)
            VALUES (?,?)
        """, [lecture_id, asked_id])

        if commit:
            self.db.commit()

    @contract
    def stop_showing_points(self, lecture_id: "int", commit: 'bool'=True):
        """
        Remove a running question that is related to lecture
        :param asked_id: asked questions id
        :return:
        """
        cursor = self.db.cursor()

        cursor.execute("""
            DELETE FROM ShowPoints
            WHERE  lecture_id = ?
        """, [lecture_id])

        if commit:
            self.db.commit()

    def get_currently_shown_points(self, lecture_id: "int"):
        cursor = self.db.cursor()

        cursor.execute("""
            SELECT *
            FROM ShowPoints
            WHERE lecture_id = ?
        """, [lecture_id])

        questions = cursor.fetchall()
        return questions