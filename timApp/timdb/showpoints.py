from contracts import contract
from timdb.tempdbbase import TempDbBase


class ShowPoints(TempDbBase):
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
        self.cursor.execute("""
            INSERT INTO ShowPoints(lecture_id, asked_id)
            VALUES (%s,%s)
        """, (lecture_id, asked_id))

        if commit:
            self.db.commit()

    @contract
    def stop_showing_points(self, lecture_id: "int", commit: 'bool'=True):
        """
        Remove a running question that is related to lecture
        :param asked_id: asked questions id
        :return:
        """
        self.cursor.execute("""
            DELETE FROM ShowPoints
            WHERE  lecture_id = %s
        """, [lecture_id])

        if commit:
            self.db.commit()

    def get_currently_shown_points(self, lecture_id: "int"):
        self.cursor.execute("""
            SELECT *
            FROM ShowPoints
            WHERE lecture_id = %s
        """, [lecture_id])

        questions = self.cursor.fetchall()
        return questions