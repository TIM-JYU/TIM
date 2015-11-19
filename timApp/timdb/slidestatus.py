from contracts import contract
from sqlalchemy.exc import IntegrityError
from timdb.tempdbbase import TempDbBase


class SlideStatuses(TempDbBase):
    @contract
    def update_or_add_status(self, doc_id: "int", status: "str"):
        status = self.table(doc_id, status)
        try:
            self.db.session.merge(status)
            self.db.session.commit()
        except IntegrityError:
            print("Info already exists")
            self.db.session.rollback()

    @contract
    def get_status(self, doc_id: "int"):
        rows = self.table.query.filter_by(doc_id=doc_id)
        row = rows.first()
        return row

    @contract
    def delete_user_info(self, lecture_id: "int", asked_id: "int", user_id: "int"):
        self.table.query.filter_by(lecture_id=lecture_id, asked_id=asked_id, user_id=user_id).delete()
        self.db.session.commit()

    @contract
    def delete_all_from_question(self, asked_id: "int"):
        self.table.query.filter_by(asked_id=asked_id).delete()
        self.db.session.commit()

    @contract
    def delete_all_from_lecture(self, lecture_id: "int"):
        self.table.query.filter_by(lecture_id=lecture_id).delete()
        self.db.session.commit()

    @contract
    def has_user_info(self, asked_id: "int", user_id: "int"):
        rows = self.table.query.filter_by(asked_id=asked_id, user_id=user_id)
        rows = rows.all()
        return len(rows) > 0
