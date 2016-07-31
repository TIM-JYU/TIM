from timdb.tempdbbase import TempDbBase


class SlideStatuses(TempDbBase):
    def update_or_add_status(self, doc_id: int, statusstr: str):
        status = self.table(doc_id, statusstr)
        self.session.merge(status)
        self.session.commit()
        rows = self.table.query.filter_by(doc_id=doc_id)
        row = rows.first()
        return row

    def get_status(self, doc_id: int):
        rows = self.table.query.filter_by(doc_id=doc_id)
        row = rows.first()
        return row
