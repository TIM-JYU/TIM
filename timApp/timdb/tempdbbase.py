from sqlalchemy.orm import scoped_session

from timApp.timdb.tim_models import db


class TempDbBase:

    def __init__(self, session: scoped_session, table: db.Model):
        self.session = session
        self.table = table
