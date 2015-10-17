__author__ = 'juuso'


class TempDbBase:

    def __init__(self, db, cursor):
        self.db = db
        self.cursor = cursor
