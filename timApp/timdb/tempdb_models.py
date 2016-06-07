from timdb.runningquestion import RunningQuestions
from timdb.useractivity import UserActivity
from timdb.newanswers import NewAnswers
from timdb.showpoints import ShowPoints
from timdb.slidestatus import SlideStatuses
from timdb.temp_info_for_user import TempInfoUserQuestion
from tim_app import db


class Runningquestion(db.Model):
    __bind_key__ = 'tempdb'
    asked_id = db.Column(db.Integer, primary_key=True)
    lecture_id = db.Column(db.Integer, primary_key=True)
    ask_time = db.Column(db.BigInteger, nullable=False)
    end_time = db.Column(db.BigInteger)

    def __init__(self, asked_id, lecture_id, ask_time, end_time=None):
        self.asked_id = asked_id
        self.lecture_id = lecture_id
        self.ask_time = ask_time
        self.end_time = end_time


class Usershown(db.Model):
    __bind_key__ = 'tempdb'
    lecture_id = db.Column(db.Integer, nullable=False)
    asked_id = db.Column(db.Integer, primary_key=True)
    user_id = db.Column(db.Integer, primary_key=True)

    def __init__(self, lecture_id, asked_id, user_id):
        self.asked_id = asked_id
        self.lecture_id = lecture_id
        self.user_id = user_id


class Useranswered(db.Model):
    __bind_key__ = 'tempdb'
    lecture_id = db.Column(db.Integer, nullable=False)
    asked_id = db.Column(db.Integer, primary_key=True)
    user_id = db.Column(db.Integer, primary_key=True)

    def __init__(self, lecture_id, asked_id, user_id):
        self.asked_id = asked_id
        self.lecture_id = lecture_id
        self.user_id = user_id


class Userextended(db.Model):
    __bind_key__ = 'tempdb'
    lecture_id = db.Column(db.Integer, nullable=False)
    asked_id = db.Column(db.Integer, primary_key=True)
    user_id = db.Column(db.Integer, primary_key=True)

    def __init__(self, lecture_id, asked_id, user_id):
        self.asked_id = asked_id
        self.lecture_id = lecture_id
        self.user_id = user_id


class Showpoints(db.Model):
    __bind_key__ = 'tempdb'
    asked_id = db.Column(db.Integer, primary_key=True)
    lecture_id = db.Column(db.Integer, nullable=False)

    def __init__(self, lecture_id, asked_id):
        self.lecture_id = lecture_id
        self.asked_id = asked_id


class Newanswer(db.Model):
    __bind_key__ = 'tempdb'
    lecture_id = db.Column(db.Integer, nullable=False)
    asked_id = db.Column(db.Integer, primary_key=True)
    user_id = db.Column(db.Integer, primary_key=True)

    def __init__(self, lecture_id, asked_id, user_id):
        self.lecture_id = lecture_id
        self.asked_id = asked_id
        self.user_id = user_id


class Useractivity(db.Model):
    __bind_key__ = 'tempdb'
    lecture_id = db.Column(db.Integer, primary_key=True)
    user_id = db.Column(db.Integer, primary_key=True)
    active = db.Column(db.Text)

    def __init__(self, lecture_id, user_id, active):
        self.lecture_id = lecture_id
        self.user_id = user_id
        self.active = active


class Pointsshown(db.Model):
    __bind_key__ = 'tempdb'
    lecture_id = db.Column(db.Integer, nullable=False)
    asked_id = db.Column(db.Integer, primary_key=True)
    user_id = db.Column(db.Integer, primary_key=True)

    def __init__(self, lecture_id, asked_id, user_id):
        self.asked_id = asked_id
        self.lecture_id = lecture_id
        self.user_id = user_id


class Pointsclosed(db.Model):
    __bind_key__ = 'tempdb'
    lecture_id = db.Column(db.Integer, nullable=False)
    asked_id = db.Column(db.Integer, primary_key=True)
    user_id = db.Column(db.Integer, primary_key=True)

    def __init__(self, lecture_id, asked_id, user_id):
        self.asked_id = asked_id
        self.lecture_id = lecture_id
        self.user_id = user_id


class SlideStatus(db.Model):
    __bind_key__ = 'tempdb'
    doc_id = db.Column(db.Integer, primary_key=True)
    status = db.Column(db.Text, nullable=False)

    def __init__(self, doc_id, status):
        self.doc_id = doc_id
        self.status = status


class TempDb(object):
    def __init__(self):
        self.runningquestions = RunningQuestions(db, Runningquestion)
        self.showpoints = ShowPoints(db, Showpoints)
        self.useractivity = UserActivity(db, Useractivity)
        self.newanswers = NewAnswers(db, Newanswer)
        self.usersshown = TempInfoUserQuestion(db, Usershown)
        self.usersextended = TempInfoUserQuestion(db, Userextended)
        self.usersanswered = TempInfoUserQuestion(db, Useranswered)
        self.pointsshown = TempInfoUserQuestion(db, Pointsshown)
        self.pointsclosed = TempInfoUserQuestion(db, Pointsclosed)
        self.slidestatuses = SlideStatuses(db, SlideStatus)

tempdb = TempDb()


def initialize_temp_database():
    print('initializing the temp database...', end='')
    db.drop_all(bind='tempdb')
    db.create_all(bind='tempdb')
    print(' done.')
