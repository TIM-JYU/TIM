"""Defines the temporary data models used by TIM.

"""
from datetime import datetime
from typing import Optional

from sqlalchemy.orm import scoped_session

from timApp.timdb.runningquestion import RunningQuestions
from timApp.timdb.showpoints import ShowPoints
from timApp.timdb.slidestatus import SlideStatuses
from timApp.timdb.temp_info_for_user import TempInfoUserQuestion
from timApp.timdb.tim_models import db
from timApp.timdb.useractivity import UserActivity


class Runningquestion(db.Model):
    __bind_key__ = 'tim_main'
    asked_id = db.Column(db.Integer, primary_key=True)
    lecture_id = db.Column(db.Integer, primary_key=True)  # TODO should not be part of primary key (asked_id is enough)
    ask_time = db.Column(db.DateTime(timezone=True), nullable=False, default=datetime.utcnow)
    end_time = db.Column(db.DateTime(timezone=True))

    def __init__(self, asked_id, lecture_id, ask_time, end_time=None):
        self.asked_id = asked_id
        self.lecture_id = lecture_id
        self.ask_time = ask_time
        self.end_time = end_time


class Usershown(db.Model):
    __bind_key__ = 'tim_main'
    lecture_id = db.Column(db.Integer, nullable=False)
    asked_id = db.Column(db.Integer, primary_key=True)
    user_id = db.Column(db.Integer, primary_key=True)

    def __init__(self, lecture_id, asked_id, user_id):
        self.asked_id = asked_id
        self.lecture_id = lecture_id
        self.user_id = user_id


class Useranswered(db.Model):
    __bind_key__ = 'tim_main'
    lecture_id = db.Column(db.Integer, nullable=False)
    asked_id = db.Column(db.Integer, primary_key=True)
    user_id = db.Column(db.Integer, primary_key=True)

    def __init__(self, lecture_id, asked_id, user_id):
        self.asked_id = asked_id
        self.lecture_id = lecture_id
        self.user_id = user_id


class Userextended(db.Model):
    __bind_key__ = 'tim_main'
    lecture_id = db.Column(db.Integer, nullable=False)
    asked_id = db.Column(db.Integer, primary_key=True)
    user_id = db.Column(db.Integer, primary_key=True)

    def __init__(self, lecture_id, asked_id, user_id):
        self.asked_id = asked_id
        self.lecture_id = lecture_id
        self.user_id = user_id


class Showpoints(db.Model):
    __bind_key__ = 'tim_main'
    asked_id = db.Column(db.Integer, primary_key=True)
    lecture_id = db.Column(db.Integer, nullable=False)

    def __init__(self, lecture_id, asked_id):
        self.lecture_id = lecture_id
        self.asked_id = asked_id


class Useractivity(db.Model):
    __bind_key__ = 'tim_main'
    lecture_id = db.Column(db.Integer, primary_key=True)
    user_id = db.Column(db.Integer, primary_key=True)
    active = db.Column(db.Text)  # TODO: Change to Timestamp

    def __init__(self, lecture_id, user_id, active):
        self.lecture_id = lecture_id
        self.user_id = user_id
        self.active = active


class Pointsshown(db.Model):
    __bind_key__ = 'tim_main'
    lecture_id = db.Column(db.Integer, nullable=False)
    asked_id = db.Column(db.Integer, primary_key=True)
    user_id = db.Column(db.Integer, primary_key=True)

    def __init__(self, lecture_id, asked_id, user_id):
        self.asked_id = asked_id
        self.lecture_id = lecture_id
        self.user_id = user_id


class Pointsclosed(db.Model):
    __bind_key__ = 'tim_main'
    lecture_id = db.Column(db.Integer, nullable=False)
    asked_id = db.Column(db.Integer, primary_key=True)
    user_id = db.Column(db.Integer, primary_key=True)

    def __init__(self, lecture_id, asked_id, user_id):
        self.asked_id = asked_id
        self.lecture_id = lecture_id
        self.user_id = user_id


class SlideStatus(db.Model):
    __bind_key__ = 'tim_main'
    doc_id = db.Column(db.Integer, primary_key=True)
    status = db.Column(db.Text, nullable=False)

    def __init__(self, doc_id, status):
        self.doc_id = doc_id
        self.status = status


class TempDb(object):

    def __init__(self, session: Optional[scoped_session]=None):
        if session is None:
            session = db.create_scoped_session()

        self.runningquestions = RunningQuestions(session, Runningquestion)
        self.showpoints = ShowPoints(session, Showpoints)
        self.useractivity = UserActivity(session, Useractivity)
        self.usersshown = TempInfoUserQuestion(session, Usershown)
        self.usersextended = TempInfoUserQuestion(session, Userextended)
        self.usersanswered = TempInfoUserQuestion(session, Useranswered)
        self.pointsshown = TempInfoUserQuestion(session, Pointsshown)
        self.pointsclosed = TempInfoUserQuestion(session, Pointsclosed)
        self.slidestatuses = SlideStatuses(session, SlideStatus)
