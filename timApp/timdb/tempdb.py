"""
Handles temp database that is needed by lecture and questions.


from timdb.runningquestion import RunningQuestions
from timdb.useractivity import UserActivity
from timdb.newanswers import NewAnswers
from timdb.showpoints import ShowPoints
from timdb.temp_info_for_user import TempInfoUserQuestion


class TempDb(object):

    def __init__(self):
        self.runningquestions = RunningQuestions(self)
        self.showpoints = ShowPoints(self)
        self.useractivity = UserActivity(self)
        self.newanswers = NewAnswers(self)
        self.usersshown = TempInfoUserQuestion(self, models.Usershown)
        self.usersextended = TempInfoUserQuestion(self, models.Userextended)
        self.usersanswered = TempInfoUserQuestion(self, models.Useranswered)
        self.pointsshown = TempInfoUserQuestion(self, models.Pointsshown)
        self.pointsclosed = TempInfoUserQuestion(self, models.Pointsclosed)
"""