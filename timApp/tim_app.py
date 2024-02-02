"""Creates the Flask application for TIM.

Insert only configuration-related things in this file. Do NOT define routes here.

"""
import inspect
import json
import mimetypes
import os
import sys

from flask import Flask, Request
from flask_migrate import Migrate
from flask_wtf import CSRFProtect
from sqlalchemy import func
from sqlalchemy.sql.ddl import CreateTable
from werkzeug.middleware.proxy_fix import ProxyFix
from flask_babel import Babel

from timApp.answer.answer import Answer, AnswerSaver
from timApp.answer.answer_models import AnswerTag, AnswerUpload, UserAnswer
from timApp.auth.auth_models import AccessTypeModel, BlockAccess
from timApp.auth.oauth2.models import OAuth2Token, OAuth2AuthorizationCode
from timApp.auth.session.model import UserSession
from timApp.auth.sessioninfo import get_current_user_object
from timApp.celery_sqlalchemy_scheduler import (
    IntervalSchedule,
    CrontabSchedule,
    SolarSchedule,
    PeriodicTaskChanged,
    PeriodicTask,
)
from timApp.document.docentry import DocEntry
from timApp.document.translation.deepl import (
    DeeplTranslationService,
    DeeplProTranslationService,
)
from timApp.document.translation.language import Language
from timApp.document.translation.reversingtranslator import ReversingTranslationService
from timApp.document.translation.translation import Translation
from timApp.document.translation.translator import (
    TranslationService,
    RegisteredTranslationService,
    TranslationServiceKey,
)
from timApp.folder.folder import Folder
from timApp.item.block import Block
from timApp.item.blockassociation import BlockAssociation
from timApp.item.blockrelevance import BlockRelevance
from timApp.item.tag import Tag
from timApp.item.taskblock import TaskBlock
from timApp.lecture.askedjson import AskedJson
from timApp.lecture.askedquestion import AskedQuestion
from timApp.lecture.lecture import Lecture
from timApp.lecture.lectureanswer import LectureAnswer
from timApp.lecture.lectureusers import LectureUsers
from timApp.lecture.message import Message
from timApp.lecture.question import Question
from timApp.lecture.questionactivity import QuestionActivity
from timApp.lecture.runningquestion import RunningQuestion
from timApp.lecture.showpoints import ShowPoints
from timApp.lecture.useractivity import UserActivity
from timApp.messaging.messagelist.messagelist_models import (
    MessageListModel,
    MessageListMember,
    MessageListExternalMember,
    MessageListTimMember,
    MessageListDistribution,
)
from timApp.messaging.timMessage.internalmessage_models import (
    InternalMessage,
    InternalMessageDisplay,
    InternalMessageReadReceipt,
)
from timApp.note.usernote import UserNote
from timApp.notification.notification import Notification
from timApp.notification.pending_notification import (
    PendingNotification,
    DocumentNotification,
    CommentNotification,
    AnswerNotification,
)
from timApp.peerreview.peerreview import PeerReview
from timApp.plugin.calendar.models import (
    Event,
    EventGroup,
    Enrollment,
    EnrollmentType,
    ExportedCalendar,
    EventTag,
    EventTagAttachment,
)
from timApp.plugin.plugintype import PluginType
from timApp.plugin.timtable.row_owner_info import RowOwnerInfo
from timApp.printing.printeddoc import PrintedDoc
from timApp.readmark.readparagraph import ReadParagraph
from timApp.sisu.scimusergroup import ScimUserGroup
from timApp.slide.slidestatus import SlideStatus
from timApp.timdb.sqa import db
from timApp.user.consentchange import ConsentChange
from timApp.user.hakaorganization import HakaOrganization
from timApp.user.newuser import NewUser
from timApp.user.personaluniquecode import PersonalUniqueCode
from timApp.user.user import User
from timApp.user.usercontact import UserContact
from timApp.user.usergroup import UserGroup
from timApp.user.usergroupdoc import UserGroupDoc
from timApp.user.usergroupmember import UserGroupMember
from timApp.user.verification.verification import (
    Verification,
    ContactAddVerification,
    SetPrimaryContactVerification,
)
from timApp.util.flask.filters import (
    map_format,
    timdate,
    humanize_timedelta,
    humanize_datetime,
)
from timApp.util.flask.user_agent import SimpleUserAgent
from timApp.util.logger import setup_logging
from timApp.util.utils import datestr_to_relative, date_to_relative
from timApp.velp.annotation_model import Annotation
from timApp.velp.velp_models import (
    Velp,
    VelpContent,
    VelpGroup,
    VelpGroupDefaults,
    VelpGroupLabel,
    VelpGroupSelection,
    VelpGroupsInDocument,
    VelpInGroup,
    VelpLabel,
    VelpLabelContent,
    VelpVersion,
    LabelInVelp,
    AnnotationComment,
)
from tim_common.timjsonencoder import TimJsonProvider

# All SQLAlchemy models must be imported in this module.
all_models = (
    AccessTypeModel,
    Annotation,
    AnnotationComment,
    Answer,
    AnswerNotification,
    AnswerSaver,
    AnswerTag,
    AnswerUpload,
    AskedJson,
    AskedQuestion,
    Block,
    BlockAccess,
    BlockAssociation,
    BlockRelevance,
    CommentNotification,
    ConsentChange,
    ContactAddVerification,
    SetPrimaryContactVerification,
    DeeplTranslationService,
    DeeplProTranslationService,
    DocEntry,
    DocumentNotification,
    Enrollment,
    EnrollmentType,
    Event,
    EventGroup,
    EventTag,
    EventTagAttachment,
    ExportedCalendar,
    Folder,
    HakaOrganization,
    InternalMessage,
    InternalMessageReadReceipt,
    InternalMessageDisplay,
    LabelInVelp,
    Language,
    Lecture,
    LectureAnswer,
    LectureUsers,
    Message,
    MessageListDistribution,
    MessageListMember,
    MessageListModel,
    MessageListTimMember,
    MessageListExternalMember,
    NewUser,
    Notification,
    OAuth2AuthorizationCode,
    OAuth2Token,
    PeerReview,
    PendingNotification,
    PersonalUniqueCode,
    PluginType,
    PrintedDoc,
    Question,
    QuestionActivity,
    ReadParagraph,
    RegisteredTranslationService,
    ReversingTranslationService,
    RowOwnerInfo,
    RunningQuestion,
    ScimUserGroup,
    ShowPoints,
    SlideStatus,
    Tag,
    TaskBlock,
    Translation,
    TranslationService,
    TranslationServiceKey,
    User,
    UserActivity,
    UserAnswer,
    UserContact,
    UserGroup,
    UserGroupDoc,
    UserGroupMember,
    UserNote,
    UserSession,
    Velp,
    VelpContent,
    VelpGroup,
    VelpGroupDefaults,
    VelpGroupLabel,
    VelpGroupSelection,
    VelpGroupsInDocument,
    VelpInGroup,
    VelpLabel,
    VelpLabelContent,
    VelpVersion,
    Verification,
    CrontabSchedule,
    IntervalSchedule,
    PeriodicTask,
    PeriodicTaskChanged,
    SolarSchedule,
)

sys.setrecursionlimit(10000)
app = Flask(__name__)

app.json = TimJsonProvider(app)
app.json_provider_class = TimJsonProvider

# The autoescape setting needs to be forced because the template file extension used in TIM is jinja2.
# The more accurate file extension helps IDEs recognize the file type better.
app.jinja_env.autoescape = True

app.jinja_env.trim_blocks = True
app.jinja_env.lstrip_blocks = True
app.config.from_pyfile("defaultconfig.py", silent=False)
app.config.from_envvar("TIM_SETTINGS", silent=True)
app.config.from_file("hosts.json", load=json.load, silent=True)
Request.user_agent_class = SimpleUserAgent
setup_logging(app)

# Compress(app)
db.init_app(app)
db.app = app
migrate = Migrate(app, db)

csrf = CSRFProtect(app)

app.jinja_env.filters["map_format"] = map_format
app.jinja_env.filters["datestr_to_relative"] = datestr_to_relative
app.jinja_env.filters["date_to_relative"] = date_to_relative
app.jinja_env.filters["timdate"] = timdate
app.jinja_env.filters["timtimedelta"] = humanize_timedelta
app.jinja_env.filters["timreldatetime"] = humanize_datetime
app.jinja_env.add_extension("jinja2.ext.do")
app.jinja_env.add_extension("jinja2.ext.i18n")


def get_locale():
    print("hello")
    with app.app_context():
        u = get_current_user_object()
        print(u)
        prefs = u.get_prefs()
        print("kieli", prefs.language)
    return prefs.language


babel = Babel(app, locale_selector=get_locale)

mimetypes.add_type("text/plain", ".scss")
# Caddy sets the following headers:
# X-Forwarded-For: <ip>
# X-Forwarded-Proto: <http/https>
# There is always one proxy (internal caddy) + possible extra proxies
num_proxies = 2 if os.environ.get("CADDY_IS_PROXIED") == "1" else 1
app.wsgi_app = ProxyFix(
    app.wsgi_app,
    x_for=num_proxies,
    x_proto=1,
    x_host=0,
    x_port=0,
    x_prefix=0,
)


@app.shell_context_processor
def make_shell_context():
    ctx = {c.__name__: c for c in all_models}
    ctx["db"] = db
    ctx["func"] = func
    return ctx


def print_schema(bind: str | None = None):
    """Prints the database schema generated by the models.

    :param bind: The bind to use.

    """
    with app.app_context():
        models = inspect.getmembers(
            sys.modules[__name__],
            lambda x: inspect.isclass(x) and hasattr(x, "__table__"),
        )
        eng = db.engines[bind]

        for _, model_class in models:
            print(CreateTable(model_class.__table__).compile(eng), end=";")
        print()
        sys.stdout.flush()


# print_schema()


def get_home_organization_group() -> UserGroup:
    return UserGroup.get_organization_group(app.config["HOME_ORGANIZATION"])
