"""Creates the Flask application for TIM.

Insert only configuration-related things in this file. Do NOT define routes here.

"""
import inspect
import mimetypes
import os
import sys
from typing import Optional

from flask import Flask, Request
from flask_migrate import Migrate
from flask_wtf import CSRFProtect
from sqlalchemy import func
from sqlalchemy.sql.ddl import CreateTable
from werkzeug.middleware.proxy_fix import ProxyFix

from timApp.answer.answer import Answer, AnswerSaver
from timApp.answer.answer_models import AnswerTag, AnswerUpload, UserAnswer
from timApp.auth.auth_models import AccessTypeModel, BlockAccess
from timApp.auth.oauth2.models import OAuth2Token, OAuth2AuthorizationCode
from timApp.celery_sqlalchemy_scheduler import IntervalSchedule, CrontabSchedule, SolarSchedule, PeriodicTaskChanged, \
    PeriodicTask
from timApp.document.docentry import DocEntry
from timApp.document.timjsonencoder import TimJsonEncoder
from timApp.document.translation.translation import Translation
from timApp.folder.folder import Folder
from timApp.item.block import Block
from timApp.item.blockassociation import BlockAssociation
from timApp.item.blockrelevance import BlockRelevance
from timApp.item.tag import Tag
from timApp.lecture.askedjson import AskedJson
from timApp.lecture.askedquestion import AskedQuestion
from timApp.lecture.lecture import Lecture
from timApp.lecture.lectureanswer import LectureAnswer
from timApp.lecture.lectureusers import LectureUsers
from timApp.lecture.message import Message
from timApp.lecture.question import Question
from timApp.lecture.questionactivity import QuestionActivity
from timApp.lecture.runningquestion import Runningquestion
from timApp.lecture.showpoints import Showpoints
from timApp.lecture.useractivity import Useractivity
from timApp.messaging.messagelist.messagelist_models import MessageListModel, MessageListMember, \
    MessageListExternalMember, MessageListTimMember, MessageListDistribution
from timApp.messaging.timMessage.internalmessage_models import InternalMessage, InternalMessageDisplay, \
    InternalMessageReadReceipt
from timApp.note.usernote import UserNote
from timApp.notification.notification import Notification
from timApp.notification.pending_notification import PendingNotification, DocumentNotification, CommentNotification
from timApp.peerreview.peerreview import PeerReview
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
from timApp.user.usergroup import UserGroup
from timApp.user.usergroupdoc import UserGroupDoc
from timApp.user.usergroupmember import UserGroupMember
from timApp.util.flask.filters import map_format, timdate, humanize_timedelta, humanize_datetime
from timApp.util.flask.user_agent import SimpleUserAgent
from timApp.util.logger import setup_logging
from timApp.util.utils import datestr_to_relative, date_to_relative
from timApp.velp.annotation_model import Annotation
from timApp.velp.velp_models import Velp, VelpContent, VelpGroup, VelpGroupDefaults, VelpGroupLabel, \
    VelpGroupSelection, VelpGroupsInDocument, VelpInGroup, VelpLabel, VelpLabelContent, VelpVersion, \
    LabelInVelp, AnnotationComment

# All SQLAlchemy models must be imported in this module.
all_models = (
    AccessTypeModel,
    Annotation,
    AnnotationComment,
    Answer,
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
    DocEntry,
    DocumentNotification,
    Folder,
    HakaOrganization,
    InternalMessage,
    InternalMessageReadReceipt,
    InternalMessageDisplay,
    LabelInVelp,
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
    RowOwnerInfo,
    Runningquestion,
    ScimUserGroup,
    Showpoints,
    SlideStatus,
    Tag,
    Translation,
    User,
    Useractivity,
    UserAnswer,
    UserGroup,
    UserGroupDoc,
    UserGroupMember,
    UserNote,
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

    CrontabSchedule,
    IntervalSchedule,
    PeriodicTask,
    PeriodicTaskChanged,
    SolarSchedule,
)

sys.setrecursionlimit(10000)
app = Flask(__name__)

# The autoescape setting needs to be forced because the template file extension used in TIM is jinja2.
# The more accurate file extension helps IDEs recognize the file type better.
app.jinja_env.autoescape = True

app.jinja_env.trim_blocks = True
app.jinja_env.lstrip_blocks = True
app.config.from_pyfile('defaultconfig.py', silent=False)
app.config.from_envvar('TIM_SETTINGS', silent=True)
app.config.from_json('hosts.json', silent=True)
Request.user_agent_class = SimpleUserAgent
setup_logging(app)

# Compress(app)

# Disabling object expiration on commit makes testing easier
# because sometimes objects would expire after calling a route.
if app.config['TESTING']:
    db.session = db.create_scoped_session({'expire_on_commit': False})

db.init_app(app)
db.app = app
migrate = Migrate(app, db)

csrf = CSRFProtect(app)

app.jinja_env.filters['map_format'] = map_format
app.jinja_env.filters['datestr_to_relative'] = datestr_to_relative
app.jinja_env.filters['date_to_relative'] = date_to_relative
app.jinja_env.filters['timdate'] = timdate
app.jinja_env.filters['timtimedelta'] = humanize_timedelta
app.jinja_env.filters['timreldatetime'] = humanize_datetime
app.jinja_env.add_extension('jinja2.ext.do')

mimetypes.add_type('text/plain', '.scss')

app.json_encoder = TimJsonEncoder

# Caddy sets the following headers:
# X-Forwarded-For: <ip>
# X-Forwarded-Proto: <http/https>
# In prod_multi, there are 2 Caddys - the one in the container and the one in the host machine.
num_proxies = 2 if os.environ.get('COMPOSE_PROFILES') == 'prod_multi' else 1
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
    ctx = {
        c.__name__: c for c in all_models
    }
    ctx['db'] = db
    ctx['func'] = func
    return ctx


def print_schema(bind: Optional[str] = None):
    """Prints the database schema generated by the models.

    :param bind: The bind to use.

    """
    models = inspect.getmembers(sys.modules[__name__], lambda x: inspect.isclass(x) and hasattr(x, '__table__'))
    eng = db.get_engine(app, bind)

    for _, model_class in models:
        print(CreateTable(model_class.__table__).compile(eng), end=';')
    print()
    sys.stdout.flush()


# print_schema()


def get_home_organization_group() -> UserGroup:
    return UserGroup.get_organization_group(app.config['HOME_ORGANIZATION'])
