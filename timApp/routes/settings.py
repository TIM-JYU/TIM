"""Routes for settings view."""

from flask import Blueprint, render_template
from flask import abort
from flask import request
from jinja2 import TemplateNotFound

from timApp.accesshelper import verify_logged_in, verify_admin
from timApp.requesthelper import get_option
from timApp.responsehelper import json_response
from timApp.sessioninfo import get_current_user_object
from timApp.theme import get_available_themes
from timApp.timdb.accesstype import AccessType
from timApp.timdb.blocktypes import blocktypes
from timApp.timdb.models.block import Block
from timApp.timdb.models.docentry import DocEntry
from timApp.timdb.models.folder import Folder
from timApp.timdb.models.user import User
from timApp.timdb.tim_models import db, BlockAccess, AnswerUpload

settings_page = Blueprint('settings_page',
                          __name__,
                          url_prefix='/settings')


@settings_page.before_request
def verify_login():
    verify_logged_in()


@settings_page.route('')
def show():
    available_css_files = [{'name': theme.filename, 'desc': theme.description} for theme in get_available_themes()]

    try:
        return render_template('settings.html', css_files=available_css_files)
    except TemplateNotFound:
        abort(404)


@settings_page.route('/save', methods=['POST'])
def save_settings():
    get_current_user_object().set_prefs(request.get_json())
    db.session.commit()
    show()  # Regenerate CSS
    return json_response(get_current_user_object().get_prefs())


@settings_page.route('/get/<name>')
def get_setting(name):
    return json_response({name: get_current_user_object().get_prefs().get(name)})


def get_user_info(u: User, include_doc_content=False):
    """Returns all data associated with a user."""
    block_query = u.get_personal_group().accesses.filter_by(type=AccessType.owner.value).with_entities(
        BlockAccess.block_id)
    docs = DocEntry.query.filter(DocEntry.id.in_(block_query)).all()
    folders = Folder.query.filter(Folder.id.in_(block_query)).all()
    images = Block.query.filter(Block.id.in_(block_query) & (Block.type_id == blocktypes.IMAGE)).all()
    files = Block.query.filter(Block.id.in_(block_query) & (Block.type_id == blocktypes.FILE)).all()
    velpgroups = Block.query.filter(Block.id.in_(block_query) & (Block.type_id == blocktypes.VELPGROUP)).all()
    answers = u.answers.all()
    answer_uploads = AnswerUpload.query.filter(AnswerUpload.answer_id.in_([a.id for a in answers])).all()
    for d in docs:
        d.serialize_content = include_doc_content

    return {
        'annotations': u.annotations.all(),
        'answers': answers,
        'answer_uploads': answer_uploads,
        'groups': u.groups,
        'lectureanswers': u.lectureanswers.all(),
        'notes': u.get_personal_group().notes.all(),
        'owned_documents': docs,
        'owned_folders': folders,
        'owned_lectures': u.owned_lectures.all(),
        'readparagraphs': u.get_personal_group().readparagraphs.all(),
        'uploaded_images': images,
        'uploaded_files': files,
        'velpgroups': velpgroups,
        'velps': u.velps.all(),
    }


@settings_page.route('/info')
@settings_page.route('/info/<username>')
def get_info_route(username=None):
    verify_logged_in()
    if username:
        verify_admin()
        u = User.get_by_name(username)
        if not u:
            abort(404, 'User not found')
    else:
        u = get_current_user_object()
    include_doc_content = get_option(request, 'content', False)
    return json_response(get_user_info(u, include_doc_content))
