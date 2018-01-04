import imghdr
import io
import mimetypes
import os
import posixpath

import magic
from flask import Blueprint, request, send_file
from flask import abort
from werkzeug.utils import secure_filename

from timApp.accesshelper import verify_view_access, verify_seeanswers_access, verify_task_access, \
    grant_access_to_session_users, get_doc_or_abort
from timApp.dbaccess import get_timdb
from timApp.plugin import Plugin
from timApp.responsehelper import json_response
from timApp.sessioninfo import get_current_user_name, get_current_user_group, logged_in
from timApp.timdb.accesstype import AccessType
from timApp.timdb.blocktypes import blocktypes
from timApp.timdb.documents import import_document
from timApp.timdb.models.block import Block
from timApp.timdb.tim_models import db
from timApp.timdb.userutils import grant_view_access, get_anon_group_id
from timApp.validation import validate_item_and_create, validate_uploaded_document_content

upload = Blueprint('upload',
                   __name__,
                   url_prefix='')


def allowed_file(filename):
    return '.' in filename and \
           filename.rsplit('.', 1)[1].lower() in ALLOWED_EXTENSIONS


DOC_EXTENSIONS = ['txt', 'md', 'markdown']
PIC_EXTENSIONS = ['png', 'jpg', 'jpeg', 'gif']
ALLOWED_EXTENSIONS = set(PIC_EXTENSIONS + DOC_EXTENSIONS)


def get_mimetype(p):
    mt, code = mimetypes.guess_type(p)
    if not mt:
        mt = "text/plain"
    return mt
    # mime = magic.Magic(mime=True)
    # mt = mime.from_file(p).decode('utf-8')


@upload.route('/uploads/<path:relfilename>')
def get_upload(relfilename: str):
    slashes = relfilename.count('/')
    if slashes < 2:
        abort(400)
    if slashes == 2:
        relfilename += '/'
    if slashes == 3 and not relfilename.endswith('/'):
        abort(400, 'Incorrect filename specification.')
    timdb = get_timdb()
    block = Block.query.filter((Block.description.startswith(relfilename)) & (
        Block.type_id == blocktypes.UPLOAD)).order_by(Block.description.desc()).first()
    if not block or (block.description != relfilename and not relfilename.endswith('/')):
        abort(404, 'The requested upload was not found.')
    if not verify_view_access(block, require=False):
        answerupload = block.answerupload.first()

        # Answerupload may only be None for early test uploads (before the AnswerUpload model was implemented)
        # or if the upload process was interrupted at a specific point
        if answerupload is None:
            abort(403)
        answer = answerupload.answer
        doc_id, task_name, _ = Plugin.parse_task_id(answer.task_id)
        d = get_doc_or_abort(doc_id)
        verify_seeanswers_access(d)

    data = timdb.uploads.get_file(block.description)
    f = io.BytesIO(data)
    p = os.path.join(timdb.uploads.blocks_path, block.description)
    mt = get_mimetype(p)
    return send_file(f, mimetype=mt, add_etags=False)


# noinspection PyUnusedLocal
@upload.route('/pluginUpload/<int:doc_id>/<task_id>/<user_id>/', methods=['POST'])
def pluginupload_file2(doc_id: int, task_id: str, user_id):
    return pluginupload_file(doc_id, task_id)


@upload.route('/pluginUpload/<int:doc_id>/<task_id>/', methods=['POST'])
def pluginupload_file(doc_id: int, task_id: str):
    d = get_doc_or_abort(doc_id)
    verify_task_access(d, task_id, AccessType.view)
    file = request.files.get('file')
    if file is None:
        abort(400, 'Missing file')
    content = file.read()
    timdb = get_timdb()
    p = os.path.join(str(doc_id), secure_filename(task_id), str(get_current_user_name()))
    answerupload = timdb.uploads.save_file(content, p,
                                           secure_filename(file.filename),
                                           get_current_user_group())
    block_id = answerupload.block.id
    grant_access_to_session_users(timdb, block_id)
    relfilename = answerupload.block.description
    p = os.path.join(timdb.uploads.blocks_path, relfilename)
    mt = get_mimetype(p)
    relfilename = os.path.join('/uploads', relfilename)
    db.session.commit()
    return json_response({"file": relfilename, "type": mt, "block": answerupload.block.id})


@upload.route('/upload/', methods=['POST'])
def upload_file():
    if not logged_in():
        abort(403, 'You have to be logged in to upload a file.')
    file = request.files.get('file')
    if file is None:
        abort(400, 'Missing file')
    folder = request.form.get('folder')
    if folder is None:
        return upload_image_or_file(file)
    path = posixpath.join(folder, os.path.splitext(secure_filename(file.filename))[0])

    content = validate_uploaded_document_content(file)
    validate_item_and_create(path, 'document', get_current_user_group())

    doc = import_document(content, path, get_current_user_group())
    db.session.commit()
    return json_response({'id': doc.doc_id})


def try_upload_image(image_file):
    content = image_file.read()
    imgtype = imghdr.what(None, h=content)
    if imgtype is not None:
        timdb = get_timdb()
        img_id, img_filename = timdb.images.saveImage(content,
                                                      secure_filename(image_file.filename),
                                                      get_current_user_group())
        grant_view_access(0, img_id)  # So far everyone can see all images
        return json_response({"file": str(img_id) + '/' + img_filename})
    else:
        abort(400, 'Invalid image type')


def upload_image_or_file(image_file):
    content = image_file.read()
    imgtype = imghdr.what(None, h=content)
    timdb = get_timdb()
    if imgtype is not None:
        img_id, img_filename = timdb.images.saveImage(content,
                                                      secure_filename(image_file.filename),
                                                      get_current_user_group())
        grant_view_access(get_anon_group_id(), img_id)  # So far everyone can see all images
        return json_response({"image": str(img_id) + '/' + img_filename})
    else:
        file_id, file_filename = timdb.files.saveFile(content,
                                                      secure_filename(image_file.filename),
                                                      get_current_user_group())
        grant_view_access(get_anon_group_id(), file_id)  # So far everyone can see all files
        return json_response({"file": str(file_id) + '/' + file_filename})


@upload.route('/files/<int:file_id>/<file_filename>')
def get_file(file_id, file_filename):
    timdb = get_timdb()
    file_filename = secure_filename(file_filename)
    b = timdb.files.get_file_block(file_id, file_filename)
    if not b:
        abort(404)
    verify_view_access(b)
    mime = magic.Magic(mime=True)
    file_path = timdb.files.getFilePath(file_id, file_filename)
    mt = mime.from_file(file_path)
    if mt == 'image/svg':
        mt += '+xml'
    if isinstance(mt, bytes):
        mt = mt.decode('utf-8')
    return send_file(file_path, mimetype=mt)
