import imghdr
import io
import mimetypes
import os
import posixpath

import magic
from bs4 import UnicodeDammit
from flask import Blueprint, request, send_file
from flask import abort
from werkzeug.utils import secure_filename

from plugin import Plugin
from routes.common import logged_in, getTimDb, jsonResponse, getCurrentUserGroup, okJsonResponse, validate_item_and_create, \
    verify_view_access, verify_task_access, getCurrentUserName, \
    verify_seeanswers_access
from timdb.tim_models import Block
from timdb.timdbbase import blocktypes

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
    if not 3 <= slashes <= 4:
        abort(400)
    if slashes == 3 and not relfilename.endswith('/'):
        abort(400, 'Incorrect filename specification.')
    timdb = getTimDb()
    block = Block.query.filter((Block.description.startswith(relfilename)) & (Block.type_id == blocktypes.UPLOAD)).order_by(Block.description.desc()).first()
    if not block or (block.description != relfilename and not relfilename.endswith('/')):
        abort(404, 'The requested upload was not found.')
    if not verify_view_access(block.id, require=False):
        answer = block.answerupload.first().answer
        doc_id, task_name, _ = Plugin.parse_task_id(answer.task_id)
        verify_seeanswers_access(doc_id)

    data = timdb.uploads.get_file(block.description)
    f = io.BytesIO(data)
    p = os.path.join(timdb.uploads.blocks_path, block.description)
    mt = get_mimetype(p)
    return send_file(f, mimetype=mt, add_etags=False)


@upload.route('/pluginUpload/<int:doc_id>/<task_id>/<user_id>/', methods=['POST'])
def pluginupload_file2(doc_id: int, task_id: str, user_id):
    return pluginupload_file(doc_id, task_id)


@upload.route('/pluginUpload/<int:doc_id>/<task_id>/', methods=['POST'])
def pluginupload_file(doc_id: int, task_id: str):
    verify_task_access(doc_id, task_id)
    file = request.files.get('file')
    if file is None:
        abort(400, 'Missing file')
    content = file.read()
    timdb = getTimDb()
    p = os.path.join(str(doc_id), secure_filename(task_id), str(getCurrentUserName()))
    answerupload = timdb.uploads.save_file(content, p,
                                           secure_filename(file.filename),
                                           getCurrentUserGroup())
    relfilename = answerupload.block.description
    p = os.path.join(timdb.uploads.blocks_path, relfilename)
    mt = get_mimetype(p)
    relfilename = os.path.join('/uploads', relfilename)
    return jsonResponse({"file": relfilename, "type": mt, "block": answerupload.block.id})


@upload.route('/upload/', methods=['POST'])
def upload_file():
    if not logged_in():
        abort(403, 'You have to be logged in to upload a file.')
    timdb = getTimDb()

    file = request.files.get('file')
    if file is None:
        abort(400, 'Missing file')
    folder = request.form.get('folder')
    if folder is None:
        return upload_image_or_file(file)
    filename = posixpath.join(folder, secure_filename(file.filename))

    validate_item_and_create(filename, 'document', getCurrentUserGroup())

    if not allowed_file(file.filename):
        abort(403, 'The file format is not allowed.')

    if filename.endswith(tuple(DOC_EXTENSIONS)):
        content = UnicodeDammit(file.read()).unicode_markup
        if not content:
            abort(400, 'Failed to convert the file to UTF-8.')
        timdb.documents.import_document(content, filename, getCurrentUserGroup())
        return okJsonResponse()
    else:
        abort(400, 'Invalid document extension')


def try_upload_image(image_file):
    content = image_file.read()
    imgtype = imghdr.what(None, h=content)
    if imgtype is not None:
        timdb = getTimDb()
        img_id, img_filename = timdb.images.saveImage(content,
                                                      secure_filename(image_file.filename),
                                                      getCurrentUserGroup())
        timdb.users.grant_view_access(0, img_id)  # So far everyone can see all images
        return jsonResponse({"file": str(img_id) + '/' + img_filename})
    else:
        abort(400, 'Invalid image type')


def upload_image_or_file(image_file):
    content = image_file.read()
    imgtype = imghdr.what(None, h=content)
    timdb = getTimDb()
    if imgtype is not None:
        img_id, img_filename = timdb.images.saveImage(content,
                                                      secure_filename(image_file.filename),
                                                      getCurrentUserGroup())
        timdb.users.grant_view_access(timdb.users.get_anon_group_id(), img_id)  # So far everyone can see all images
        return jsonResponse({"image": str(img_id) + '/' + img_filename})
    else:
        file_id, file_filename = timdb.files.saveFile(content,
                                                       secure_filename(image_file.filename),
                                                       getCurrentUserGroup())
        timdb.users.grant_view_access(timdb.users.get_anon_group_id(), file_id)  # So far everyone can see all files
        return jsonResponse({"file": str(file_id) + '/' + file_filename})


@upload.route('/files/<int:file_id>/<file_filename>')
def get_file(file_id, file_filename):
    timdb = getTimDb()
    file_filename = secure_filename(file_filename)
    if not timdb.files.fileExists(file_id, file_filename):
        abort(404)
    verify_view_access(file_id)
    mime = magic.Magic(mime=True)
    file_path = timdb.files.getFilePath(file_id, file_filename)
    mt = mime.from_file(file_path).decode('utf-8')
    return send_file(file_path, mimetype=mt)
