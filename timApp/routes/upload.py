import imghdr
import io
import os

import posixpath

import magic
from bs4 import UnicodeDammit
from flask import Blueprint, request, send_file
from werkzeug.utils import secure_filename

from routes.common import logged_in, getTimDb, jsonResponse, getCurrentUserGroup, okJsonResponse, validate_item_and_create, \
    verify_view_access
from flask import abort

upload = Blueprint('upload',
                   __name__,
                   url_prefix='')


def allowed_file(filename):
    return '.' in filename and \
           filename.rsplit('.', 1)[1].lower() in ALLOWED_EXTENSIONS


DOC_EXTENSIONS = ['txt', 'md', 'markdown']
PIC_EXTENSIONS = ['png', 'jpg', 'jpeg', 'gif']
ALLOWED_EXTENSIONS = set(PIC_EXTENSIONS + DOC_EXTENSIONS)


@upload.route('/uploads/<path:relfilename>')
def get_upload(relfilename):
    timdb = getTimDb()
    #if not timdb.images.imageExists(image_id, image_filename):
    #    abort(404)
    #verify_view_access(image_id) # TODO UPLOAD: verify user has right to see the file
    data = timdb.uploads.get_file(relfilename)
    # imgtype = imghdr.what(None, h=data)
    f = io.BytesIO(data)
    mime = magic.Magic(mime=True)
    p = os.path.join(timdb.uploads.blocks_path, relfilename)
    mt = mime.from_file(p).decode('utf-8')
    return send_file(f, mimetype=mt)
    # return send_file(f, mimetype='image/' + imgtype)


@upload.route('/pluginUpload/<doc_id>/<plugin_id>/<user_id>/', methods=['POST'])
def pluginUpload_file(doc_id: str, plugin_id: str, user_id: str):
    if not logged_in():
        abort(403, 'You have to be logged in to upload a file.')
    # TODO UPLOAD: Check user has right to upload to this task
    timdb = getTimDb()

    file = request.files.get('file')
    if file is None:
        abort(400, 'Missing file')
    folder = request.form.get('folder')
    if folder is None:
        # return upload_image_or_file(file)
        content = file.read()
        # imgtype = imghdr.what(None, h=content)
        timdb = getTimDb()
        p = os.path.join(secure_filename(doc_id), secure_filename(plugin_id), secure_filename(user_id))
        relfilename = timdb.uploads.save_file(content, p,
                                              secure_filename(file.filename),
                                              getCurrentUserGroup())
        mime = magic.Magic(mime=True)
        p = os.path.join(timdb.uploads.blocks_path, relfilename)
        mt = mime.from_file(p).decode('utf-8')
        # timdb.users.grant_view_access(timdb.users.get_anon_group_id(), img_id)  # So far everyone can see all images
        # TODO put here the rights for the user or group
        relfilename = os.path.join('/uploads',relfilename)
        return jsonResponse({"file": relfilename,"type": mt})

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
        file_id, file_filename = timdb.files.save_file(content,
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
