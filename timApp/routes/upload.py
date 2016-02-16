import imghdr

import posixpath
from bs4 import UnicodeDammit
from flask import Blueprint, request
from werkzeug.utils import secure_filename

from routes.common import logged_in, getTimDb, jsonResponse, getCurrentUserGroup, okJsonResponse, validate_item
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

    validate_item(filename, 'document', getCurrentUserGroup())

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
