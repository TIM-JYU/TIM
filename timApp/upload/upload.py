import imghdr
import io
import json
import mimetypes
import os
import posixpath
from os import path as os_path
from pathlib import Path

import magic
from flask import Blueprint, request, send_file
from flask import abort
from werkzeug.utils import secure_filename

import timApp.util.pdftools
from timApp.auth.accesshelper import verify_view_access, verify_seeanswers_access, verify_task_access, \
    grant_access_to_session_users, get_doc_or_abort
from timApp.auth.accesstype import AccessType
from timApp.auth.sessioninfo import get_current_user_group, logged_in, get_current_user_object
from timApp.document.documents import import_document
from timApp.item.block import Block, BlockType
from timApp.item.validation import validate_item_and_create, validate_uploaded_document_content
from timApp.plugin.plugin import Plugin
from timApp.timdb.dbaccess import get_timdb
from timApp.timdb.sqa import db
from timApp.upload.uploadedfile import UploadedFile, PluginUpload, PluginUploadInfo
from timApp.user.userutils import grant_view_access, get_anon_group_id
from timApp.util.flask.responsehelper import json_response

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
    block = Block.query.filter((Block.description.startswith(relfilename)) & (
            Block.type_id == BlockType.Upload.value)).order_by(Block.description.desc()).first()
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

    up = PluginUpload(block)
    p = up.filesystem_path.as_posix()
    mt = get_mimetype(p)
    return send_file(p, mimetype=mt, add_etags=False)


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
    u = get_current_user_object()
    f = UploadedFile.save_new(
        content,
        file.filename,
        BlockType.Upload,
        upload_info=PluginUploadInfo(
            task_id_name=task_id,
            user=u,
            doc=d))
    f.block.set_owner(u.get_personal_group())
    grant_access_to_session_users(timdb, f.id)
    mt = get_mimetype(f.filesystem_path.as_posix())
    db.session.commit()
    return json_response(
        {
            "file": (Path('/uploads') / f.relative_filesystem_path).as_posix(),
            "type": mt,
            "block": f.id,
        })


@upload.route('/upload/', methods=['POST'])
def upload_file():
    if not logged_in():
        abort(403, 'You have to be logged in to upload a file.')
    file = request.files.get('file')
    if file is None:
        abort(400, 'Missing file')

    try:
        attachment_params = json.loads(request.form.get('attachmentParams'))
        autostamp = attachment_params[len(attachment_params) - 1]
    except:
        # Just go on with normal upload if necessary conditions are not met.
        pass
    else:
        if autostamp:
            # Only go here if attachment params are valid enough and autostamping is valid and true
            # because otherwise normal uploading may be interrupted.
            try:
                stamp_data = timApp.util.pdftools.attachment_params_to_dict(attachment_params)
                return upload_and_stamp_attachment(file, stamp_data)
            # If attachment isn't a pdf, gives an error too (since it's in 'showPdf' plugin)
            except timApp.util.pdftools.PdfError as e:
                abort(400, str(e))

    folder = request.form.get('folder')

    if folder is None:
        return upload_image_or_file(file)
    path = posixpath.join(folder, os.path.splitext(secure_filename(file.filename))[0])

    content = validate_uploaded_document_content(file)
    validate_item_and_create(path, 'document', get_current_user_group())

    doc = import_document(content, path, get_current_user_group())
    db.session.commit()
    return json_response({'id': doc.doc_id})


def upload_and_stamp_attachment(file, stamp_data):
    """
    Uploads the file and makes a stamped version of it into the same folder.
    :param file: The file to upload and stamp.
    :param stamp_data: Stamp data (attachment and list ids and format) without the path.
    :return: Json response containing the stamped file path.
    """

    # TODO: Get this from a global variable.
    # Currently multiple changes in different modules are needed
    # if the upload folder is changed.
    attachment_folder = "/tim_files/blocks/files"
    content = file.read()

    f = save_file_and_grant_access(content, file, BlockType.File)

    # If format not set or invalid, default format set in pdftools will be used.
    # Additional check is done inside pdftools-module,
    # since this may not work correctly.
    try:
        stamp_format = stamp_data[0]['format']
    except:
        stamp_format = timApp.util.pdftools.default_stamp_format

    # Add the uploaded file path (the one to stamp) to stamp data.
    stamp_data[0]['file'] = os_path.join(attachment_folder, f"{f.id}/{f.filename}")

    output = timApp.util.pdftools.stamp_pdfs(
            stamp_data,
            dir_path=os_path.join(attachment_folder, str(f.id) + "/"),
            stamp_text_format=stamp_format)[0]

    stamped_filename = timApp.util.pdftools.get_base_filename(output)

    # TODO: In case of raised errors give proper no-upload response?
    return json_response({"file": f"{str(f.id)}/{stamped_filename}"})


def upload_image_or_file(image_file):
    content = image_file.read()
    imgtype = imghdr.what(None, h=content)
    if imgtype is not None:
        f = save_file_and_grant_access(content, image_file, BlockType.Image)
        db.session.commit()
        return json_response({"image": f'{f.id}/{f.filename}'})
    else:
        f = save_file_and_grant_access(content, image_file, BlockType.File)
        db.session.commit()
        return json_response({"file": f'{f.id}/{f.filename}'})


def save_file_and_grant_access(content, image_file, block_type: BlockType) -> UploadedFile:
    f = UploadedFile.save_new(content, image_file.filename, block_type)
    f.block.set_owner(get_current_user_object().get_personal_group())
    grant_view_access(get_anon_group_id(), f.id)  # So far everyone can see all files
    return f


@upload.route('/files/<int:file_id>/<file_filename>')
def get_file(file_id, file_filename):
    f = UploadedFile.find_by_id_and_type(file_id, BlockType.File)
    if not f:
        abort(404, 'File not found')
    verify_view_access(f)
    mime = magic.Magic(mime=True)
    file_path = f.filesystem_path.as_posix()
    if file_filename != f.filename:
        abort(404, 'File not found')
    mt = mime.from_file(file_path)
    if mt == 'image/svg':
        mt += '+xml'
    if isinstance(mt, bytes):
        mt = mt.decode('utf-8')
    return send_file(file_path, mimetype=mt)


@upload.route('/images/<int:image_id>/<image_filename>')
def get_image(image_id, image_filename):
    f = UploadedFile.find_by_id_and_type(image_id, BlockType.Image)
    if not f:
        abort(404, 'Image not found')
    verify_view_access(f)
    if image_filename != f.filename:
        abort(404, 'Image not found')
    img_data = f.data
    imgtype = imghdr.what(None, h=img_data)
    f = io.BytesIO(img_data)
    return send_file(f, mimetype='image/' + imgtype)
