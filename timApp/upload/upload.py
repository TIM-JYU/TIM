import io
import json
import os
import posixpath
import subprocess
from dataclasses import dataclass
from pathlib import Path, PurePosixPath
from urllib.parse import unquote, urlparse

from PIL import Image
from PIL import UnidentifiedImageError
from PIL.Image import DecompressionBombError, registered_extensions
from flask import Blueprint, request, send_file, Response, url_for
from img2pdf import convert
from sqlalchemy import case, select
from werkzeug.utils import secure_filename

from timApp.auth.accesshelper import (
    verify_view_access,
    verify_seeanswers_access,
    verify_task_access,
    grant_access_to_session_users,
    get_doc_or_abort,
    verify_edit_access,
    AccessDenied,
    verify_answer_access,
)
from timApp.auth.accesstype import AccessType
from timApp.auth.sessioninfo import get_current_user_object, user_context_with_logged_in
from timApp.auth.sessioninfo import logged_in, get_current_user_group_object
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.documents import import_document
from timApp.document.viewcontext import default_view_ctx
from timApp.item.block import Block
from timApp.item.block import BlockType
from timApp.item.validation import (
    validate_item_and_create_intermediate_folders,
    validate_uploaded_document_content,
)
from timApp.plugin.pluginexception import PluginException
from timApp.plugin.taskid import TaskId, TaskIdAccess
from timApp.timdb.dbaccess import get_files_path
from timApp.timdb.sqa import db, run_sql
from timApp.upload.uploadedfile import (
    PluginUpload,
    PluginUploadInfo,
    UploadedFile,
    get_mimetype,
    is_script_safe_mimetype,
)
from timApp.user.user import User
from timApp.util.file_utils import guess_image_type
from timApp.util.flask.requesthelper import (
    use_model,
    RouteException,
    NotExist,
    get_option,
)
from timApp.util.flask.responsehelper import (
    json_response,
    ok_response,
    add_csp_header,
    safe_redirect,
)
from timApp.util.pdftools import (
    StampDataInvalidError,
    default_stamp_format,
    AttachmentStampData,
    PdfError,
    stamp_pdfs,
    create_tex_file,
    stamp_model_default_path,
    compress_pdf_if_not_already,
    CompressionError,
)

upload = Blueprint("upload", __name__, url_prefix="")


@upload.after_request
def set_csp(resp: Response):
    mime = resp.mimetype
    add_csp_if_not_script_safe(resp, mime)
    return resp


def add_csp_if_not_script_safe(resp: Response, mime: str, value: str = "sandbox"):
    if not is_script_safe_mimetype(mime):
        add_csp_header(resp, value)


def allowed_file(filename):
    return "." in filename and filename.rsplit(".", 1)[1].lower() in ALLOWED_EXTENSIONS


DOC_EXTENSIONS = ["txt", "md", "markdown"]
PIC_EXTENSIONS = ["png", "jpg", "jpeg", "gif"]
ALLOWED_EXTENSIONS = set(PIC_EXTENSIONS + DOC_EXTENSIONS)

# The folder for stamped and original pdf files.
default_attachment_folder = get_files_path() / "blocks/files"


@upload.get("/uploads/<path:relfilename>")
def get_upload(relfilename: str):
    mt, up = get_pluginupload(relfilename)
    return send_file(up.filesystem_path.as_posix(), mimetype=mt, etag=False)


def check_and_format_filename(relfilename: str) -> str:
    slashes = relfilename.count("/")
    if slashes < 2:
        raise RouteException()
    if slashes == 2:
        relfilename += "/"
    if slashes == 3 and not relfilename.endswith("/"):
        raise RouteException("Incorrect filename specification.")
    return relfilename


def get_pluginupload(relfilename: str) -> tuple[str, PluginUpload]:
    from timApp.peerreview.util.peerreview_utils import is_peerreview_enabled

    relfilename = check_and_format_filename(relfilename)
    block = (
        run_sql(
            select(Block)
            .filter(
                (Block.description.startswith(relfilename))
                & (Block.type_id == BlockType.Upload.value)
            )
            .order_by(Block.description.desc())
            .limit(1)
        )
        .scalars()
        .first()
    )
    if not block or (
        block.description != relfilename and not relfilename.endswith("/")
    ):
        raise RouteException("The requested upload was not found.")
    if not verify_view_access(block, require=False):
        answerupload = block.answerupload.first()

        # Answerupload may only be None for early test uploads (before the AnswerUpload model was implemented)
        # or if the upload process was interrupted at a specific point
        if answerupload is None:
            raise AccessDenied()
        answer = answerupload.answer
        if not answer:
            raise RouteException(
                "Upload has not been associated with any answer; it should be re-uploaded"
            )
        tid = TaskId.parse(answer.task_id)
        d = get_doc_or_abort(tid.doc_id)
        if (
            not verify_seeanswers_access(d, require=False)
            and get_current_user_object() not in answer.users_all
            and not is_peerreview_enabled(d)
        ):
            raise AccessDenied(
                "Sorry, you don't have permission to access this upload."
            )

    up = PluginUpload(block)
    p = up.filesystem_path.as_posix()
    mt = get_mimetype(p)
    return mt, up


def get_multiple_pluginuploads(relfilenames: list[str]) -> list[PluginUpload]:
    filenames = [check_and_format_filename(r) for r in relfilenames]
    ordering = case(
        {description: index for index, description in enumerate(filenames)},
        value=Block.description,
    )
    blocks: list[Block] = (
        run_sql(
            select(Block)
            .filter(
                (Block.description.in_(filenames))
                & (Block.type_id == BlockType.Upload.value)
            )
            .order_by(ordering)
        )
        .scalars()
        .all()
    )
    if len(blocks) < len(
        relfilenames
    ):  # TODO: Check if block can have multiple rows with equal desc
        for filename in filenames:
            found = False
            for b in blocks:
                if b.description.startswith(filename):
                    found = True
                    break
            if not found:
                raise RouteException(f"Requested upload {filename} could not be found")
    blocks_without_view_access = [
        b for b in blocks if not verify_view_access(b, require=False)
    ]
    doc_set = set()
    for block in blocks_without_view_access:
        answerupload = block.answerupload.first()
        if answerupload is None:
            raise AccessDenied()
        answer = answerupload.answer
        if not answer:
            raise RouteException(
                f"Upload {block.description} has not been associated with any answer; it should be re-uploaded"
            )
        tid = TaskId.parse(answer.task_id)
        if tid.doc_id not in doc_set:
            d = get_doc_or_abort(tid.doc_id)
            if (
                not verify_seeanswers_access(d, require=False)
                and get_current_user_object() not in answer.users_all
            ):
                raise AccessDenied(
                    "Sorry, you don't have permission to access this upload."
                )
            doc_set.add(tid.doc_id)
    ups = [PluginUpload(block) for block in blocks]
    return ups


# noinspection PyUnusedLocal
@upload.post("/pluginUpload/<int:doc_id>/<task_id>/<user_id>/")
def pluginupload_file2(doc_id: int, task_id: str, user_id):
    return pluginupload_file(doc_id, task_id)


@upload.post("/pluginUpload/<int:doc_id>/<task_id>/")
def pluginupload_file(doc_id: int, task_id: str):
    d = get_doc_or_abort(doc_id)
    try:
        tid = TaskId.parse(task_id, require_doc_id=False, allow_block_hint=False)
    except PluginException:
        raise RouteException()
    tid.doc_id = d.id
    task_access = verify_task_access(
        d,
        tid,
        AccessType.view,
        TaskIdAccess.ReadWrite,
        user_context_with_logged_in(None),
        default_view_ctx,
    )
    file = request.files.get("file")
    if file is None:
        raise RouteException("Missing file")
    content = file.read()
    u = get_current_user_object()
    f = UploadedFile.save_new(
        file.filename,
        BlockType.Upload,
        file_data=content,
        upload_info=PluginUploadInfo(task_id_name=task_id, user=u, doc=d),
    )
    f.block.set_owner(u.get_personal_group())
    grant_access_to_session_users(f)
    if f.is_content_pdf:
        try:
            compress_pdf_if_not_already(f)
        except CompressionError:
            raise RouteException(
                f"Failed to post-process {f.filesystem_path.name}. "
                f"Please make sure the PDF is not broken."
            )
    p = task_access.plugin
    if p.type == "reviewcanvas":
        returninfo = convert_pdf_or_compress_image(f, u, d, task_id)
    else:
        returninfo = [
            {
                "file": (Path("/uploads") / f.relative_filesystem_path).as_posix(),
                "type": f.content_mimetype,
                "block": f.id,
            }
        ]
    db.session.commit()
    return json_response(returninfo)


def simple_exif_transpose(image: Image):
    """
    Attempts to rotate an image according to exif data.

    ..note:: This method does not update the exif data

    """
    exif = image.getexif()
    orientation = exif.get(0x0112)
    method = {
        2: Image.Transpose.FLIP_LEFT_RIGHT,
        3: Image.Transpose.ROTATE_180,
        4: Image.Transpose.FLIP_TOP_BOTTOM,
        5: Image.Transpose.TRANSPOSE,
        6: Image.Transpose.ROTATE_270,
        7: Image.Transpose.TRANSVERSE,
        8: Image.Transpose.ROTATE_90,
    }.get(orientation)
    if method is not None:
        transposed_image = image.transpose(method)
        return transposed_image
    return image.copy()


def _downsample_image_canvas(img_path: Path) -> None:
    """
    Downsamples an image to fit into JS canvas area.

    ..note:: Different browsers have different limits for max canvas size, see
        https://developer.mozilla.org/en-US/docs/Web/HTML/Element/canvas#maximum_canvas_size

    ..note:: This method overwrites the original file.

    :param img: Path to image file
    """
    ext_mapping = registered_extensions()
    ext = img_path.suffix

    with Image.open(img_path) as img:
        # currently some common inputs are around 8000x18000 pixels, which should still be readable after resize
        # TODO: Split overly large images if necessary, warn users about large downsampling after upload
        img_format = ext_mapping.get(ext) or img.format
        img.thumbnail((2048, 8192))
        # some exif datas cause PIL.ImageOps.exif_transpose to fail,
        # so we just do the rotation part and remove the exif data
        img = simple_exif_transpose(img)
        img.getexif().clear()
        # Ensure that JPEG does not have alpha channel
        if img_format == "JPEG" and img.mode != "RGB":
            img = img.convert("RGB")
        img.save(img_path, format=img_format)


def convert_pdf_or_compress_image(f: UploadedFile, u: User, d: DocInfo, task_id: str):
    p = f.filesystem_path
    returninfo = []
    if f.content_mimetype.startswith("image/"):
        try:
            _downsample_image_canvas(p)
        except UnidentifiedImageError:
            raise RouteException(
                f"Unable to process image {f.filename}, image may be corrupt"
            )
        except DecompressionBombError as e:
            raise RouteException(
                f"Image is too large to upload {e.args[0][10:e.args[0].find(' exceeds')]}, please reduce the image size"
            )
        returninfo.append(
            {
                "file": (Path("/uploads") / f.relative_filesystem_path).as_posix(),
                "type": f.content_mimetype,
                "block": f.id,
            }
        )
    elif f.is_content_pdf:
        tempfolder = p.parent / "temp"
        tempfolder.mkdir()
        subprocess.run(
            [
                "gs",
                "-dNOPAUSE",
                "-dBATCH",
                "-sDEVICE=png16m",
                "-r200",  # TODO: Find decent value, default 72 too blurry for small text
                f"-sOutputFile={p.parent}/temp/{p.stem}_image-%d.png",
                p,
            ],
            capture_output=True,
        )
        pdf_image_pages = os.listdir(tempfolder)
        pdf_image_pages.sort(
            key=lambda x: int(x[len(p.stem) + 7 : -4])  # filename_image-KEY.png
        )
        for imagepath in pdf_image_pages:
            file = tempfolder / imagepath
            try:
                # TODO: Some pdfs have only one, very large page. Downsampling them to 2048px can make
                #   them unreadable, and should in some cases be split into smaller images instead
                _downsample_image_canvas(file)
            except UnidentifiedImageError:
                raise RouteException(
                    f"Unable to process pdf {f.filename}, some pages may be corrupt"
                )
            except DecompressionBombError as e:
                raise RouteException(
                    f"The pdf has a too large page to upload {e.args[0][10:e.args[0].find(' exceeds')]}, please reduce the size or split the pdf to smaller pages"
                )
            uf = UploadedFile.save_new(
                imagepath,
                BlockType.Upload,
                original_file=file,
                upload_info=PluginUploadInfo(task_id_name=task_id, user=u, doc=d),
            )
            uf.block.set_owner(u.get_personal_group())
            grant_access_to_session_users(uf)
            returninfo.append(
                {
                    "file": (Path("/uploads") / uf.relative_filesystem_path).as_posix(),
                    "type": uf.content_mimetype,
                    "block": uf.id,
                }
            )
        tempfolder.rmdir()
    else:
        raise RouteException("Upload needs to be an image or a pdf file")
    return returninfo


@upload.post("/upload/")
def upload_file():
    if not logged_in():
        raise AccessDenied("You have to be logged in to upload a file.")
    file = request.files.get("file")
    if file is None:
        raise RouteException("Missing file")
    folder = request.form.get("folder")
    if folder is not None:
        return upload_document(folder, file)
    doc_id = request.form.get("doc_id")
    if not doc_id:
        raise RouteException("Missing doc_id")
    d = DocEntry.find_by_id(int(doc_id))
    verify_edit_access(d)
    try:
        attachment_params = json.loads(request.form.get("attachmentParams"))
        autostamp = attachment_params[len(attachment_params) - 1]
        # TODO: Notify the user that the file type cannot be stamped
        if file.mimetype not in STAMPABLE_MIMETYPES and autostamp:
            raise StampDataInvalidError("Cannot stamp file")
    except:
        # Just go on with normal upload if necessary conditions are not met.
        return upload_image_or_file(d, file)
    else:
        if autostamp:
            # Only go here if attachment params are valid enough and autostamping is valid and true
            # because otherwise normal uploading may be interrupted.
            if len(attachment_params) < 6:
                raise StampDataInvalidError(
                    "Request missing parameters", attachment_params
                )
            try:
                stamp_format = attachment_params[1]
                # If stampformat is empty (as it's set to be if undefined in pareditor.ts), use default.
                if not stamp_format:
                    stamp_format = default_stamp_format
                stamp_data = AttachmentStampData(
                    date=attachment_params[0],
                    attachment=attachment_params[3],
                    issue=attachment_params[4],
                )
                custom_stamp_model = attachment_params[len(attachment_params) - 2]
                return upload_and_stamp_attachment(
                    d, file, stamp_data, stamp_format, custom_stamp_model
                )
            # If attachment isn't a pdf, gives an error too (since it's in 'showPdf' plugin)
            except PdfError as e:
                raise RouteException(str(e))


@dataclass
class AttachmentModel:
    issueNumber: int | str
    attachmentLetter: str
    uploadUrl: str
    upToDate: bool | None = None


@dataclass
class RestampModel:
    attachments: list[AttachmentModel]
    meetingDate: str
    stampFormat: str | None = None
    customStampModel: str | None = None


@upload.post("/upload/restamp")
@use_model(RestampModel)
def restamp_attachments(args: RestampModel):
    """
    Route for updating stamps for one or more uploaded attachments.
    :return: Ok response or an error in stamping process.
    """
    attachments, meeting_date, stamp_format, custom_stamp_model_content = (
        args.attachments,
        args.meetingDate,
        args.stampFormat,
        args.customStampModel,
    )

    if not stamp_format:
        stamp_format = default_stamp_format
    stamp_data_list = []
    attachment_folder = default_attachment_folder
    for a in attachments:
        stamp_data = AttachmentStampData(
            date=meeting_date, attachment=a.attachmentLetter, issue=a.issueNumber
        )
        # Parse link path and find unstamped attachment.
        # In case of errors abort the whole process.
        attachment_path = PurePosixPath(unquote(urlparse(a.uploadUrl).path))
        try:
            stamp_data.file = (
                attachment_folder
                / attachment_path.parts[-2]
                / attachment_path.parts[-1].replace("_stamped", "")
            )
        except IndexError:
            raise RouteException(f'Invalid attachment url: "{attachment_path}"')
        file = UploadedFile.find_by_id(attachment_path.parts[-2])
        if not file:
            raise RouteException(f'Attachment not found: "{attachment_path}"')

        verify_edit_access(file, check_parents=True)
        stamp_data_list.append(stamp_data)

    stamp_model_path = (
        create_tex_file(custom_stamp_model_content)
        if custom_stamp_model_content
        else stamp_model_default_path
    )
    stamp_pdfs(
        stamp_data_list,
        stamp_text_format=stamp_format,
        stamp_model_path=stamp_model_path,
    )

    return ok_response()


def upload_document(folder, file):
    path = posixpath.join(folder, os.path.splitext(secure_filename(file.filename))[0])

    content = validate_uploaded_document_content(file)
    validate_item_and_create_intermediate_folders(
        path, BlockType.Document, get_current_user_group_object()
    )

    doc = import_document(content, path, get_current_user_group_object())
    db.session.commit()
    return json_response({"id": doc.id})


STAMPABLE_MIMETYPES = {"application/pdf", "application/x-pdf"}


def upload_and_stamp_attachment(
    d: DocInfo,
    file,
    stamp_data: AttachmentStampData,
    stampformat: str,
    custom_stamp_model_content: str | None = None,
):
    """
    Uploads the file and makes a stamped version of it into the same folder.

    :param d: Document info.
    :param file: The file to upload and stamp.
    :param stamp_data: Stamp data object (attachment and list ids) without the path.
    :param stampformat: Formatting of stamp text.
    :param custom_stamp_model_content: LaTeX-string for a custom stamp.
    :return: Json response containing the stamped file path.
    """

    attachment_folder = default_attachment_folder
    content = file.read()

    f = save_file_and_grant_access(d, content, file, BlockType.File)

    # Add the uploaded file path (the one to stamp) to stamp data.

    stamp_data.file = attachment_folder / f"{f.id}/{f.filename}"

    stamp_model_path = (
        create_tex_file(custom_stamp_model_content)
        if custom_stamp_model_content
        else stamp_model_default_path
    )
    output = stamp_pdfs(
        [stamp_data], stamp_text_format=stampformat, stamp_model_path=stamp_model_path
    )[0]

    stamped_filename = output.name
    db.session.commit()

    # TODO: In case of raised errors give proper no-upload response?
    return json_response({"file": f"{str(f.id)}/{stamped_filename}"})


def upload_image_or_file(d: DocInfo, file):
    f, type_str = upload_image_or_file_impl(d, file)
    db.session.commit()
    return json_response({type_str: f"{f.id}/{f.filename}"})


def upload_image_or_file_impl(d: DocInfo, file):
    content = file.read()
    imgtype = guess_image_type(content)
    type_str = "image" if imgtype else "file"
    f = save_file_and_grant_access(d, content, file, BlockType.from_str(type_str))
    return f, type_str


def save_file_and_grant_access(
    d: DocInfo, content, file, block_type: BlockType
) -> UploadedFile:
    f = UploadedFile.save_new(file.filename, block_type, file_data=content)
    f.block.set_owner(get_current_user_object().get_personal_group())
    d.block.children.append(f.block)
    return f


@upload.get("/files/<path:file_id>/<file_filename>")
def get_file(file_id: str, file_filename: str) -> Response:
    if file_id.isdigit():
        f = UploadedFile.get_by_id_and_filename(int(file_id), file_filename)
    else:
        f = UploadedFile.get_by_doc_and_filename(file_id, file_filename)
    if not f:
        raise NotExist("File not found")
    verify_view_access(f, check_parents=True)
    file_path = f.filesystem_path.as_posix()
    send_plain = get_option(request, "plain", False)
    if send_plain:
        mime_type = "text/plain"
        conditional = False
    else:
        mime_type = get_mimetype(file_path)
        conditional = True
    res = send_file(file_path, mimetype=mime_type, conditional=conditional)
    # Some browsers (e.g. Firefox) seem to request ranges only when the server responds with Accept-Ranges: bytes
    # Here, we add the header to let the browser know they are supported
    if conditional:
        res.headers["Accept-Ranges"] = "bytes"
    return res


@upload.get("/reviewcanvaspdf/<user_name>_<doc_id>_<task_id>_<int:answer_id>.pdf")
def get_reviewcanvas_pdf(user_name: str, doc_id: int, task_id: str, answer_id: int):
    answer, _ = verify_answer_access(
        answer_id,
        get_current_user_object().id,
        default_view_ctx,
        require_teacher_if_not_own=True,
    )
    if not answer:
        raise RouteException("Invalid answer id")
    try:
        files = json.loads(answer.content)["uploadedFiles"]
        paths = [f["path"].removeprefix("/uploads/") for f in files]
    except (json.JSONDecodeError, KeyError):
        raise RouteException("Invalid answer format")
    blocks = get_multiple_pluginuploads(paths)
    if len(blocks) != len(files):
        raise RouteException(
            "Some images could no longer be found, please delete the broken images first"
        )
    byte_images = []
    for block, file in zip(blocks, files):
        img = Image.open(io.BytesIO(block.data))
        try:
            rotation = file["rotation"]
        except KeyError:
            raise RouteException("Invalid answer format")
        use_raw = True
        filetype = img.format  # lost at rotate
        if img.mode == "RGBA":
            use_raw = False
            img = img.convert("RGB")
        if rotation != 0:
            use_raw = False
            img = img.rotate(rotation * (-90), expand=True)
        if use_raw:
            byte_images.append(block.data)
        else:
            img = simple_exif_transpose(img)
            img_byte_arr = io.BytesIO()
            img.save(img_byte_arr, format=filetype)
            byte_images.append(img_byte_arr.getvalue())
    pdf = convert(byte_images)
    return send_file(
        io.BytesIO(pdf), download_name=f"{user_name}_{doc_id}_{task_id}_{answer_id}.pdf"
    )


@upload.get("/images/<path:image_id>/<image_filename>")
def get_image(image_id: str, image_filename: str) -> Response:
    if image_id.isdigit():
        f = UploadedFile.get_by_id_and_filename(int(image_id), image_filename)
    else:
        f = UploadedFile.get_by_doc_and_filename(image_id, image_filename)
    if not f:
        raise NotExist("Image not found")
    verify_view_access(f, check_parents=True)
    if image_filename != f.filename:
        raise NotExist("Image not found")
    imgtype = guess_image_type(f.filesystem_path)
    # Redirect if we can't deduce the image type
    if not imgtype:
        return safe_redirect(
            url_for("upload.get_file", file_id=image_id, file_filename=image_filename)
        )
    f = io.BytesIO(f.data)
    return send_file(f, mimetype="image/" + imgtype)
