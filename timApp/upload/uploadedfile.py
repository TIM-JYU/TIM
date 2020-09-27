import os
import re
from pathlib import Path
from typing import Optional, NamedTuple, Union

import magic
from werkzeug.utils import secure_filename

from timApp.answer.answer_models import AnswerUpload
from timApp.document.docinfo import DocInfo
from timApp.item.block import insert_block, Block, BlockType
from timApp.item.item import ItemBase
from timApp.timdb.dbaccess import get_files_path
from timApp.timdb.exceptions import TimDbException
from timApp.timdb.sqa import db
from timApp.user.user import User

DIR_MAPPING = {
    BlockType.File: 'files',
    BlockType.Image: 'images',
    BlockType.Upload: 'uploads',
}


class PluginUploadInfo(NamedTuple):
    """Additional information required for saving a :class:`PluginUpload`."""
    task_id_name: str
    doc: DocInfo
    user: User


def get_storage_path(block_type: BlockType):
    """Gets the storage path for the given block type.

    :param block_type: The block type.
    :return: The storage path.
    """
    return (get_files_path()
            / 'blocks'
            / DIR_MAPPING[block_type])


class UploadedFile(ItemBase):
    """A file that has been uploaded by a user.
    """

    def __init__(self, b: Block):
        self._block = b

    @staticmethod
    def find_by_id(block_id: int) -> Optional['UploadedFile']:
        b: Optional[Block] = Block.query.get(block_id)
        if not b:
            return None
        klass = CLASS_MAPPING.get(BlockType(b.type_id))
        if not klass:
            return None
        return klass(b)

    @staticmethod
    def get_by_url(url: str) -> Optional['UploadedFile']:
        """
        Get file matching the given URL string.
        :param url: File url as a string.
        :return: UploadedFile, StampedPDF, or None, if neither was found.
        """
        match = re.search(r'/files/(?P<id>\d+)/(?P<filename>.+)', url)
        if not match:
            return None
        file_id = int(match.group('id'))
        filename = str(match.group('filename'))
        return UploadedFile.get_by_id_and_filename(file_id, filename)

    @staticmethod
    def get_by_id_and_filename(file_id: int, filename: str) -> Optional[Union['UploadedFile', 'StampedPDF']]:
        """
        Get uploaded file or its stamped version in case file name differs (i.e. it has "_stamped" in it).
        :param file_id: File id.
        :param filename: File name, which may contain "_stamped".
        :return: UploadedFile, StampedPDF, or None, if neither was found.
        """
        f = UploadedFile.find_by_id(file_id)
        if not f:
            return None
        if filename != f.filename:
            # Try to find stamped PDF file.
            s = StampedPDF(f.block)
            if filename != s.filename:
                return None
            if not s.filesystem_path.exists():
                return None
            f = s
        return f

    @property
    def id(self):
        return self.block.id

    @property
    def block_type(self):
        return BlockType(self.block.type_id)

    @property
    def relative_filesystem_path(self):
        assert self.id is not None
        return Path(str(self.id)) / self.filename

    @property
    def base_path(self):
        return get_storage_path(self.block_type)

    @property
    def filesystem_path(self):
        return self.base_path / self.relative_filesystem_path

    @property
    def filename(self):
        return self.block.description

    @property
    def data(self):
        with self.filesystem_path.open(mode='rb') as f:
            return f.read()

    @property
    def size(self):
        return os.path.getsize(self.filesystem_path)

    @property
    def content_mimetype(self):
        return get_mimetype(self.filesystem_path.as_posix())

    @property
    def is_content_pdf(self):
        return self.content_mimetype == 'application/pdf'

    @classmethod
    def save_new(cls, file_data: bytes, file_filename: str, block_type: BlockType,
                 upload_info: PluginUploadInfo = None) -> 'UploadedFile':
        if block_type not in DIR_MAPPING:
            raise TimDbException(f'Invalid block type given: {block_type}')
        secured_name = secure_filename(file_filename)
        if block_type == BlockType.Upload:
            assert upload_info
            base_path = get_storage_path(block_type)
            path = (base_path
                    / str(upload_info.doc.id)
                    / upload_info.task_id_name
                    / upload_info.user.name)
            path.mkdir(parents=True, exist_ok=True)
            file_id = len(os.listdir(path)) + 1
            path = path / str(file_id) / secured_name
            file_block = insert_block(
                block_type=block_type,
                description=path.relative_to(base_path).as_posix(),
            )
            au = AnswerUpload(block=file_block)
            db.session.add(au)
        else:
            file_block = insert_block(block_type=block_type, description=secured_name)
        db.session.flush()
        f = CLASS_MAPPING[block_type](file_block)
        p = f.filesystem_path
        p.parent.mkdir(parents=True)
        with p.open(mode='wb') as fi:
            fi.write(file_data)
        return f


class StampedPDF(UploadedFile):

    @property
    def filename(self):
        return Path(self.block.description).stem + '_stamped.pdf'


class PluginUpload(UploadedFile):
    """A file that is associated with an :class:`~.Answer`.
    """

    @property
    def relative_filesystem_path(self) -> Path:
        return Path(self.block.description)

    @property
    def filename(self):
        return self.relative_filesystem_path.parts[-1]


CLASS_MAPPING = {
    BlockType.File: UploadedFile,
    BlockType.Image: UploadedFile,
    BlockType.Upload: PluginUpload,
}

WHITELIST_MIMETYPES = {
    'application/pdf',
    'image/gif',
    'image/jpeg',
    'image/jpg',
    'image/png',
    'image/svg+xml',
    'text/plain',
    'text/xml',
    'application/octet-stream',
    'application/msword',
    'application/vnd.openxmlformats-officedocument.wordprocessingml.document',
    'application/vnd.openxmlformats-officedocument.wordprocessingml.template',
    'application/vnd.ms-word.document.macroEnabled.12',
    'application/vnd.ms-word.template.macroEnabled.12',
    'application/vnd.ms-excel',
    'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
    'application/vnd.openxmlformats-officedocument.spreadsheetml.template',
    'application/vnd.ms-excel.sheet.macroEnabled.12',
    'application/vnd.ms-excel.template.macroEnabled.12',
    'application/vnd.ms-excel.addin.macroEnabled.12',
    'application/vnd.ms-excel.sheet.binary.macroEnabled.12',
    'application/vnd.ms-powerpoint',
    'application/vnd.openxmlformats-officedocument.presentationml.presentation',
    'application/vnd.openxmlformats-officedocument.presentationml.template',
    'application/vnd.openxmlformats-officedocument.presentationml.slideshow',
    'application/vnd.ms-powerpoint.addin.macroEnabled.12',
    'application/vnd.ms-powerpoint.presentation.macroEnabled.12',
    'application/vnd.ms-powerpoint.template.macroEnabled.12',
    'application/vnd.ms-powerpoint.slideshow.macroEnabled.12',
    'application/vnd.ms-access',
}


def get_mimetype(p):
    mime = magic.Magic(mime=True)
    mt = mime.from_file(p)
    if mt == 'image/svg':
        mt += '+xml'
    if isinstance(mt, bytes):
        mt = mt.decode('utf-8')
    if mt not in WHITELIST_MIMETYPES:
        if mt.startswith('text/'):
            mt = 'text/plain'
        else:
            mt = 'application/octet-stream'
    return mt
