from typing import Optional, Union, List

from timApp.document.document import Document
from timApp.document.docinfo import DocInfo
from timApp.timdb.exceptions import ItemAlreadyExistsException
from timApp.gamification.gamificationdocument import GamificationDocument
from timApp.item.block import insert_block, BlockType
from timApp.document.translation.translation import Translation
from timApp.timdb.sqa import db
from timApp.timtypes import UserType
from timApp.util.utils import split_location


class DocEntry(db.Model, DocInfo):
    __bind_key__ = 'tim_main'
    __tablename__ = 'docentry'
    name = db.Column(db.Text, primary_key=True)
    id = db.Column(db.Integer, db.ForeignKey('block.id'), nullable=False)
    public = db.Column(db.Boolean, nullable=False, default=True)

    _block = db.relationship('Block', back_populates='docentries', lazy='joined')

    @property
    def path(self):
        return self.name

    @property
    def path_without_lang(self):
        return self.name

    @property
    def lang_id(self):
        tr = Translation.query.filter_by(src_docid=self.id, doc_id=self.id).first()
        if tr:
            return tr.lang_id
        return None

    # noinspection PyMethodOverriding
    @lang_id.setter
    def lang_id(self, value):
        tr = Translation.query.filter_by(src_docid=self.id, doc_id=self.id).first()
        if tr:
            tr.lang_id = value
        else:
            # noinspection PyArgumentList
            tr = Translation(src_docid=self.id, doc_id=self.id, lang_id=value)
            db.session.add(tr)

    @property
    def translations(self) -> List['Translation']:
        trs = Translation.find_by_docentry(self)
        if not any(tr.doc_id == self.id for tr in trs):
            # noinspection PyArgumentList
            tr = Translation(src_docid=self.id, doc_id=self.id, lang_id='')
            tr.docentry = self
            trs.append(tr)
        return trs

    @staticmethod
    def get_all() -> List['DocEntry']:
        return DocEntry.query.all()

    @staticmethod
    def find_all_by_id(doc_id: int) -> List['DocEntry']:
        return DocEntry.query.filter_by(id=doc_id).all()

    @staticmethod
    def find_by_id(doc_id: int, try_translation=False) -> Optional['DocInfo']:
        d = DocEntry.query.filter_by(id=doc_id).first()
        if d:
            return d
        if try_translation:
            d = Translation.query.get(doc_id)
        return d

    @staticmethod
    def find_by_path(path: str, fallback_to_id=False, try_translation=False) -> Union['DocEntry', 'Translation', None]:
        d = DocEntry.query.get(path)
        if d:
            return d
        # try translation
        if try_translation:
            base_doc_path, lang = split_location(path)
            entry = DocEntry.find_by_path(base_doc_path)
            if entry is not None:
                tr = Translation.query.filter_by(src_docid=entry.id, lang_id=lang).first()
                if tr is not None:
                    tr.docentry = entry
                    return tr
        if fallback_to_id:
            try:
                return DocEntry.find_by_id(int(path))
            except ValueError:
                return None
        return d

    @staticmethod
    def get_dummy(title):
        # noinspection PyArgumentList
        return DocEntry(id=-1, name=title)

    @staticmethod
    def create(path: str, owner_group_id: Optional[int] = None, title: Optional[str] = None, from_file=None, initial_par=None,
               settings=None, is_gamified: bool = False) -> 'DocEntry':
        """Creates a new document with the specified name.

        :param from_file: If provided, loads the document content from a file.
        :param initial_par: The initial paragraph for the document.
        :param settings: The settings for the document.
        :param title: The document title.
        :param path: The path of the document to be created (can be None). If None, no DocEntry is actually added
         to the database; only Block and Document objects are created.
        :param owner_group_id: The id of the owner group.
        :param is_gamified: Boolean value indicating whether the document is gamified.
        :returns: The newly created document object.

        """

        location, _ = split_location(path)
        from timApp.folder.folder import Folder
        Folder.create(location, owner_group_id=owner_group_id)

        document = create_document_and_block(owner_group_id, title or path)

        # noinspection PyArgumentList
        docentry = DocEntry(id=document.doc_id, name=path, public=True)
        if path is not None:
            if Folder.find_by_path(path):
                db.session.rollback()
                raise ItemAlreadyExistsException(f'A folder already exists at path {path}')
            db.session.add(docentry)

        if from_file is not None:
            with open(from_file, encoding='utf-8') as f:
                document.add_text(f.read())
        elif initial_par is not None:
            document.add_text(initial_par)
        if settings is not None:
            document.set_settings(settings)
        if is_gamified:
            GamificationDocument.create(document.doc_id)

        return docentry


def create_document_and_block(owner_group_id: int, desc: Optional[str]=None):
    document_id = insert_block(BlockType.Document, desc, owner_group_id).id
    document = Document(document_id, modifier_group_id=owner_group_id)
    document.create()
    return document


def get_documents(include_nonpublic: bool = False,
                  filter_folder: str = None,
                  search_recursively: bool = True,
                  filter_user: Optional[UserType]=None) -> List[DocEntry]:
    """Gets all the documents in the database matching the given criteria.

    :param filter_user: If specified, returns only the documents that the user has view access to.
    :param search_recursively: Whether to search recursively.
    :param filter_folder: Optionally restricts the search to a specific folder.
    :param include_nonpublic: Whether to include non-public document names or not.
    :returns: A list of DocEntry objects.

    """

    q = DocEntry.query
    if not include_nonpublic:
        q = q.filter_by(public=True)
    if filter_folder:
        filter_folder = filter_folder.strip('/') + '/'
        if filter_folder == '/':
            filter_folder = ''
        q = q.filter(DocEntry.name.like(filter_folder + '%'))
    if not search_recursively:
        q = q.filter(DocEntry.name.notlike(filter_folder + '%/%'))
    result = q.all()
    if not filter_user:
        return result
    return [r for r in result if filter_user.has_view_access(r)]


def get_documents_in_folder(folder_pathname: str,
                            include_nonpublic: bool = False) -> List[DocEntry]:
    """Gets all the documents in a folder.

    :param folder_pathname: path to be searched for documents without ending '/'
    :param include_nonpublic: Whether to include non-public document names or not.
    :returns: A list of dictionaries of the form {'id': <doc_id>, 'name': 'document_name'}

    """
    return get_documents(include_nonpublic=include_nonpublic,
                         filter_folder=folder_pathname,
                         search_recursively=False)
