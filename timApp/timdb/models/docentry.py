from typing import Optional, Union, List, Iterable

from timApp.documentmodel.document import Document
from timApp.timdb.gamification_models.gamificationdocument import GamificationDocument
from timApp.timdb.docinfo import DocInfo
from timApp.timdb.tim_models import db
from timApp.timdb.models.translation import Translation
from timApp.timdb.blocktypes import blocktypes
from timApp.timdb.dbutils import insert_block
from timApp.timdb.timdbexception import TimDbException
from timApp.utils import split_location


class DocEntry(db.Model, DocInfo):
    __bind_key__ = 'tim_main'
    __tablename__ = 'docentry'
    name = db.Column(db.Text, primary_key=True)
    id = db.Column(db.Integer, db.ForeignKey('block.id'), nullable=False)
    public = db.Column(db.Boolean, nullable=False, default=True)

    _block = db.relationship('Block', backref=db.backref('docentries', lazy='dynamic'))

    @property
    def path(self):
        return self.name

    @property
    def path_without_lang(self):
        return self.name

    # noinspection PyMethodOverriding
    @DocInfo.lang_id.setter
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
    def create(path: Optional[str], owner_group_id: int, title: Optional[str] = None, from_file=None, initial_par=None,
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

        if path is not None and '\0' in path:
            raise TimDbException('Document name cannot contain null characters.')

        if isinstance(path, str):
            location, _ = split_location(path)
            from timApp.timdb.models.folder import Folder
            Folder.create(location, owner_group_id=owner_group_id, commit=False)

        document_id = insert_block(title or path, owner_group_id, blocktypes.DOCUMENT, commit=False).id
        document = Document(document_id, modifier_group_id=owner_group_id)
        document.create()

        # noinspection PyArgumentList
        docentry = DocEntry(id=document_id, name=path, public=True)
        if path is not None:
            from timApp.timdb.models.folder import Folder
            if Folder.find_by_path(path):
                db.session.rollback()
                raise TimDbException('A folder already exists at path {}'.format(path))
            db.session.add(docentry)

        if from_file is not None:
            with open(from_file, encoding='utf-8') as f:
                document.add_text(f.read())
        elif initial_par is not None:
            document.add_text(initial_par)
        if settings is not None:
            document.set_settings(settings)
        if is_gamified:
            GamificationDocument.create(document_id)

        db.session.commit()
        return docentry


def get_documents(include_nonpublic: bool = False,
                  filter_ids: Optional[Iterable[int]] = None,
                  filter_folder: str = None,
                  search_recursively: bool = True) -> List[DocEntry]:
    """Gets all the documents in the database matching the given criteria.

    :param search_recursively: Whether to search recursively.
    :param filter_folder: Optionally restricts the search to a specific folder.
    :param filter_ids: An optional iterable of document ids for filtering the documents.
           Must be non-empty if supplied.
    :param include_nonpublic: Whether to include non-public document names or not.
    :returns: A list of DocEntry objects.

    """

    q = DocEntry.query
    if not include_nonpublic:
        q = q.filter_by(public=True)
    if filter_ids:
        q = q.filter(DocEntry.id.in_(filter_ids))
    if filter_folder:
        filter_folder = filter_folder.strip('/') + '/'
        if filter_folder == '/':
            filter_folder = ''
        q = q.filter(DocEntry.name.like(filter_folder + '%'))
    if not search_recursively:
        q = q.filter(DocEntry.name.notlike(filter_folder + '%/%'))
    return q.all()


def get_documents_in_folder(folder_pathname: str,
                            include_nonpublic: bool = False,
                            filter_ids: Optional[Iterable[int]] = None) -> List[DocEntry]:
    """Gets all the documents in a folder.

    :param filter_ids: An optional iterable of document ids for filtering the documents.
           Must be non-empty if supplied.
    :param folder_pathname: path to be searched for documents without ending '/'
    :param include_nonpublic: Whether to include non-public document names or not.
    :returns: A list of dictionaries of the form {'id': <doc_id>, 'name': 'document_name'}

    """
    return get_documents(include_nonpublic=include_nonpublic,
                         filter_folder=folder_pathname,
                         search_recursively=False,
                         filter_ids=filter_ids)
