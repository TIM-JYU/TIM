from __future__ import annotations

from typing import TYPE_CHECKING, Any, List

from sqlalchemy import select, ForeignKey, Index
from sqlalchemy.orm import foreign, mapped_column, Mapped, relationship

from timApp.document.docinfo import DocInfo
from timApp.document.document import Document
from timApp.document.translation.translation import Translation
from timApp.folder.createopts import FolderCreationOptions
from timApp.item.block import BlockType, Block
from timApp.item.block import insert_block
from timApp.timdb.exceptions import ItemAlreadyExistsException
from timApp.timdb.sqa import db, run_sql
from timApp.user.usergroup import UserGroup, get_admin_group_id
from timApp.util.utils import split_location

if TYPE_CHECKING:
    from timApp.user.user import User


class DocEntry(db.Model, DocInfo):
    """Represents a TIM document in the directory hierarchy.

    A document can have several aliases, which is why the primary key is "name" column and not "id".

    Most of the time you should use DocInfo class instead of this.
    """

    name: Mapped[str] = mapped_column(primary_key=True)
    """Full path of the document.
    
    TODO: Improve the name.
    """

    id: Mapped[int] = mapped_column(ForeignKey("block.id"))  # type: ignore
    """Document identifier."""

    public: Mapped[bool] = mapped_column(default=True)
    """Whether the document is visible in directory listing."""

    _block: Mapped["Block"] = relationship(back_populates="docentries", lazy="joined")

    trs: Mapped[List[Translation]] = relationship(
        primaryjoin=id == foreign(Translation.src_docid),
        back_populates="docentry",
        # When a DocEntry object is deleted, we don't want to touch the translation objects at all.
        # Otherwise SQLAlchemy would try to null the src_docid column of the corresponding Translation object.
        # TODO: This feels slightly hacky. This relationship attribute might be better in Block class, although that
        #  doesn't sound ideal either.
        passive_deletes="all",
    )

    __table_args__ = (Index("docentry_id_idx", "id"),)

    @property
    def tr(self) -> Translation | None:
        return next((tr for tr in self.trs if tr.doc_id == self.id), None)

    @property
    def path(self) -> str:
        return self.name

    @property
    def path_without_lang(self) -> str:
        return self.name

    @property
    def lang_id(self) -> str | None:
        if not self.tr:
            return None
        setattr(self, "lang_id", self.tr.lang_id)
        return self.tr.lang_id if self.tr else None

    @lang_id.setter
    def lang_id(self, value: str) -> None:
        tr = self.tr
        if tr:
            tr.lang_id = value
        else:
            self.trs.append(
                Translation(src_docid=self.id, lang_id=value, doc_id=self.id)
            )

    @property
    def translations(self) -> list[Translation]:
        trs = self.trs
        if not self.tr:
            self.trs.append(Translation(src_docid=self.id, doc_id=self.id, lang_id=""))
        return trs

    @staticmethod
    def get_all() -> list[DocEntry]:
        return run_sql(select(DocEntry)).scalars().all()  # type: ignore

    @staticmethod
    def find_all_by_id(doc_id: int) -> list[DocEntry]:
        return run_sql(select(DocEntry).filter_by(id=doc_id)).scalars().all()  # type: ignore

    @staticmethod
    def find_by_id(doc_id: int, docentry_load_opts: Any = None) -> DocInfo | None:
        """Finds a DocInfo by id.

        TODO: This method doesn't really belong in DocEntry class.
        """
        stmt = select(DocEntry).filter_by(id=doc_id)
        if docentry_load_opts:
            stmt = stmt.options(*docentry_load_opts)
        d = run_sql(stmt.limit(1)).scalars().first()
        if d:
            return d
        return db.session.get(Translation, doc_id)

    @staticmethod
    def find_by_path(
        path: str,
        fallback_to_id: bool = False,
        try_translation: bool = True,
        docentry_load_opts: Any = None,
    ) -> DocInfo | None:
        """Finds a DocInfo by path, falling back to id if specified.

        TODO: This method doesn't really belong in DocEntry class.
        """
        if docentry_load_opts is None:
            docentry_load_opts = []
        d = db.session.get(DocEntry, path, options=docentry_load_opts)
        if d:
            return d
        # try translation
        if try_translation:
            base_doc_path, lang = split_location(path)
            entry = DocEntry.find_by_path(
                base_doc_path,
                try_translation=False,
                docentry_load_opts=docentry_load_opts,
            )
            if entry is not None:
                # Match lang id using LIKE to allow for partial matches.
                # This is a simple way to allow mapping /en to newer /en-US or /en-GB.
                tr = (
                    run_sql(
                        select(Translation)
                        .filter(
                            (Translation.src_docid == entry.id)
                            & (Translation.lang_id.like(f"{lang}%"))
                        )
                        .limit(1)
                    )
                    .scalars()
                    .first()
                )
                if tr is not None:
                    return tr
        if fallback_to_id:
            try:
                return DocEntry.find_by_id(
                    int(path), docentry_load_opts=docentry_load_opts
                )
            except ValueError:
                return None
        return d

    @staticmethod
    def get_dummy(title: str) -> DocEntry:
        return DocEntry(id=-1, name=title)

    @staticmethod
    def create(
        path: str,
        owner_group: UserGroup | None = None,
        title: str | None = None,
        from_file: str | None = None,
        initial_par: str | None = None,
        settings: dict | None = None,
        folder_opts: FolderCreationOptions = FolderCreationOptions(),
    ) -> DocEntry:
        """Creates a new document with the specified properties.

        :param from_file: If provided, loads the document content from a file.
        :param initial_par: The initial paragraph for the document.
        :param settings: The settings for the document.
        :param title: The document title.
        :param path: The path of the document to be created (can be None). If None, no DocEntry is actually added
         to the database; only Block and Document objects are created.
        :param owner_group: The owner group.
        :param folder_opts: Options for creating intermediate folders.
        :returns: The newly created document object.

        """

        location, _ = split_location(path)
        from timApp.folder.folder import Folder

        Folder.create(location, owner_groups=owner_group, creation_opts=folder_opts)

        document = create_document_and_block(owner_group, title or path)

        docentry = DocEntry(id=document.doc_id, name=path, public=True)
        docentry._doc = document
        if path is not None:
            if Folder.find_by_path(path):
                db.session.rollback()
                raise ItemAlreadyExistsException(
                    f"A folder already exists at path {path}"
                )
            db.session.add(docentry)

        if from_file is not None:
            with open(from_file, encoding="utf-8") as f:
                document.add_text(f.read())
        elif initial_par is not None:
            document.add_text(initial_par)
        if settings is not None:
            document.set_settings(settings)

        return docentry


def create_document_and_block(
    owner_group: UserGroup | None, desc: str | None = None
) -> Document:
    block = insert_block(
        BlockType.Document, desc, [owner_group] if owner_group else None
    )
    # Must flush because we need to know the document id in order to create the document in the filesystem.
    db.session.flush()
    document_id = block.id
    document = Document(
        document_id,
        modifier_group_id=owner_group.id if owner_group else get_admin_group_id(),
    )
    document.create()
    return document


def get_documents(
    include_nonpublic: bool = False,
    filter_folder: str | None = None,
    search_recursively: bool = True,
    filter_user: User | None = None,
    custom_filter: Any = None,
    query_options: Any = None,
) -> list[DocEntry]:
    """Gets all the documents in the database matching the given criteria.

    :param filter_user: If specified, returns only the documents that the user has view access to.
    :param search_recursively: Whether to search recursively.
    :param filter_folder: Optionally restricts the search to a specific folder.
    :param include_nonpublic: Whether to include non-public document names or not.
    :param custom_filter: Any custom filter to use.
    :param query_options: Any additional options for the query.
    :returns: A list of DocEntry objects.

    """

    stmt = select(DocEntry)
    if not include_nonpublic:
        stmt = stmt.filter_by(public=True)
    if filter_folder is not None:
        filter_folder = filter_folder.strip("/") + "/"
        if filter_folder == "/":
            filter_folder = ""
        stmt = stmt.filter(DocEntry.name.like(filter_folder + "%"))
        if not search_recursively:
            stmt = stmt.filter(DocEntry.name.notlike(filter_folder + "%/%"))
    if custom_filter is not None:
        stmt = stmt.filter(custom_filter)
    if query_options is not None:
        stmt = stmt.options(query_options)
    result = run_sql(stmt).scalars().all()
    if not filter_user:
        return result  # type: ignore
    return [r for r in result if filter_user.has_view_access(r)]


def get_documents_in_folder(
    folder_pathname: str, include_nonpublic: bool = False
) -> list[DocEntry]:
    """Gets all the documents in a folder.

    :param folder_pathname: path to be searched for documents without ending '/'
    :param include_nonpublic: Whether to include non-public document names or not.
    :returns: A list of DocEntry objects.

    """
    return get_documents(
        include_nonpublic=include_nonpublic,
        filter_folder=folder_pathname,
        search_recursively=False,
    )
