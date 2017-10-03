import argparse
import sys
from typing import Generator, Tuple, Optional, Callable, Union

from timApp.documentmodel.docparagraph import DocParagraph
from timApp.tim_app import app
from timApp.timdb.docinfo import DocInfo
from timApp.timdb.models.docentry import DocEntry
from timApp.timdb.models.folder import Folder
from timApp.timdb.timdbexception import TimDbException


def enum_docs(folder: Optional[Folder] = None) -> Generator[DocInfo, None, None]:
    visited_docs = set()
    if not folder:
        folder = Folder.get_root()
    for d in folder.get_all_documents(include_subdirs=True):
        for t in d.translations:
            if t.id in visited_docs:
                continue
            visited_docs.add(t.id)
            yield t


def iterate_pars_skip_exceptions(d: DocInfo):
    i = d.document.__iter__()
    while True:
        try:
            yield next(i)
        except StopIteration:
            raise
        except TimDbException as e:
            print(e)


def enum_pars(item: Union[Folder, DocInfo, None] = None) -> Generator[Tuple[DocInfo, DocParagraph], None, None]:
    if isinstance(item, Folder) or item is None:
        collection = enum_docs(item)
    else:
        collection = [item]
    for d in collection:
        for p in iterate_pars_skip_exceptions(d):
            yield d, p


def process_items(func: Callable[[DocInfo, bool], None]):
    parser = argparse.ArgumentParser(sys.modules['__main__'].__file__)
    parser.add_argument('--doc', help='doc id or path to inspect')
    parser.add_argument('--folder', help='folder path or id to inspect')
    parser.add_argument('--dry-run', dest='dryrun', action='store_true',
                        help='show what would be fixed')
    parser.add_argument('--no-dry-run', dest='dryrun', action='store_false', help='do the fixes')
    parser.set_defaults(dryrun=True)
    opts = parser.parse_args()
    if opts.doc is not None and opts.folder is not None:
        print('Cannot provide both --doc and --folder')
        sys.exit(1)
    if opts.doc is None and opts.folder is None:
        print('Must provide --doc or --folder')
        sys.exit(1)
    with app.app_context():
        doc_to_fix = DocEntry.find_by_path(opts.doc, fallback_to_id=True,
                                           try_translation=True) if opts.doc is not None else None
        folder_to_fix = Folder.find_by_path(opts.folder, fallback_to_id=True) if opts.folder is not None else None
        if opts.doc is not None and not doc_to_fix:
            print('Document not found.')
            sys.exit(1)
        if opts.folder is not None and not folder_to_fix:
            print('Folder not found.')
            sys.exit(1)
        if doc_to_fix:
            func(doc_to_fix, opts.dryrun)
        elif folder_to_fix:
            print(f'Processing paragraphs in folder {folder_to_fix.path}')
            for d in enum_docs(folder=folder_to_fix):
                print(f'Processing document {d.path}')
                func(d, opts.dryrun)
        else:
            assert False
