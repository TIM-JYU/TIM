import sys
from argparse import ArgumentParser
from typing import Generator, Tuple, Optional, Callable, Union, TypeVar, Any, Iterable, Set

import attr
import sre_constants

import click
from flask import current_app

from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.docparagraph import DocParagraph
from timApp.folder.folder import Folder
from timApp.tim_app import app
from timApp.timdb.exceptions import TimDbException
from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup


@attr.s
class BasicArguments:
    progress: bool = attr.ib(kw_only=True)
    doc: str = attr.ib(kw_only=True)
    folder: str = attr.ib(kw_only=True)
    urlpath: Optional[str] = attr.ib(kw_only=True, default=None)


@attr.s
class DryrunnableOnly:
    dryrun: bool = attr.ib(kw_only=True, default=False)


@attr.s
class DryrunnableArguments(BasicArguments, DryrunnableOnly):
    pass


def enum_docs(folder: Optional[Folder] = None) -> Generator[DocInfo, None, None]:
    visited_docs: Set[int] = set()
    admin_id = UserGroup.get_admin_group().id
    if not folder:
        folder = Folder.get_root()
    for d in folder.get_all_documents(include_subdirs=True):
        for t in d.translations:
            t.document.modifier_group_id = admin_id
            if t.id in visited_docs:
                continue
            visited_docs.add(t.id)
            yield t


def iterate_pars_skip_exceptions(d: DocInfo) -> Generator[DocParagraph, None, None]:
    i = d.document.__iter__()
    while True:
        try:
            yield next(i)
        except StopIteration:
            return
        except TimDbException as e:
            print(e)


def enum_pars(item: Union[Folder, DocInfo, None] = None) -> Generator[Tuple[DocInfo, DocParagraph], None, None]:
    if isinstance(item, Folder) or item is None:
        collection: Iterable[DocInfo] = enum_docs(item)
    else:
        item.document.modifier_group_id = UserGroup.get_admin_group().id
        collection = [item]
    for d in collection:
        for p in iterate_pars_skip_exceptions(d):
            yield d, p


T = TypeVar('T', bound=BasicArguments)


def process_items(func: Callable[[DocInfo, T], int], parser: ArgumentParser) -> None:
    opts: Any = parser.parse_args()
    with app.app_context():  # type: ignore[no-untyped-call]
        doc_to_fix = DocEntry.find_by_path(opts.doc, fallback_to_id=True) if opts.doc is not None else None
        folder_to_fix = Folder.find_by_path(opts.folder, fallback_to_id=True) if opts.folder is not None else None
        if opts.doc is not None and not doc_to_fix:
            print('Document not found.')
            sys.exit(1)
        if opts.folder is not None and not folder_to_fix:
            print('Folder not found.')
            sys.exit(1)
        total_pars = 0
        total_docs = 0
        if doc_to_fix:
            docs: Iterable[DocInfo] = [doc_to_fix]
        elif folder_to_fix:
            if opts.progress:
                print(f'Processing paragraphs in folder {folder_to_fix.path}')
            docs = enum_docs(folder=folder_to_fix)
        else:
            assert False
        try:
            if getattr(opts, 'dryrun', True):
                print('Dry run mode enabled - the following only shows what WOULD be done.')
            for d in docs:
                if opts.progress:
                    print(f'Processing document {d.path}')
                pars = func(d, opts)
                total_pars += pars
                if pars > 0:
                    total_docs += 1
        except sre_constants.error as e:
            print(f'Invalid regular expression: {e}')
        if hasattr(opts, 'dryrun'):
            print(f'Total paragraphs that {"would be" if opts.dryrun else "were"} '
                  f'affected: {total_pars} in {total_docs} documents')
        else:
            print(f'Total paragraphs found: {total_pars} in {total_docs} documents')
        if not getattr(opts, 'dryrun', True):
            db.session.commit()


def create_argparser(description: str, readonly: bool=False) -> ArgumentParser:
    parser = ArgumentParser(description=description)
    group_item = parser.add_mutually_exclusive_group(required=True)
    group_item.add_argument('--doc', help='doc id or path to process')
    group_item.add_argument('--folder', help='folder path or id to process')
    if not readonly:
        group_dryrun = parser.add_mutually_exclusive_group()
        group_dryrun.add_argument('--dry-run', dest='dryrun', action='store_true',
                                  help='show what would be fixed')
        group_dryrun.add_argument('--no-dry-run', dest='dryrun', action='store_false', help='do the fixes')
        group_dryrun.set_defaults(dryrun=True)
    parser.add_argument('--progress', dest='progress', action='store_true',
                        help='show progress')
    parser.add_argument('--url-path', dest='urlpath', help='start path of URLs, e.g. view/manage/velp')
    parser.set_defaults(urlpath='view')
    parser.set_defaults(progress=False)
    return parser


def get_url_for_match(args: BasicArguments, d: DocInfo, p: DocParagraph) -> str:
    host = current_app.config['TIM_HOST']
    return f'{host}/{args.urlpath}/{d.path}#{p.get_id()}'


def print_match(args: DryrunnableArguments, d: DocInfo, p: DocParagraph, msg: str) -> None:
    print(f'{"Found" if args.dryrun else "Fixed"} {get_url_for_match(args, d, p)}: {msg}')


def commit_if_not_dry(dry_run: bool) -> None:
    if not dry_run:
        db.session.commit()
    else:
        click.echo('Dry run enabled, nothing changed.')
