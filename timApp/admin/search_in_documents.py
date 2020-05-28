import re
from argparse import ArgumentParser
from typing import NamedTuple, Generator, Match, Optional

import attr

from timApp.admin.util import enum_pars, create_argparser, process_items, get_url_for_match, \
    BasicArguments
from timApp.document.docinfo import DocInfo
from timApp.document.docparagraph import DocParagraph


@attr.s
class SearchArgumentsBasic:
    """Arguments for a search operation."""

    term: str = attr.ib(kw_only=True, default='')
    """The search term."""

    regex: bool = attr.ib(kw_only=True, default=False)
    """If true, interpret term as a regular expression."""

    format: str = attr.ib(kw_only=True, default='{0}')
    """Format string to print matches."""

    onlyfirst: Optional[int] = attr.ib(kw_only=True, default=None)
    """If given, only search the first x paragraphs from each document."""

    filter_attr: Optional[str] = attr.ib(kw_only=True, default=None)
    """If given, only search the paragraphs that have the specified attribute and value."""


@attr.s
class SearchArgumentsBase(BasicArguments, SearchArgumentsBasic):
    pass


@attr.s
class SearchArgumentsCLI(SearchArgumentsBase):
    """Command-line arguments for a search operation."""
    docsonly: bool = attr.ib(kw_only=True)
    exported: bool = attr.ib(kw_only=True)


class SearchResult(NamedTuple):
    """A single search result."""

    doc: DocInfo
    """The document where the match occurred."""

    par: DocParagraph
    """The paragraph where the match occurred."""

    match: Match[str]
    """The match object."""

    num_results: int
    """The number of found results so far."""

    num_pars: int
    """The number of paragraphs processed so far."""

    num_pars_found: int
    """The number of paragraphs found so far."""

    def format_match(self, args: SearchArgumentsBase) -> str:
        m = self.match
        gps = tuple((m.group(0), *m.groups()))
        r = self
        return args.format.format(
            *gps,
            doc_id=r.doc.id,
            par_id=r.par.get_id(),
            url=get_url_for_match(args, r.doc, r.par),
        )


def matches_attr_filter(p: DocParagraph, key: Optional[str], value: Optional[str]) -> bool:
    if key is None:
        return True
    a = p.get_attr(key)
    if a is not None:
        return True if value is None else a == value
    else:
        return False


def search(d: DocInfo, args: SearchArgumentsBasic, use_exported: bool) -> Generator[SearchResult, None, None]:
    """Performs a search operation for the specified document, yielding SearchResults.

    :param args: The search arguments.
    :param d: The document to process.
    :param use_exported: Whether to search in the exported form of paragraphs.
    """
    results_found = 0
    pars_processed = 0
    pars_found = 0
    filter_attr_key = None
    filter_attr_value = None
    if args.filter_attr:
        parts = args.filter_attr.split('=')
        filter_attr_key = parts[0]
        if len(parts) > 1:
            filter_attr_value = parts[1]

    regex = re.compile(args.term if args.regex else re.escape(args.term), re.DOTALL)
    for d, p in enum_pars(d):
        pars_processed += 1
        md = p.get_exported_markdown(skip_tr=True) if use_exported else p.get_markdown()
        matches = list(regex.finditer(md))
        if matches and matches_attr_filter(p, filter_attr_key, filter_attr_value):
            pars_found += 1
            for m in matches:
                results_found += 1
                yield SearchResult(
                    doc=d,
                    par=p,
                    num_results=results_found,
                    num_pars=pars_processed,
                    num_pars_found=pars_found,
                    match=m,
                )
        if args.onlyfirst and pars_processed >= args.onlyfirst:
            break


def search_and_print(d: DocInfo, args: SearchArgumentsCLI) -> int:
    """Same as :func:`search`, but prints the matches according to the provided format."""
    found = 0
    for result in search(d, args, use_exported=args.exported):
        found = result.num_pars_found
        if args.docsonly:
            print(d.url)
            break
        header = get_url_for_match(args, d, result.par)
        if args.format:
            print(result.format_match(args))
            continue
        print(f"""
{header}
{'-' * len(header)}
{result.match.string}
""".strip() + "\n")
    return found


def create_basic_search_argparser(desc: str, is_readonly: bool=True, require_term: bool=True) -> ArgumentParser:
    parser = create_argparser(desc, readonly=is_readonly)
    parser.add_argument('--term', required=require_term, help='search term')
    parser.add_argument('--only-first', help='search only first x paragraphs in each document', dest='onlyfirst',
                        type=int)
    to_param = '' if is_readonly else ', to'
    format_default = '{url}: {0}' if is_readonly else '{url}: {0} -> {to}'
    parser.add_argument('--format',
                        help='format string to print regular expression matches, '
                             'e.g. "{doc_id}#{par_id}: {0}". Available variables: '
                             f'indices 0 through number of subgroups in the regex, doc_id, par_id, url{to_param}.',
                        default=format_default)
    parser.add_argument('--regex', help='interpret search term as a regular expression', action='store_true')
    parser.add_argument('--filter_attr', help='filter paragraphs by attribute[=value]')
    return parser


def main() -> None:
    parser = create_basic_search_argparser('Searches in documents')

    parser.add_argument('--exported', help='use the exported form of markdown when searching', action='store_true')
    parser.add_argument('--docs-only', help='print found documents only, not individual paragraphs', dest='docsonly',
                        action='store_true')
    process_items(search_and_print, parser)


if __name__ == '__main__':
    main()
