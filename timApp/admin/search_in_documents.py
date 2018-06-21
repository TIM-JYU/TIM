import re
from typing import NamedTuple, Generator, Match

from timApp.admin.util import enum_pars, create_argparser, process_items, get_url_for_match, \
    BasicArguments
from timApp.document.docinfo import DocInfo
from timApp.document.docparagraph import DocParagraph


class SearchArguments(BasicArguments):
    def __init__(self):
        super().__init__()
        self.term = ''
        self.onlyfirst = False
        self.docsonly = False
        self.regex = False
        self.format = ''


class SearchResult(NamedTuple):
    doc: DocInfo
    par: DocParagraph
    match: Match[str]
    num_results: int
    num_pars: int
    num_pars_found: int

    def format_match(self, args: SearchArguments):
        m = self.match
        gps = tuple((m.group(0), *m.groups()))
        return args.format.format(*gps, doc_id=self.doc.id, par_id=self.par.get_id(),
                                  url=get_url_for_match(args, self.doc, self.par))


def search(d: DocInfo, args: SearchArguments) -> Generator[SearchResult, None, None]:
    results_found = 0
    pars_processed = 0
    pars_found = 0
    regex = re.compile(args.term if args.regex else re.escape(args.term), re.DOTALL)
    for d, p in enum_pars(d):
        pars_processed += 1
        md = p.get_exported_markdown(skip_tr=True)
        matches = list(regex.finditer(md))
        if matches:
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


def search_and_print(d: DocInfo, args: SearchArguments):
    found = 0
    for result in search(d, args):
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


if __name__ == '__main__':
    parser = create_argparser('Searches in documents', readonly=True)
    parser.add_argument('--term', required=True, help='search term')
    parser.add_argument('--only-first', help='search only first x paragraphs in each document', dest='onlyfirst',
                        type=int)
    parser.add_argument('--docs-only', help='print found documents only, not individual paragraphs', dest='docsonly',
                        action='store_true')
    parser.add_argument('--format',
                        help='format string to print regular expression matches, '
                             'e.g. "{doc_id}#{par_id}: {0}". Available variables: '
                             'indices 0 through number of subgroups in the regex, doc_id, par_id, url.')
    parser.add_argument('--regex', help='interpret search term as a regular expression', action='store_true')
    process_items(search_and_print, parser)
