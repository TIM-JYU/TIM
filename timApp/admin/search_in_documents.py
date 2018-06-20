import re

from timApp.admin.util import enum_pars, create_argparser, process_items, get_url_for_match, \
    BasicArguments
from timApp.document.docinfo import DocInfo


class SearchArguments(BasicArguments):
    def __init__(self):
        super().__init__()
        self.term = ''
        self.onlyfirst = False
        self.docsonly = False
        self.regex = False
        self.format = ''


def search(d: DocInfo, args: SearchArguments):
    found = 0
    processed = 0
    regex = re.compile(args.term if args.regex else re.escape(args.term), re.DOTALL)
    for d, p in enum_pars(d):
        md = p.get_exported_markdown(skip_tr=True)
        matches = list(regex.finditer(md))
        if matches:
            found += 1
            if args.docsonly:
                print(d.url)
                break
            header = get_url_for_match(args, d, p)
            if args.format:
                for m in matches:
                    gps = tuple((m.group(0), *m.groups()))
                    print(args.format.format(*gps, doc_id=d.id, par_id=p.get_id(), url=header))
                continue
            print(f"""
{header}
{'-' * len(header)}
{md}
""".strip() + "\n")
        processed += 1
        if args.onlyfirst and processed >= args.onlyfirst:
            break
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
    process_items(search, parser)
