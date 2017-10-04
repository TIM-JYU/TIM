from timApp.maintenance.util import enum_pars, BasicArguments, create_argparser, process_items
from timApp.timdb.docinfo import DocInfo


class SearchArguments(BasicArguments):
    def __init__(self):
        super().__init__()
        self.term = ''
        self.onlyfirst = False
        self.docsonly = False


def search(d: DocInfo, args: SearchArguments):
    found = 0
    processed = 0
    for d, p in enum_pars(d):
        md = p.get_exported_markdown(skip_tr=True)
        if args.term in md:
            found += 1
            if args.docsonly:
                print(d.url)
                break
            header = f"{d.url}#{p.get_id()}"
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
    parser.add_argument('--term', required=True, help='string to search for')
    parser.add_argument('--only-first', help='search only first x paragraphs in each document', dest='onlyfirst',
                        type=int)
    parser.add_argument('--docs-only', help='print found documents only, not individual paragraphs', dest='docsonly',
                        action='store_true')
    process_items(search, parser)
