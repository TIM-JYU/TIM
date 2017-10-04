from timApp.maintenance.util import enum_pars, BasicArguments, create_argparser, process_items
from timApp.timdb.docinfo import DocInfo


class SearchArguments(BasicArguments):
    def __init__(self):
        super().__init__()
        self.term = ''


def search(d: DocInfo, args: SearchArguments):
    found = 0
    for d, p in enum_pars(d):
        md = p.get_exported_markdown(skip_tr=True)
        if args.term in md:
            header = f"{d.url}#{p.get_id()}"
            found += 1
            print(f"""
{header}
{'-' * len(header)}
{md}
""".strip() + "\n")
    return found


if __name__ == '__main__':
    parser = create_argparser('Searches in documents', readonly=True)
    parser.add_argument('--term', required=True, help='String to search for')
    process_items(search, parser)
