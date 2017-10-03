from argparse import ArgumentTypeError

from timApp.maintenance.util import enum_pars, create_argparser, BasicArguments, process_items
from timApp.timdb.docinfo import DocInfo
from timApp.timdb.models.usergroup import UserGroup


class ReplaceArguments(BasicArguments):
    def __init__(self):
        super().__init__()
        self.f = ''
        self.t = ''


def min_replacement_length(x: str):
    if len(x) < 3:
        raise ArgumentTypeError("String to replace must be at least 3 characters.")
    return x


def perform_replace(d: DocInfo, args: ReplaceArguments):
    """Replaces all occurrences of the specified from_str with to_str in all documents.

    :param args: The replacement arguments.
    :param d: The document to process.
    """
    log_str = 'Would replace' if args.dryrun else 'Replacing'
    modified_pars = 0
    for d, p in enum_pars(d):
        old_md = p.get_markdown()
        match_count = old_md.count(args.f)
        if match_count > 0:
            modified_pars += 1
            print(
                f'{log_str} {args.f} -> {args.t} in document {d.path}, paragraph {p.get_id()} ({match_count} match{"" if match_count == 1 else "es"})')
            if not args.dryrun:
                p.doc.modifier_group_id = UserGroup.get_admin_group().id
                p.set_markdown(old_md.replace(args.f, args.t))
                p.save()
    return modified_pars


if __name__ == '__main__':
    parser = create_argparser('Replaces strings in documents')
    parser.add_argument('-f', help='string to replace', required=True, type=min_replacement_length)
    parser.add_argument('-t', help='replacement', required=True)
    process_items(perform_replace, parser)
