import argparse

from timApp.maintenance.util import enum_pars
from timApp.tim_app import app
from timApp.timdb.models.usergroup import UserGroup


def perform_replace(from_str: str, to_str: str, dry_run: bool):
    """Replaces all occurrences of the specified from_str with to_str in all documents.

    :param from_str: The string to replace.
    :param to_str: The replacement.
    :param dry_run: If True, show what would be replaced.
    """
    log_str = 'Would replace' if dry_run else 'Replacing'
    modified_pars = 0
    for d, p in enum_pars():
        old_md = p.get_markdown()
        match_count = old_md.count(from_str)
        if match_count > 0:
            modified_pars += 1
            print('{} {} -> {} in document {}, paragraph {} ({} match{})'
                  .format(log_str, from_str, to_str, d.path, p.get_id(), match_count,
                          '' if match_count == 1 else 'es'))
            if not dry_run:
                p.doc.modifier_group_id = UserGroup.get_admin_group().id
                p.set_markdown(old_md.replace(from_str, to_str))
                p.save()
    print('{} paragraphs {} modified.'.format(modified_pars, 'would be' if dry_run else 'were'))


if __name__ == '__main__':
    parser = argparse.ArgumentParser('Replaces strings in documents.')
    parser.add_argument('-f', help='string to replace', required=True)
    parser.add_argument('-t', help='replacement', required=True)
    parser.add_argument('--dry-run', dest='dryrun', action='store_true',
                        help='show what would get replaced')
    parser.add_argument('--no-dry-run', dest='dryrun', action='store_false', help='do the replacement')
    parser.set_defaults(dryrun=True)
    opts = parser.parse_args()
    if len(opts.f) < 3:
        print('String to replace must be at least 3 characters.')
    with app.test_request_context():
        perform_replace(opts.f, opts.t, opts.dryrun)
