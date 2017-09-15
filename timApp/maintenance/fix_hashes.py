import argparse
import sys

from timApp.tim_app import app
from timApp.timdb.docinfo import DocInfo
from timApp.timdb.models.docentry import DocEntry


def fix_hashes(doc: DocInfo, dry_run=True):
    """Due to bugs, the computed hashes in a paragraph file or paragraph list may be incorrect.
    This fixes them.
    :param doc: The document to fix.
    :param dry_run: True to only show what would be done.
    """
    d = doc.document
    d.ensure_par_ids_loaded()
    errors = 0
    for p, list_hash in zip(d.get_paragraphs(), d.par_hashes):
        old_hash = p.get_hash()
        p._compute_hash()
        new_hash = p.get_hash()
        if old_hash != new_hash:
            errors += 1
            if not dry_run:
                d.modify_paragraph_obj(p.get_id(), p)
            print(
                f'{"Found" if dry_run else "Fixed"} invalid hash in {p.get_id()}: expected {new_hash} but was {old_hash}')
        elif new_hash != list_hash:
            errors += 1
            if not dry_run:
                d.modify_paragraph_obj(p.get_id(), p)
            print(
                f'{"Found" if dry_run else "Fixed"} invalid hash in paragraph list: expected {new_hash} but was {list_hash}')
    print(f'Found {errors} errors.')


if __name__ == '__main__':
    parser = argparse.ArgumentParser('fix_hashes.py')
    parser.add_argument('--doc', help='doc id to inspect', required=True)
    parser.add_argument('--dry-run', dest='dryrun', action='store_true',
                        help='show what would be fixed')
    parser.add_argument('--no-dry-run', dest='dryrun', action='store_false', help='do the fixes')
    parser.set_defaults(dryrun=True)
    opts = parser.parse_args()
    with app.app_context():
        doc_to_fix = DocEntry.find_by_path(opts.doc, fallback_to_id=True, try_translation=True)
        if not doc_to_fix:
            print('Document not found.')
            sys.exit(1)
        fix_hashes(doc_to_fix, opts.dryrun)
