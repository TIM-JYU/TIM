from timApp.maintenance.util import process_items
from timApp.timdb.docinfo import DocInfo


def fix_hashes(doc: DocInfo, dry_run=True):
    """Due to bugs, the computed hashes in a paragraph file or paragraph list may be incorrect.
    This fixes them.
    :param doc: The document to fix.
    :param dry_run: True to only show what would be done.
    """
    d = doc.document
    d.ensure_par_ids_loaded()
    errors = 0
    for p, list_hash in zip(d, d.par_hashes):
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
    process_items(fix_hashes)
