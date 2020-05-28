from timApp.admin.util import process_items, create_argparser, DryrunnableArguments, print_match
from timApp.document.docinfo import DocInfo


def fix_hashes(doc: DocInfo, args: DryrunnableArguments) -> int:
    """Due to bugs, the computed hashes in a paragraph file or paragraph list may be incorrect.
    This fixes them.
    :param doc: The document to fix.
    :param args: The arguments.
    """
    d = doc.document
    d.ensure_par_ids_loaded()
    errors = 0
    assert d.par_hashes is not None
    for p, list_hash in zip(d.get_paragraphs(), d.par_hashes):
        old_hash = p.get_hash()
        p._compute_hash()
        new_hash = p.get_hash()
        if old_hash != new_hash:
            errors += 1
            if not args.dryrun:
                d.modify_paragraph_obj(p.get_id(), p)
            print_match(args, doc, p, f'invalid hash in {p.get_id()}: expected {new_hash} but was {old_hash}')
        elif new_hash != list_hash:
            errors += 1
            if not args.dryrun:
                d.modify_paragraph_obj(p.get_id(), p)
            print_match(args, doc, p, f'invalid hash in paragraph list: expected {new_hash} but was {list_hash}')
    return errors


if __name__ == '__main__':
    process_items(fix_hashes, create_argparser('Fixes paragraph hashes in documents'))
