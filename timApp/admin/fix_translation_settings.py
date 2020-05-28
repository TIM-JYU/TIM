"""Due to 6c77b8756d2060dea1c3d920a743263dcb740461, all settings paragraphs are synchronized to the translated
document from the original one. Existing translated documents need to be fixed, which is what this script does."""
from timApp.admin.util import DryrunnableArguments, enum_pars, process_items, create_argparser, print_match
from timApp.document.docinfo import DocInfo


def fix_translation(d: DocInfo, args: DryrunnableArguments) -> int:
    fixed = 0
    if d.is_original_translation:
        return fixed
    source = d.src_doc
    pars = source.document.get_paragraphs()
    if not pars:
        return fixed
    if not pars[0].is_setting():
        return fixed
    setting_id = pars[0].get_id()
    tr_pars = list((p for _, p in enum_pars(d)))
    if not tr_pars:
        return fixed
    first_tr = tr_pars[0]
    if not first_tr.is_setting():
        return fixed
    if any(p.get_attr('r') == 'tr' and p.get_attr('rp') == setting_id for p in tr_pars):
        return fixed
    if not args.dryrun:
        first_tr.set_attr('r', 'tr')
        first_tr.set_attr('rp', setting_id)
        first_tr.save()
    fixed += 1
    print_match(args, d, first_tr, 'settings')
    return fixed


if __name__ == '__main__':
    process_items(fix_translation,
                  create_argparser('Fixes settings paragraphs in translated documents.'))
