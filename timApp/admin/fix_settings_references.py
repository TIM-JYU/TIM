"""Due to b3fc1c622309e8474183ed5ef5ea7bace2479c4d, settings attribute is always required even when referencing other settings
paragraphs. This adds missing settings attributes where it is missing."""
from timApp.admin.util import enum_pars, process_items, create_argparser, DryrunnableArguments, print_match
from timApp.document.docinfo import DocInfo
from timApp.timdb.exceptions import InvalidReferenceException


def fix_settings_references(d: DocInfo, args: DryrunnableArguments) -> int:
    num_found = 0
    for _, p in enum_pars(d):
        if p.is_reference() and not p.is_translation():
            try:
                ref = p.get_referenced_pars()[0]
            except InvalidReferenceException:
                continue
            else:
                if ref.is_setting():
                    if not p.is_setting():
                        num_found += 1
                        if not args.dryrun:
                            p.set_attr('settings', '')
                            d.document.modify_paragraph_obj(p.get_id(), p)
                        print_match(args, d, p, 'a settings reference without settings attribute')
                    continue
                else:
                    break
        if not p.is_setting():
            break
    return num_found


if __name__ == '__main__':
    process_items(fix_settings_references,
                  create_argparser('Adds "settings" attribute in paragraphs that reference settings paragraphs'))
