from timApp.maintenance.util import enum_pars, process_items
from timApp.timdb.docinfo import DocInfo
from timApp.timdb.invalidreferenceexception import InvalidReferenceException


def fix_settings_references(d: DocInfo, dryrun: bool = True):
    """A paragraph that refers to a settings paragraph is required to have a settings attribute itself.
    This adds the missing settings attributes.
    """
    for _, p in enum_pars(d):
        if p.is_reference() and not p.is_translation():
            try:
                ref = p.get_referenced_pars(set_html=False)[0]
            except InvalidReferenceException:
                continue
            else:
                if ref.is_setting():
                    if not p.is_setting():
                        if not dryrun:
                            p.set_attr('settings', '')
                            d.document.modify_paragraph_obj(p.get_id(), p)
                        print(
                            f'{"Found" if dryrun else "Fixed"} a settings reference without settings attribute: {d.url}#{p.get_id()}')
                    continue
                else:
                    return
        if not p.is_setting():
            return


if __name__ == '__main__':
    process_items(fix_settings_references)
