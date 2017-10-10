from timApp.utils import split_location

FORCED_TEMPLATE_NAME = 'force'
TEMPLATE_FOLDER_NAME = 'templates'
PRINT_FOLDER_NAME = 'printing'
PREAMBLE_FOLDER_NAME = 'preamble'
SPECIAL_NAMES = [TEMPLATE_FOLDER_NAME, PRINT_FOLDER_NAME]
DEFAULT_PREAMBLE_DOC = 'preamble'


def check_for_special_name(item_path: str) -> str:
    """
    Check if shortname is one of the special names and if it is, change the typing correctly
    :param item_path: name to check
    :return: names case changed correctly if special name
    """
    ipath, sname = split_location(item_path)

    for sn in SPECIAL_NAMES:
        if sn.upper() == sname.upper():
            return ipath + sn
    return item_path
