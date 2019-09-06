from timApp.folder.folder import Folder
from timApp.user.user import User
from timApp.user.usergroup import UserGroup


def check_velp_group_folder_path(root_path: str, owner_group: UserGroup, doc_name: str):
    """Checks if velp group folder path exists and if not, creates it.

    :param root_path: Root path where method was called from
    :param owner_group: Owner group for the new folder if one is to be created
    :param doc_name:
    :return: Path for velp group folder

    """
    group_folder_name = "velp-groups"  # Name of the folder all velp groups end up in
    if root_path != "":
        velps_folder_path = root_path + "/" + group_folder_name
    else:
        velps_folder_path = group_folder_name
    doc_folder_path = velps_folder_path + "/" + doc_name
    velps_folder = False
    doc_velp_folder = False
    folders = Folder.get_all_in_path(root_path)

    # Check if velps folder exist
    for folder in folders:
        if folder.name == group_folder_name:
            velps_folder = True

    # If velps folder exists, check if folder for document exists
    if velps_folder is True:
        doc_folders = Folder.get_all_in_path(velps_folder_path)
        for folder in doc_folders:
            if folder.name == doc_name:
                doc_velp_folder = True

    # If velps folder doesn't exist, create one
    if velps_folder is False:
        new_block = Folder.create(velps_folder_path, owner_group)

    if doc_name == "":
        return velps_folder_path

    # If folder for document in velps folder doesn't exists, create one
    if doc_velp_folder is False:
        new_block = Folder.create(doc_folder_path, owner_group)

    return doc_folder_path


def check_personal_velp_folder(user: User):
    """Checks if personal velp group folder path exists and if not, creates it.

    :param user: Username of current user
    :return:

    """
    group_folder_name = "velp-groups"
    user_folder = user.get_personal_folder().path
    user_velps_path = user_folder + "/" + group_folder_name
    folders = Folder.get_all_in_path(user_folder)
    velps_folder = False

    for folder in folders:
        if folder.name == group_folder_name:
            velps_folder = True

    if velps_folder is False:
        new_block = Folder.create(user_velps_path, user.get_personal_group())

    return user_velps_path
