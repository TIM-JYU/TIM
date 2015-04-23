import os


def change_permission_and_retry(func, path, exc_info):
    import stat

    # Change permission of the path so that it is deletable
    os.chmod(path, stat.S_IWUSR | stat.S_IXUSR)
    func(path)
