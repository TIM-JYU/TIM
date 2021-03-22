import os


from typing import Callable, Any


def change_permission_and_retry(func: Callable[[str], None], path: str, exc_info: Any) -> None:
    import stat

    # Change permission of the path so that it is deletable
    os.chmod(path, stat.S_IWUSR | stat.S_IXUSR)
    func(path)
