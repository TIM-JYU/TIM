from functools import cache
from pathlib import Path


# from timApp.timdb.timdb import TimDb
#
#
# def get_timdb() -> TimDb:
#     """Returns the TimDb object and stores it in the Flask g object."""
#     if not hasattr(g, "timdb"):
#         from timApp.auth.sessioninfo import get_current_user_object
#
#         g.timdb = TimDb(
#             files_root_path=get_files_path(),
#             current_user_name=get_current_user_object().name,
#             route_path=request.path,
#         )
#     return g.timdb


@cache
def get_files_path() -> Path:
    from timApp.tim_app import app

    return Path(app.config["FILES_PATH"])
