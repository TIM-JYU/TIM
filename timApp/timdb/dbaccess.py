from flask import g, request, current_app

from timApp.timdb.timdb import TimDb


def get_timdb() -> TimDb:
    """Returns the TimDb object and stores it in the Flask g object."""
    if not hasattr(g, 'timdb'):
        from timApp.auth.sessioninfo import get_current_user_name
        g.timdb = TimDb(files_root_path=current_app.config['FILES_PATH'],
                        current_user_name=get_current_user_name(),
                        route_path=request.path)
    return g.timdb
