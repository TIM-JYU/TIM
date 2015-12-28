from flask import Blueprint, abort

from routes.common import verify_admin, getTimDb, jsonResponse

groups = Blueprint('groups',
                   __name__,
                   url_prefix='/groups')


@groups.route('/show/<groupname>')
def show_members(groupname):
    verify_admin()
    timdb = getTimDb()
    if not timdb.users.group_exists(groupname):
        abort(404, 'Usergroup does not exist.')
    members = timdb.users.get_users_for_group(groupname)
    return jsonResponse(members)
