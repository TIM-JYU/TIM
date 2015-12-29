from flask import Blueprint, abort

from routes.common import verify_admin, getTimDb, jsonResponse, okJsonResponse

groups = Blueprint('groups',
                   __name__,
                   url_prefix='/groups')


def get_uid_gid(groupname, username):
    timdb = getTimDb()
    uid = timdb.users.getUserByName(username)
    if uid is None:
        abort(404, 'User does not exist.')
    gid = timdb.users.getUserGroupByName(groupname)
    if gid is None:
        abort(404, 'Usergroup does not exist.')
    return gid, uid


@groups.route('/show/<groupname>')
def show_members(groupname):
    verify_admin()
    timdb = getTimDb()
    if not timdb.users.group_exists(groupname):
        abort(404, 'Usergroup does not exist.')
    members = timdb.users.get_users_for_group(groupname)
    return jsonResponse(members)


@groups.route('/addmember/<username>/<groupname>')
def add_member(username, groupname):
    verify_admin()
    timdb = getTimDb()
    gid, uid = get_uid_gid(groupname, username)
    if uid in (u['id'] for u in timdb.users.get_users_for_group(groupname)):
        abort(400, 'User already belongs to this group.')
    timdb.users.addUserToGroup(gid, uid)
    return okJsonResponse()


@groups.route('/removemember/<username>/<groupname>')
def remove_member(username, groupname):
    verify_admin()
    timdb = getTimDb()
    gid, uid = get_uid_gid(groupname, username)
    count = timdb.users.remove_membership(uid, gid)
    assert count <= 1
    if count == 0:
        abort(400, 'User does not belong to this group.')
    else:
        return okJsonResponse()
