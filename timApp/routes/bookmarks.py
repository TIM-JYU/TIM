from flask import Blueprint
from flask import g

from routes.common import verifyLoggedIn, getCurrentUserId, jsonResponse, verify_json_params
from timdb.bookmarks import Bookmarks
from timdb.models.user import User

bookmarks = Blueprint('bookmarks',
                      __name__,
                      url_prefix='/bookmarks')


@bookmarks.before_request
def verify_login():
    verifyLoggedIn()
    g.bookmarks = Bookmarks(User.query.get(getCurrentUserId()))


@bookmarks.route('/add', methods=['POST'])
def add_bookmark():
    groupname, item_name, item_path = verify_json_params('group', 'name', 'link')
    g.bookmarks.add_bookmark(groupname, item_name, item_path).save_bookmarks()
    return get_bookmarks()


@bookmarks.route('/edit', methods=['POST'])
def edit_bookmark():
    old, new = verify_json_params('old', 'new')
    old_group = old['group']
    old_name = old['name']
    groupname = new['group']
    item_name = new['name']
    item_path = new['link']
    g.bookmarks.delete_bookmark(old_group, old_name).add_bookmark(groupname, item_name, item_path).save_bookmarks()
    return get_bookmarks()


@bookmarks.route('/createGroup/<groupname>', methods=['POST'])
def create_bookmark_group(groupname):
    g.bookmarks.add_group(groupname).save_bookmarks()
    return get_bookmarks()


@bookmarks.route('/deleteGroup', methods=['POST'])
def delete_bookmark_group():
    groupname, = verify_json_params('group')
    g.bookmarks.delete_group(groupname).save_bookmarks()
    return get_bookmarks()


@bookmarks.route('/delete', methods=['POST'])
def delete_bookmark():
    groupname, item_name = verify_json_params('group', 'name')
    g.bookmarks.delete_bookmark(groupname, item_name).save_bookmarks()
    return get_bookmarks()


@bookmarks.route('/get')
@bookmarks.route('/get/<int:user_id>')
def get_bookmarks(user_id=None):
    """Gets user bookmark data for the currently logged in user.

    Parameter user_id is unused for now.
    """

    return jsonResponse(g.bookmarks.as_json())
