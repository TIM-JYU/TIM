from flask import Blueprint
from flask import g

from routes.common import verifyLoggedIn, getCurrentUserId, jsonResponse
from timdb.bookmarks import Bookmarks
from timdb.models.user import User

bookmarks = Blueprint('bookmarks',
                      __name__,
                      url_prefix='/bookmarks')


@bookmarks.before_request
def verify_login():
    verifyLoggedIn()
    g.bookmarks = Bookmarks(User.query.get(getCurrentUserId()))


@bookmarks.route('/add/<groupname>/<item_name>/<path:item_path>', methods=['POST'])
def add_bookmark(groupname, item_name, item_path):
    g.bookmarks.add_bookmark(groupname, item_name, item_path)
    return get_bookmarks()


@bookmarks.route('/createGroup/<groupname>', methods=['POST'])
def create_bookmark_group(groupname):
    g.bookmarks.add_group(groupname)
    return get_bookmarks()


@bookmarks.route('/deleteGroup/<groupname>', methods=['POST'])
def delete_bookmark_group(groupname):
    g.bookmarks.delete_group(groupname)
    return get_bookmarks()


@bookmarks.route('/delete/<groupname>/<item_name>', methods=['POST'])
def delete_bookmark(groupname, item_name):
    g.bookmarks.delete_bookmark(groupname, item_name)
    return get_bookmarks()


@bookmarks.route('/get')
@bookmarks.route('/get/<int:user_id>')
def get_bookmarks(user_id=None):
    """Gets user id data for the currently logged in user.

    Parameter user_id is unused for now.
    """
    bms = g.bookmarks.get_bookmarks()
    result = []
    for group in bms:
        group_name = next(group.__iter__())
        items = group[group_name]
        result_items = []
        for i in items:
            item_name = next(i.__iter__())
            result_items.append({'name': item_name, 'path': i[item_name]})
        result.append({'name': group_name, 'items': result_items, 'editable': group_name != 'Last edited'})
    return jsonResponse(result)
