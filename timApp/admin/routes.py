import os
import shutil
from pprint import pprint
from typing import List, Optional

import click
from dataclasses import dataclass
from flask import flash, url_for, Blueprint, abort
from flask.cli import AppGroup

from timApp.auth.accesshelper import verify_admin
from timApp.auth.accesstype import AccessType
from timApp.document.docinfo import move_document
from timApp.item.block import Block, BlockType
from timApp.tim_app import app
from timApp.timdb.sqa import db
from timApp.user.user import User, UserInfo
from timApp.user.usergroup import UserGroup
from timApp.util.flask.requesthelper import use_model
from timApp.util.flask.responsehelper import safe_redirect, json_response

admin_bp = Blueprint('admin',
                     __name__,
                     url_prefix='')


@dataclass
class ExceptionRouteModel:
    db_error: bool = False


@admin_bp.route('/exception', methods=['GET', 'POST', 'PUT', 'DELETE'])
@use_model(ExceptionRouteModel)
def throw_ex(m: ExceptionRouteModel):
    verify_admin()
    if m.db_error:
        db.session.add(UserGroup(name='test'))
        db.session.add(UserGroup(name='test'))
        db.session.flush()
        raise Exception('Flush should have raised an exception!')
    raise Exception('This route throws an exception intentionally for testing purposes.')


@admin_bp.route('/restart')
def restart_server():
    """Restarts the server by sending HUP signal to Gunicorn."""
    verify_admin()
    pid_path = '/var/run/gunicorn.pid'
    if os.path.exists(pid_path):
        os.system(f'kill -HUP $(cat {pid_path})')
        flash('Restart signal was sent to Gunicorn.')
    else:
        flash('Gunicorn PID file was not found. TIM was probably not started with Gunicorn.')
    return safe_redirect(url_for('start_page'))


@admin_bp.route('/resetcss')
def reset_css():
    """Removes CSS cache directories and thereby forces SASS to regenerate them the next time they are needed.

    This does not have to be called anymore because webassets watches the SCSS changes.

    Requires admin privilege.
    """
    verify_admin()

    assets_dir = os.path.join('static', '.webassets-cache')

    if os.path.exists(assets_dir):
        shutil.rmtree(assets_dir)

    gen_dir = os.path.join('static', app.config['SASS_GEN_PATH'])

    if os.path.exists(gen_dir):
        shutil.rmtree(gen_dir)
    flash('The resetcss route does not have to be called anymore.')
    return safe_redirect(url_for('start_page'))


@admin_bp.route('/users/search/<term>')
def search_users(term: str):
    verify_admin()
    result = User.query.filter(
        User.name.ilike(f'%{term}%') |
        User.real_name.ilike(f'%{term}%') |
        User.email.ilike(f'%{term}%')).order_by(User.id).all()
    return json_response(result)


def has_anything_in_common(u1: User, u2: User):
    u1_set = {u1.name.lower(), *u1.real_name.lower().split(' '), u1.email_name_part.lower()}
    u2_set = {u2.name.lower(), *u2.real_name.lower().split(' '), u2.email_name_part.lower()}
    if u1_set & u2_set:
        return True
    # This allows e.g. testuser1 and testuser2 to be merged.
    return bool(set(n[:-1] for n in u1_set) & set(n[:-1] for n in u2_set))


user_cli = AppGroup('user')


@user_cli.command('merge')
@click.argument('primary')
@click.argument('secondary')
def merge_users(primary, secondary):
    """Merges two users by moving data from secondary account to primary account.

    This does not delete accounts.
    """
    with app.test_request_context():
        moved_data = do_merge(primary, secondary)
        db.session.commit()
    pprint(moved_data)
    return moved_data


def do_merge(primary: str, secondary: str):
    u_prim = User.get_by_name(primary)
    u_sec = User.get_by_name(secondary)
    if not u_prim:
        return abort(404, f'User {primary} not found')
    if not u_sec:
        return abort(404, f'User {secondary} not found')
    if u_prim.is_special:
        return abort(400, f'User {primary} is a special user')
    if u_sec.is_special:
        return abort(400, f'User {secondary} is a special user')
    if u_prim == u_sec:
        return abort(400, 'Users cannot be the same')
    if not has_anything_in_common(u_prim, u_sec):
        return abort(400, f'Users {primary} and {secondary} do not appear to be duplicates. '
        f'Merging not allowed to prevent accidental errors.')

    moved_data = {}
    for a in ('owned_lectures', 'lectureanswers', 'messages', 'answers', 'annotations', 'velps'):
        a_alt = a + '_alt'
        moved_data[a] = len(getattr(u_sec, a_alt))
        getattr(u_prim, a_alt).extend(getattr(u_sec, a_alt))
        setattr(u_sec, a_alt, [])

    u_prim_group = u_prim.get_personal_group()
    u_sec_group = u_sec.get_personal_group()

    u_prim_folder = u_prim.get_personal_folder()
    u_sec_folder = u_sec.get_personal_folder()
    docs = u_sec_folder.get_all_documents(include_subdirs=True)
    for d in docs:
        move_document(d, u_prim_folder)

    for a in ('readparagraphs', 'notes', 'accesses'):
        a_alt = a + '_alt'
        moved_data[a] = len(getattr(u_sec_group, a_alt))
        getattr(u_prim_group, a_alt).extend(getattr(u_sec_group, a_alt))
        setattr(u_sec_group, a_alt, [])

    # Restore ownership of secondary's personal folder:
    # * all users are allowed to have at most one personal folder
    # * if we don't restore access for secondary user, a new personal folder would be created when logging in
    for a in u_prim_group.accesses:
        if a.block_id == u_sec_folder.block.id and a.type == AccessType.owner.value:
            moved_data['accesses'] -= 1
            u_prim_group.accesses.remove(a)
            u_sec_group.accesses.append(a)
            break
    return moved_data


@user_cli.command('soft_delete')
@click.argument('name')
def soft_delete(name: str):
    do_soft_delete(name)


def do_soft_delete(name):
    u = User.get_by_name(name)
    if not u:
        abort(404, 'User not found.')
    d_suffix = '_deleted'
    if u.name.endswith(d_suffix) or u.email.endswith(d_suffix):
        return abort(400, 'User is already soft-deleted.')
    u.update_info(UserInfo(username=u.name + d_suffix, email=u.email + d_suffix, full_name=u.real_name))
    db.session.commit()


app.cli.add_command(user_cli)


item_cli = AppGroup('item')
app.cli.add_command(item_cli)


@item_cli.command('cleanup_default_rights_names')
def cleanup_default_right_doc_names():
    bs: List[Block] = Block.query.filter(
        Block.description.in_(
            [
                'templates/DefaultDocumentRights',
                'templates/DefaultFolderRights',
                '$DefaultFolderRights',
                '$DefaultDocumentRights',
            ]) & (Block.type_id == BlockType.Document.value)
    ).all()
    num_changed = len(bs)
    for b in bs:
        b.description = b.description.replace('templates/', '').replace('$', '')
    db.session.commit()
    print(f'Fixed titles of {num_changed} default rights documents.')
