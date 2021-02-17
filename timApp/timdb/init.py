"""Initializes the TIM database."""

import logging
import sys

import flask_migrate
import sqlalchemy
import sqlalchemy.exc
from alembic.runtime.environment import EnvironmentContext
from alembic.runtime.migration import MigrationContext
from alembic.script import ScriptDirectory
from sqlalchemy_utils import database_exists, create_database

from timApp.auth.accesstype import AccessType
from timApp.auth.auth_models import AccessTypeModel
from timApp.document.documents import import_document_from_file
from timApp.document.specialnames import TEMPLATE_FOLDER_NAME, PRINT_FOLDER_NAME, PREAMBLE_FOLDER_NAME, \
    DEFAULT_PREAMBLE_DOC
from timApp.folder.folder import Folder
from timApp.tim_app import app
from timApp.timdb.dbaccess import get_files_path
from timApp.timdb.sqa import db, get_tim_main_engine
from timApp.timdb.timdb import TimDb
from timApp.user.user import User, UserInfo
from timApp.user.usergroup import UserGroup, ORG_GROUP_SUFFIX
from timApp.user.users import create_special_usergroups
from timApp.util.logger import log_info, enable_loggers, log_error
from timApp.util.utils import static_tim_doc


def check_db_version(_, context: MigrationContext):
    if context.get_current_revision() != context.environment_context.get_head_revision():
        enable_loggers()
        log_error('Your database is not up to date. To upgrade, run: ./r flask db upgrade')
        sys.exit(-1)
    return []


def postgre_create_database(db_uri: str):
    if not database_exists(db_uri):
        create_database(db_uri)
        return True
    return False


def database_has_tables():
    return bool(sqlalchemy.inspect(get_tim_main_engine()).get_table_names())


def initialize_database(create_docs: bool = True) -> None:
    files_root_path = get_files_path()
    db_uri = app.config['DB_URI']
    was_created = postgre_create_database(db_uri)
    if was_created:
        log_info(f'Database {db_uri} was created.')
    timdb = TimDb(files_root_path=files_root_path)
    sess = timdb.session
    if database_has_tables():
        pass
    else:
        log_info('Creating database tables...')
        db.create_all()
        if not app.config['TESTING']:
            with app.app_context():
                flask_migrate.stamp()
        # Alembic disables loggers for some reason
        enable_loggers()
        sess.add(AccessTypeModel(id=1, name='view'))
        sess.add(AccessTypeModel(id=2, name='edit'))
        sess.add(AccessTypeModel(id=3, name='teacher'))
        sess.add(AccessTypeModel(id=4, name='manage'))
        sess.add(AccessTypeModel(id=5, name='see answers'))
        sess.add(AccessTypeModel(id=6, name='owner'))
        sess.add(AccessTypeModel(id=7, name='copy'))

        create_special_usergroups(sess)
        sess.add(UserGroup.create(app.config['HOME_ORGANIZATION'] + ORG_GROUP_SUFFIX))
        precomputed_hashes = [
            '$2b$04$zXpqPI7SNOWkbmYKb6QK9ePEUe.0pxZRctLybWNE1nxw0/WMiYlPu',  # test1pass
            '$2b$04$B0mE/VeD5Uzucfa2juzY5.8aObzCqQSDVK//bxdiQ5Ayv59PwWsVq',  # test2pass
            '$2b$04$ajl88D949ur6IF0OE7ZU2OLojkZiOwU5JtUkGTcBnwUi6W7ZIfXPe',  # test3pass
        ]
        for i in range(1, 4):
            u, _ = User.create_with_group(UserInfo(
                username=f'testuser{i}',
                full_name=f'Test user {i}',
                email=f'test{i}@example.com',
            ))
            u.pass_ = precomputed_hashes[i - 1]
        admin_group = UserGroup.get_admin_group()

        # Create users folder explicitly with admin as owner.
        # Otherwise its owner would be whoever logs in to TIM instance first.
        Folder.create('users', owner_groups=admin_group)

        if create_docs:
            t1g = UserGroup.get_by_name('testuser1')
            import_document_from_file(
                static_tim_doc('initial/programming_examples.md'),
                'tim/Eri-ohjelmointikielia',
                t1g,
                title='Eri ohjelmointikieliä',
            )
            print_base = import_document_from_file(
                static_tim_doc('initial/print_base.md'),
                f'{TEMPLATE_FOLDER_NAME}/{PRINT_FOLDER_NAME}/base',
                admin_group,
                title='Default print template',
            )
            print_base.block.add_rights([UserGroup.get_logged_in_group()], AccessType.view)
            group_preamble = import_document_from_file(
                static_tim_doc('initial/group_preamble.md'),
                f'groups/{TEMPLATE_FOLDER_NAME}/{PREAMBLE_FOLDER_NAME}/{DEFAULT_PREAMBLE_DOC}',
                admin_group,
                title='preamble',
            )
            group_preamble.block.add_rights([UserGroup.get_logged_in_group()], AccessType.view)

        sess.commit()
        log_info('Database initialization done.')

    if not app.config['TESTING']:
        exit_if_not_db_up_to_date()
    timdb.close()


def exit_if_not_db_up_to_date():
    with app.app_context():
        config = app.extensions['migrate'].migrate.get_config(None)
        script = ScriptDirectory.from_config(config)
        env = EnvironmentContext(config, script, fn=check_db_version)
        prev_level = logging.getLogger('alembic').level
        logging.getLogger('alembic').level = logging.WARN
        with env:
            script.run_env()
        logging.getLogger('alembic').level = prev_level
        enable_loggers()


if __name__ == "__main__":
    initialize_database()
