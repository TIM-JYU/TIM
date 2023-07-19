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

from timApp.admin.language_cli import add_all_supported_languages
from timApp.admin.translationservice_cli import add_all_tr_services_to_session
from timApp.auth.accesstype import AccessType
from timApp.auth.auth_models import AccessTypeModel
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.documents import import_document_from_file
from timApp.document.specialnames import (
    TEMPLATE_FOLDER_NAME,
    PRINT_FOLDER_NAME,
    PREAMBLE_FOLDER_NAME,
    DEFAULT_PREAMBLE_DOC,
)
from timApp.folder.folder import Folder
from timApp.item.block import BlockType
from timApp.messaging.messagelist.messagelist_utils import MESSAGE_LIST_DOC_PREFIX
from timApp.plugin.calendar.models import EnrollmentType
from timApp.tim_app import app
from timApp.timdb.sqa import db, get_tim_main_engine
from timApp.user.settings.style_utils import (
    OFFICIAL_STYLES_PATH,
    USER_STYLES_PATH,
    STYLES_FOLDER_PREFIX,
)
from timApp.user.special_group_names import ADMIN_GROUPNAME
from timApp.user.user import User, UserInfo
from timApp.user.usergroup import UserGroup, ORG_GROUP_SUFFIX
from timApp.user.users import create_special_usergroups
from timApp.user.userutils import grant_default_access
from timApp.util.error_handlers import ERROR_CODES_FOLDER
from timApp.util.logger import log_info, enable_loggers, log_error
from timApp.util.utils import static_tim_doc, get_static_tim_doc_path


def check_db_version(_, context: MigrationContext):
    if (
        context.get_current_revision()
        != context.environment_context.get_head_revision()
    ):
        enable_loggers()
        log_error("Your database is not up to date. To upgrade, run: ./tim update db")
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
    db_uri = app.config["DB_URI"]
    was_created = postgre_create_database(db_uri)
    if was_created:
        log_info(f"Database {db_uri} was created.")

    with app.app_context():
        sess = db.session
        if database_has_tables():
            pass
        else:
            log_info("Creating database tables...")
            db.create_all()
            if not app.config["TESTING"]:
                with app.app_context():
                    flask_migrate.stamp()
            # Alembic disables loggers for some reason
            enable_loggers()
            sess.add(AccessTypeModel(id=1, name="view"))
            sess.add(AccessTypeModel(id=2, name="edit"))
            sess.add(AccessTypeModel(id=3, name="teacher"))
            sess.add(AccessTypeModel(id=4, name="manage"))
            sess.add(AccessTypeModel(id=5, name="see answers"))
            sess.add(AccessTypeModel(id=6, name="owner"))
            sess.add(AccessTypeModel(id=7, name="copy"))

            create_enrollment_types(sess)

            create_special_usergroups(sess)
            sess.add(
                UserGroup.create(app.config["HOME_ORGANIZATION"] + ORG_GROUP_SUFFIX)
            )
            precomputed_hashes = [
                "$2b$04$zXpqPI7SNOWkbmYKb6QK9ePEUe.0pxZRctLybWNE1nxw0/WMiYlPu",  # test1pass
                "$2b$04$B0mE/VeD5Uzucfa2juzY5.8aObzCqQSDVK//bxdiQ5Ayv59PwWsVq",  # test2pass
                "$2b$04$ajl88D949ur6IF0OE7ZU2OLojkZiOwU5JtUkGTcBnwUi6W7ZIfXPe",  # test3pass
            ]
            for i in range(1, 4):
                u, _ = User.create_with_group(
                    UserInfo(
                        username=f"testuser{i}",
                        full_name=f"Test user {i}",
                        email=f"test{i}@example.com",
                    )
                )
                u.pass_ = precomputed_hashes[i - 1]
            admin_group = UserGroup.get_admin_group()

            # Create users folder explicitly with admin as owner.
            # Otherwise its owner would be whoever logs in to TIM instance first.
            Folder.create("users", owner_groups=admin_group)

            if create_docs:
                t1g = UserGroup.get_by_name("testuser1")
                import_document_from_file(
                    static_tim_doc("initial/programming_examples.md"),
                    "tim/Eri-ohjelmointikielia",
                    t1g,
                    title="Eri ohjelmointikieliä",
                )
                print_base = import_document_from_file(
                    static_tim_doc("initial/print_base.md"),
                    f"{TEMPLATE_FOLDER_NAME}/{PRINT_FOLDER_NAME}/base",
                    admin_group,
                    title="Default print template",
                )
                print_base.block.add_rights(
                    [UserGroup.get_logged_in_group()], AccessType.view
                )
                group_preamble = import_document_from_file(
                    static_tim_doc("initial/group_preamble.md"),
                    f"groups/{TEMPLATE_FOLDER_NAME}/{PREAMBLE_FOLDER_NAME}/{DEFAULT_PREAMBLE_DOC}",
                    admin_group,
                    title="preamble",
                )
                group_preamble.block.add_rights(
                    [UserGroup.get_logged_in_group()], AccessType.view
                )

                messagelist_preamble = import_document_from_file(
                    static_tim_doc("initial/messagelist_preamble.md"),
                    f"{MESSAGE_LIST_DOC_PREFIX}/{TEMPLATE_FOLDER_NAME}/{PREAMBLE_FOLDER_NAME}/{DEFAULT_PREAMBLE_DOC}",
                    admin_group,
                    title="preamble",
                )
                messagelist_preamble.block.add_rights(
                    [UserGroup.get_logged_in_group()], AccessType.view
                )

                admin_group = UserGroup.get_by_name(ADMIN_GROUPNAME)
                error_codes_folder = Folder.create(
                    ERROR_CODES_FOLDER, admin_group, title="Error code database"
                )
                grant_default_access(
                    [admin_group],
                    error_codes_folder,
                    AccessType.owner,
                    BlockType.Document,
                )

                _ = import_document_from_file(
                    static_tim_doc("initial/contact_verify_message.md"),
                    "settings/verify-templates/contact",
                    admin_group,
                    title="Contact verify",
                )
                verify_contact_message_template = import_document_from_file(
                    static_tim_doc("initial/primary_contact_verify_message.md"),
                    "settings/verify-templates/primary-contact",
                    admin_group,
                    title="New primary contact verify",
                )
                verify_contact_message_template.document.set_settings(
                    {"subject": "Verify new contact", "textplain": True}
                )
                verify_contact_message_template.block.add_rights(
                    [UserGroup.get_logged_in_group()], AccessType.view
                )

                create_style_docs()

            # Add the machine-translators to database with their default values.
            add_all_tr_services_to_session()

            # Create and add all supported languages to the database
            add_all_supported_languages()

            sess.commit()
            log_info("Database initialization done.")

    if not app.config["TESTING"]:
        exit_if_not_db_up_to_date()


def create_style_docs() -> tuple[list[Folder], list[DocInfo]]:
    admin_group = UserGroup.get_by_name(ADMIN_GROUPNAME)
    logged_in_group = UserGroup.get_logged_in_group()

    folders = []
    docs = []

    f1 = Folder.find_by_path(OFFICIAL_STYLES_PATH)
    if not f1:
        f1 = Folder.create(OFFICIAL_STYLES_PATH, owner_groups=[admin_group])
        f1.block.add_rights([logged_in_group], AccessType.view)
    folders.append(f1)

    f2 = Folder.find_by_path(USER_STYLES_PATH)
    if not f2:
        f2 = Folder.create(USER_STYLES_PATH, owner_groups=[admin_group])
        f2.block.add_rights([logged_in_group], AccessType.view)
    folders.append(f2)

    pd = DocEntry.find_by_path(f"{STYLES_FOLDER_PREFIX}/templates/preambles/preamble")
    if not pd:
        pd = import_document_from_file(
            static_tim_doc("initial/style_preamble.md"),
            f"{STYLES_FOLDER_PREFIX}/templates/preambles/preamble",
            admin_group,
            title="preamble",
        )
    docs.append(pd)

    style_docs_path = get_static_tim_doc_path() / "style_docs"

    if not style_docs_path.exists():
        return folders, docs

    for doc in style_docs_path.glob("*.md"):
        name = doc.name[:-3]
        d = DocEntry.find_by_path(f"{OFFICIAL_STYLES_PATH}/{name}")
        if not d:
            d = import_document_from_file(
                doc.as_posix(),
                f"{OFFICIAL_STYLES_PATH}/{name}",
                admin_group,
                title=name,
            )
            d.block.add_rights([logged_in_group], AccessType.view)
        docs.append(d)

    return folders, docs


def exit_if_not_db_up_to_date():
    with app.app_context():
        config = app.extensions["migrate"].migrate.get_config(None)
        script = ScriptDirectory.from_config(config)
        env = EnvironmentContext(config, script, fn=check_db_version)
        prev_level = logging.getLogger("alembic").level
        logging.getLogger("alembic").level = logging.WARN
        with env:
            script.run_env()
        logging.getLogger("alembic").level = prev_level
        enable_loggers()


def create_enrollment_types(sess):
    """Initializes the enrollment types used in TIM-calendar event enrollments"""
    sess.add(EnrollmentType(enroll_type_id=0, enroll_type="booking"))


if __name__ == "__main__":
    initialize_database()
