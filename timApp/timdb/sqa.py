"""Defines the SQLAlchemy object "db" that is used by all model classes by inheriting from DbModel.

__tablename__ is not mandatory but recommended in order to maintain the naming convention for tables. The default table
name is class name in lowercase.

Use Flask-Migrate for database migrations. See <http://flask-migrate.readthedocs.io/en/latest/>.

"""
import multiprocessing
import os
from typing import Optional

from flask_sqlalchemy import SQLAlchemy
from sqlalchemy import func, text, Executable, Result
from sqlalchemy.orm import mapped_column
from sqlalchemy.orm.base import instance_state, Mapped

from timApp.timdb.types import datetime_tz, DbModel

session_options = {}

cpus = multiprocessing.cpu_count()
pg_max_connections = os.environ.get("PG_MAX_CONNECTIONS")
max_pool_all_workers = int(pg_max_connections or cpus * 3 + 5) - 5
SQLALCHEMY_POOL_SIZE = (max_pool_all_workers // cpus) - 1
SQLALCHEMY_POOL_TIMEOUT = 15
SQLALCHEMY_MAX_OVERFLOW = (max_pool_all_workers - SQLALCHEMY_POOL_SIZE * cpus) // cpus

if os.environ.get("TIM_TESTING", None):
    # Disabling object expiration on commit makes testing easier
    # because sometimes objects would expire after calling a route.
    session_options["expire_on_commit"] = False

    # Increase pool size in tests because sessions are persisted for longer
    SQLALCHEMY_POOL_SIZE = 50
    SQLALCHEMY_MAX_OVERFLOW = 100
elif os.environ.get("COMPOSE_PROFILES", None) == "dev":
    SQLALCHEMY_POOL_SIZE = 100
    SQLALCHEMY_MAX_OVERFLOW = 150


db = SQLAlchemy(
    session_options=session_options,
    model_class=DbModel,
    disable_autonaming=True,
    engine_options={
        "pool_size": SQLALCHEMY_POOL_SIZE,
        "pool_timeout": SQLALCHEMY_POOL_TIMEOUT,
        "max_overflow": SQLALCHEMY_MAX_OVERFLOW,
    },
)


# TODO: Switch models to use dataclasses instead
#   See https://docs.sqlalchemy.org/en/20/orm/dataclasses.html#declarative-dataclass-mapping
#   This should fix DeeplTranslationService's extra args, see https://docs.sqlalchemy.org/en/20/orm/dataclasses.html#using-non-mapped-dataclass-fields


class TimeStampMixin:
    created: Mapped[Optional[datetime_tz]] = mapped_column(
        nullable=True, default=func.now()
    )
    modified: Mapped[Optional[datetime_tz]] = mapped_column(
        nullable=True,
        default=func.now(),
        onupdate=func.now(),
    )


def run_sql(statement: Executable, *args, **kwargs) -> Result:
    """
    Runs a SQL statement and returns the result.

    The arguments are passed to SQLAlchemy's session.execute() function.

    :param statement: The SQL statement to run
    :param args: Arguments to pass to session.execute()
    :param kwargs: Arguments to pass to session.execute()
    :return: Result of the SQL statement as a SQLAlchemy Result object that represents the returned rows.
    """
    return db.session.execute(statement, *args, **kwargs)


def tim_main_execute(sql: str, params=None):
    return db.session.execute(
        text(sql), params, bind_arguments={"bind": get_tim_main_engine()}
    )


def get_tim_main_engine():
    return db.engine


def include_if_loaded(attr_name: str, obj, key_name=None):
    return (
        {(key_name or attr_name): getattr(obj, attr_name)}
        if is_attribute_loaded(attr_name, obj)
        else {}
    )


def is_attribute_loaded(attr_name, obj):
    return obj and attr_name not in instance_state(obj).unloaded


def include_if_exists(attr_name: str, obj):
    return {attr_name: getattr(obj, attr_name)} if hasattr(obj, attr_name) else {}
