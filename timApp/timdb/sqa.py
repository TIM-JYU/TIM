"""Defines the SQLAlchemy object "db" that is used by all model classes by inheriting from db.Model.

__tablename__ is not mandatory but recommended in order to maintain the naming convention for tables. The default table
name is class name in lowercase.

Use Flask-Migrate for database migrations. See <http://flask-migrate.readthedocs.io/en/latest/>.

"""

from flask_sqlalchemy import SQLAlchemy
from sqlalchemy import func
from sqlalchemy.orm.base import instance_state

db = SQLAlchemy()


class TimeStampMixin:
    created = db.Column(db.DateTime(timezone=True), nullable=True, default=func.now())
    modified = db.Column(db.DateTime(timezone=True), nullable=True, default=func.now(), onupdate=func.now())

# UserGroupMember = db.Table('usergroupmember',
#                            db.Column('usergroup_id', db.Integer, db.ForeignKey('usergroup.id'), primary_key=True),
#                            db.Column('user_id', db.Integer, db.ForeignKey('useraccount.id'), primary_key=True),
#                            )


def tim_main_execute(sql: str, params=None):
    return db.session.execute(sql, params, bind=get_tim_main_engine())


def get_tim_main_engine():
    return db.get_engine()


def include_if_loaded(attr_name: str, obj, key_name=None):
    return {(key_name or attr_name): getattr(obj, attr_name)} if is_attribute_loaded(attr_name, obj) else {}


def is_attribute_loaded(attr_name, obj):
    return obj and attr_name not in instance_state(obj).unloaded


def include_if_exists(attr_name: str, obj):
    return {attr_name: getattr(obj, attr_name)} if hasattr(obj, attr_name) else {}
