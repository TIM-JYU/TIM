"""Defines the SQLAlchemy object "db" that is used by all model classes by inheriting from db.Model.

Each model MUST have 'tim_main' as the __bind_key__ attribute.

__tablename__ is not mandatory but recommended in order to maintain the naming convention for tables. The default table
name is class name in lowercase.

Use Flask-Migrate for database migrations. See <http://flask-migrate.readthedocs.io/en/latest/>.

"""

from flask_sqlalchemy import SQLAlchemy

db = SQLAlchemy()


# UserGroupMember = db.Table('usergroupmember',
#                            db.Column('usergroup_id', db.Integer, db.ForeignKey('usergroup.id'), primary_key=True),
#                            db.Column('user_id', db.Integer, db.ForeignKey('useraccount.id'), primary_key=True),
#                            info={'bind_key': 'tim_main'}
#                            )


def tim_main_execute(sql: str, params=None):
    return db.session.execute(sql, params, bind=db.get_engine(bind='tim_main'))
