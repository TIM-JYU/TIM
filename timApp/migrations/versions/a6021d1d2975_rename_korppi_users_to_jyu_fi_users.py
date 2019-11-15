"""Rename 'Korppi users' to 'jyu.fi users'

Revision ID: a6021d1d2975
Revises: 88ee56b0e588
Create Date: 2019-11-18 12:20:54.858929

"""

# revision identifiers, used by Alembic.
from typing import Any

from sqlalchemy.orm import scoped_session, sessionmaker

from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup

revision = 'a6021d1d2975'
down_revision = '88ee56b0e588'

from alembic import op


def upgrade():
    bind = op.get_bind()
    tmp: Any = scoped_session(session_factory=sessionmaker(bind=bind))
    db.session = tmp
    ug = UserGroup.get_by_name('Korppi users')
    ug.name = 'jyu.fi users'
    db.session.flush()


def downgrade():
    pass
