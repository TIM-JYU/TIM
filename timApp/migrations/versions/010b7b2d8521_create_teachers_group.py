"""Create teachers group

Revision ID: 010b7b2d8521
Revises: abcc9a7a0d91
Create Date: 2018-06-21 09:52:22.512960

"""

# revision identifiers, used by Alembic.
from timApp.user.special_group_names import TEACHERS_GROUPNAME
from timApp.user.usergroup import UserGroup

revision = '010b7b2d8521'
down_revision = 'abcc9a7a0d91'

from alembic import op
import sqlalchemy as sa
from timApp.timdb.sqa import db


def upgrade():
    db.session.add(UserGroup(name=TEACHERS_GROUPNAME))
    db.session.commit()


def downgrade():
    db.session.delete(UserGroup.get_by_name(TEACHERS_GROUPNAME))
    db.session.commit()
