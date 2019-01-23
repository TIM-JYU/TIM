"""Add group admins group

Revision ID: 2b268a2d1cc2
Revises: 2236e3460ce2
Create Date: 2019-01-22 11:02:21.455137

"""

# revision identifiers, used by Alembic.
from timApp.user.special_group_names import GROUPADMIN_GROUPNAME
from timApp.user.usergroup import UserGroup

revision = '2b268a2d1cc2'
down_revision = '2236e3460ce2'


def upgrade():
    UserGroup.create(GROUPADMIN_GROUPNAME)


def downgrade():
    pass
