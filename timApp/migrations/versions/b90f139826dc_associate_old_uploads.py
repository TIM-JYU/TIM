"""Associate old uploads with documents

Revision ID: b90f139826dc
Revises: 010b7b2d8521
Create Date: 2018-06-28 08:18:41.512816

"""

# revision identifiers, used by Alembic.
from timApp.admin.associate_old_uploads import associate_old_uploads

revision = 'b90f139826dc'
down_revision = '010b7b2d8521'


def upgrade():
    associate_old_uploads()


def downgrade():
    raise NotImplementedError
