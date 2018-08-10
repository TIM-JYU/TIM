"""Add index to DocEntry id.

Revision ID: 8aee6b154c6b
Revises: b90f139826dc
Create Date: 2018-08-09 12:01:29.424143

"""

# revision identifiers, used by Alembic.
revision = '8aee6b154c6b'
down_revision = 'b90f139826dc'

from alembic import op


def upgrade():
    op.create_index('docentry_id_idx', 'docentry', ['id'], unique=False)


def downgrade():
    op.drop_index('docentry_id_idx', table_name='docentry')
