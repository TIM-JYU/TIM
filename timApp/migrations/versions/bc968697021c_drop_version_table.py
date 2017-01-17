"""Drop version table.

Revision ID: bc968697021c
Revises: f85eec396435
Create Date: 2017-01-17 17:10:29.334570

"""

# revision identifiers, used by Alembic.
revision = 'bc968697021c'
down_revision = 'f85eec396435'

from alembic import op
import sqlalchemy as sa
from sqlalchemy.dialects import postgresql


def upgrade():
    op.drop_table('version')


def downgrade():
    op.create_table('version',
    sa.Column('id', sa.INTEGER(), nullable=False),
    sa.Column('updated_on', postgresql.TIMESTAMP(timezone=True), autoincrement=False, nullable=True),
    sa.PrimaryKeyConstraint('id', name='version_pkey')
    )
