"""Add a unique constraint to useraccount.name column.

Revision ID: 5adee46df88b Revises: 504a7d9779b9 Create Date: 2016-11-07 16:00:57.562777

"""

# revision identifiers, used by Alembic.
revision = '5adee46df88b'
down_revision = '504a7d9779b9'

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.execute("update useraccount set name=name || abs(id) where name='Anonymous' and id < 0")
    op.execute('ALTER TABLE useraccount DROP CONSTRAINT IF EXISTS useraccount_name_key')
    op.execute("""ALTER TABLE useraccount
                  ADD CONSTRAINT useraccount_name_key
                  UNIQUE (name)""")


def downgrade():
    op.execute('ALTER TABLE useraccount DROP CONSTRAINT IF EXISTS useraccount_name_key')
