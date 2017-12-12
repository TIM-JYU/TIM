"""Add unique constraint to folder(name, location).

Revision ID: 199fc0d202ef
Revises: c0d6e136511e
Create Date: 2017-12-07 12:23:48.525604

"""

# revision identifiers, used by Alembic.
revision = '199fc0d202ef'
down_revision = 'c0d6e136511e'

from alembic import op


def upgrade():
    op.create_unique_constraint('folder_uc', 'folder', ['name', 'location'])


def downgrade():
    op.drop_constraint('folder_uc', 'folder', type_='unique')
