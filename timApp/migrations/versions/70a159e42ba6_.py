"""Add a unique constraint on translation (src_docid, lang_id).

Revision ID: 70a159e42ba6 Revises: 5adee46df88b Create Date: 2016-12-13 11:55:35.335541

"""

# revision identifiers, used by Alembic.
revision = '70a159e42ba6'
down_revision = '5adee46df88b'

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.create_unique_constraint('translation_uc', 'translation', ['src_docid', 'lang_id'])


def downgrade():
    op.drop_constraint('translation_uc', 'translation', type_='unique')
