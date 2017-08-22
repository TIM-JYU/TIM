"""Add index to readparagraphs on (doc_id, par_id).

Revision ID: 82c3b050ba5d
Revises: 5af448e72f83
Create Date: 2017-08-22 08:09:45.987568

"""

# revision identifiers, used by Alembic.
revision = '82c3b050ba5d'
down_revision = '5af448e72f83'

from alembic import op


def upgrade():
    op.create_index('readparagraphs_doc_id_par_id_idx', 'readparagraphs', ['doc_id', 'par_id'])


def downgrade():
    op.drop_index('readparagraphs_doc_id_par_id_idx')
