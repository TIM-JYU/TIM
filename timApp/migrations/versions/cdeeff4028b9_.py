"""Drop translation.doc_title column.

Revision ID: cdeeff4028b9
Revises: 70a159e42ba6
Create Date: 2016-12-14 15:00:33.269003

"""

# revision identifiers, used by Alembic.
revision = 'cdeeff4028b9'
down_revision = 'b9e81ff20758'

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.execute("""UPDATE block SET description = translation.doc_title
                  FROM translation
                  WHERE block.id = translation.doc_id""")
    op.drop_column('translation', 'doc_title')


def downgrade():
    op.add_column('translation', sa.Column('doc_title', sa.TEXT(), autoincrement=False, nullable=True))
    op.execute("""UPDATE translation SET doc_title = block.description
                  FROM block
                  WHERE block.id = translation.doc_id""")
