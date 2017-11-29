"""Allow multiple read markings in a document block per user.

Revision ID: abcc9a7a0d91
Revises: c0d6e136511e
Create Date: 2017-11-28 09:47:03.777660

"""

# revision identifiers, used by Alembic.
from sqlalchemy.dialects.postgresql import ENUM

revision = 'abcc9a7a0d91'
down_revision = 'c0d6e136511e'


from alembic import op
import sqlalchemy as sa


read_enum = ENUM('on_screen', 'hover_par', 'click_par', 'click_red', name='readparagraphtype', create_type=False)


def upgrade():
    op.create_table('readparagraph',
    sa.Column('id', sa.Integer(), nullable=False),
    sa.Column('usergroup_id', sa.Integer(), nullable=False),
    sa.Column('doc_id', sa.Integer(), nullable=True),
    sa.Column('par_id', sa.Text(), nullable=False),
    sa.Column('type', read_enum, nullable=False),
    sa.Column('par_hash', sa.Text(), nullable=False),
    sa.Column('timestamp', sa.DateTime(timezone=True), nullable=False),
    sa.ForeignKeyConstraint(['doc_id'], ['block.id'], ),
    sa.PrimaryKeyConstraint('id')
    )
    op.create_index('readparagraph_doc_id_par_id_idx', 'readparagraph', ['doc_id', 'par_id'], unique=False)
    op.create_index('readparagraph_doc_id_usergroup_id_idx', 'readparagraph', ['doc_id', 'usergroup_id'], unique=False)
    op.execute("""INSERT INTO readparagraph(usergroup_id, doc_id, par_id, type, par_hash, timestamp)
                  SELECT usergroup_id, doc_id, par_id, type, par_hash, timestamp
                  FROM readparagraphs""")


def downgrade():
    op.drop_index('readparagraph_doc_id_usergroup_id_idx', table_name='readparagraph')
    op.drop_index('readparagraph_doc_id_par_id_idx', table_name='readparagraph')
    op.drop_table('readparagraph')
