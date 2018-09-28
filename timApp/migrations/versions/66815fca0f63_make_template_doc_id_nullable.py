"""Make template_doc_id nullable in printed_doc.

Revision ID: 66815fca0f63
Revises: 8aee6b154c6b
Create Date: 2018-09-28 12:06:58.345541

"""

# revision identifiers, used by Alembic.
revision = '66815fca0f63'
down_revision = '8aee6b154c6b'

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.alter_column('printed_doc', 'template_doc_id',
                    existing_type=sa.INTEGER(),
                    nullable=True)


def downgrade():
    op.alter_column('printed_doc', 'template_doc_id',
                    existing_type=sa.INTEGER(),
                    nullable=False)
