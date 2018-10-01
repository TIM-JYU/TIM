"""Create pendingnotification table.

Revision ID: dadad1cd3387
Revises: 66815fca0f63
Create Date: 2018-10-03 09:59:53.729384

"""

# revision identifiers, used by Alembic.
revision = 'dadad1cd3387'
down_revision = '66815fca0f63'

from alembic import op
import sqlalchemy as sa

e = sa.Enum('DocModified', 'ParAdded', 'ParModified', 'ParDeleted', 'CommentAdded', 'CommentModified', 'CommentDeleted',
            name='notificationtype')


def upgrade():
    op.create_table(
        'pendingnotification',
        sa.Column('id', sa.Integer(), nullable=False),
        sa.Column('user_id', sa.Integer(), nullable=False),
        sa.Column('doc_id', sa.Integer(), nullable=False),
        sa.Column('discriminant', sa.Text(), nullable=False),
        sa.Column('par_id', sa.Text(), nullable=True),
        sa.Column('text', sa.Text(), nullable=True),
        sa.Column('created', sa.DateTime(timezone=True), nullable=False),
        sa.Column('processed', sa.DateTime(timezone=True), nullable=True),
        sa.Column('kind', e, nullable=False),
        sa.Column('version_change', sa.Text(), nullable=True),
        sa.ForeignKeyConstraint(['doc_id'], ['block.id'], ),
        sa.ForeignKeyConstraint(['user_id'], ['useraccount.id'], ),
        sa.PrimaryKeyConstraint('id')
    )


def downgrade():
    op.drop_table('pendingnotification')
