"""Add task_id to answer notification

Revision ID: be3366b49cb9
Revises: a6614a0e59d3
Create Date: 2022-11-28 12:49:44.457326

"""

# revision identifiers, used by Alembic.
revision = "be3366b49cb9"
down_revision = "a6614a0e59d3"

from alembic import op
import sqlalchemy as sa


def upgrade():
    # ### commands auto generated by Alembic - please adjust! ###
    op.add_column("pendingnotification", sa.Column("task_id", sa.Text(), nullable=True))
    # ### end Alembic commands ###


def downgrade():
    # ### commands auto generated by Alembic - please adjust! ###
    op.drop_column("pendingnotification", "task_id")
    # ### end Alembic commands ###
