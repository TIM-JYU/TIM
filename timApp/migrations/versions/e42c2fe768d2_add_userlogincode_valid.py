"""Add UserLoginCode.valid

Revision ID: e42c2fe768d2
Revises: 417094192806
Create Date: 2024-03-04 07:16:19.212168

"""

# revision identifiers, used by Alembic.
revision = "e42c2fe768d2"
down_revision = "417094192806"

from alembic import op
import sqlalchemy as sa


def upgrade():
    # ### commands auto generated by Alembic - please adjust! ###
    with op.batch_alter_table("userlogincode", schema=None) as batch_op:
        batch_op.add_column(
            sa.Column("valid", sa.Boolean(), nullable=True, server_default="true")
        )

    # ### end Alembic commands ###


def downgrade():
    # ### commands auto generated by Alembic - please adjust! ###
    with op.batch_alter_table("userlogincode", schema=None) as batch_op:
        batch_op.drop_column("valid")

    # ### end Alembic commands ###
