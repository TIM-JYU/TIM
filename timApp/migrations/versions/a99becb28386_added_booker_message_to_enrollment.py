"""added booker_message to enrollment

Revision ID: a99becb28386
Revises: 338d9318ccbb
Create Date: 2022-05-05 09:59:31.736615

"""

# revision identifiers, used by Alembic.
revision = "a99becb28386"
down_revision = "338d9318ccbb"

from alembic import op
import sqlalchemy as sa


def upgrade():
    # ### commands auto generated by Alembic - please adjust! ###
    op.add_column("enrollment", sa.Column("booker_message", sa.Text(), nullable=True))
    # ### end Alembic commands ###


def downgrade():
    # ### commands auto generated by Alembic - please adjust! ###
    op.drop_column("enrollment", "booker_message")
    # ### end Alembic commands ###