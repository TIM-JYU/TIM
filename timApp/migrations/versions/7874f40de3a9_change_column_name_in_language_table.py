"""Change column name in Language table

Revision ID: 7874f40de3a9
Revises: 3d7bf80c2650
Create Date: 2022-03-21 14:35:31.405721

"""

# revision identifiers, used by Alembic.
revision = "7874f40de3a9"
down_revision = "3d7bf80c2650"

from alembic import op
import sqlalchemy as sa


def upgrade():
    op.alter_column("language", "antonym", new_column_name="autonym")


def downgrade():
    op.alter_column("language", "autonym", new_column_name="antonym")
