"""Add tables for translation features

Revision ID: 3d7bf80c2650
Revises: 98d090356548
Create Date: 2022-03-18 13:19:06.674377

"""

# revision identifiers, used by Alembic.
revision = "3d7bf80c2650"
down_revision = "98d090356548"

from alembic import op
import sqlalchemy as sa


def upgrade():
    # ### commands auto generated by Alembic - please adjust! ###
    op.create_table(
        "language",
        sa.Column("lang_code", sa.Text(), nullable=False),
        sa.Column("lang_name", sa.Text(), nullable=False),
        sa.Column("flag_uri", sa.Text(), nullable=True),
        sa.Column("antonym", sa.Text(), nullable=False),
        sa.PrimaryKeyConstraint("lang_code"),
    )
    op.create_table(
        "translationservice",
        sa.Column("id", sa.Integer(), nullable=False),
        sa.Column("service_name", sa.Text(), nullable=False),
        sa.Column("service_url", sa.Text(), nullable=False),
        sa.Column("ignore_tag", sa.Text(), nullable=False),
        sa.PrimaryKeyConstraint("id"),
        sa.UniqueConstraint("service_name"),
    )
    op.create_table(
        "translationservicekey",
        sa.Column("id", sa.Integer(), nullable=False),
        sa.Column("api_key", sa.Text(), nullable=False),
        sa.Column("group_id", sa.Integer(), nullable=False),
        sa.Column("service_id", sa.Integer(), nullable=False),
        sa.ForeignKeyConstraint(
            ["group_id"],
            ["usergroup.id"],
        ),
        sa.ForeignKeyConstraint(
            ["service_id"],
            ["translationservice.id"],
        ),
        sa.PrimaryKeyConstraint("id"),
    )
    # ### end Alembic commands ###


def downgrade():
    # ### commands auto generated by Alembic - please adjust! ###
    op.drop_table("translationservicekey")
    op.drop_table("translationservice")
    op.drop_table("language")
    # ### end Alembic commands ###
