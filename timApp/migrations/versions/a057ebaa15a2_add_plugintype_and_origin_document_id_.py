"""Add PluginType and origin document ID to answers

Revision ID: a057ebaa15a2
Revises: 132d3c908b54
Create Date: 2021-08-11 11:43:57.533871

"""

# revision identifiers, used by Alembic.
revision = "a057ebaa15a2"
down_revision = "132d3c908b54"

import sqlalchemy as sa
from alembic import op


def upgrade():
    # ### commands auto generated by Alembic - please adjust! ###
    op.create_table(
        "plugintype",
        sa.Column("id", sa.Integer(), nullable=False),
        sa.Column("type", sa.Text(), nullable=False),
        sa.PrimaryKeyConstraint("id"),
        sa.UniqueConstraint("type"),
    )
    op.add_column("answer", sa.Column("origin_doc_id", sa.Integer(), nullable=True))
    op.add_column("answer", sa.Column("plugin_type_id", sa.Integer(), nullable=True))
    op.create_foreign_key(
        "answer_origin_doc_id_fkey", "answer", "block", ["origin_doc_id"], ["id"]
    )
    op.create_foreign_key(
        "answer_plugin_type_id_fkey", "answer", "plugintype", ["plugin_type_id"], ["id"]
    )
    # ### end Alembic commands ###


def downgrade():
    # ### commands auto generated by Alembic - please adjust! ###
    op.drop_constraint("answer_origin_doc_id_fkey", "answer", type_="foreignkey")
    op.drop_constraint("answer_plugin_type_id_fkey", "answer", type_="foreignkey")
    op.drop_column("answer", "plugin_type_id")
    op.drop_column("answer", "origin_doc_id")
    op.drop_table("plugintype")
    # ### end Alembic commands ###
