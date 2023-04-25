"""Rename HakaOrganization to ExternalOrganization

Revision ID: 903414693eec
Revises: 14a649d37bc3
Create Date: 2023-04-25 12:06:02.998734

"""

# revision identifiers, used by Alembic.
revision = "903414693eec"
down_revision = "14a649d37bc3"

from alembic import op


def upgrade():
    # Rename haka_organization to external_organization
    op.rename_table("haka_organization", "external_organization")


def downgrade():
    # Rename external_organization to haka_organization
    op.rename_table("external_organization", "haka_organization")
