"""Removes non-ascii and special characters from document and folder paths by replacing ä -> a etc. and removing
others.

Revision ID: fc12e43686f9
Revises: cdeeff4028b9
Create Date: 2016-12-21 13:55:28.930245

"""

# revision identifiers, used by Alembic.
revision = 'fc12e43686f9'
down_revision = 'cdeeff4028b9'

from alembic import op


def upgrade():
    op.execute("""UPDATE block SET description = regexp_replace(name, '^.*/', '')
                  FROM docentry
                  WHERE block.id = docentry.id""")
    op.execute("""UPDATE block SET description = name
                  FROM folder
                  WHERE block.id = folder.id""")

    op.execute("""UPDATE docentry
                  SET name     = regexp_replace(translate(name, 'åäöÅÄÖ ', 'aaoAAO-'), '[^a-zA-Z0-9/_-]', '')""")
    op.execute("""UPDATE folder
                  SET name     = regexp_replace(translate(name, 'åäöÅÄÖ ', 'aaoAAO-'), '[^a-zA-Z0-9/_-]', ''),
                      location = regexp_replace(translate(location, 'åäöÅÄÖ ', 'aaoAAO-'), '[^a-zA-Z0-9/_-]', '')""")


def downgrade():
    pass
