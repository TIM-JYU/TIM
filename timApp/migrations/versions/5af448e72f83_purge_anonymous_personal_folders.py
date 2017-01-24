"""Purge anonymous personal folders.

Revision ID: 5af448e72f83
Revises: fc12e43686f9
Create Date: 2017-01-23 16:41:42.994476

"""

revision = '5af448e72f83'
down_revision = 'fc12e43686f9'

from alembic import op


def upgrade():
    op.execute("""DELETE FROM docentry
                  WHERE name ~ 'users/Anonymous[0-9]+/.*'""")
    op.execute("""DELETE FROM blockaccess
                  WHERE block_id IN (
                    SELECT id FROM block
                    WHERE type_id = 0
                      AND id NOT IN (
                        SELECT id FROM docentry
                      )
                      AND id NOT IN (
                        SELECT doc_id FROM translation
                      )
                  )""")
    op.execute("""DELETE FROM block
                  WHERE type_id = 0
                    AND id NOT IN (
                      SELECT id FROM docentry
                    )
                    AND id NOT IN (
                      SELECT doc_id FROM translation
                    )""")


def downgrade():
    raise NotImplementedError
