"""Moves owner to blockaccess.

Revision ID: b9e81ff20758
Revises: bc968697021c
Create Date: 2017-01-18 12:08:55.004521

"""

revision = 'b9e81ff20758'
down_revision = 'bc968697021c'

from alembic import op


def upgrade():
    op.execute("""INSERT INTO accesstype(id, name) VALUES(6, 'owner')""")
    op.execute("""INSERT INTO blockaccess(block_id, usergroup_id, type, accessible_from)
                  SELECT id, usergroup_id, 6, created
                  FROM block
                  WHERE id NOT IN (
                     SELECT id FROM docentry
                     WHERE name LIKE '%/$Default%Rights'
                  )
                  """)

    op.drop_constraint('block_usergroup_id_fkey', 'block', type_='foreignkey')
    op.drop_column('block', 'usergroup_id')


def downgrade():
    raise NotImplementedError
