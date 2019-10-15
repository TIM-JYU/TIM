"""Add copy right

Revision ID: 88ee56b0e588
Revises: c8a64d9429b1
Create Date: 2019-10-15 08:11:51.709602

"""

# revision identifiers, used by Alembic.
from sqlalchemy import orm

from timApp.auth.auth_models import AccessTypeModel

revision = '88ee56b0e588'
down_revision = 'c8a64d9429b1'

from alembic import op


def upgrade():
    bind = op.get_bind()
    s = orm.Session(bind=bind)
    s.add(AccessTypeModel(id=7, name='copy'))
    s.flush()


def downgrade():
    pass
