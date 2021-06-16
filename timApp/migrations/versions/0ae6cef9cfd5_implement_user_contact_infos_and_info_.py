"""Implement user contact infos and info ownership verification

Revision ID: 0ae6cef9cfd5
Revises: 132d3c908b54
Create Date: 2021-06-16 22:31:07.849739

"""

# revision identifiers, used by Alembic.
revision = '0ae6cef9cfd5'
down_revision = '132d3c908b54'

from alembic import op
import sqlalchemy as sa
from sqlalchemy.dialects import postgresql

channel_enum = sa.Enum('TIM_MESSAGE', 'EMAIL_LIST', 'EMAIL', name='channel')


def upgrade():
    op.create_table('user_contacts',
                    sa.Column('id', sa.Integer(), nullable=False),
                    sa.Column('user_id', sa.Integer(), nullable=True),
                    sa.Column('contact', sa.Text(), nullable=False),
                    # This breaks upgrade with psycopg2.errors.DuplicateObject error, so add this column outside create
                    # table.
                    # sa.Column('channel', sa.Enum('TIM_MESSAGE', 'EMAIL_LIST', 'EMAIL', name='channel'),
                    # nullable=False),
                    sa.Column('primary', sa.Boolean(), nullable=False),
                    sa.Column('verified', sa.Boolean(), nullable=False),
                    sa.ForeignKeyConstraint(['user_id'], ['useraccount.id'], ),
                    sa.PrimaryKeyConstraint('id')
                    )
    op.drop_table('user_emails')
    # Add a column to user_contacts table here, because of the reason stated in create_table.
    op.add_column('user_contacts', sa.Column('channel', channel_enum, nullable=False))
    op.add_column('verifications', sa.Column('contact_id', sa.Integer(), nullable=False))
    op.add_column('verifications', sa.Column('verification_token', sa.Text(), nullable=False))
    op.add_column('verifications', sa.Column('verified_at', sa.DateTime(timezone=True), nullable=True))
    op.alter_column('verifications', 'verification_type',
                    existing_type=postgresql.ENUM('LIST_JOIN', 'EMAIL_OWNERSHIP', name='verificationtype'),
                    nullable=False)
    op.create_foreign_key(None, 'verifications', 'user_contacts', ['contact_id'], ['id'])
    op.drop_column('verifications', 'verification_link')
    op.drop_column('verifications', 'verified')


def downgrade():
    op.add_column('verifications',
                  sa.Column('verified', postgresql.TIMESTAMP(timezone=True), autoincrement=False, nullable=True))
    op.add_column('verifications', sa.Column('verification_link', sa.TEXT(), autoincrement=False, nullable=True))
    # This is automatically generated, but breaks downgrade.
    # op.drop_constraint(None, 'verifications', type_='foreignkey')
    op.alter_column('verifications', 'verification_type',
                    existing_type=postgresql.ENUM('LIST_JOIN', 'EMAIL_OWNERSHIP', name='verificationtype'),
                    nullable=True)
    op.drop_column('verifications', 'verified_at')
    op.drop_column('verifications', 'verification_token')
    op.drop_column('verifications', 'contact_id')
    op.create_table('user_emails',
                    sa.Column('id', sa.INTEGER(), autoincrement=False, nullable=False),
                    sa.Column('additional_email', sa.TEXT(), autoincrement=False, nullable=True),
                    sa.Column('is_primary_email', sa.BOOLEAN(), autoincrement=False, nullable=True),
                    sa.Column('address_verified', postgresql.TIMESTAMP(timezone=True), autoincrement=False,
                              nullable=True),
                    sa.ForeignKeyConstraint(['id'], ['useraccount.id'], name='user_emails_id_fkey'),
                    sa.PrimaryKeyConstraint('id', name='user_emails_pkey'),
                    sa.UniqueConstraint('additional_email', name='user_emails_additional_email_key')
                    )
    op.drop_table('user_contacts')
