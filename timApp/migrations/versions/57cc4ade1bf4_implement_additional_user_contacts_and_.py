"""Implement additional user contacts and their verification

Revision ID: 57cc4ade1bf4
Revises: 132d3c908b54
Create Date: 2021-06-18 09:56:55.241594

"""

# revision identifiers, used by Alembic.
revision = '57cc4ade1bf4'
down_revision = '132d3c908b54'

from alembic import op
import sqlalchemy as sa
from sqlalchemy.dialects import postgresql

# This enum has a value 'EMAIL' added to it in it's class in between migrations, but adding a new enum value in db
# requires jumping through some hoops. See upgrade function for detail.
channel_enum = sa.Enum('TIM_MESSAGE', 'EMAIL_LIST', name='channel')


def upgrade():
    op.create_table('user_contact',
                    sa.Column('id', sa.Integer(), nullable=False),
                    sa.Column('user_id', sa.Integer(), nullable=False),
                    sa.Column('contact', sa.Text(), nullable=False),
                    sa.Column('primary', sa.Boolean(), nullable=False),
                    sa.Column('verified', sa.Boolean(), nullable=False),
                    sa.ForeignKeyConstraint(['user_id'], ['useraccount.id'], ),
                    sa.PrimaryKeyConstraint('id')
                    )
    op.drop_table('user_emails')
    # Add a column to user_contacts table here, because the enum used here already exists and it gives
    # psycopg2.errors.DuplicateObject error if used in create_table.
    op.add_column('user_contact', sa.Column('channel', channel_enum, nullable=False))
    op.add_column('verifications', sa.Column('contact_id', sa.Integer(), nullable=True))
    op.add_column('verifications', sa.Column('verification_token', sa.Text(), nullable=False))
    op.add_column('verifications', sa.Column('verified_at', sa.DateTime(timezone=True), nullable=True))
    op.alter_column('verifications', 'verification_type',
                    existing_type=postgresql.ENUM('LIST_JOIN', 'EMAIL_OWNERSHIP', name='verificationtype'),
                    nullable=False)
    op.create_foreign_key(None, 'verifications', 'user_contact', ['contact_id'], ['id'])
    op.drop_column('verifications', 'verification_link')
    op.drop_column('verifications', 'verified')
    with op.get_context().autocommit_block():
        # The enum 'channel' is already in use and has a new value since it was first used, so we need to manually
        # update it's value.ALTER TYPE cannot be used in a transaction context in postgresql v. < 12, so we need to wrap
        # this in an autocommit block.
        op.execute("""ALTER TYPE channel ADD VALUE IF NOT EXISTS 'EMAIL'""")
        # verificationtype enum had it's value changed between migrations. It is safer to just add a new value,
        # and leave the existing one in, to ensure that downgrades can be done safely.
        op.execute("""ALTER TYPE verificationtype ADD VALUE IF NOT EXISTS 'CONTACT_OWNERSHIP'""")


def downgrade():
    op.add_column('verifications',
                  sa.Column('verified', postgresql.TIMESTAMP(timezone=True), autoincrement=False, nullable=True))
    op.add_column('verifications', sa.Column('verification_link', sa.TEXT(), autoincrement=False, nullable=True))
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
    op.drop_table('user_contact')
    # Intentionally don't remove enums that are added in upgrade. There are no good corresponding db states for this
    # change in the opposite direction.

