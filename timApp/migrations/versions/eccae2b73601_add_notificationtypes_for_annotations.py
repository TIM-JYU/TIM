"""Add NotificationTypes for Annotations

Revision ID: eccae2b73601
Revises: a1485d740d43
Create Date: 2025-03-19 12:23:55.539477

"""

# revision identifiers, used by Alembic.
revision = "eccae2b73601"
down_revision = "a1485d740d43"

from alembic import op
import sqlalchemy as sa

e = sa.Enum(
    "DocModified",
    "ParAdded",
    "ParModified",
    "ParDeleted",
    "CommentAdded",
    "CommentModified",
    "CommentDeleted",
    "AnswerAdded",
    "AnnotationAdded",
    "AnnotationModified",
    "AnnotationDeleted",
    name="notificationtype",
)


def upgrade():
    # Alter the existing enum type to include new values
    op.execute("ALTER TYPE notificationtype ADD VALUE IF NOT EXISTS 'AnnotationAdded'")
    op.execute(
        "ALTER TYPE notificationtype ADD VALUE IF NOT EXISTS 'AnnotationModified'"
    )
    op.execute(
        "ALTER TYPE notificationtype ADD VALUE IF NOT EXISTS 'AnnotationDeleted'"
    )


def downgrade():
    # Downgrade logic if needed (e.g., removing values, though PostgreSQL doesn't allow removing values from enums)
    pass
