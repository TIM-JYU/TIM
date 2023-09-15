"""Make special folder shortnames lowercase (Templates -> templates, Printing -> printing).

Revision ID: c0d6e136511e
Revises: 82c3b050ba5d
Create Date: 2017-10-10 10:48:20.601314

"""
from sqlalchemy import select

# revision identifiers, used by Alembic.
from timApp.folder.folder import Folder
from timApp.timdb.sqa import db

revision = "c0d6e136511e"
down_revision = "82c3b050ba5d"


def upgrade():
    for f in db.session.execute(select(Folder)).scalars().all():
        if f.path.endswith("/Templates/printing") or f.path == "Templates/printing":
            f.rename("printing_old")
    for f in db.session.execute(select(Folder)).scalars().all():
        if f.name == "templates":
            f.rename("templates_old")
    for f in db.session.execute(select(Folder)).scalars().all():
        if f.path.endswith("/Templates/Printing") or f.path == "Templates/Printing":
            f.rename("printing")
    for f in db.session.execute(select(Folder)).scalars().all():
        if f.name == "Templates":
            f.rename("templates")
    db.session.commit()


def downgrade():
    for f in db.session.execute(select(Folder)).scalars().all():
        if f.path.endswith("/templates/printing") or f.path == "templates/printing":
            f.rename("Printing")
    for f in db.session.execute(select(Folder)).scalars().all():
        if f.name == "templates":
            f.rename("Templates")
    db.session.commit()
