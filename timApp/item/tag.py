"""
A tag with associated document id, tag name and expiration date.
"""

from typing import List

from timApp.timdb.sqa import db


class Tag(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'tag'
    block_id = db.Column(db.Integer, db.ForeignKey('block.id'), primary_key=True)
    tag = db.Column(db.Text, primary_key=True)
    expires = db.Column(db.DateTime(timezone=True))

    block = db.relationship('Block', back_populates='tags')

    def __json__(self):
        return ['block_id', 'tag', 'expires']
