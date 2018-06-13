from timApp.timdb.sqa import db


class UserNote(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'usernotes'
    id = db.Column(db.Integer, primary_key=True)
    usergroup_id = db.Column(db.Integer, db.ForeignKey('usergroup.id'), nullable=False)
    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'), nullable=False)
    par_id = db.Column(db.Text, nullable=False)
    par_hash = db.Column(db.Text, nullable=False)
    content = db.Column(db.Text, nullable=False)
    created = db.Column(db.DateTime(timezone=True), nullable=False)
    modified = db.Column(db.DateTime(timezone=True))
    access = db.Column(db.Text, nullable=False)
    tags = db.Column(db.Text, nullable=False)
    html = db.Column(db.Text)

    usergroup = db.relationship('UserGroup', back_populates='notes')
