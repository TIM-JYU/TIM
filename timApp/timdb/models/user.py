from tim_app import db
from timdb.models.folder import Folder
from timdb.models.usergroup import UserGroup
from timdb.timdbexception import TimDbException


class User(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'useraccount'
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.Text, nullable=False)  # TODO Should be unique?
    real_name = db.Column(db.Text)
    email = db.Column(db.Text)
    prefs = db.Column(db.Text)
    pass_ = db.Column('pass', db.Text)
    yubikey = db.Column(db.Text)

    def get_personal_group(self) -> UserGroup:
        g = UserGroup.query.filter_by(name=self.name).first()
        if g:
            return g
        g = UserGroup.query.filter_by(name='group of user ' + self.name).first()
        if g:
            return g
        raise TimDbException('Personal usergroup for user {} was not found!'.format(self.name))

    def get_personal_folder(self):
        path = 'users/' + self.name
        f = Folder.find_by_full_path(path)
        if f is None:
            return Folder.create(path, self.get_personal_group().id, apply_default_rights=True)
        return f
