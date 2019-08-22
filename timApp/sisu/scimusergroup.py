import re

from timApp.timdb.sqa import db


external_id_re = re.compile(
    r'((jy-[A-Z]+-\d+)-(jy-studysubgroup-\d+-)?)(teachers|responsible-teachers|students|administrative-persons|studysubgroup-teachers|studysubgroup-students)'
)

class ScimUserGroup(db.Model):
    __tablename__ = 'scimusergroup'
    group_id = db.Column(db.Integer, db.ForeignKey('usergroup.id'), primary_key=True)
    external_id = db.Column(db.Text, unique=True, nullable=False)

    @property
    def is_studysubgroup(self):
        # Important: "-jy-" prefix must be here because otherwise this would match
        # the collective group studysubgroup-teachers or studysubgroup-students.
        return '-jy-studysubgroup-' in self.external_id

    @property
    def course_id(self):
        m = external_id_re.fullmatch(self.external_id)
        if m:
            return m.group(2)
        return None

    @property
    def without_role(self):
        m = external_id_re.fullmatch(self.external_id)
        if m:
            return m.group(1)
        return None

    @property
    def is_teacher(self):
        return self.external_id.endswith('-teachers')
