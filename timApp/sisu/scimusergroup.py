import re

from timApp.timdb.sqa import db


external_id_re = re.compile(
    r'(?P<norole>(?P<courseid>jy-[A-Z]+-\d+)-(jy-studysubgroup-\d+-)?)(?P<role>teachers|responsible-teachers|students|administrative-persons|studysubgroup-teachers|studysubgroup-students)'
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
    def course_id(self) -> str:
        m = external_id_re.fullmatch(self.external_id)
        return m.group('courseid')

    @property
    def without_role(self) -> str:
        m = external_id_re.fullmatch(self.external_id)
        return m.group('norole')

    @property
    def is_teacher(self):
        return self.external_id.endswith('-teachers')

    @property
    def is_responsible_teacher(self):
        return self.external_id.endswith('-responsible-teachers')

    @property
    def is_student(self):
        return self.external_id.endswith('-students')

    @property
    def is_administrative_person(self):
        return self.external_id.endswith('-administrative-persons')
