"""Defines all data models related to velps."""
from datetime import datetime

from tim_app import db


class Annotation(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'annotation'
    id = db.Column(db.Integer, primary_key=True)
    velp_version_id = db.Column(db.Integer, db.ForeignKey('velpversion.id'), nullable=False)
    icon_id = db.Column(db.Integer, db.ForeignKey('icon.id'))
    annotator_id = db.Column(db.Integer, db.ForeignKey('useraccount.id'), nullable=False)
    points = db.Column(db.Float)
    creation_time = db.Column(db.DateTime(timezone=True), nullable=False, default=datetime.utcnow)
    valid_from = db.Column(db.DateTime(timezone=True), default=datetime.utcnow)
    valid_until = db.Column(db.DateTime(timezone=True))
    visible_to = db.Column(db.Integer)
    document_id = db.Column(db.Integer, db.ForeignKey('block.id'))
    answer_id = db.Column(db.Integer, db.ForeignKey('answer.id'))
    paragraph_id_start = db.Column(db.Text)
    paragraph_id_end = db.Column(db.Text)
    offset_start = db.Column(db.Integer)
    node_start = db.Column(db.Integer)
    depth_start = db.Column(db.Integer)
    offset_end = db.Column(db.Integer)
    node_end = db.Column(db.Integer)
    depth_end = db.Column(db.Integer)
    hash_start = db.Column(db.Text)
    hash_end = db.Column(db.Text)
    element_path_start = db.Column(db.Text)
    element_path_end = db.Column(db.Text)


class AnnotationComment(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'annotationcomment'
    id = db.Column(db.Integer, primary_key=True)
    annotation_id = db.Column(db.Integer, db.ForeignKey('annotation.id'), nullable=False)
    comment_time = db.Column(db.DateTime(timezone=True), nullable=False, default=datetime.utcnow)
    commenter_id = db.Column(db.Integer, db.ForeignKey('useraccount.id'), nullable=False)
    content = db.Column(db.Text)


class Icon(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'icon'
    id = db.Column(db.Integer, primary_key=True)
    filename = db.Column(db.Text)  # TODO Should this be not null?


class ImportedVelpGroups(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'importedvelpgroups'
    user_group = db.Column(db.Integer, db.ForeignKey('usergroup.id'), primary_key=True)
    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'), primary_key=True)
    target_type = db.Column(db.Integer, nullable=False)  # 0 = document, 1 = paragraph, 2 = area
    target_id = db.Column(db.Text, primary_key=True)
    velp_group_id = db.Column(db.Integer, db.ForeignKey('velpgroup.id'), primary_key=True)


class LabelInVelp(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'labelinvelp'
    label_id = db.Column(db.Integer, db.ForeignKey('velplabel.id'), primary_key=True)
    velp_id = db.Column(db.Integer, db.ForeignKey('velp.id'), primary_key=True)


class LabelInVelpGroup(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'labelinvelpgroup'
    velp_group_id = db.Column(db.Integer, db.ForeignKey('velpgroup.id'), primary_key=True)
    group_label_id = db.Column(db.Integer, db.ForeignKey('velpgrouplabel.id'), primary_key=True)


class Velp(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'velp'
    id = db.Column(db.Integer, primary_key=True)
    creator_id = db.Column(db.Integer, db.ForeignKey('useraccount.id'), nullable=False)
    creation_time = db.Column(db.DateTime(timezone=True), nullable=False, default=datetime.utcnow)
    default_points = db.Column(db.Float)
    icon_id = db.Column(db.Integer, db.ForeignKey('icon.id'))
    valid_from = db.Column(db.DateTime(timezone=True), default=datetime.utcnow)
    valid_until = db.Column(db.DateTime(timezone=True))


class VelpContent(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'velpcontent'
    version_id = db.Column(db.Integer, db.ForeignKey('velpversion.id'), primary_key=True)
    language_id = db.Column(db.Text, primary_key=True)
    content = db.Column(db.Text)


class VelpGroup(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'velpgroup'
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.Text)
    creation_time = db.Column(db.DateTime(timezone=True), nullable=False, default=datetime.utcnow)
    valid_from = db.Column(db.DateTime(timezone=True), default=datetime.utcnow)
    valid_until = db.Column(db.DateTime(timezone=True))
    default_group = db.Column(db.Boolean, default=False)


class VelpGroupDefaults(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'velpgroupdefaults'
    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'), primary_key=True)
    target_type = db.Column(db.Integer, nullable=False)  # 0 = document, 1 = paragraph, 2 = area
    target_id = db.Column(db.Text, primary_key=True)
    velp_group_id = db.Column(db.Integer, db.ForeignKey('velpgroup.id'), primary_key=True)
    selected = db.Column(db.Boolean, default=False)


class VelpGroupLabel(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'velpgrouplabel'
    id = db.Column(db.Integer, primary_key=True)
    content = db.Column(db.Text, nullable=False)


class VelpGroupSelection(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'velpgroupselection'
    user_id = db.Column(db.Integer, db.ForeignKey('useraccount.id'), primary_key=True)
    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'), primary_key=True)
    target_type = db.Column(db.Integer, nullable=False)  # 0 = document, 1 = paragraph, 2 = area
    target_id = db.Column(db.Text, primary_key=True)
    selected = db.Column(db.Boolean, default=False)
    velp_group_id = db.Column(db.Integer, db.ForeignKey('velpgroup.id'), primary_key=True)


class VelpGroupsInDocument(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'velpgroupsindocument'
    user_id = db.Column(db.Integer, db.ForeignKey('useraccount.id'), primary_key=True)
    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'), primary_key=True)
    velp_group_id = db.Column(db.Integer, db.ForeignKey('velpgroup.id'), primary_key=True)


class VelpInGroup(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'velpingroup'
    velp_group_id = db.Column(db.Integer, db.ForeignKey('velpgroup.id'), primary_key=True)
    velp_id = db.Column(db.Integer, db.ForeignKey('velp.id'), primary_key=True)
    points = db.Column(db.Float)


class VelpLabel(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'velplabel'
    id = db.Column(db.Integer, primary_key=True)
    language_id = db.Column(db.Text)
    content = db.Column(db.Text)


class VelpVersion(db.Model):
    __bind_key__ = 'tim_main'
    __tablename__ = 'velpversion'
    id = db.Column(db.Integer, primary_key=True)
    velp_id = db.Column(db.Integer, db.ForeignKey('velp.id'), nullable=False)
    modify_time = db.Column(db.DateTime(timezone=True), nullable=False, default=datetime.utcnow)
