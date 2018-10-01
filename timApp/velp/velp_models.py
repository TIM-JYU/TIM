"""Defines all data models related to velps."""
from datetime import datetime

from timApp.timdb.sqa import db


class Annotation(db.Model):
    """An annotation that can be associated with an Answer or with a DocParagraph in a Document.

    The annotation can start and end in specific positions, in which case the annotation is supposed to be displayed
    as highlighted text in the corresponding location.
    """
    __tablename__ = 'annotation'
    id = db.Column(db.Integer, primary_key=True)
    """Annotation identifier."""

    velp_version_id = db.Column(db.Integer, db.ForeignKey('velpversion.id'), nullable=False)
    """Id of the velp that has been used for this annotation."""

    icon_id = db.Column(db.Integer, db.ForeignKey('icon.id'))
    """Icon id that has been used for this annotation. Currently not used."""

    annotator_id = db.Column(db.Integer, db.ForeignKey('useraccount.id'), nullable=False)
    """Id of the User who created the annotation."""

    points = db.Column(db.Float)
    """Points associated with the annotation."""

    creation_time = db.Column(db.DateTime(timezone=True), nullable=False, default=datetime.utcnow)
    """Creation time."""

    valid_from = db.Column(db.DateTime(timezone=True), default=datetime.utcnow)
    """Since when should this annotation be valid."""

    valid_until = db.Column(db.DateTime(timezone=True))
    """Until when should this annotation be valid."""

    visible_to = db.Column(db.Integer)
    """Who should this annotation be visible to.
    
    Possible values are denoted by AnnotationVisibility enum:
    
    myself = 1
    owner = 2
    teacher = 3
    everyone = 4
    
    """

    document_id = db.Column(db.Integer, db.ForeignKey('block.id'))
    """Id of the document in case this is a paragraph annotation."""

    answer_id = db.Column(db.Integer, db.ForeignKey('answer.id'))
    """Id of the Answer in case this is an answer annotation."""

    paragraph_id_start = db.Column(db.Text)
    """The id of the paragraph where this annotation starts from (in case this is a paragraph annotation)."""

    paragraph_id_end = db.Column(db.Text)
    """The id of the paragraph where this annotation ends (in case this is a paragraph annotation)."""

    offset_start = db.Column(db.Integer)
    """Positional information about the annotation."""

    node_start = db.Column(db.Integer)
    """Positional information about the annotation."""

    depth_start = db.Column(db.Integer)
    """Positional information about the annotation."""

    offset_end = db.Column(db.Integer)
    """Positional information about the annotation."""

    node_end = db.Column(db.Integer)
    """Positional information about the annotation."""

    depth_end = db.Column(db.Integer)
    """Positional information about the annotation."""

    hash_start = db.Column(db.Text)
    """Positional information about the annotation."""

    hash_end = db.Column(db.Text)
    """Positional information about the annotation."""

    color = db.Column(db.Text)
    """Color for the annotation."""

    element_path_start = db.Column(db.Text)
    """Positional information about the annotation."""

    element_path_end = db.Column(db.Text)
    """Positional information about the annotation."""

    annotator = db.relationship('User', back_populates='annotations')


class AnnotationComment(db.Model):
    """A comment in an Annotation."""
    __tablename__ = 'annotationcomment'
    id = db.Column(db.Integer, primary_key=True)
    """Comment identifier."""

    annotation_id = db.Column(db.Integer, db.ForeignKey('annotation.id'), nullable=False)
    """Annotation id."""

    comment_time = db.Column(db.DateTime(timezone=True), nullable=False, default=datetime.utcnow)
    """Comment timestamp."""

    commenter_id = db.Column(db.Integer, db.ForeignKey('useraccount.id'), nullable=False)
    """Commenter user id."""

    content = db.Column(db.Text)
    """Comment text."""


class Icon(db.Model):
    """An icon that can be associated with an Annotation.

    Currently not used (0 rows in production DB as of 5th July 2018).
    """
    __tablename__ = 'icon'
    id = db.Column(db.Integer, primary_key=True)
    """Icon identifier."""

    filename = db.Column(db.Text)  # TODO Should this be not null?
    """Icon file name."""


class ImportedVelpGroups(db.Model):
    """Currently not used (0 rows in production DB as of 5th July 2018)."""
    __tablename__ = 'importedvelpgroups'
    user_group = db.Column(db.Integer, db.ForeignKey('usergroup.id'), primary_key=True)
    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'), primary_key=True)
    target_type = db.Column(db.Integer, nullable=False)  # 0 = document, 1 = paragraph, 2 = area
    target_id = db.Column(db.Text, primary_key=True)
    velp_group_id = db.Column(db.Integer, db.ForeignKey('velpgroup.id'), primary_key=True)


class LabelInVelp(db.Model):
    """Associates VelpLabels with Velps."""
    __tablename__ = 'labelinvelp'
    label_id = db.Column(db.Integer, db.ForeignKey('velplabel.id'), primary_key=True)
    velp_id = db.Column(db.Integer, db.ForeignKey('velp.id'), primary_key=True)


class LabelInVelpGroup(db.Model):
    """Currently not used (0 rows in production DB as of 5th July 2018)."""
    __tablename__ = 'labelinvelpgroup'
    velp_group_id = db.Column(db.Integer, db.ForeignKey('velpgroup.id'), primary_key=True)
    group_label_id = db.Column(db.Integer, db.ForeignKey('velpgrouplabel.id'), primary_key=True)


class Velp(db.Model):
    """A Velp is a kind of category for Annotations and is visually represented by a Post-it note."""
    __tablename__ = 'velp'
    id = db.Column(db.Integer, primary_key=True)
    creator_id = db.Column(db.Integer, db.ForeignKey('useraccount.id'), nullable=False)
    creation_time = db.Column(db.DateTime(timezone=True), nullable=False, default=datetime.utcnow)
    default_points = db.Column(db.Float)
    icon_id = db.Column(db.Integer, db.ForeignKey('icon.id'))
    valid_from = db.Column(db.DateTime(timezone=True), default=datetime.utcnow)
    valid_until = db.Column(db.DateTime(timezone=True))
    color = db.Column(db.Text)
    visible_to = db.Column(db.Integer, nullable=False)

    creator = db.relationship('User', back_populates='velps')


class VelpContent(db.Model):
    """The actual content of a Velp."""
    __tablename__ = 'velpcontent'
    version_id = db.Column(db.Integer, db.ForeignKey('velpversion.id'), primary_key=True)
    language_id = db.Column(db.Text, primary_key=True)
    content = db.Column(db.Text)
    default_comment = db.Column(db.Text)


class VelpGroup(db.Model):
    """Represents a group of Velps."""
    __tablename__ = 'velpgroup'
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.Text)
    creation_time = db.Column(db.DateTime(timezone=True), nullable=False, default=datetime.utcnow)
    valid_from = db.Column(db.DateTime(timezone=True), default=datetime.utcnow)
    valid_until = db.Column(db.DateTime(timezone=True))
    default_group = db.Column(db.Boolean, default=False)


class VelpGroupDefaults(db.Model):
    __tablename__ = 'velpgroupdefaults'
    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'), primary_key=True)
    target_type = db.Column(db.Integer, nullable=False)  # 0 = document, 1 = paragraph, 2 = area
    target_id = db.Column(db.Text, primary_key=True)
    velp_group_id = db.Column(db.Integer, db.ForeignKey('velpgroup.id'), primary_key=True)
    selected = db.Column(db.Boolean, default=False)


class VelpGroupLabel(db.Model):
    """Currently not used (0 rows in production DB as of 5th July 2018)."""
    __tablename__ = 'velpgrouplabel'
    id = db.Column(db.Integer, primary_key=True)
    content = db.Column(db.Text, nullable=False)


class VelpGroupSelection(db.Model):
    __tablename__ = 'velpgroupselection'
    user_id = db.Column(db.Integer, db.ForeignKey('useraccount.id'), primary_key=True)
    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'), primary_key=True)
    target_type = db.Column(db.Integer, nullable=False)  # 0 = document, 1 = paragraph, 2 = area
    target_id = db.Column(db.Text, primary_key=True)
    selected = db.Column(db.Boolean, default=False)
    velp_group_id = db.Column(db.Integer, db.ForeignKey('velpgroup.id'), primary_key=True)


class VelpGroupsInDocument(db.Model):
    """

    TODO: This table contains lots of rows in production DB (about 19000 as of 5th July 2018).
    TODO: Possibly needs some optimizations.
    """
    __tablename__ = 'velpgroupsindocument'
    user_id = db.Column(db.Integer, db.ForeignKey('useraccount.id'), primary_key=True)
    doc_id = db.Column(db.Integer, db.ForeignKey('block.id'), primary_key=True)
    velp_group_id = db.Column(db.Integer, db.ForeignKey('velpgroup.id'), primary_key=True)


class VelpInGroup(db.Model):
    __tablename__ = 'velpingroup'
    velp_group_id = db.Column(db.Integer, db.ForeignKey('velpgroup.id'), primary_key=True)
    velp_id = db.Column(db.Integer, db.ForeignKey('velp.id'), primary_key=True)
    points = db.Column(db.Float)


class VelpLabel(db.Model):
    """A label that can be assigned to a Velp."""
    __tablename__ = 'velplabel'
    id = db.Column(db.Integer, primary_key=True)


class VelpLabelContent(db.Model):
    __tablename__ = 'velplabelcontent'
    velplabel_id = db.Column(db.Integer, db.ForeignKey('velplabel.id'), primary_key=True)
    language_id = db.Column(db.Text, primary_key=True)
    content = db.Column(db.Text)


class VelpVersion(db.Model):
    __tablename__ = 'velpversion'
    id = db.Column(db.Integer, primary_key=True)
    velp_id = db.Column(db.Integer, db.ForeignKey('velp.id'), nullable=False)
    modify_time = db.Column(db.DateTime(timezone=True), nullable=False, default=datetime.utcnow)
