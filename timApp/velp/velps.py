"""The module contains the database functions related to velps and velp labels. This includes adding and modifying velps
and their labels. The module also retrieves the data related to velps and their labels from the database.

:authors: Joonas Lattu, Petteri Palojärvi
:copyright: 2016 Timber project members
:version: 1.0.0

"""

from typing import Optional, List, Tuple, Dict, Iterable

from sqlalchemy import func
from sqlalchemy.orm import joinedload

from timApp.timdb.sqa import db
from timApp.velp.velp_models import Velp, VelpVersion, VelpLabel, VelpLabelContent, VelpContent, LabelInVelp, \
    VelpInGroup, VelpGroupsInDocument, VelpGroup


def _create_velp(creator_id: int, default_points: Optional[float], valid_until: Optional[str] = None, visible_to: Optional[int] = None,
                 color: Optional[str] = None) -> Velp:
    """Creates a new entry to the velp table.

    :param creator_id: User ID of creator.
    :param default_points: Default points for velp.
    :param valid_until: Time after velp becomes unusable.
    :param visible_to: Default visibility to annotation.
    :return: ID of velp that was just created.

    """
    if not visible_to:
        visible_to = 4
    v = Velp(creator_id=creator_id,
             default_points=default_points,
             color=color,
             valid_until=valid_until,
             visible_to=visible_to)
    db.session.add(v)
    return v


def create_velp_version(velp: Velp) -> VelpVersion:
    """Creates a new version for a velp to use.

    :param velp: The velp we're adding version for

    """

    vv = VelpVersion(velp=velp)
    db.session.add(vv)
    return vv


def add_velp_label_translation(label: VelpLabel, language_id: str, content: str):
    """Adds new translation to an existing label.

    :param label: Label
    :param language_id: Language chosen
    :param content: New translation

    """
    vlc = VelpLabelContent(velplabel=label, language_id=language_id, content=content)
    db.session.add(vlc)


def create_velp_content(version: VelpVersion, language_id: str, content: str, default_comment: str):
    """Method to create content (text) for velp.

    :param version: The VelpVersion
    :param language_id: Language id
    :param content: Text of velp
    :param default_comment: Default comment for velp

    """
    vc = VelpContent(velp_version=version, language_id=language_id, content=content, default_comment=default_comment)
    db.session.add(vc)


def create_new_velp(creator_id: int, content: str, default_points: Optional[float] = None,
                    default_comment: Optional[str] = None, valid_until: Optional[str] = None, language_id: str = "FI",
                    visible_to: Optional[int] = None, color: Optional[int] = None) -> Tuple[Velp, VelpVersion]:
    """Creates a new velp with all information.

    Creates a new velp with all necessary information in one function using three others.

    :param default_comment: Default comment for velp
    :param creator_id: User ID of creator.
    :param content: Text for velp.
    :param default_points: Default points for velp, None if not given.
    :param valid_until: Time after velp becomes unusable.
    :param language_id: Language ID of velp.
    :param visible_to: Default visibility to annotation.
    :param color: Velp color
    :return: A tuple of (velp id, velp version id).

    """
    new_velp = _create_velp(creator_id, default_points, valid_until, visible_to, color)
    new_version = create_velp_version(new_velp)
    create_velp_content(new_version, language_id, content, default_comment)
    db.session.flush()
    return new_velp, new_version


def update_velp(velp_id: int, default_points: str, color: str, visible_to: int):
    """Changes the non-versioned properties of a velp. Does not update labels.

    :param velp_id: ID of velp that's being updated
    :param default_points: New default points
    :param color: Velp color
    :param visible_to: Velp visibility

    """
    if not visible_to:
        visible_to = 4
    v: Velp = Velp.query.get(velp_id)
    if v:
        v.default_points = default_points
        v.color = color
        v.visible_to = visible_to


def add_labels_to_velp(velp_id: int, labels: Iterable[int]):
    """Associates a set of labels to a velp. (Appends to existing labels)

    Do note that update_velp_labels depends on this method

    :param velp_id: id of the velp that
    :param labels: list of label ids

    """

    if labels:  # Labels list can theoretically be null at some situations
        for label_id in labels:
            db.session.add(LabelInVelp(label_id=label_id, velp_id=velp_id))


def update_velp_labels(velp_id: int, labels: Iterable[int]):
    """Replaces the labels of a velp with new ones.

    :param velp_id: velp ID
    :param labels: list of label IDs.

    """
    # First nuke existing labels.
    LabelInVelp.query.filter_by(velp_id=velp_id).delete()
    # Then add the new ones.
    add_labels_to_velp(velp_id, labels)


def get_latest_velp_version(velp_id: int, language_id: str = "FI") -> Optional[VelpContent]:
    """Method to fetch the latest version for velp in specific language.

    :param velp_id: ID of velp we're checking
    :param language_id: ID of language
    :return: Dictionary containing ID and content of velp version.

    """
    return (VelpContent.query
            .filter_by(language_id=language_id)
            .join(VelpVersion)
            .filter_by(velp_id=velp_id)
            .order_by(VelpVersion.id.desc())
            .with_entities(VelpContent)
            .first())


def get_velp_content_for_document(doc_id: int, user_id: int, language_id: str = 'FI') -> List[Velp]:
    """Gets velps for document.

    Uses VelpGroupsInDocument table data to determine which velp groups and via those which velps are usable
    for specific user in specific document.

    :param doc_id: ID of document in question
    :param user_id: ID of current user
    :param language_id: ID of language used
    """

    sq = (VelpVersion.query
          .group_by(VelpVersion.velp_id)
          .with_entities(VelpVersion.velp_id, func.max(VelpVersion.id).label('ver'))
          .subquery())
    vq = (
        Velp.query
            .join(sq, sq.c.velp_id == Velp.id)
            .join(VelpContent, sq.c.ver == VelpContent.version_id)
            .filter(VelpContent.language_id == language_id)
            .filter((Velp.valid_until == None) | (Velp.valid_until >= func.current_timestamp()))
            .join(VelpInGroup)
            .join(VelpGroupsInDocument, VelpInGroup.velp_group_id == VelpGroupsInDocument.velp_group_id)
            .join(VelpGroup)
            .filter((VelpGroupsInDocument.user_id == user_id) & (VelpGroupsInDocument.doc_id == doc_id))
            .with_entities(Velp)
            .options(joinedload(Velp.groups, innerjoin=True).raiseload(VelpGroup.block))
            .options(joinedload(Velp.velp_versions, innerjoin=True).joinedload(VelpVersion.content, innerjoin=True))
    )
    return vq.all()


def get_velp_label_content_for_document(doc_id: int, user_id: int, language_id: str = 'FI') -> Dict:
    """Gets velp label content for document.

    Uses VelpGroupsInDocument table data to determine which velp groups and via those which velp labels are usable
    for specific user in specific document.

    :param doc_id: ID of document in question
    :param user_id: ID of current user
    :param language_id: ID of language used
    :return: List of dicts containing velp label ids and content

    """
    vlcs = (
        VelpLabelContent.query
            .filter_by(language_id=language_id)
            .join(LabelInVelp, VelpLabelContent.velplabel_id == LabelInVelp.label_id)
            .join(Velp)
            .filter((Velp.valid_until >= func.current_timestamp()) | (Velp.valid_until == None))
            .join(VelpInGroup)
            .join(VelpGroupsInDocument, VelpInGroup.velp_group_id == VelpGroupsInDocument.velp_group_id)
            .filter_by(doc_id=doc_id, user_id=user_id)
            .with_entities(VelpLabelContent)
            .all()
    )
    return vlcs
