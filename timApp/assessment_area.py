from enum import Enum
from typing import Optional


class AssessmentArea:
    """
    Used as a helper to refer to an assessment area without signifying the type of the target. You can also retrieve sql
    scripts for querying the database about the area. To query areas that contain this one, you need to create those
    areas yourself. This should be the natural place to include sql to check access rights for viewing velps, but this
    is not yet implemented.
    """

    class _AssessmentAreaTypes(Enum):
        """Enum to save what kind of object are we referring to."""
        folder = 1
        document = 2
        area = 3
        paragraph = 4

    def __init__(self, folder_id: Optional[int], document_id: Optional[int], area_id: Optional[int],
                 paragraph_id: Optional[str]):
        """
        Acceptable combinations of parameters:
        * folder_id with all others None.
        * document_id with all others None.
        * document_id with either area_id or paragraph_id not None and folder_id none.
        :param folder_id: Id of the folder
        :param document_id: Id of the document
        :param area_id: Id of the area
        :param paragraph_id: Id of the paragraph
        """

        # Sanity check for arguments.
        assert ((folder_id is not None and document_id is None and area_id is None and paragraph_id is None) or
                (folder_id is None and document_id is not None and not (
                    area_id is not None and paragraph_id is not None)))

        if folder_id is not None:
            self._type = AssessmentArea._AssessmentAreaTypes.folder
        elif area_id is None and paragraph_id is None:
            self._type = AssessmentArea._AssessmentAreaTypes.document
        elif area_id is not None:
            self._type = AssessmentArea._AssessmentAreaTypes.area
        else:
            self._type = AssessmentArea._AssessmentAreaTypes.paragraph
        self._folder_id = folder_id
        self._document_id = document_id
        self._area_id = area_id
        self._paragraph_id = paragraph_id

    def get_parameters_for_velp_ids(self) -> list:
        """
        Returns a list suitable for placeholder substitution in a parametrized sql statement.
        :return: List of values.
        """
        if self._type is AssessmentArea._AssessmentAreaTypes.folder:
            return [self._folder_id]
        if self._type is AssessmentArea._AssessmentAreaTypes.document:
            return [self._document_id]
        if self._type is AssessmentArea._AssessmentAreaTypes.area:
            return [self._document_id, self._area_id]
        if self._type is AssessmentArea._AssessmentAreaTypes.paragraph:
            return [self._document_id, self._document_id]

    def get_sql_for_velp_ids(self) -> str:
        """Returns an sql script which will query the database for velps in use in this assessment area.
        Only returns velps that are still valid.
        :return: string of sql script.
        """
        if self._type is AssessmentArea._AssessmentAreaTypes.document:
            return """
                   SELECT Velp.id
                   FROM Velp
                   WHERE (Velp.valid_until >= current_timestamp OR Velp.valid_until ISNULL) AND Velp.id IN (
                     SELECT VelpInGroup.velp_id
                     FROM VelpInGroup
                     WHERE VelpInGroup.velp_group_id IN (
                       SELECT velp_group_id
                       FROM VelpGroupInDocument
                       WHERE document_id = ?
                     )
                   )
                   """
        else:
            raise NotImplementedError


# Helpers for object creation. Default parameters can't support all cases.
# Use of these is recommended instead of the __init__
def assessment_area_from_document(document_id: int) -> AssessmentArea:
    """
    Creates an assessment area for a document.
    :param document_id: Id of the document
    :return: an AssessmentArea
    """
    return AssessmentArea(None, document_id, None, None)


def assessment_area_from_folder(folder_id: int) -> AssessmentArea:
    """
    Creates an assessment area for a folder.
    :param folder_id: Id of the folder
    :return: an AssessmentArea
    """
    return AssessmentArea(folder_id, None, None, None)


def assessment_area_from_area(document_id: int, area_id: str) -> AssessmentArea:
    """
    Creates an assessment area for an area in a document.
    :param document_id Id of the document the area belongs to.
    :param area_id: Id of the area.
    :return: an AssessmentArea
    """
    return AssessmentArea(None, document_id, area_id, None)


def assessment_area_from_paragraph(document_id: int, paragraph_id: str) -> AssessmentArea:
    """
    Creates an assessment area for a paragraph in a document.
    :param document_id: Id of the document
    :param paragraph_id: Id of the paragraph.
    :return: an AssessmentArea
    """
    return AssessmentArea(None, document_id, None, paragraph_id, None)
