from contracts import contract
from enum import Enum


# Helpers for object creation. Default parameters can't support all cases.
# Use of these is recommended instead of the __init__
@contract
def assessment_area_from_document(document_id: 'int') -> 'AssessmentArea':
    """
    Creates an assessment area for a document.
    :param document_id: Id of the document
    :return: an AssessmentArea
    """
    return AssessmentArea(None, document_id, None, None)


@contract
def assessment_area_from_folder(folder_id: 'int') -> 'AssessmentArea':
    """
    Creates an assessment area for a folder.
    :param folder_id: Id of the folder
    :return: an AssessmentArea
    """
    return AssessmentArea(folder_id, None, None, None)


@contract
def assessment_area_from_area(document_id: 'int', area_id: 'str') -> 'AssessmentArea':
    """
    Creates an assessment area for an area in a document.
    :param document_id Id of the document the area belongs to.
    :param area_id: Id of the area.
    :return: an AssessmentArea
    """
    return AssessmentArea(None, document_id, area_id, None)


@contract
def assessment_area_from_paragraph(document_id: 'int', paragraph_id: 'str') -> 'AssessmentArea':
    """
    Creates an assessment area for a paragraph in a document.
    :param document_id: Id of the document
    :param paragraph_id: Id of the paragraph.
    :return: an AssessmentArea
    """
    return AssessmentArea(None, document_id, None, paragraph_id, None)


class AssessmentArea:
    """
    Used as a helper to refer to an assessment area without signifying the type of the target. You can also retrieve sql
    blobs for querying the database about the area.
    """

    class _AssessmentAreaTypes(Enum):
        """Enum to save what kind of object are we referring to."""
        folder = 1
        document = 2
        area = 3
        paragraph = 4

    @contract
    def __init__(self, folder_id: 'int|None', document_id: 'int|None', area_id: 'str|None', paragraph_id: 'str|None'):
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
            self._type=AssessmentArea._AssessmentAreaTypes.paragraph