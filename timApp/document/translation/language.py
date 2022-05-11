"""
Contains implementation of the Language-database model, which is used to unify
TIM's translation-documents' languages.
"""

__authors__ = [
    "Noora Jokela",
    "Riku Lehkonen",
    "Vili Moisala",
    "Juho Tarkkanen",
    "Sami Viitanen",
]
__license__ = "MIT"
__date__ = "25.4.2022"


from dataclasses import dataclass
from typing import Optional

import langcodes

from timApp.timdb.sqa import db


@dataclass
class Language(db.Model):
    """Represents a standardized language code used for example with
    translation documents.

    NOTE: You should always use the provided class-methods for creating new
    instances!
    """

    __tablename__ = "language"

    lang_code = db.Column(db.Text, nullable=False, primary_key=True)
    """Standardized code of the language."""

    # TODO should this be unique?
    lang_name = db.Column(db.Text, nullable=False)
    """IANA's name for the language."""

    flag_uri = db.Column(db.Text)
    """Path to a picture representing the language."""

    autonym = db.Column(db.Text, nullable=False)
    """Native name for the language."""

    def __init__(
        self, lang_code: str, lang_name: str, autonym: str, flag_uri: str | None = None
    ):
        """
        Initialize a custom language by standardizing the tag.
        """
        # Standardize primary key with langcodes before creating db-object.
        # TODO/NOTE The boolean check is here because of implementation on
        #  DeeplTranslationService.get_languages. Maybe see about not using
        #  it that way?
        standard_code = langcodes.standardize_tag(lang_code) if lang_code else lang_code
        self.lang_code = standard_code
        self.lang_name = lang_name
        self.autonym = autonym
        self.flag_uri = flag_uri

    @classmethod
    def create_from_name(cls, name: str) -> "Language":
        """
        Create an instance of Language that follows a standard. Note that this
        should always be used when creating a new Language especially when
        adding it to database.

        :param name: Natural name of the language
        :return: A corresponding Language-object newly created.
        :raises LookupError: if the language is not found from langcodes'
         database.
        """
        lang = langcodes.find(name)
        return Language(
            lang_code=lang.to_tag(),
            lang_name=lang.language_name(),
            autonym=lang.autonym(),
        )

    @classmethod
    def query_by_code(cls, code: str) -> Optional["Language"]:
        """
        Query the database to find a single match for language tag

        :param code: The IETF tag for the language.
        :return: The corresponding Language-object in database or None if not
         found.
        """
        # TODO Instead of the code -parameter being str-type, could
        #  langcodes.Language type be more convenient to caller?
        return cls.query.get(code)

    @classmethod
    def query_all(cls) -> list["Language"]:
        """
        Query the database for all the languages

        :return: All the languages found from database.
        """
        return cls.query.all()

    def __str__(self) -> str:
        """
        Create a string representation of the Language instance.

        :return: Nice format for users to read
        """
        return f"'{self.lang_name}' ({self.lang_code})"

    def to_json(self) -> dict:
        """
        Create a JSON representation of the Language instance.

        :return: The Language instance's fields in a dict.
        """
        return {
            "code": self.lang_code,
            "standardName": self.lang_name,
            "flagUri": self.flag_uri,
            "name": self.autonym,
        }
