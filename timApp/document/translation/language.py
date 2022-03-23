import langcodes as lc
from timApp.timdb.sqa import db
from typing import Optional


class Language(db.Model):
    """Represents a standardized language code used for example with translation documents.

    NOTE: You should always use the provided class-methods for creating new instances!
    """

    __tablename__ = "language"

    lang_code = db.Column(db.Text, nullable=False, primary_key=True)
    """Standardized code of the language."""

    # TODO should this be unique?
    # TODO in what language? Couldn't this be solved by using the langcodes library and asking for antonym every time a new translation is made? Is the idea to limit the use of langcodes library?
    lang_name = db.Column(db.Text, nullable=False)
    """IANA's name for the language."""

    flag_uri = db.Column(db.Text)
    """Path to a picture representing the language."""

    autonym = db.Column(db.Text, nullable=False)
    """Native name for the language."""

    @classmethod
    def create_from_name(cls, s: str) -> "Language":
        """
        Create an instance of Language that follows a standard. Note that this should always be used when creating a new Language especially when adding it to database.
        :param s: Natural name of the language
        :return: A corresponding Language-object newly created
        """
        lang = lc.find(s)
        return Language(
            lang_code=lang.to_tag(),
            lang_name=lang.language_name(),
            autonym=lang.autonym(),
        )

    @classmethod
    def query_by_code(cls, code: str) -> Optional["Language"]:
        """
        Query the database to find a single match for language tag
        :param code: The IETF tag for the language
        :return: The corresponding Language-object in database or None if not found
        """
        # TODO Instead of type str -code, could langcodes.Language type lessen boilerplate at caller?
        return cls.query.get(code)

    @classmethod
    def query_all(cls) -> list["Language"]:
        """
        Query the database for all the languages
        :return: All the languages found from database
        """
        return cls.query.all()

    def __str__(self) -> str:
        """:return: Nice format for users to read"""
        return lc.get(self.lang_code).display_name()
