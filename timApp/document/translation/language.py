from timApp.timdb.sqa import db
from timApp.document.translation.translation import Translation


class Language(db.Model):
    """Represents a standardized language code used for example with translation documents"""

    __tablename__ = "language"

    lang_code = db.Column(db.Text, nullable=False, primary_key=True)
    """Standardized code of the language."""

    # TODO should this be unique?
    # TODO in what language? Couldn't this be solved by using the langcodes library and asking for antonym every time a new translation is made? Is the idea to limit the use of langcodes library?
    lang_name = db.Column(db.Text, nullable=False)
    """IANA's name for the language."""

    flag_uri = db.Column(db.Text)
    """Path to a picture representing the language."""

    antonym = db.Column(db.Text, nullable=False)
    """Native name for the language."""


def find_by_str(s: str) -> Language:
    return Language.query.filter(Language.lang_code == s)
