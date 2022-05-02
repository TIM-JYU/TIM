"""
Enables adding Languages to TIM's database from the command line
"""

__authors__ = [
    "Noora Jokela",
    "Riku Lehkonen",
    "Vili Moisala",
    "Juho Tarkkanen",
    "Sami Viitanen",
]
__license__ = "MIT"
__date__ = "8.4.2022"

import langcodes
import click
from flask.cli import AppGroup

from timApp.tim_app import app
from timApp.timdb.sqa import db
from timApp.document.translation.language import Language
from timApp.util.logger import log_error, log_info, log_debug

language_cli = AppGroup("language")


@language_cli.command()
@click.argument("lang_code")
def remove(lang_code: str) -> None:
    """
    Remove a specific language from the database by specifying its language code.

    :param lang_code: IETF language tag for the language.
    :return: None
    """

    exists = Language.query.filter(lang_code == Language.lang_code).first()
    if exists:
        if click.confirm("This action cannot be reversed. Continue?"):
            click.echo(
                f"Removing language '{exists.lang_name} ({exists.lang_code})' from the database."
            )
            db.session.delete(exists)
            db.session.commit()
        else:
            click.echo("Removal of language aborted by user.")
    else:
        click.echo(f"Language code '{lang_code}' could not be found in the database.")


@language_cli.command()
@click.argument("lang_name")
def add(lang_name: str) -> None:
    """
    Add a standard language to the language database based on the language name.

    :param lang_name: Natural name of the language.
    :return: None
    """

    try:
        lang = Language.create_from_name(lang_name)
    except Exception as e:
        click.echo(f"Failed to create language: {str(e)}")
        return

    exists = Language.query.filter(lang.lang_code == Language.lang_code).first()
    if exists:
        click.echo(f"Language code '{lang.lang_code}' already exists in the database.")
    else:
        click.echo(f"Adding new language '{lang.lang_name} ({lang.lang_code})'")
        db.session.add(lang)
        db.session.commit()


@language_cli.command()
def add_all_languages() -> None:
    """
    TODO Cannot call this name from cli `./r flask language add_all_languages`
    Add languages defined in configuration into database from command line.

    :return: None.
    """
    add_all_the_languages()


def add_all_the_languages() -> None:
    """
    Add all supported languages to the database. Supported languages are
    defined in the configuration variable LANGUAGES in timApp\defaultconfig.py.

    This separate function allows adding all the languages at the
    db-initialization.

    :return: None.
    """
    # Add to the database the languages found in config and skip existing ones.
    langset = {x[0] for x in Language.query.with_entities(Language.lang_code).all()}
    for l in app.config["LANGUAGES"]:
        if type(l) is dict:
            # Standardize primary key with langcodes before inserting into db.
            standard_code = langcodes.standardize_tag(l["lang_code"])
            lang = Language(
                lang_code=standard_code,
                lang_name=l["lang_name"],
                autonym=l["autonym"],
            )
        else:
            try:
                lang = Language.create_from_name(l)
            except Exception as e:
                log_error(f"Failed to create language: {str(e)}")
                click.echo(f"Failed to create language: {str(e)}")
        if lang.lang_code not in langset:
            log_info(f"Adding new language '{lang.lang_name} ({lang.lang_code})'")
            click.echo(f"Adding new language '{lang.lang_name} ({lang.lang_code})'")
            db.session.add(lang)
        else:
            log_info(
                f"Skipping language '{lang.lang_name} ({lang.lang_code})': Already in database"
            )
            click.echo(
                f"Language code '{lang.lang_code}' already exists in the database."
            )
    db.session.commit()


@language_cli.command()
@click.option("--langcode", prompt="IETF language tag")
@click.option("--langname", prompt="Natural name for the language in English")
@click.option("--autonym", prompt="Native name for the language")
@click.option("--flag_uri", prompt="URI for an image file", default="")
def create(langcode: str, langname: str, autonym: str, flag_uri: str) -> None:
    """
    Creates a new custom language and adds it to the database.

    :param langcode: IETF custom language tag.
    :param langname: Natural name for the language in English.
    :param autonym: Native name for the language.
    :param flag_uri: Optional URI for an image file, representing the country of the language.
    :return: None
    """

    # Standardize the primary key with langcodes before inserting into db.
    # Note: variant name must not exceed 8 characters in length.
    #       Example: "en-gibberis" is acceptable while "en-gibberish" is not.
    try:
        standard_code = langcodes.standardize_tag(langcode)
        valid = langcodes.tag_is_valid(standard_code)
        if not valid:
            click.echo(
                f"Invalid custom language code {standard_code}. Code must be a valid IETF language tag."
            )
            return
    except Exception as e:
        click.echo(f"Failed to create new language: {str(e)}")
        return

    exists = Language.query.filter(standard_code == Language.lang_code).first()
    if exists:
        click.echo(f"Language code '{standard_code}' already exists in the database.")
    else:
        click.echo(f"Creating new language '{langname} ({standard_code})'")
        lang = Language(
            lang_code=standard_code,
            lang_name=langname,
            autonym=autonym,
            flag_uri=flag_uri,
        )
        click.echo("New custom language created.")
        db.session.add(lang)
        db.session.commit()
        click.echo(
            f"Custom language '{langname} ({standard_code})' has been added to the database."
        )
