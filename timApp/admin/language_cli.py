import langcodes
import click
from flask.cli import AppGroup
from timApp.timdb.sqa import db
from timApp.document.translation.language import Language

language_cli = AppGroup("language")


@language_cli.command()
@click.argument("lang_code")
def remove(lang_code: str) -> None:
    """Remove a specific language from the database by specifying its language code."""

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
    """Add a standard language to the language database based on the language name."""

    try:
        lang = Language.create_from_name(lang_name)
    except Exception as e:
        click.echo(f"Failed to create language; try some other value: {str(e)}")

    exists = Language.query.filter(lang.lang_code == Language.lang_code).first()
    if exists:
        click.echo(f"Language code '{lang.lang_code}' already exists in the database.")
    else:
        click.echo(f"Adding new language '{lang.lang_name} ({lang.lang_code})'")
        db.session.add(lang)
        db.session.commit()


@language_cli.command()
@click.option("--langcode", prompt="IANA language code")
@click.option(
    "--langname", prompt="Natural name for the language in English", default=""
)
@click.option("--autonym", prompt="Native name for the language", default="")
@click.option("--flag_uri", prompt="URI for an image file", default="")
def create(langcode: str, langname: str, autonym: str, flag_uri: str) -> None:
    """Creates a new custom language and adds it to the database."""

    # Standardize the primary key with langcodes before inserting into db.
    # Note: variant name must not exceed 8 characters in length.
    #       Example: "en-gibberis" is acceptable while "en-gibberish" is not.
    standard_code = langcodes.standardize_tag(langcode)
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
