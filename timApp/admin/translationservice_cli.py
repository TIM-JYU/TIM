"""
Enables adding TranslationServices to TIM's database from the command line.
"""

__authors__ = [
    "Noora Jokela",
    "Riku Lehkonen",
    "Vili Moisala",
    "Juho Tarkkanen",
    "Sami Viitanen",
]
__license__ = "MIT"
__date__ = "5.5.2022"

import click
from flask.cli import AppGroup
from sqlalchemy import select

from timApp.document.translation.translator import TranslationService
from timApp.tim_app import app
from timApp.timdb.sqa import db, run_sql

tr_service_cli = AppGroup("trservice")


@tr_service_cli.command()
def add_all_new() -> None:
    """
    Try adding all the translation services defined in configuration into
    database from command line.

    This can be used to add new TranslationService implementations into
    database without initializing the database.

    :return: None.
    """
    add_all_tr_services_to_session(True)
    db.session.commit()


def add_all_tr_services_to_session(log: bool = False) -> None:
    """
    Add all supported translation services to be committed to database.
    Note: session.commit must be called afterwards to save the changes!

    Supported translation services must be implemented and are also listed in
    the configuration variable MACHINE_TRANSLATORS in timApp.defaultconfig.py.

    This separate function allows adding all the translation services also at
    the db-initialization on first TIM-startup.

    :return: None.
    """
    existing_services = {
        x for x in run_sql(select(TranslationService.service_name)).scalars()
    }
    for translator, init_data in app.config["MACHINE_TRANSLATORS"]:
        service_name = translator.__mapper_args__["polymorphic_identity"]
        if service_name in existing_services:
            if log:
                click.echo(
                    f"Skipping adding translation service '{service_name}': Already in database."
                )
        else:
            # Call the TranslationService's constructor with the specified
            # values if any.
            # TODO Implement some better method of constructing
            #  TranslationServices without dynamic typing like thought here.
            service = translator() if init_data is None else translator(init_data)
            if log:
                click.echo(f"Adding new translation service '{service_name}'")
            db.session.add(service)
