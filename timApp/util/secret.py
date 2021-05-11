from flask import current_app

from timApp.util.flask.requesthelper import RouteException


def check_secret(secret: str, config_key: str) -> None:
    expected_secret = get_secret_or_abort(config_key)
    if secret != expected_secret:
        raise RouteException('Wrong secret')


def get_secret_or_abort(config_key: str) -> str:
    secret = current_app.config[config_key]
    if secret is None:
        raise RouteException(f'{config_key} not configured.')
    return secret
