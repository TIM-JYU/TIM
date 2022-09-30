import functools
from dataclasses import field
from datetime import timedelta
from pathlib import Path

import filelock
import gnupg
import pytz
from flask import render_template_string, Response, url_for
from isodate import datetime_isoformat
from marshmallow import EXCLUDE

from timApp.tim_app import app
from timApp.util.flask.cache import cache
from timApp.util.flask.responsehelper import text_response
from timApp.util.flask.typedblueprint import TypedBlueprint
from timApp.util.logger import log_info
from timApp.util.utils import get_current_time
from tim_common.marshmallow_dataclass import dataclass, class_schema

securitytxt = TypedBlueprint("securitytxt", __name__, url_prefix="")


@functools.cache
def get_gpg() -> gnupg.GPG:
    gpg_home = Path("/certs/gpg")
    if not gpg_home.exists():
        gpg_home.mkdir(parents=True, exist_ok=True)

    # We create a special keyring with the only key being used for signing
    gpg = gnupg.GPG(
        gnupghome=str(gpg_home),
        keyring="tim_securitytxt.gpg",
        secret_keyring="tim_securitytxt.secret.gpg",
    )
    gpg.encoding = "utf-8"

    with filelock.FileLock("/tmp/tim_securitytxt_gpg.lock"):
        keys = gpg.list_keys()
        if not keys:
            log_info("No GPG key for signing security.txt found, generating new ones")
            gpg_gen_input = gpg.gen_key_input(
                key_type="RSA",
                no_protection=True,
                key_length=4096,
                name_real="TIM Sec Support",
                name_email=app.config["HELP_EMAIL"],
            )
            key = gpg.gen_key(gpg_gen_input)
            if key.status != "ok":
                raise RuntimeError(f"GPG key generation failed: {key.stderr}")

    return gpg


@dataclass
class SecurityInfo:
    extra_contacts: list[str] = field(default_factory=list)
    acknowledgements_url: str | None = None
    preferred_languages: list[str] = field(default_factory=list)
    extra_canonical_hosts: list[str] = field(default_factory=list)
    security_policy_url: str | None = None


_security_info = class_schema(SecurityInfo)().load(
    app.config["SECURITY_INFO"],
    unknown=EXCLUDE,
)

_security_txt_template = """
{%- for contact in contacts %}
Contact: {{ contact }}
{% endfor -%}
Expires: {{ expires }}
{% if encryption_key_url %}
Encryption: {{ encryption_key_url }}
{% endif -%}
{% if acknowledgements_url %}
Acknowledgements: {{ acknowledgements_url }}
{% endif -%}
{% if preferred_languages %}
Preferred-Languages: {{ preferred_languages|join(",") }}
{% endif -%}
{% for canonical_host in canonical_hosts %}
Canonical: {{ canonical_host }}/security.txt
Canonical: {{ canonical_host }}/.well-known/security.txt
{% endfor %}
{% if security_policy_url %}
Policy: {{ security_policy_url }}
{% endif -%}
"""


@cache.memoize(timeout=3600 * 24 * 30, source_check=True)
def _get_security_txt(info: SecurityInfo) -> str:
    contacts = [f"mailto:{app.config['HELP_EMAIL']}", *info.extra_contacts]
    expires = (get_current_time() + timedelta(days=31)).astimezone(pytz.utc)
    expires = expires.replace(hour=0, minute=0, second=0, microsecond=0)
    canonical_hosts = [app.config["TIM_HOST"], *info.extra_canonical_hosts]

    security_text = render_template_string(
        _security_txt_template,
        contacts=contacts,
        expires=datetime_isoformat(expires),
        encryption_key_url=url_for("securitytxt.pgp_key", _external=True),
        acknowledgements_url=info.acknowledgements_url,
        preferred_languages=info.preferred_languages,
        canonical_hosts=canonical_hosts,
        security_policy_url=info.security_policy_url,
    )

    key = get_gpg().list_keys()[0]
    security_text_signed = get_gpg().sign(security_text, keyid=key["keyid"])
    return security_text_signed.data.decode(encoding="utf-8")


@functools.cache
def _get_pgp_public_key() -> str:
    gpg = get_gpg()
    key = gpg.list_keys()[0]
    return gpg.export_keys(key["keyid"], expect_passphrase=False)


@securitytxt.get("/security.txt")
@securitytxt.get("/.well-known/security.txt")
def security_txt() -> Response:
    return text_response(_get_security_txt(_security_info))


@securitytxt.get("/pgp-key.txt")
def pgp_key() -> Response:
    return text_response(_get_pgp_public_key())
