import itertools
from pathlib import Path
from typing import Callable

from saml2.client import Saml2Client
from saml2.config import Config as Saml2Config
from saml2.mdstore import MetadataStore, MetaDataLoader
from saml2.sigver import SignatureError

from timApp.tim_app import app
from timApp.util.error_handlers import report_error
from timApp.util.flask.requesthelper import RouteException
from timApp.util.logger import log_warning

# Add an external loader that allows checking for certs
_metadata_load = MetadataStore.load


def _metadatastore_load(self: MetadataStore, *args: list, **kwargs: dict) -> None:
    typ = args[0]

    if typ == "loadex":
        key = kwargs["loader"]
        md = MetaDataLoader(
            self.attrc,
            kwargs["loader"],
            cert=kwargs.get("cert"),
            security=self.security,
            filename="metadata.xml",
        )
        md.load()
        self.metadata[key] = md
    else:
        _metadata_load(self, *args, **kwargs)


MetadataStore.load = _metadatastore_load


def get_saml_config(
    service_name: str, metadata_loader: Callable[[], bytes]
) -> Saml2Config:
    """
    Get the SAML2 configuration for the client.

    :param metadata_loader: Custom loader that provides the XML metadata.
    :return: SAML2 configuration
    """

    def _do_get_saml_config(try_new_cert: bool, try_new_metadata: bool) -> Saml2Config:
        saml_path = Path(app.config["SAML_PATH"]) / service_name
        if try_new_cert:
            saml_path /= "new"

        config_file = saml_path / "config.py"
        saml2_config = Saml2Config()

        # We load the config file manually so that we can fill it with the extra info
        try:
            globals_dict = globals().copy()
            locals_dict: dict = {}
            exec(config_file.read_text(), globals_dict, locals_dict)
            saml2_config.load_file(str(config_file))
            config_dict = locals_dict["CONFIG"]
        except FileNotFoundError:
            raise FileNotFoundError(f"Could not find SAML config file.")
        except KeyError:
            raise KeyError(f"Could not find CONFIG dict in SAML config file.")

        metadata_cert_file_name = (
            "metadata_new.crt" if try_new_metadata else "metadata.crt"
        )
        metadata_cert_file = saml_path / "certs" / metadata_cert_file_name

        sp_key = saml_path / "certs" / "sp.key"
        sp_cert = saml_path / "certs" / "sp.crt"

        if sp_key.exists() and sp_cert.exists():
            sp_key_str = str(sp_key)
            sp_cert_str = str(sp_cert)
            config_dict["key_file"] = sp_key_str
            config_dict["cert_file"] = sp_cert_str
            # Encryption keypairs seem to be a different option, but e.g., in HAKA the same keys are used for encrypting
            # requests and decrypting responses
            config_dict["encryption_keypairs"] = [
                {
                    "key_file": sp_key_str,
                    "cert_file": sp_cert_str,
                }
            ]
        config_dict["metadata"] = {
            "loadex": [
                {
                    "loader": metadata_loader,
                    "cert": str(saml_path / "certs" / metadata_cert_file)
                    if app.config["SAML_VERIFY_METADATA"]
                    else None,
                }
            ]
        }
        config_dict["attribute_map_dir"] = str(
            saml_path.parent.parent / "attributemaps"
        )
        config_dict["allow_unknown_attributes"] = True
        saml2_config.load(config_dict)

        return saml2_config

    errors = []
    for new_cert, new_meta in itertools.product((False, True), (False, True)):
        try:
            return _do_get_saml_config(try_new_cert=new_cert, try_new_metadata=new_meta)
        except FileNotFoundError as e:
            err = f"SAML (new_cert={new_cert}, new_meta={new_meta}): Could not load SAML config: {e}"
            log_warning(err)
            errors.append(err)
        except SignatureError as e:
            err = f"SAML (new_cert={new_cert}, new_meta={new_meta}): Could not validate SAML metadata: {e}"
            log_warning(err)
            errors.append(err)

    report_error(
        "Failed to validate SAML metadata signature. SAML login (HAKA) is not available.\n\n"
        f"Errors:\n" + "\n".join(f"* {e}" for e in errors)
    )
    raise RouteException(
        "Failed to validate SAML metadata signature. Administrators have been notified."
    )


def get_saml_client(
    service_name: str, metadata_loader: Callable[[], bytes]
) -> Saml2Client:
    """
    Get the SAML2 client used to make SAML requests.

    :param metadata_loader: Custom loader that provides the XML metadata.
    :return: SAML2 client
    """
    saml2_client = Saml2Client(config=get_saml_config(service_name, metadata_loader))
    return saml2_client
