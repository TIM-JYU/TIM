from saml2 import BINDING_HTTP_POST
from saml2.saml import NAMEID_FORMAT_PERSISTENT

from timApp.util.flask.requesthelper import get_active_host_url

CONFIG = {
    "entityid": "https://timdevs02.it.jyu.fi/saml/mpass",
    "name": "Jyvaskylan yliopiston TIM (testipalvelin)",
    "description": "",
    "service": {
        "sp": {
            "endpoints": {
                "assertion_consumer_service": [
                    (f"{get_active_host_url()}saml/mpass/acs", BINDING_HTTP_POST),
                ],
            },
            # "required_attributes": [
            #     "cn",
            #     "displayName",
            #     "eduPersonAssurance",
            #     "eduPersonPrincipalName",
            #     "givenName",
            #     "mail",
            #     "preferredLanguage",
            #     "sn",
            # ],
            # "optional_attributes": [
            #     "schacPersonalUniqueCode",
            # ],
            "authn_requests_signed": True,
            "want_response_signed": False,
            "want_assertions_signed": False,
            "want_assertions_or_response_signed": True,
            "name_id_format": NAMEID_FORMAT_PERSISTENT,
        },
    },
}
