from saml2.saml import NAME_FORMAT_URI

MAP = {
    "identifier": NAME_FORMAT_URI,
    "fro": {
        "urn:oid:2.5.4.3": "cn",
        "urn:oid:2.16.840.1.113730.3.1.241": "displayName",
        "urn:oid:1.3.6.1.4.1.5923.1.1.1.11": "eduPersonAssurance",
        "urn:oid:1.3.6.1.4.1.5923.1.1.1.6": "eduPersonPrincipalName",
        "urn:oid:2.5.4.42": "givenName",
        "urn:oid:0.9.2342.19200300.100.1.3": "mail",
        "urn:oid:2.16.840.1.113730.3.1.39": "preferredLanguage",
        "urn:oid:2.5.4.4": "sn",
        "urn:oid:1.3.6.1.4.1.25178.1.2.14": "schacPersonalUniqueCode",
    },
}
