from saml2.saml import NAME_FORMAT_URI

MAP = {
    "identifier": NAME_FORMAT_URI,
    "fro": {
        "urn:oid:2.5.4.4": "family_name",
        "urn:oid:2.5.4.42": "given_name",
        "urn:mpass.id:uid": "uid",
        "urn:mpass.id:schoolCode": "schoolCode",
        "urn:mpass.id:school": "school",
        "urn:mpass.id:schoolInfo": "schoolInfo",
        "urn:mpass.id:class": "class",
        "urn:mpass.id:classLevel": "classLevel",
        "urn:mpass.id:learningMaterialsCharge": "learningMaterialsCharge",
        "urn:mpass.id:role": "role",
        "urn:oid:1.3.6.1.4.1.16161.1.1.27": "studentNumber",
        "urn:mpass.id:educationProviderId": "educationProviderId",
        "urn:mpass.id:educationProvider": "educationProvider",
        "urn:mpass.id:educationProviderInfo": "educationProviderInfo",
    },
}
