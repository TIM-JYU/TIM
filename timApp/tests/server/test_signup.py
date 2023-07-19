import base64
from dataclasses import dataclass
from unittest import mock

from sqlalchemy import select, delete

from timApp.auth.login import (
    test_pws,
    create_or_update_user,
    set_single_user_to_session,
)
from timApp.messaging.messagelist.listinfo import Channel
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.tim_app import get_home_organization_group, app
from timApp.timdb.sqa import db
from timApp.user.newuser import NewUser
from timApp.user.personaluniquecode import SchacPersonalUniqueCode
from timApp.user.user import User, UserOrigin, UserInfo
from timApp.user.usercontact import ContactOrigin
from timApp.user.usergroup import UserGroup
from timApp.user.userutils import create_password_hash

test_pw = "somepwd123"

samltestresp = """
<?xml version="1.0" encoding="UTF-8"?>
<saml2p:Response Destination="https://timdevs02-5.it.jyu.fi/saml/acs" ID="_30b59448c21e50af2c8036ff2a4231b4" InResponseTo="ONELOGIN_90a94dc7e17071d644293b671f5ff14d3740355d" IssueInstant="2019-11-18T10:27:23.230Z" Version="2.0" xmlns:saml2p="urn:oasis:names:tc:SAML:2.0:protocol"><saml2:Issuer xmlns:saml2="urn:oasis:names:tc:SAML:2.0:assertion">https://testidp.funet.fi/idp/shibboleth</saml2:Issuer><ds:Signature xmlns:ds="http://www.w3.org/2000/09/xmldsig#"><ds:SignedInfo><ds:CanonicalizationMethod Algorithm="http://www.w3.org/2001/10/xml-exc-c14n#"/><ds:SignatureMethod Algorithm="http://www.w3.org/2001/04/xmldsig-more#rsa-sha256"/><ds:Reference URI="#_30b59448c21e50af2c8036ff2a4231b4"><ds:Transforms><ds:Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature"/><ds:Transform Algorithm="http://www.w3.org/2001/10/xml-exc-c14n#"/></ds:Transforms><ds:DigestMethod Algorithm="http://www.w3.org/2001/04/xmlenc#sha256"/><ds:DigestValue>ThUprr9LNOd9fdihBLzLZl7f9JsSTCn8Y8/G+W47LXA=</ds:DigestValue></ds:Reference></ds:SignedInfo><ds:SignatureValue>imoO3Ci8p3vsKs2+20iflGKehSTq08nKIDvR59DsDmTXMGJa2b/OSiFroe3J1U+df3pOxA8OSexPSfvhxZASZQMMjHd6UErKAdr8qV3VRzbt3NcWHSYvxrHbZ0/tGnICSXcH6zGv6sGelyRUIlLp8sZV3ev/U3xTx3+Z2kWNBK0khdhIgWrmMHPVXVmABsZMLD8njkq/V9gE3yc5q+mJ17316Uw2E2XX1pRnve/3r4SAG6R/DlTyswibOMUTENB58/X+OBSUz2+iHev00C5kPSmISgw65Vfv/MaYIQnI6nGgKrhG/UrRougPYpBdFv8XCN4Ml96AzJbXDVXVXVlFyvEMzmAYWMr7yN2s0omkI/cSzFSpTdsn+j72XkUEVopOiAcoxAfsEU2UMMtRFoZdxPYd0zMpnIl1mgajeJps2YOKTfNWPStVY2UVYOyBZ1zOuCdJrVHKKXpKy7jRv4rgLpyThIhQlFPtJRpKGwn0PblzjQiTNAUcD1ngLe8ztNNBp9J6qyBgOSjkZcA28Gur/BzpwjnCfOqnHhrbv+RujAYi73zxzNEwLZT3VRT35ec3vSqbLHzgmxQDM+g550B7vrs8iZsxkr9XO5zmfgv5IlBfWzqWcr6lv6qRRqCR2JygUfyT2q6xW7FI86jGor+8t2FttCRdGXkc5+NOiaBAOfA=</ds:SignatureValue><ds:KeyInfo><ds:X509Data><ds:X509Certificate>MIIFSjCCAzICCQC8J53bDLsjJzANBgkqhkiG9w0BAQsFADBmMQswCQYDVQQGEwJGSTEQMA4GA1UE
CAwHVVVTSU1BQTEOMAwGA1UEBwwFRXNwb28xDDAKBgNVBAsMA0lBTTEMMAoGA1UECgwDQ1NDMRkw
FwYDVQQDDBB0ZXN0aWRwLmZ1bmV0LmZpMCAXDTE3MDQxMTExNTQ0OVoYDzIxMTcwMzE4MTE1NDQ5
WjBmMQswCQYDVQQGEwJGSTEQMA4GA1UECAwHVVVTSU1BQTEOMAwGA1UEBwwFRXNwb28xDDAKBgNV
BAsMA0lBTTEMMAoGA1UECgwDQ1NDMRkwFwYDVQQDDBB0ZXN0aWRwLmZ1bmV0LmZpMIICIjANBgkq
hkiG9w0BAQEFAAOCAg8AMIICCgKCAgEAtGhIKCmcbHqBsJyiHxkS/lxyH10uLlNE3IZEIlLteNpC
+ibV2aEf2VRZkF2DV6GKpOAjP8NfJfSIsja516bcPLPo84RLWCcArOOZKrnHoM60wOcBkBMuggCu
T+GFtx2065ESdSn0qMWlpzZLBzpGwD1EfigzQ/7fF0oyYg8pfc4LOBG55UVmYlbmF2eGYEEaSU1c
Q5KOV0cApm8FXtopHYlmxlberuJfK1Ve5upkJ1SE+D/4A2lJT2+ruiPtL5dGN9zd/SrrVSD8hCMk
KwIuZthgqycXzvO92ccZcWKycWiON5Y47XEyWnozofyAr91qp0I1kXQiFJtAbIQz97vYte6nb0VW
U2iwOHRPJnMcrRbRFH4W7NuaiI0Zr9dhDnt/v/JlfQrcRhjdM7WDvt5qEH7+0d8XJC6ua/AUklns
aaJTUaFkAUerrZCnwMCws/uP5zQCSq+tTUrSVDFwDBqs6+6KB6LxHNjq5mFLw+QtOfhBLD6XAbaH
FWcmxfcHepQb8pGi+WJTizGQ6UX/glQe15CqhIG3vPVrDXvuxNYwxQsWaVuFZl/JfPLbpjQudI7/
BmnD3aVECj7hk2VQCfAUlSm3ALiolkFiRArzKra3AF7GAcpcnIcwagaTQr4JT9i7icpIShiCkwuY
GBVEcCRNe9NBQ8PVX4EHh6HjqIGkIWkCAwEAATANBgkqhkiG9w0BAQsFAAOCAgEACyl5UqLdHYjl
c9JigjuLzCqnPqgc8tSHp4DWGYonw30usAy2Wd6k8sHaMKiV9C+Z9UWP8IOAvNlTG1MTkSoRu954
rcEmTiYljUVpdecno94WteS8c9gfijUehnNE7yvfTImFyHwtWTsmVWX8aNX+JpKU2tX+MBqq38eY
GQUZE5uTO/ChgEeC30vTapiGX9wA48RboMv06BNW2UKu3qdeEK67VGTDTqzSKtPuiXLuU5BTuNW0
gR9wrv64rb5HcaVi0xQ2lqXWhllQbI2ltuEMmVbwXFBeQNaJJrLSYf5qinRuVqhNvp833MNW9rln
zV+rdru0Uxrz4j6yDdHtUhvUOe3ddjLQV47FLiHPGHBXIoE0hCTtXhAdlYlK+4Ec8Y31GwMXakod
Fy9s1qWogiIt55JAOdFKvUfKEsAdIRn2Nd4I933gNmrAa5/95hEQFfhTp9v6NbIYhAKaIbZnW6EV
CUGKJt6KQAf5Vzwb3/jNVg5ywyE/EScVEHS6Z/Ba+cdKymJSJDpOlrlXbPW36e5irpZYTA8UOuPd
GaOi6fti7chnnqLPMQKuYQ5bDBFs3RncTK03qDt5s/RB2sXs6tcxxGtXaCb8kb/XS6zQLPPJ93fz
d2MKDvRZAXhZJOsvMksSIWK3KmQoANTbLHv1Dl1QIlSGo1we6cPpzdt4j9jPZo0=</ds:X509Certificate></ds:X509Data></ds:KeyInfo></ds:Signature><saml2p:Status xmlns:saml2p="urn:oasis:names:tc:SAML:2.0:protocol"><saml2p:StatusCode Value="urn:oasis:names:tc:SAML:2.0:status:Success"/></saml2p:Status><saml2:EncryptedAssertion xmlns:saml2="urn:oasis:names:tc:SAML:2.0:assertion"><xenc:EncryptedData Id="_9f9ca9fb5c93e09ec645e15b0dab48fd" Type="http://www.w3.org/2001/04/xmlenc#Element" xmlns:xenc="http://www.w3.org/2001/04/xmlenc#"><xenc:EncryptionMethod Algorithm="http://www.w3.org/2001/04/xmlenc#aes128-cbc" xmlns:xenc="http://www.w3.org/2001/04/xmlenc#"/><ds:KeyInfo xmlns:ds="http://www.w3.org/2000/09/xmldsig#"><xenc:EncryptedKey Id="_11f1444188ac3046d3628db1ed601af2" Recipient="https://timdevs02-5.it.jyu.fi/saml" xmlns:xenc="http://www.w3.org/2001/04/xmlenc#"><xenc:EncryptionMethod Algorithm="http://www.w3.org/2001/04/xmlenc#rsa-oaep-mgf1p" xmlns:xenc="http://www.w3.org/2001/04/xmlenc#"><ds:DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1" xmlns:ds="http://www.w3.org/2000/09/xmldsig#"/></xenc:EncryptionMethod><ds:KeyInfo><ds:X509Data><ds:X509Certificate>MIIDITCCAgmgAwIBAgIUAWhFFBc+nXt5iMh0TJEcuzfbEYEwDQYJKoZIhvcNAQELBQAwIDEeMBwG
A1UEAwwVdGltZGV2czAyLTUuaXQuanl1LmZpMB4XDTE5MTAxNjEwMjA1MFoXDTIwMTAxNTEwMjA1
MFowIDEeMBwGA1UEAwwVdGltZGV2czAyLTUuaXQuanl1LmZpMIIBIjANBgkqhkiG9w0BAQEFAAOC
AQ8AMIIBCgKCAQEA6To23Uz1UcTkK2SxK0725kyx8cslbV/XAsAD/J1leRUYOJdg9gwm07KJT5J1
ox+/6jp1DRIzfGxFfRlnUsPTfK/pIiHVgQ3sfOzf1BfBFvz76VZxuW6r2ZqIcZ+8+yX5Sn8kHOZQ
F+safEdPtJ8GtWNw5p+cZBQVePpnFvwTydJ7dQladhDm9dTKW28jooflWYjsMgVMANHQbAMcAXBc
Av6hcBH43kvyRCXWgCk7dRoH6b7EthZyX6E6Td3tR7bcSExQCOTzz1TbcvRz/3qJ8vzDXwY9CcLP
+Y6bpoygAzibHwxKJFq3D/CgqcCBp7Pm6SiH+16gp/E+UjrAvcTAXQIDAQABo1MwUTAdBgNVHQ4E
FgQUEB1Dt5aPrKtRYkOwUzqJQwVSs6swHwYDVR0jBBgwFoAUEB1Dt5aPrKtRYkOwUzqJQwVSs6sw
DwYDVR0TAQH/BAUwAwEB/zANBgkqhkiG9w0BAQsFAAOCAQEAkF0JWoxQcnKMTzMRJ3e3Lunu27Gb
XHrigx2QmQHuRQv9m1ZMo7OBLS0ulIO3y6JqfOuQWY0XBRpi46J9qwL9SmNt/BzTijlLg4hr7wY+
ZeIzhaxaupWZGugfSHEDUqKO/WUMQE33Kic4yx7T07dclqv3rT1mf+vQDLXC4FHNy3Y9yKWdt2qd
mNsI40Eg4jYAehQfCy2xyVKkO+9RgtXus7g7rQcyIJu9lxVjUxMFmTcwiV2NjOYUdiw6xW1oqUZb
tCX2sSld13xoUUAcjI2XSIIjJv42GYpaxXYpJFHcr88gLMSEeszdkaHyW2hWZ13S/c307qE/6mXC
MF+BrXA+3Q==</ds:X509Certificate></ds:X509Data></ds:KeyInfo><xenc:CipherData xmlns:xenc="http://www.w3.org/2001/04/xmlenc#"><xenc:CipherValue>285BF4pbGbGob+65I1Q6k8twCYkRiJ93CDUx7q+f+zAwZEL2rxvToiwUby4O70wxtKlxw9grezyBB+S6rrzdEv4AGKdzMfMLMGC2pFWS2D12DwktAxq7UrpbDLXNjX0w47kNhylZJFLVJHsUcwQpaKUkJp2+xaLHaG1/fbkUWfW7bv2Q7Rzh/OpuKV2LBT4L8yKdb5uYilqQXiUasGwgpbCPPV+94ZLze2LmotbGrdATrurO3ysUWkuSnKCZTejrkpNjRSx1nUq+XZgspVgCUBBGc/ubs3FPG0XJfgwS/nCdc82nNnnP3+llsGI9sJNat6sXZuUArBsoSimzuYvI1g==</xenc:CipherValue></xenc:CipherData></xenc:EncryptedKey></ds:KeyInfo><xenc:CipherData xmlns:xenc="http://www.w3.org/2001/04/xmlenc#"><xenc:CipherValue>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</xenc:CipherValue></xenc:CipherData></xenc:EncryptedData></saml2:EncryptedAssertion></saml2p:Response>
""".strip()


LOW_ASSURANCE = "https://refeds.org/assurance/IAP/low"
MEDIUM_ASSURANCE = "https://refeds.org/assurance/IAP/medium"
HIGH_ASSURANCE = "https://refeds.org/assurance/IAP/high"
LOCAL_ENTERPRISE_ASSURANCE = "https://refeds.org/assurance/IAP/local-enterprise"


@dataclass
class SamlSSOResponseMock:
    info: UserInfo
    assurance_levels: list[str]
    mock_missing_uniquecode: bool = False

    def get_identity(self) -> dict:
        res = {
            "mail": [self.info.email],
            "eduPersonPrincipalName": [self.info.username],
            "eduPersonAssurance": self.assurance_levels,
            "displayName": [self.info.full_name],
            "preferredLanguage": ["fi"],
            "cn": [self.info.full_name],
            "sn": [self.info.last_name],
            "givenName": [self.info.given_name],
        }
        if not self.mock_missing_uniquecode:
            res["schacPersonalUniqueCode"] = [
                uq.to_urn() for uq in self.info.unique_codes
            ]
        return res


acs_url = "/saml/acs"


class TestSignUp(TimRouteTest):
    def setUp(self):
        super().setUp()
        self.logout()

    def test_block_bot_signup(self):
        bot_email = "bot@example.com"
        self.json_post(
            "/emailSignup",
            {
                "email": bot_email,
                "url": "http://www.example.com",
            },
        )
        self.get("/")  # refresh session
        self.assertIsNone(
            db.session.execute(select(NewUser).filter_by(email=bot_email).limit(1))
            .scalars()
            .first()
        )

        for allowed_email in ("test@jyu.fi", "test@gmail.com"):
            self.json_post(
                "/emailSignup",
                {
                    "email": allowed_email,
                    "url": "http://www.example.com",
                },
            )
            self.get("/")  # refresh session
            self.assertIsNotNone(
                db.session.execute(
                    select(NewUser).filter_by(email=allowed_email).limit(1)
                )
                .scalars()
                .first()
            )
        db.session.execute(delete(NewUser))
        db.session.commit()

    def test_signup_case_insensitive(self):
        email = "SomeOneCase@example.com"
        self.json_post("/emailSignup", {"email": email})
        self.assertEqual(
            db.session.execute(select(NewUser.email)).scalars().all(),
            ["someonecase@example.com"],
        )
        self.json_post(
            "/checkTempPass",
            {"email": email, "token": test_pws[-1]},
            expect_content={"status": "ok"},
        )
        self.json_post(
            "/emailSignupFinish",
            {
                "realname": "Testing Signup",
                "email": email,
                "token": test_pws[-1],
                "password": test_pw,
                "passconfirm": test_pw,
            },
            expect_contains="registered",
            json_key="status",
        )
        self.login(force=True, email=email, passw=test_pw)

    def test_signup_whitespace(self):
        email = "whitespace@example.com "
        self.json_post("/emailSignup", {"email": email})
        self.assertEqual(
            db.session.execute(select(NewUser.email)).scalars().all(),
            ["whitespace@example.com"],
        )
        self.json_post(
            "/checkTempPass",
            {"email": email, "token": test_pws[-1]},
            expect_content={"status": "ok"},
        )
        self.json_post(
            "/emailSignupFinish",
            {
                "realname": "Testing Signup",
                "email": email,
                "token": test_pws[-1],
                "password": test_pw,
                "passconfirm": test_pw,
            },
            expect_contains="registered",
            json_key="status",
        )
        self.login(force=True, email=email, passw=test_pw)
        self.login(force=True, email=email.strip(), passw=test_pw)

    def test_login_case_insensitive(self):
        email = "SomeOneCase2@example.com"
        User.create_with_group(
            UserInfo(
                username=email, email=email, full_name="Some One", password="testing"
            )
        )
        db.session.commit()
        self.login(email=email.lower(), passw="testing", force=True)
        email = "SomeOnecase2@example.com"
        User.create_with_group(
            UserInfo(
                username=email, email=email, full_name="Some One", password="testing"
            )
        )
        db.session.commit()
        self.login(
            email=email.lower(),
            passw="testing",
            force=True,
            expect_status=400,
            expect_content="AmbiguousAccount",
        )

    def test_signup(self):
        email = "testingsignup@example.com"
        self.json_post("/emailSignup", {"email": email})
        self.assertEqual(db.session.execute(select(NewUser.email)).scalars().all(), [email])
        self.json_post("/emailSignup", {"email": email})
        self.assertEqual(db.session.execute(select(NewUser.email)).scalars().all(), [email])
        self.json_post(
            "/emailSignupFinish",
            {
                "realname": "Testing Signup",
                "email": email,
                "token": test_pws[-1],
                "password": test_pw,
                "passconfirm": test_pw,
            },
            expect_contains="registered",
            json_key="status",
        )
        self.assertEqual(db.session.execute(select(NewUser.email)).scalars().all(), [])
        self.assertEqual("Testing Signup", self.current_user.real_name)
        self.assertEqual(UserOrigin.Email, self.current_user.origin)
        self.assertEqual(email, self.current_user.email)
        self.assertEqual(email, self.current_user.primary_email_contact.contact)

        # TODO needs a better error message
        self.json_post(
            "/emailSignupFinish",
            {
                "realname": "Testing Signup",
                "token": test_pws[-1],
                "email": email,
                "password": test_pw,
                "passconfirm": test_pw,
            },
            expect_content="WrongTempPassword",
            expect_status=400,
        )

        old_pw_hash = self.current_user.pass_
        self.json_post("/emailSignup", {"email": email})
        self.json_post(
            "/emailSignupFinish",
            {
                "realname": "Testing Signup2",
                "email": email,
                "token": test_pws[-1],
                "password": "changedpass",
                "passconfirm": "changedpass",
            },
            expect_contains="updated",
            json_key="status",
        )
        # Name change not allowed.
        self.assertEqual("Testing Signup", self.current_user.real_name)
        self.assertNotEqual(old_pw_hash, self.current_user.pass_)

    def test_password_mismatch(self):
        email = "testingsignup@example.com"
        self.json_post("/emailSignup", {"email": email})
        self.json_post(
            "/emailSignupFinish",
            {
                "realname": "Testing Signup",
                "email": email,
                "token": test_pws[-1],
                "password": test_pw,
                "passconfirm": "somepwd1232",
            },
            expect_content="PasswordsNotMatch",
            expect_status=400,
        )
        self.assertFalse(self.is_logged_in)

    def test_too_short_password(self):
        email = "testingsignup@example.com"
        self.json_post("/emailSignup", {"email": email})
        self.json_post(
            "/emailSignupFinish",
            {
                "realname": "Testing Signup",
                "email": email,
                "token": test_pws[-1],
                "password": "test",
                "passconfirm": "test",
            },
            expect_content="PasswordTooShort",
            expect_status=400,
        )
        self.assertFalse(self.is_logged_in)

    def test_temp_password_wrong(self):
        email = "testingsignup@example.com"
        self.json_post("/emailSignup", {"email": email})
        self.json_post(
            "/emailSignupFinish",
            {
                "realname": "Testing Signup",
                "email": email,
                "token": "asdasd",
                "password": test_pw,
                "passconfirm": test_pw,
            },
            expect_content="WrongTempPassword",
            expect_status=400,
        )
        self.assertFalse(self.is_logged_in)

    def test_invalid_email(self):
        old_len = len(test_pws)
        self.json_post("/emailSignup", {"email": "invalid"})
        self.assertFalse(self.is_logged_in)
        self.assertEqual(old_len, len(test_pws))

    def test_korppi_signup(self):
        """Korppi signup succeeds."""
        self.create_or_update_test_user(
            "johmadoenew",
            "Doe John Matt",
            "john.m.doenew@student.jyu.fi",
        )
        self.assertEqual("Doe John Matt", self.current_user.real_name)
        self.assertEqual("johmadoenew", self.current_user.name)
        self.assertEqual("john.m.doenew@student.jyu.fi", self.current_user.email)
        self.assertEqual(
            {g.name for g in self.current_user.groups},
            {"johmadoenew", get_home_organization_group().name},
        )

    def create_or_update_test_user(
        self,
        username="johmadoe",
        real_name="Doe John Matt",
        email="john.m.doe@student.jyu.fi",
    ):
        u = create_or_update_user(
            UserInfo(
                email=email,
                full_name=real_name,
                username=username,
                origin=UserOrigin.Korppi,
            ),
            group_to_add=get_home_organization_group(),
        )
        db.session.commit()
        set_single_user_to_session(u)

    def test_korppi_info_change(self):
        """TIM can handle cases where some information about the user changes in Korppi."""
        self.create_or_update_test_user()
        curr_id = self.current_user.id
        curr_name = self.current_user.name
        curr_email = self.current_user.email

        # real name changes
        self.create_or_update_test_user(
            real_name="Doe John Matthew",
            username="johmadoe",
            email="john.m.doe@student.jyu.fi",
        )

        self.assertEqual(self.current_user.id, curr_id)
        self.assertEqual(self.current_user.name, curr_name)
        self.assertEqual(self.current_user.email, curr_email)
        self.assertEqual(self.current_user.real_name, "Doe John Matthew")
        self.assertEqual(UserOrigin.Korppi, self.current_user.origin)

        # email changes
        self.create_or_update_test_user(
            real_name="Doe John Matthew",
            username="johmadoe",
            email="john.doe@student.jyu.fi",
        )

        self.assertEqual(self.current_user.id, curr_id)
        self.assertEqual(self.current_user.name, curr_name)
        self.assertEqual(self.current_user.email, "john.doe@student.jyu.fi")
        self.assertEqual(self.current_user.real_name, "Doe John Matthew")

        # username changes
        self.create_or_update_test_user(
            real_name="Doe John Matthew",
            username="johmadoz",
            email="john.doe@student.jyu.fi",
        )

        self.assertEqual(self.current_user.id, curr_id)
        self.assertEqual(self.current_user.name, "johmadoz")
        self.assertEqual(self.current_user.email, "john.doe@student.jyu.fi")
        self.assertEqual(self.current_user.real_name, "Doe John Matthew")
        self.assertEqual(
            {g.name for g in self.current_user.groups},
            {"johmadoz", get_home_organization_group().name},
        )

        # If both username and email is different, there's no way to identify the user.
        self.create_or_update_test_user(
            real_name="Doe John Matthew",
            username="johmadox",
            email="john.doex@student.jyu.fi",
        )
        self.assertNotEqual(self.current_user.id, curr_id)

    def test_korppi_email_signup(self):
        """A Korppi user can update their password (and real name) by signing up."""
        self.create_or_update_test_user()
        curr_id = self.current_user.id
        curr_name = self.current_user.name
        curr_real_name = self.current_user.real_name
        curr_email = self.current_user.email
        self.json_post("/emailSignup", {"email": curr_email})
        pw = test_pw
        self.json_post(
            "/emailSignupFinish",
            {
                "realname": "Johnny John",
                "email": curr_email,
                "token": test_pws[-1],
                "password": pw,
                "passconfirm": pw,
            },
            expect_contains="updated",
            json_key="status",
        )
        self.assertEqual(self.current_user.id, curr_id)
        self.assertEqual(self.current_user.name, curr_name)
        self.assertEqual(self.current_user.email, curr_email)
        self.assertEqual(
            self.current_user.real_name, "Doe John Matt"
        )  # changing name not allowed for organization users
        self.assertTrue(self.current_user.check_password(pw))

        self.logout()
        self.assertIsNone(self.current_user)
        self.login(email=curr_email, passw=pw, force=True)
        self.assertEqual(self.current_user.id, curr_id)

        self.create_or_update_test_user()
        self.assertEqual(self.current_user.id, curr_id)
        self.assertEqual(self.current_user.name, curr_name)
        self.assertEqual(self.current_user.email, curr_email)
        self.assertEqual(self.current_user.real_name, curr_real_name)
        self.assertTrue(self.current_user.check_password(pw))

    def test_email_user_to_korppi(self):
        """When an email user logs in with Korppi, no new account is created but the current account information is updated."""
        self.login_test3()
        curr_id = self.current_user.id
        curr_pw = self.current_user.pass_
        self.assertFalse(get_home_organization_group() in self.current_user.groups)
        self.create_or_update_test_user(
            "t3", "Mr Test User 3", email=self.current_user.email
        )
        self.assertEqual(self.current_user.id, curr_id)
        self.assertEqual(self.current_user.name, "t3")
        self.assertEqual(self.current_user.real_name, "Mr Test User 3")
        self.assertEqual(self.current_user.pass_, curr_pw)
        self.assertTrue(get_home_organization_group() in self.current_user.groups)

    def test_email_login_without_pass(self):
        self.create_or_update_test_user("someone", "Some One", "someone@example.com")
        u = User.get_by_name("someone")
        u.pass_ = None
        db.session.commit()
        self.login(
            email="someone@example.com",
            passw="something",
            force=True,
            expect_status=403,
        )

    def test_email_login_with_korppi_username(self):
        self.create_or_update_test_user("someone2", "Some One", "someone2@example.com")
        u = User.get_by_name("someone2")
        u.pass_ = create_password_hash("somepass")
        db.session.commit()
        self.login(email="someone2", passw="somepass", force=True)

    def test_korppi_user_reset_pass_with_username(self):
        """A Korppi user can reset their password using their username."""
        self.create_or_update_test_user()
        curr_name = self.current_user.name
        self.json_post("/emailSignup", {"email": curr_name})
        pw = test_pw
        self.json_post(
            "/emailSignupFinish",
            {
                "realname": "Johnny John",
                "email": curr_name,
                "token": test_pws[-1],
                "password": pw,
                "passconfirm": pw,
            },
            expect_contains="updated",
            json_key="status",
        )

    def test_login_fail(self):
        self.login(
            email="a@example.com",
            passw="somepass",
            force=True,
            expect_status=403,
            expect_content="EmailOrPasswordNotMatch",
        )
        self.login(
            email="a@jyu.fi",
            passw="somepass",
            force=True,
            expect_status=403,
            expect_content="EmailOrPasswordNotMatchUseHaka",
        )

    def test_haka_invalid_settings(self):
        self.json_post(
            acs_url,
            {},
            expect_status=400,
            expect_content="No entityID in session.",
        )
        self.get(
            "/saml/sso",
            query_string={
                "entityID": "https://testidp.funet.fi/idp/shibboleth",
                "return_to": "/",
            },
            expect_status=302,
        )
        self.post(
            acs_url,
            data={},
            expect_status=400,
            expect_content="SAML Response is missing",
        )
        self.post(
            acs_url,
            data={
                "SAMLResponse": base64.encodebytes(b"x").decode(),
            },
            expect_status=400,
            expect_content="syntax error: line 1, column 0",
        )
        self.post(
            acs_url,
            data={
                "SAMLResponse": base64.encodebytes(samltestresp.encode()).decode(),
            },
            expect_status=400,
            expect_contains="Unsolicited response:",
        )

    def test_haka_login(self):
        teppo_email = "teppo@mailinator.com"
        puc = SchacPersonalUniqueCode.parse(
            "urn:schac:personalUniqueCode:int:studentID:jyu.fi:12345X"
        )

        def mock_acs_teppo():
            self.do_acs_mock(
                UserInfo(
                    email=teppo_email,
                    username="teppo@yliopisto.fi",
                    last_name="Testaaja",
                    given_name="Teppo",
                    full_name="Teppo Testaaja",
                    unique_codes=[puc],
                )
            )

        for i in range(0, 2):
            mock_acs_teppo()
            u = User.get_by_name("yliopisto.fi:teppo")
            self.assert_primary_contact(
                u, Channel.EMAIL, ContactOrigin.Haka, teppo_email
            )
            self.assertEqual("Teppo", u.given_name)
            self.assertEqual("Testaaja", u.last_name)
            self.assertEqual("Testaaja Teppo", u.real_name)
            uq = next(iter(u.uniquecodes.values()))
            self.assertEqual("12345X", uq.code)
            self.assertEqual("studentID", uq.type)
            self.assertEqual("jyu.fi", uq.organization.name)
            self.assertIn(UserGroup.get_organization_group("yliopisto.fi"), u.groups)
            self.assertIn(UserGroup.get_haka_group(), u.groups)

        # Test two additional cases for contacts
        # Case 1: Teppo has a custom email set as primary and Haka email as secondary
        #      => Nothing changes
        u = User.get_by_name("yliopisto.fi:teppo")
        teppo_second_email = "teppo2ndemail@example.com"
        u.email = teppo_second_email
        db.session.commit()
        mock_acs_teppo()
        u = User.get_by_name("yliopisto.fi:teppo")
        self.assert_primary_contact(
            u, Channel.EMAIL, ContactOrigin.Custom, teppo_second_email
        )
        self.assert_contacts(
            u,
            Channel.EMAIL,
            [
                (ContactOrigin.Custom, teppo_second_email),
                (ContactOrigin.Haka, teppo_email),
            ],
        )

        # Case 2: Teppo has only custom email and Haka email is custom
        #     =>  Haka email becomes managed, primary email does not change
        u.email = teppo_second_email
        db.session.commit()
        uc = next(uc for uc in u.contacts if uc.contact_origin == ContactOrigin.Haka)
        uc.contact_origin = ContactOrigin.Custom
        db.session.commit()

        mock_acs_teppo()
        u = User.get_by_name("yliopisto.fi:teppo")
        self.assert_primary_contact(
            u, Channel.EMAIL, ContactOrigin.Custom, teppo_second_email
        )
        self.assert_contacts(
            u,
            Channel.EMAIL,
            [
                (ContactOrigin.Custom, teppo_second_email),
                (ContactOrigin.Haka, teppo_email),
            ],
        )

        self.do_acs_mock(
            UserInfo(
                email=teppo_email,
                username="matti@jyu.fi",
                last_name="Meikäläinen",
                given_name="Matti",
                full_name="Matti Meikäläinen",
            )
        )

        u = User.get_by_name("matti")
        self.assertIsNotNone(u)
        self.assertIn(UserGroup.get_organization_group("jyu.fi"), u.groups)
        self.assertIn(UserGroup.get_haka_group(), u.groups)
        self.assertIsNone(User.get_by_name("jyu.fi:matti"))

    def test_student_id_login_match(self):
        self.do_acs_mock(
            UserInfo(
                username="xxxx@jyu.fi",
                full_name="X Test",
                last_name="X",
                given_name="Test",
                email="xxxx@example.com",
                origin=UserOrigin.Haka,
                unique_codes=[
                    SchacPersonalUniqueCode(
                        codetype="studentID", code="1234X", org="jyu.fi"
                    )
                ],
            )
        )
        # Make sure the user is identified by student id even when when username or email do not match.
        self.do_acs_mock(
            UserInfo(
                username="xxxx2@jyu.fi",
                full_name="X Test",
                last_name="X",
                given_name="Test",
                email="xxxx2@example.com",
                origin=UserOrigin.Haka,
                unique_codes=[
                    SchacPersonalUniqueCode(
                        codetype="studentID", code="1234X", org="jyu.fi"
                    )
                ],
            )
        )
        self.assertIsNone(User.get_by_name("xxxx"))
        u = User.get_by_name("xxxx2")
        self.assertIsNotNone(u)
        db.session.refresh(u)
        self.assertEqual("xxxx2@example.com", u.primary_email_contact.contact)

    def test_haka_login_email_conflict(self):
        self.create_or_update_test_user(
            username="somep@example.com",
            real_name="Someperson",
            email="somep@example.com",
        )
        self.create_or_update_test_user(
            username="sp", email="sp@jyu.fi", real_name="Person Söme"
        )
        self.do_acs_mock(
            UserInfo(
                username="sp@jyu.fi",
                email="somep@example.com",
                origin=UserOrigin.Haka,
                full_name="Person Söme",
                last_name="Person",
                given_name="Söme",
            )
        )

    def test_missing_uniquecode(self):
        self.do_acs_mock(
            UserInfo(
                username="xxxx@jyu.fi",
                full_name="X Test",
                last_name="X",
                given_name="Test",
                email="xxxx@example.com",
                origin=UserOrigin.Haka,
                unique_codes=[
                    SchacPersonalUniqueCode(
                        codetype="studentID", code="1234X", org="jyu.fi"
                    )
                ],
            ),
            missing_uniquecode=True,
        )

    def do_acs_mock(
        self, info: UserInfo, missing_uniquecode=False, assurance_levels=None
    ):
        self.get(
            "/saml/sso",
            query_string={
                "entityID": "https://testidp.funet.fi/idp/shibboleth",
                "return_to": "/",
            },
            expect_status=302,
        )
        with mock.patch("timApp.auth.saml.routes._get_saml_response") as m:
            m.return_value = SamlSSOResponseMock(
                info=info,
                mock_missing_uniquecode=missing_uniquecode,
                assurance_levels=assurance_levels or [LOW_ASSURANCE],
            )
            self.post(
                acs_url,
                data={},
                expect_status=302,
            )

    def test_simple_login(self):
        simple_email = "simple@example.com"
        self.json_post(
            "/simpleLogin/email",
            {"email": simple_email},
            expect_content="Simple email login is not enabled.",
            expect_status=400,
        )
        app.config["SIMPLE_EMAIL_LOGIN"] = True
        self.json_post(
            "/simpleLogin/email",
            {"email": simple_email},
            expect_content={"status": "ok"},
        )
        self.json_post(
            "/simpleLogin/password",
            {
                "email": simple_email,
                "password": test_pws[-1],
            },
            expect_content={
                "data": {"can_change_name": True, "name": None},
                "type": "registration",
            },
        )
        self.json_post(
            "/emailSignupFinish",
            {
                "email": simple_email,
                "password": test_pw,
                "passconfirm": test_pw,
                "token": test_pws[-1],
                "realname": None,
            },
            expect_content={"status": "registered"},
        )

        app.config["EMAIL_REGISTRATION_ENABLED"] = False
        pwcount = len(test_pws)
        self.json_post(
            "/simpleLogin/email",
            {"email": simple_email},
            expect_content={"status": "ok"},
        )
        self.assertEqual(pwcount, len(test_pws))
        self.json_post(
            "/emailSignup",
            {"email": simple_email},
            expect_content="Email registration is disabled.",
            expect_status=403,
        )
        self.json_post(
            "/emailSignup",
            {
                "email": simple_email,
                "reset_password": True,
            },
            expect_content={"status": "ok"},
        )
        self.assertEqual(pwcount + 1, len(test_pws))
        s_e = User.get_by_email(simple_email)
        s_e.pass_ = None
        db.session.commit()
        self.json_post(
            "/simpleLogin/email",
            {"email": simple_email},
            expect_content={"status": "ok"},
        )
        self.assertEqual(pwcount + 2, len(test_pws))
        self.json_post(
            "/simpleLogin/email",
            {"email": "simple2@example.com"},
            expect_content={"status": "ok"},
        )
        # No new mails because registration disabled.
        self.assertEqual(pwcount + 2, len(test_pws))
        app.config["EMAIL_REGISTRATION_ENABLED"] = True

    def test_no_password_reset(self):
        self.login_test1()
        with self.temp_config(
            {
                "PASSWORD_RESET_ENABLED": False,
                "EMAIL_REGISTRATION_ENABLED": False,
            }
        ):
            self.json_post(
                "/emailSignup",
                {
                    "email": "test1@example.com",
                    "reset_password": True,
                },
                expect_status=403,
                expect_content="PasswordResetDisabled",
            )
            self.json_post(
                "/emailSignup",
                {
                    "email": "test1@example.com",
                    "reset_password": False,
                },
                expect_status=403,
                expect_content="Email registration is disabled.",
            )
