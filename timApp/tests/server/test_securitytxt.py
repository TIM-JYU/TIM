import re

from timApp.tests.server.timroutetest import TimRouteTest

pgp_message_re = re.compile(
    r"-----BEGIN PGP SIGNED MESSAGE-----\nHash: SHA512(.*)-----BEGIN PGP SIGNATURE-----.*-----END PGP SIGNATURE-----",
    re.DOTALL,
)


class TestSecurityTxt(TimRouteTest):
    def test_security_txt(self):
        r1 = self.get("/.well-known/security.txt")
        r2 = self.get("/security.txt")

        self.assertEqual(r1, r2)

        match = pgp_message_re.match(r1)
        self.assertIsNotNone(match)
        security_txt = match.group(1).strip()
        security_txt_re = re.compile(
            r"""
Contact: mailto:tim@jyu.fi
Contact: https://tim/contact
Expires: .*
Encryption: http://localhost/pgp-key.txt
Acknowledgements: https://tim/acknowledgements
Preferred-Languages: en,fi
Canonical: http://localhost/security.txt
Canonical: http://localhost/.well-known/security.txt
Canonical: https://tim/security.txt
Canonical: https://tim/.well-known/security.txt
Policy: https://tim/security_policy
""".strip()
        )
        self.assertIsNotNone(security_txt_re.match(security_txt))
