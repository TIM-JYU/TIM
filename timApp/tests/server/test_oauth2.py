from authlib.oauth2 import OAuth2Client
from authlib.oauth2.rfc6749.parameters import parse_authorization_code_response

from timApp.tests.server.timroutetest import TimRouteTest
from timApp.tim_app import app


class OAuth2Test(TimRouteTest):
    def test_oauth2_login(self):
        test_client = app.config["OAUTH2_CLIENTS"][0]
        callback_uri = test_client["redirect_urls"][0]
        client = OAuth2Client(None, test_client["client_id"], test_client["client_secret"],
                              scope=":".join(test_client["allowed_scopes"]))

        uri, state = client.create_authorization_url("/oauth/authorize")

        self.logout()
        # The authorize URI must be valid and return 200
        self.get(uri)
        # Authorization cannot be done for anonymous users
        self.post(uri, expect_status=403, data={"confirm": "true"})

        # User can only authorize if they are logged in
        self.login_test1()
        res: str = self.post(uri, expect_status=302, data={"confirm": "true"})
        self.assertTrue(res.startswith(callback_uri),
                        f"Returned callback URL does not match: {res} does not start with {callback_uri}")

        # Ensure that returned authorization code can be used to obtain a token
        auth_code = parse_authorization_code_response(res, state)
        token = self.post("/oauth/token", data={
            "grant_type": "authorization_code",
            "redirect_uri": "http://test/login_redirect",
            "client_id": client.client_id,
            "client_secret": client.client_secret,
            **auth_code
        })
        self.assertIsNotNone(token["access_token"])

        # Try to access OAuth-protected resource
        res = self.get("/oauth/profile", headers=[("Authorization", f"Bearer {token['access_token']}")])
        self.assertEqual(res, {'id': self.test_user_1.id,
                               'emails': [{'email': self.test_user_1.email, 'verified': True}],
                               'last_name': self.test_user_1.last_name,
                               'given_name': self.test_user_1.given_name,
                               'real_name': self.test_user_1.real_name,
                               'username': self.test_user_1.name
                               }
                         )
