"""Tests for the AskTIM routes that handle LLM API keys."""

from timApp.modules.asktim.asktim_main import _api_key_to_dict
from timApp.tests.server.timroutetest import TimRouteTest


class AskTimRouteTest(TimRouteTest):
    def test_remove_group_right_requires_login(self):
        self.logout()
        self.json_post(
            "/asktim/removeGroupRight/1",
            {"public_key": "some-alias"},
            expect_status=403,
        )

    def test_get_models_requires_login(self):
        self.logout()
        self.json_post(
            "/asktim/getModels",
            {"public_key": "some-alias"},
            expect_status=403,
        )

    def test_route_with_a_request_model_refuses_anonymous(self):
        """AskTIM is not available to anonymous users at all.

        ``ask`` carries no ``verify_logged_in`` of its own: the gate lives in
        ``register_route``. This covers the wrapper used by routes that take a
        request model. Kept separate from the no-model case below so that each
        wrapper is verified on its own rather than one hiding behind the other.
        """
        self.logout()
        self.json_post(
            "/asktim/ask",
            {"document_id": 1, "input": "hi"},
            expect_status=403,
        )

    def test_route_without_a_request_model_refuses_anonymous(self):
        """``getProviders`` carries no ``verify_logged_in`` of its own.

        This covers ``register_route``'s other wrapper, for routes registered
        without a request model.
        """
        self.logout()
        self.get("/asktim/getProviders", expect_status=403)

    def test_short_api_key_is_masked_completely(self):
        """The first 6 + last 4 mask would show a short key almost in full."""
        short = _api_key_to_dict(("alias", "openai", "sk-short", [], []))
        self.assertEqual("...", short["APIkey"])

        long_key = "sk-abcdefghijklmnop"
        long = _api_key_to_dict(("alias", "openai", long_key, [], []))
        self.assertEqual("sk-abc...mnop", long["APIkey"])
        self.assertNotEqual(long_key, long["APIkey"])
