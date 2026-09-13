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

    def test_short_api_key_is_masked_completely(self):
        """The first 6 + last 4 mask would show a short key almost in full."""
        short = _api_key_to_dict(("alias", "openai", "sk-short", [], []))
        self.assertEqual("...", short["APIkey"])

        long_key = "sk-abcdefghijklmnop"
        long = _api_key_to_dict(("alias", "openai", long_key, [], []))
        self.assertEqual("sk-abc...mnop", long["APIkey"])
        self.assertNotEqual(long_key, long["APIkey"])
