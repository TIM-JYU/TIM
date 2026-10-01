"""Tests for the access rules of AskTIM's user-managed LLM API keys."""

from timApp.modules.asktim.llm_rule import LLMRule
from timApp.tests.db.timdbtest import TimDbTest
from timApp.timdb.sqa import db
from timApp.user.usergroup import UserGroup


class AskTimApiKeyTest(TimDbTest):
    """API key rows are the ``LLMRule`` rows with ``document_id <= 0``."""

    def create_api_key(
        self,
        owner_id: int,
        alias: str = "key1",
        groups: list[int] | None = None,
        paths: list[str] | None = None,
    ) -> LLMRule:
        rule = LLMRule(
            document_id=0,
            owner=owner_id,
            public_key=alias,
            provider="openai",
            api_key="sk-secret-api-key-value",
            groups=groups if groups is not None else [],
            paths=paths if paths is not None else [],
        )
        db.session.add(rule)
        db.session.commit()
        return rule

    def create_group(self, name: str) -> UserGroup:
        g = UserGroup.create(name)
        db.session.commit()
        return g

    def test_access_api_key_owner_only(self):
        owner = self.test_user_1
        other = self.test_user_2
        self.create_api_key(owner.id, alias="owned")

        self.assertIsNotNone(LLMRule.access_api_key(owner.id, "owned"))
        self.assertIsNone(LLMRule.access_api_key(other.id, "owned"))
        self.assertIsNone(LLMRule.access_api_key(owner.id, "no-such-alias"))

    def test_access_api_key_via_shared_group(self):
        owner = self.test_user_1
        other = self.test_user_2
        g = self.create_group("asktim-shared-1")
        self.create_api_key(owner.id, alias="shared", groups=[g.id])

        # Not a member yet.
        self.assertIsNone(LLMRule.access_api_key(other.id, "shared"))

        other.groups.append(g)
        db.session.commit()
        self.assertIsNotNone(LLMRule.access_api_key(other.id, "shared"))

    def test_remove_api_key_group_leaves_a_list(self):
        """The removed group is gone and the column holds a plain list."""
        owner = self.test_user_1
        g1 = self.create_group("asktim-remove-1")
        g2 = self.create_group("asktim-remove-2")
        self.create_api_key(owner.id, alias="grouped", groups=[g1.id, g2.id])

        LLMRule.remove_api_key_group(owner.id, "grouped", g1.id)

        db.session.expire_all()
        rule = LLMRule.get_owner_api_key(owner.id, "grouped")
        self.assertIsNotNone(rule)
        self.assertIsInstance(rule.groups, list)
        self.assertEqual([g2.id], rule.groups)

    def test_usable_api_key_requires_a_matching_path(self):
        owner = self.test_user_1
        d = self.create_doc()

        no_paths = self.create_api_key(owner.id, alias="nopaths")
        self.assertIsNone(LLMRule.usable_api_key(owner.id, "nopaths", d.id))
        db.session.delete(no_paths)
        db.session.commit()

        self.create_api_key(owner.id, alias="withpath", paths=[d.path])
        self.assertIsNotNone(LLMRule.usable_api_key(owner.id, "withpath", d.id))

        other_doc = self.create_doc()
        self.assertIsNone(LLMRule.usable_api_key(owner.id, "withpath", other_doc.id))

    def test_usable_api_key_follows_group_revocation(self):
        """The chat-time check must notice a group that was revoked later."""
        owner = self.test_user_1
        other = self.test_user_2
        d = self.create_doc()
        g = self.create_group("asktim-revoke-1")
        other.groups.append(g)
        self.create_api_key(owner.id, alias="revocable", groups=[g.id], paths=[d.path])
        db.session.commit()

        self.assertIsNotNone(LLMRule.usable_api_key(other.id, "revocable", d.id))

        LLMRule.remove_api_key_group(owner.id, "revocable", g.id)
        db.session.expire_all()

        self.assertIsNone(LLMRule.usable_api_key(other.id, "revocable", d.id))
        # The owner still has access.
        self.assertIsNotNone(LLMRule.usable_api_key(owner.id, "revocable", d.id))
