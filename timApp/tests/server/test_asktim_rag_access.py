"""AskTIM must not index, or retrieve from, documents the user has no rights to.

Two separate gates are covered here:

* indexing, where the instance owner must own or manage every document they add;
* retrieval, where the scope is narrowed to the documents the *asking* user may view,
  since the indexed set is chosen by the instance owner and not by the asker.
"""

from timApp.auth.accesstype import AccessType
from timApp.modules.asktim.asktim_main import plugincore
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db


class AskTimRagAccessTest(TimRouteTest):
    def test_indexing_refuses_a_document_without_rights(self):
        """Regression: the gate compared rights against None, so it passed for all.

        ``UserItemRights`` values are booleans, so ``right.get("owner")`` is False
        for a user without the right, and ``False is not None`` is True.
        """
        self.login_test1()
        d = self.create_doc(initial_par="material of another user")
        doc = d.document
        doc.docinfo = d

        with self.assertRaises(PermissionError):
            plugincore._has_rights_one_of(
                self.test_user_2.id, [doc], ["owner", "manage"]
            )

    def test_indexing_allows_the_owner(self):
        self.login_test1()
        d = self.create_doc(initial_par="own material")
        doc = d.document
        doc.docinfo = d

        self.assertTrue(
            plugincore._has_rights_one_of(
                self.test_user_1.id, [doc], ["owner", "manage"]
            )
        )

    def test_view_access_alone_does_not_allow_indexing(self):
        """Indexing needs owner or manage; being able to read is not enough."""
        self.login_test1()
        d = self.create_doc(initial_par="material")
        self.test_user_2.grant_access(d, AccessType.view)
        db.session.commit()
        # The session does not expire on commit, so an already loaded
        # block.accesses would not show the grant that was just made.
        db.session.expire_all()
        doc = d.document
        doc.docinfo = d

        with self.assertRaises(PermissionError):
            plugincore._has_rights_one_of(
                self.test_user_2.id, [doc], ["owner", "manage"]
            )

    def test_retrieval_scope_is_filtered_by_view_access(self):
        self.login_test1()
        readable = self.create_doc(initial_par="the asking user may read this")
        hidden = self.create_doc(initial_par="the asking user may not read this")
        self.test_user_2.grant_access(readable, AccessType.view)
        db.session.commit()
        # The session does not expire on commit, so an already loaded
        # block.accesses would not show the grant that was just made.
        db.session.expire_all()

        self.assertEqual(
            [readable.id],
            plugincore.viewable_document_ids(
                self.test_user_2.id, [readable.id, hidden.id]
            ),
        )
        # The owner of both documents still gets both.
        self.assertEqual(
            [readable.id, hidden.id],
            plugincore.viewable_document_ids(
                self.test_user_1.id, [readable.id, hidden.id]
            ),
        )

    def test_retrieval_scope_is_empty_for_an_unknown_user(self):
        self.login_test1()
        d = self.create_doc(initial_par="material")

        self.assertEqual([], plugincore.viewable_document_ids(-1, [d.id]))
