"""Only what a plain view user can see may be indexed for AskTIM's RAG mode.

The indexer resolves each document as a logged-in user with no privileges of
their own -- AskTIM refuses anonymous callers, so that is the least privileged
reader it can actually have. Anything hidden from that reader is left out, along
with settings paragraphs, preamble paragraphs and plugins.
"""

import json
import tempfile
from pathlib import Path
from unittest import mock

from timApp.document.docinfo import DocInfo
from timApp.document.specialnames import (
    TEMPLATE_FOLDER_NAME,
    PREAMBLE_FOLDER_NAME,
    DEFAULT_PREAMBLE_DOC,
)
from timApp.modules.asktim.indexer import (
    INDEX_POLICY_VERSION,
    EmbeddingResponse,
    Indexer,
)
from timApp.tests.server.timroutetest import TimRouteTest


class FakeEmbeddingModel:
    """Counts calls so re-indexing can be observed without a provider."""

    def __init__(self) -> None:
        self.calls = 0

    def generate(self, text_chunks: list[str]) -> EmbeddingResponse:
        self.calls += 1
        return EmbeddingResponse(
            embeddings=[[1.0, 0.0] for _ in text_chunks], used_tokens=1
        )

    def change_key(self, new_key: str) -> None:
        pass

    def get_model_type(self) -> str:
        return "test-embedding-model"


class AskTimIndexingTest(TimRouteTest):
    def indexed_text(self, d: DocInfo) -> str:
        """The text AskTIM would embed for the given document."""
        with tempfile.TemporaryDirectory() as tmp:
            blocks, _ = Indexer(tmp).get_blocks(d.document)
        return "\n".join(b.text for b in blocks)

    def test_visible_content_is_indexed(self):
        """Guard: every exclusion test below would pass on an empty index."""
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#-
first visible paragraph

#-
second visible paragraph
"""
        )
        text = self.indexed_text(d)
        self.assertIn("first visible paragraph", text)
        self.assertIn("second visible paragraph", text)

    def test_settings_block_is_not_indexed(self):
        self.login_test1()
        d = self.create_doc(
            settings={"macros": {"unused": "SETTINGSSECRET"}},
            initial_par="ordinary content",
        )
        text = self.indexed_text(d)
        self.assertIn("ordinary content", text)
        self.assertNotIn("SETTINGSSECRET", text)

    def test_preamble_is_not_indexed(self):
        self.login_test1()
        folder = self.current_user.get_personal_folder().path
        preamble_path = (
            f"{folder}/{TEMPLATE_FOLDER_NAME}/{PREAMBLE_FOLDER_NAME}/"
            f"{DEFAULT_PREAMBLE_DOC}"
        )
        preamble = self.create_doc(preamble_path)
        preamble.document.add_text("PREAMBLECONTENT")
        d = self.create_doc(f"{folder}/a/withpreamble", initial_par="own content")

        # Preamble paragraphs enter the document's paragraph list once they have
        # been inserted, which is what happens while a view is rendered. Assert
        # the setup really put them there, so the test cannot pass vacuously.
        d.document.insert_preamble_pars()
        self.assertIn(
            "PREAMBLECONTENT",
            "\n".join(p.get_markdown() for p in d.document.get_paragraphs()),
            "the preamble should be part of the document at this point",
        )

        text = self.indexed_text(d)
        self.assertIn("own content", text)
        self.assertNotIn("PREAMBLECONTENT", text)

    def test_paragraph_hidden_by_visible_attribute_is_not_indexed(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#-
public paragraph

#- {visible=false}
HIDDENPARAGRAPH
"""
        )
        text = self.indexed_text(d)
        self.assertIn("public paragraph", text)
        self.assertNotIn("HIDDENPARAGRAPH", text)

    def test_paragraph_in_a_hidden_area_is_not_indexed(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#-
public paragraph

#- {area=secret visible=false}

#-
HIDDENAREACONTENT

#- {area_end=secret}
"""
        )
        text = self.indexed_text(d)
        self.assertIn("public paragraph", text)
        self.assertNotIn("HIDDENAREACONTENT", text)

    def test_plugins_are_not_indexed(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#-
public paragraph

#- {#t plugin=textfield}
PLUGINMARKUP
"""
        )
        text = self.indexed_text(d)
        self.assertIn("public paragraph", text)
        self.assertNotIn("PLUGINMARKUP", text)

    def test_content_visible_to_logged_in_users_is_indexed(self):
        """The reference reader is a logged-in user, not an anonymous visitor.

        AskTIM refuses anonymous callers, so content gated on being logged in is
        visible to every user of the assistant and belongs in the index.
        """
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#-
public paragraph

#- {nocache=true visible="%%'Logged-in users'|belongs%%"}
LOGGEDINONLYCONTENT
"""
        )
        text = self.indexed_text(d)
        self.assertIn("public paragraph", text)
        self.assertIn("LOGGEDINONLYCONTENT", text)

    def test_stale_index_policy_forces_reindexing(self):
        """A document indexed under older filtering rules must be redone."""
        self.login_test1()
        d = self.create_doc(initial_par="indexable content")

        with tempfile.TemporaryDirectory() as tmp:
            indexer = Indexer(tmp)
            fake = FakeEmbeddingModel()
            with mock.patch(
                "timApp.modules.asktim.indexer.create_embedder", return_value=fake
            ):
                indexer.create_embeddings(("openai", "sk-x"), [d.document])
                path = Path(indexer._get_file_name(d.id, fake.get_model_type()))
                data = json.loads(path.read_text(encoding="utf-8"))
                self.assertEqual(
                    INDEX_POLICY_VERSION, data["index_policy_version"]
                )
                self.assertGreater(fake.calls, 0)

                # An unchanged document indexed under the current rules is
                # not embedded again.
                fake.calls = 0
                indexer.create_embeddings(("openai", "sk-x"), [d.document])
                self.assertEqual(0, fake.calls)

                # The same document indexed under older rules is.
                del data["index_policy_version"]
                path.write_text(json.dumps(data), encoding="utf-8")
                fake.calls = 0
                indexer.create_embeddings(("openai", "sk-x"), [d.document])
                self.assertGreater(fake.calls, 0)
                refreshed = json.loads(path.read_text(encoding="utf-8"))
                self.assertEqual(
                    INDEX_POLICY_VERSION, refreshed["index_policy_version"]
                )
