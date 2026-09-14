"""Only what a plain view user can see may be indexed for AskTIM's RAG mode.

The indexer resolves each document with an anonymous user context, so anything
hidden from the least privileged reader is left out, along with settings
paragraphs, preamble paragraphs and plugins.
"""

import tempfile

from timApp.document.docinfo import DocInfo
from timApp.document.specialnames import (
    TEMPLATE_FOLDER_NAME,
    PREAMBLE_FOLDER_NAME,
    DEFAULT_PREAMBLE_DOC,
)
from timApp.modules.asktim.indexer import Indexer
from timApp.tests.server.timroutetest import TimRouteTest


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
