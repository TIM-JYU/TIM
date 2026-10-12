"""The AskTIM indexer must search only the documents of the asking instance.

Retrieval used to read a process-wide ``Indexer.indexed_page_ids`` list that every
plugin instance appended to, so one instance could retrieve context embedded for
another instance's documents. The scope is now a parameter.
"""

import json
import tempfile
import unittest
from pathlib import Path
from unittest import mock

from timApp.modules.asktim.indexer import EmbeddingResponse, Indexer

MODEL = "test-embedding-model"


def write_index_file(indexer: Indexer, doc_id: int, text: str) -> None:
    """Write an embeddings file where the indexer expects to find one."""
    data = {
        "indexed_document_version": "2026-09-13T00:00:00",
        "embeddings": [
            {
                "embedding": [1.0, 0.0],
                "text": text,
                "tim_block_id": "b1",
                "sub_block_id": 0,
                "document_id": doc_id,
            }
        ],
    }
    path = indexer._get_file_name(doc_id, MODEL)
    Path(path).write_text(json.dumps(data), encoding="utf-8")


class AskTimIndexerScopeTest(unittest.TestCase):
    def test_only_the_given_documents_are_read(self):
        with tempfile.TemporaryDirectory() as tmp:
            indexer = Indexer(tmp)
            write_index_file(indexer, 1, "content of document one")
            write_index_file(indexer, 2, "content of document two")

            pages = indexer.get_embeddings([1], MODEL)
            texts = [c["text"] for p in pages for c in p["embeddings"]]
            self.assertEqual(["content of document one"], texts)

            both = indexer.get_embeddings([1, 2], MODEL)
            texts_both = sorted(c["text"] for p in both for c in p["embeddings"])
            self.assertEqual(
                ["content of document one", "content of document two"], texts_both
            )

    def test_empty_scope_reads_nothing(self):
        """An instance with no indexed documents must see no other document."""
        with tempfile.TemporaryDirectory() as tmp:
            indexer = Indexer(tmp)
            write_index_file(indexer, 7, "content belonging to another instance")

            self.assertEqual([], indexer.get_embeddings([], MODEL))

    def test_empty_scope_does_not_call_the_provider(self):
        with tempfile.TemporaryDirectory() as tmp:
            indexer = Indexer(tmp)
            # A provider request with this key would fail, so reaching one shows up
            # as an error rather than as an empty result.
            res = indexer.get_context(
                prompt="anything",
                api_key=("openai", "sk-not-a-real-key"),
                doc_ids=[],
            )
            self.assertEqual("", res.context)
            self.assertEqual(0, res.tokens_used)
            self.assertEqual([], res.used_context)


class FakeEmbeddingModel:
    """Deterministic stand-in so retrieval can be tested without a provider."""

    def __init__(self, vector: list[float]):
        self.vector = vector

    def generate(self, text_chunks: list[str]) -> EmbeddingResponse:
        return EmbeddingResponse(
            embeddings=[self.vector for _ in text_chunks], used_tokens=1
        )

    def change_key(self, new_key: str) -> None:
        pass

    def get_model_type(self) -> str:
        return MODEL


class AskTimRetrievalScopeTest(unittest.TestCase):
    def test_context_never_comes_from_another_instances_document(self):
        """Both documents embed identically, so only the scope can exclude one."""
        with tempfile.TemporaryDirectory() as tmp:
            indexer = Indexer(tmp)
            write_index_file(indexer, 1, "content of document one")
            write_index_file(indexer, 2, "content of document two")

            with mock.patch(
                "timApp.modules.asktim.indexer.create_embedder",
                return_value=FakeEmbeddingModel([1.0, 0.0]),
            ):
                res = indexer.get_context(
                    prompt="anything",
                    api_key=("openai", "sk-not-a-real-key"),
                    doc_ids=[1],
                    k=5,
                )

            self.assertIn("content of document one", res.context)
            self.assertNotIn("content of document two", res.context)
            self.assertEqual([(1, "b1")], res.used_context)
