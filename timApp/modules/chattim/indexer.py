from dataclasses import dataclass
from json import JSONDecodeError
from typing import Protocol
import json
from openai import OpenAI
import numpy as np
import os

from timApp.document.document import Document
from datetime import datetime

# Max tokens for API requests. Approximately 32000 characters
MAX_TOKENS_CHUNK = 8192
CHUNK_TOKEN_TARGET = 512
CHARACTERS_PER_TOKEN = 3


@dataclass
class TextBlock:
    """contains text from tim chunk and corresponding tim block id and sub block id"""

    text: str
    tim_block_id: str
    sub_block_id: int


@dataclass
class EmbeddingResponse:
    """list containing embeddings returned from the model"""

    embeddings: list[list[float]]
    used_tokens: int


@dataclass
class ContextResponse:
    """list containing context returned from the model"""

    context: str
    tokens_used: int
    used_chunks: list[str]


# maybe useless?
@dataclass
class EmbeddingData:
    """
    :param embedding: embedding vector
    :param text: text chunk
    :param tim_block_id: id of the chunk in the tim document
    :param sub_block_id: id of the sub block in the tim block
    :param document_id: id of the tim document
    """

    embedding: list[float]
    text: str
    tim_block_id: int
    sub_block_id: int
    document_id: int
    embeddings_created: str


class EmbeddingModel(Protocol):
    def generate(self, text_chunks: list[str]) -> EmbeddingResponse:
        ...

    def change_key(self, new_key: str) -> None:
        ...

    def get_model_type(self) -> str:
        ...


class GeminiEmbeddingModel(EmbeddingModel):
    """gemini implementation of embedding model"""

    def __init__(self, api_key: str, model_type: str = "gemini-embedding-001"):
        self.model_type = model_type
        self.api_key = api_key
        self.client = OpenAI(
            api_key=self.api_key,
            base_url="https://generativelanguage.googleapis.com/v1beta/openai/",
        )

    def generate(self, chunks: list[str]) -> EmbeddingResponse:
        """generates embeddings from provided chunks"""

        text = chunks
        try:
            result = self.client.embeddings.create(input=text, model=self.model_type)
            # result = self.client.models.embed_content(model="gemini-embedding-001",contents=text,)
        except Exception as e:
            print(f"Error generating embeddings {e}")
            return EmbeddingResponse(embeddings=[], used_tokens=0)

        embeddings = [x.embedding for x in result.data]
        # Usage field seems to be missing from the response when using gemini with openai library
        try:
            tokens_used = result.usage.total_tokens
        except Exception as e:
            print(f"Error getting tokens used {e}")
            tokens_used = 0
        return EmbeddingResponse(embeddings=embeddings, used_tokens=tokens_used)

    def change_key(self, key: str) -> None:
        self.api_key = key

    def get_model_type(self) -> str:
        return self.model_type


class OpenAiEmbeddingModel(EmbeddingModel):
    """openai implementation of embedding model"""

    def __init__(self, api_key: str, model_type: str = "text-embedding-3-small"):
        self.model_type = model_type
        self.api_key = api_key
        self.client = OpenAI(api_key=self.api_key)

    def generate(self, chunks: list[str]) -> EmbeddingResponse:
        """generates embeddings from provided chunks"""

        text = chunks

        try:
            result = self.client.embeddings.create(input=text, model=self.model_type)
        except Exception as r:
            print("Error generating embeddings", r)
            return EmbeddingResponse(embeddings=[], used_tokens=0)

        embeddings = [x.embedding for x in result.data]

        return EmbeddingResponse(
            embeddings=embeddings, used_tokens=result.usage.total_tokens
        )

    def change_key(self, key: str) -> None:
        self.api_key = key

    def get_model_type(self) -> str:
        return self.model_type


def create_embedder(embedder_provider: str, api_key: str) -> EmbeddingModel | None:
    """creates embedding model based on provider, defaults to openai
    :param provider: provider of the embedding model"""
    if embedder_provider.lower() == "openai":
        return OpenAiEmbeddingModel(api_key=api_key)
    elif embedder_provider.lower() == "google":
        return GeminiEmbeddingModel(api_key=api_key)

    return None


class Indexer:
    def __init__(self, file_path: str):
        """
        :param file_path: root directory for storing index files
        """
        self.embedding_models: dict[int, EmbeddingModel] = {}
        self.root_path = os.path.join(file_path, "embeddings", "chattim")
        self.indexed_page_ids: list[int] = []
        os.makedirs(self.root_path, exist_ok=True)

    def delete_page(self, doc_id: int, model_type: str) -> bool:
        """Deletes embedded page stored on disk
        :param doc_id: id of the page to delete
        :param model_type: name of embedding model used for the page"""
        path = self._get_file_name(doc_id, model_type)
        if os.path.exists(path):
            os.remove(path)
            return True

        return False

    @staticmethod
    def chunk_text(text: str, max_chunk_size: int, overlap: int) -> list[str]:
        """Split the text into overlapping chunks."""
        chunk_size = max_chunk_size - overlap * 2
        chunks: list[str] = []
        index = 0
        while index < len(text):
            start = max(0, index - overlap)
            end = min(len(text), index + chunk_size + overlap)
            chunk = text[start:end]
            chunks.append(chunk)
            index += chunk_size
        return chunks

    def chunk_block(
        self, block: dict, max_chunk_size: int = 2500, overlap: int = 200
    ) -> tuple[list[TextBlock], int]:
        """ " splits a chunk into smaller chunks
        :param block: tim block
        :param max_chunk_size: maximum size of the chunk
        :param overlap: overlap between chunks"""
        chunks: list[TextBlock] = []
        text = block["md"]
        if not text:
            return chunks, 0
        block_id = block["id"]
        sub_block_id = 0
        sentences = text.split(". ")
        if not sentences:
            return chunks, 0
        current_chunk = sentences[0]

        for sentence in sentences[1:]:
            if (len(current_chunk) + len(sentence)) < max_chunk_size:
                current_chunk += sentence + ". "
                continue
            if len(current_chunk) >= self._max_chunk_size_characters:
                for c in self.chunk_text(current_chunk, max_chunk_size, overlap):
                    chunks.append(
                        TextBlock(
                            text=c, tim_block_id=block_id, sub_block_id=sub_block_id
                        ),
                    )
                    sub_block_id += 1
            else:
                chunks.append(
                    TextBlock(
                        text=current_chunk,
                        tim_block_id=block_id,
                        sub_block_id=sub_block_id,
                    )
                )
                sub_block_id += 1
            overlapping_text = current_chunk[-overlap:]
            current_chunk = overlapping_text + ". " + sentence

        if len(current_chunk) >= self._max_chunk_size_characters:
            for c in self.chunk_text(current_chunk, max_chunk_size, overlap):
                chunks.append(
                    TextBlock(text=c, tim_block_id=block_id, sub_block_id=sub_block_id),
                )
                sub_block_id += 1
        elif (len(current_chunk)) > 0:
            chunks.append(
                TextBlock(
                    text=current_chunk, tim_block_id=block_id, sub_block_id=sub_block_id
                )
            )
            sub_block_id += 1

        return chunks, len(text)

    # TODO ei haeta mahdollisia plugin lohkoja
    def get_blocks(self, doc: Document) -> tuple[list[TextBlock], int]:
        """returns the text chunks from provided tim document and splits long chunks into smaller chunks"""
        total_content_len: int = 0
        blocks: list[TextBlock] = []
        try:
            tim_blocks = doc.export_raw_data()

            for tim_block in tim_blocks:
                sub_blocks, content_len = self.chunk_block(
                    tim_block, self._chunk_size_characters
                )
                blocks.extend(sub_blocks)
                total_content_len += content_len
        except Exception as e:
            print(f"Error getting tim blocks {e}")

        return blocks, total_content_len

    @staticmethod
    def get_generate_batches(
        chunks: list[str], content_len: int, max_batch_size: int
    ) -> list[list[str]]:
        """Divide the chunks into multiple batches."""
        if content_len <= max_batch_size:
            return [chunks]
        batches: list[list[str]] = []
        current_batch: list[str] = []
        batch_len: int = 0
        for chunk in chunks:
            chunk_len = len(chunk)
            if batch_len + chunk_len >= max_batch_size and current_batch:
                batches.append(current_batch)
                current_batch = []
                batch_len = 0
            current_batch.append(chunk)
            batch_len += chunk_len
        return batches

    def create_embeddings(
        self, identifier: int, documents: list[Document]
    ) -> tuple[int, int]:
        """generates the data object containing embeddings and corresponding text chunks
        Throws on non-existing embedder
        :param identifier id of the plugin instance
        :param documents: list of tim documents
        :return: number of tokens used"""

        embedding_model: EmbeddingModel = self.embedding_models[identifier]
        model_type: str = embedding_model.get_model_type()

        tokens_used = 0

        failed_embeddings = 0
        os.makedirs(self.root_path, exist_ok=True)
        for document in documents:
            file_name = self._get_file_name(document.doc_id, model_type)
            document_last_edited: datetime = datetime.now()

            try:
                changelog = document.get_changelog(max_entries=1)
                document_last_edited = changelog.entries[0].time
                with open(file_name, "r") as file:
                    embedding_file = json.load(file)
                    if (
                        embedding_file
                        and isinstance(embedding_file, dict)
                        and "indexed_document_version" in embedding_file
                        and "embeddings" in embedding_file
                        and embedding_file["embeddings"]
                    ):
                        embeddings_created = embedding_file["indexed_document_version"]

                        if document_last_edited <= datetime.fromisoformat(
                            embeddings_created
                        ):
                            self.indexed_page_ids.append(document.doc_id)

                            continue
            except FileNotFoundError as e:
                print(e)
                pass
            except JSONDecodeError:  # Invalid embeddings file
                pass
            except IndexError:  # No changelog
                pass

            chunks, content_len = self.get_blocks(doc=document)
            texts = [chunk.text for chunk in chunks]
            if len(texts) == 0:
                failed_embeddings += 1
                continue

            embeddings: list[list[float]] = []

            # Generate in batches to prevent exceeding maximum input len
            batches = self.get_generate_batches(
                texts, content_len, self._max_chunk_size_characters
            )
            for batch in batches:
                batch_embeddings = embedding_model.generate(batch)
                tokens_used += batch_embeddings.used_tokens
                embeddings.extend(batch_embeddings.embeddings)

            document_id = document.doc_id
            data = {
                "indexed_document_version": document_last_edited.isoformat(),
                "embeddings": [
                    {
                        "embedding": embedding,
                        "text": chunk.text,
                        "tim_block_id": chunk.tim_block_id,
                        "sub_block_id": chunk.sub_block_id,
                        "document_id": document_id,
                    }
                    for embedding, chunk in zip(embeddings, chunks)
                ],
            }

            try:
                with open(file_name, "w") as f:
                    # print(self.root_path)
                    json.dump(data, f, indent=2)
                    self.indexed_page_ids.append(document.doc_id)
            except Exception as e:
                failed_embeddings += 1
                print(f"Error saving embeddings {e}")

        return tokens_used, failed_embeddings

    def get_embeddings(
        self,
        doc_ids: list[int],
        model_type: str,
    ) -> list[dict]:
        """returns embeddings for the indexed pages"""

        page_embeddings: list[dict] = []
        for doc_id in doc_ids:
            file_name = self._get_file_name(doc_id, model_type)
            try:
                with open(file_name, "r") as file:
                    page_embeddings.append(json.load(file))
            except Exception as e:
                print(f"Error retrieving embeddings {e}")

        return page_embeddings

    @staticmethod
    def calculate_similarity(
        embeddings: list[list[float]], prompt_embedding: list[float]
    ) -> list[float]:
        """Compute cosine similarity between a prompt embedding and
        each row vector in the embeddings list."""
        embeddings_np = np.array(embeddings)
        prompt_embedding_np = np.array(prompt_embedding)

        dot_product = embeddings_np @ prompt_embedding_np
        norm_embeddings = np.linalg.norm(embeddings_np, axis=1)
        norm_prompt = float(np.linalg.norm(prompt_embedding_np))
        similarities = dot_product / (norm_embeddings * norm_prompt)
        return similarities.tolist()

    def get_context(self, prompt: str, identifier: int, k: int = 3) -> ContextResponse:
        """returns the context for the prompt as list of text,and the number of tokens used

        :param prompt: prompt that is used to search for context
        :param identifier id of the plugin instance
        :param k: number of tim chunks to return
        :return: ContextResponse object containing the context and the number of tokens used
        """
        embedding_model = self.embedding_models[identifier]

        tokens_used = 0

        try:
            prompt_result = embedding_model.generate([prompt])
            tokens_used = prompt_result.used_tokens
            prompt_embedding = prompt_result.embeddings[0]
        except Exception as e:
            print(f"Prompt embedding error: {e}")
            return ContextResponse(context="", tokens_used=tokens_used, used_chunks=[])

        page_embeddings = self.get_embeddings(
            self.indexed_page_ids, embedding_model.get_model_type()
        )

        embeddings: list[list[float]] = []
        texts: list[str] = []

        for page in page_embeddings:
            for chunk in page.get("embeddings", []):
                embedding = chunk.get("embedding")
                text = chunk.get("text")
                if not embedding or not text:
                    continue
                embeddings.append(embedding)
                texts.append(text)

        if not embeddings or not prompt_embedding:
            return ContextResponse(context="", tokens_used=tokens_used, used_chunks=[])

        similarities: list[float] = self.calculate_similarity(
            embeddings=embeddings, prompt_embedding=prompt_embedding
        )
        data = [[t, e] for t, e in zip(texts, similarities)]

        data.sort(key=lambda x: x[1], reverse=True)

        best_chunks = data[0:k]
        context = []
        for text, similarity in best_chunks:
            context.append(text)
        context_string = ", ".join(context)

        return ContextResponse(
            context=context_string, tokens_used=tokens_used, used_chunks=context
        )

    def add_embedder(self, identifier: int, embedder: EmbeddingModel) -> None:
        self.embedding_models[identifier] = embedder

    def remove_embedder(self, identifier: int) -> None:
        del self.embedding_models[identifier]

    def change_key(self, identifier: int, key: str) -> None:
        self.embedding_models[identifier].change_key(key)

    def _get_file_name(self, doc_id: int, model_type: str) -> str:
        return f"{self.root_path}/{doc_id}_{model_type}.json"

    @property
    def _chunk_size_characters(self) -> int:
        return CHUNK_TOKEN_TARGET * CHARACTERS_PER_TOKEN

    @property
    def _max_chunk_size_characters(self) -> int:
        return MAX_TOKENS_CHUNK * CHARACTERS_PER_TOKEN
