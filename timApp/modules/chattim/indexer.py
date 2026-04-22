from dataclasses import dataclass, asdict
from typing import Protocol
import json
from openai import OpenAI
import numpy as np
import os
from timApp.document.document import Document
from timApp.modules.chattim.database_handler import TimDatabase


# TODO mallien määrittely/valinta Indexer luokkaan?
@dataclass
class TextChunks:
    """text chunks to vectorize"""

    chunks: list[str]


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


@dataclass
class EmbeddingData:
    """
    :param embedding: embedding vector
    :param text: text chunk
    :param block_id: id of the chunk in the tim document
    :param document_id: id of the tim document
    """

    embedding: list[float]
    text: str
    block_id: int
    document_id: int
    # filename: str


# TODO mallin valinta,
#  mahdollisesti mallikohtaisia asetuksia?(task type,vektorin koko jne)
class EmbeddingModel(Protocol):
    def generate(self, text_chunks: TextChunks) -> EmbeddingResponse:
        ...


class GeminiEmbeddingModel(EmbeddingModel):
    """gemini implementation of embedding model"""

    def __init__(self, api_key: str):
        self.api_key = api_key
        self.client = None

    def generate(self, chunks: TextChunks) -> EmbeddingResponse:
        """generates embeddings from provided chunks"""
        if self.client is None:
            self.client = OpenAI(
                api_key=self.api_key,
                base_url="https://generativelanguage.googleapis.com/v1beta/openai/",
            )
            # self.client = genai.Client(api_key=self.api_key)

        text = chunks.chunks

        try:
            result = self.client.embeddings.create(
                input=text, model="gemini-embedding-001"
            )
            # result = self.client.models.embed_content(model="gemini-embedding-001",contents=text,)
        except Exception as e:
            print(f"Error generating embeddings {e}")
            return EmbeddingResponse(embeddings=[], used_tokens=0)

        embeddings = [x.embedding for x in result.data]

        return EmbeddingResponse(
            embeddings=embeddings, used_tokens=result.usage.total_tokens
        )


class OpenAiEmbeddingModel(EmbeddingModel):
    """openai implementation of embedding model"""

    def __init__(self, api_key: str):
        self.api_key = api_key
        self.client = OpenAI(api_key=self.api_key)

    def generate(self, chunks: TextChunks) -> EmbeddingResponse:
        """generates embeddings from provided chunks"""

        text = chunks.chunks

        try:
            result = self.client.embeddings.create(
                input=text, model="text-embedding-3-small"
            )

        except Exception as r:
            print("Error generating embeddings", r)
            return EmbeddingResponse(embeddings=[], used_tokens=0)

        embeddings = [x.embedding for x in result.data]

        return EmbeddingResponse(
            embeddings=embeddings, used_tokens=result.usage.total_tokens
        )


# TODO tekstin paloitteluun eri vaihtoehtoja
class Indexer:
    def __init__(self, embedding_model: EmbeddingModel, file_path: str):
        """

        :param embedding_model: embedding model object used for generating embeddings and for searching context
        :param indexed_page_ids: list of page ids that are currently indexed
        :param root_dir: root directory for storing index files
        """
        self.embedding_model = embedding_model
        self.indexed_page_ids: list[int] = []
        self.root_path = os.path.join(file_path, "embeddings", "chattim")

        # self.text_chunker = text_chunker

    def delete_page(self, doc_id: int) -> bool:
        """Deletes the page from the index.
        :param doc_id: id of the page to delete
        :return: True if the page was deleted, otherwise False"""
        if doc_id in self.indexed_page_ids:
            self.indexed_page_ids.remove(doc_id)
            return True
        else:
            return False

    # tätä ei ehkä tarvita enään

    def chunk_text(
        self, text: str, max_chunk_size: int = 600, overlap: int = 100
    ) -> TextChunks:
        chunks = []
        sentences = text.split(". ")
        current_chunk = ""

        for sentence in sentences:
            if (len(current_chunk) + len(sentence)) < max_chunk_size:
                current_chunk += sentence + ". "
            else:
                chunks.append(current_chunk)
                overlapping_text = current_chunk[-overlap:]
                current_chunk = overlapping_text + ". " + sentence
        if (len(current_chunk)) > 0:
            chunks.append(current_chunk)
        return TextChunks(chunks=chunks)

    # TODO ei haeta mahdollisia plugin lohkoja
    def get_tim_blocks(self, doc: Document) -> TextChunks:
        """returns the text chunks from provided tim document"""
        try:
            blocks = doc.export_raw_data()
            text = [block["md"] for block in blocks]
        except Exception as e:
            print(f"Error getting tim blocks {e}")

        return TextChunks(chunks=text)

    def create_embeddings(self, documents: list[Document]) -> int:
        """generates the data object containing embeddings and corresponding text chunks
        :param documents: list of tim documents
        :return: number of tokens used"""
        tokens_used = 0
        for document in documents:
            chunks = self.get_tim_blocks(doc=document)

            embeddings = self.embedding_model.generate(chunks)
            tokens_used += embeddings.used_tokens
            block_ids = list(range(len(chunks.chunks)))
            document_id = document.doc_id
            data = [
                EmbeddingData(
                    embedding=embedding, text=text, block_id=i, document_id=document_id
                )
                for (embedding, text, i) in zip(
                    embeddings.embeddings, chunks.chunks, block_ids
                )
            ]
            data_dict = [asdict(obj) for obj in data]
            file_name = document.doc_id
            os.makedirs(self.root_path, exist_ok=True)
            try:
                with open(f"{self.root_path}/{file_name}.json", "w") as f:
                    # print(self.root_path)
                    json.dump(data_dict, f, indent=2)
                    self.indexed_page_ids.append(file_name)
            except Exception as e:
                print(f"Error saving embeddings {e}")

        return tokens_used

    # TODO dataclass for page_embeddings?
    def get_embeddings(
        self,
    ):
        """returns embeddings for the indexed pages"""
        page_embeddings = []
        for doc_id in self.indexed_page_ids:
            try:
                with open(f"{self.root_path}/{doc_id}.json", "r") as file:
                    page_embeddings.append(json.load(file))
            except Exception as e:
                print(f"Error retrieving embeddings {e}")

        return page_embeddings

    def get_context(self, prompt: str, k: int = 3) -> ContextResponse:
        """returns the context for the prompt as list of text,and the number of tokens used
        :param prompt: prompt that is used to search for context
        :param k: number of tim chunks to return
        :return: ContextResponse object containing the context and the number of tokens used
        """

        tokens_used = 0
        try:
            prompt_embedding = self.embedding_model.generate(
                TextChunks(chunks=[prompt])
            )
            tokens_used = prompt_embedding.used_tokens
            prompt_embedding = np.array(prompt_embedding.embeddings[0])

        except Exception as e:
            print(f"Prompt embedding error: {e}")
            ContextResponse(context="", tokens_used=tokens_used)
        page_embeddings = self.get_embeddings()

        embeddings: list[float] = []
        texts = []
        for page in page_embeddings:
            for chunk in page:
                print(chunk["embedding"])
                embeddings.append(chunk["embedding"])
                texts.append(chunk["text"])

        embeddings = np.array(embeddings)

        # manual cosine similarity
        dot_product = embeddings @ prompt_embedding
        norm_embeddings = np.linalg.norm(embeddings, axis=1)
        norm_prompt = np.linalg.norm(prompt_embedding)
        similarities = dot_product / (norm_embeddings * norm_prompt)

        data = [[t, e] for t, e in zip(texts, similarities)]

        data.sort(key=lambda x: x[1], reverse=True)

        best_chunks = data[0:k]
        context = []
        for text, similarity in best_chunks:
            context.append(text)
        context_string = ", ".join(context)
        return ContextResponse(context=context_string, tokens_used=tokens_used)
