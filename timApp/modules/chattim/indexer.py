from dataclasses import dataclass, asdict
from typing import Protocol
from bs4 import BeautifulSoup
import json
import requests

# from openai import OpenAI
import numpy as np


# TODO mallien määrittely/valinta Indexer luokkaan?
@dataclass
class TextChunks:
    """text chunks to vectorize"""

    chunks: list[str]


@dataclass
class EmbeddingResponse:
    """list containing embeddings returned from the model"""

    embeddings: list[list[float]]


@dataclass
class EmbeddingData:
    """embedding and
    corresponding text chunk.
    file/tim page name or address?
    chunk id
    """

    embedding: list[float]
    text: str
    id: int
    # filename: str


class TextChunkerHTML:
    def __init__(self, text: str):
        self.text = text

    def split_sentence(self) -> TextChunks:
        soup = BeautifulSoup(self.text, "html.parser")
        self.text = soup.get_text(strip=True)
        # needs better sentence splitting
        chunks = TextChunks(chunks=self.text.split(". "))
        return chunks

    def split_paragraph(self) -> TextChunks:
        soup = BeautifulSoup(self.text, "html.parser")
        paragraphs = soup.find_all("p")

        paragraphs_text = []
        for p in paragraphs:
            text = p.get_text()
            paragraphs_text.append(text)
        # vain ensimmäiset 20 kappaletta ilmaisen avaimen takia
        paragraphs = TextChunks(chunks=paragraphs_text[0:20])
        return paragraphs


class TextChunker:

    def __init__(self, text: str):
        self.text = text

    def split_sentence(self) -> TextChunks:
        # needs better sentence splitting
        return TextChunks(chunks=self.text.split(". "))

    # splits every 600 characters, includes 100 from last chunk
    def split(self, chunk_size: int = 600, overlap: int = 100):

        chunks = []
        start = 0
        while start < len(self.text):
            end = start + chunk_size
            chunks.append(self.text[start:end])
            start += chunk_size - overlap
        return TextChunks(chunks=chunks)

    # splits the chunks at end of sentence and adds some overlap between chunks
    def split2(self, max_chunk_size: int = 600, overlap: int = 100):
        chunks = []
        sentences = self.text.split(". ")
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
        return TextChunks(chunks=chunks[0:20])


# TODO mallin valinta,
#  mahdollisesti mallikohtaisia asetuksia?(task type,vektorin koko jne)
class EmbeddingModel(Protocol):
    def generate(self, text_chunks: TextChunks) -> EmbeddingResponse: ...


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
            return f"Error generating embeddings {e}"

        embeddings = [x.embedding for x in result.data]

        return EmbeddingResponse(embeddings=embeddings)


class OpenAiEmbeddingModel(EmbeddingModel):
    """openai implementation of embedding model"""

    def __init__(self, api_key: str):
        self.api_key = api_key
        self.client = OpenAI(api_key=self.api_key)

    def generate(self, chunks: TextChunks):
        """generates embeddings from provided chunks"""
        text = chunks.chunks

        try:
            result = self.client.embeddings.create(
                input=text, model="text-embedding-3-small"
            )
        except Exception as r:
            print("Error generating embeddings", r)
            return EmbeddingResponse(embeddings=[])

        embeddings = [x.embedding for x in result.data]
        return EmbeddingResponse(embeddings=embeddings)


class GeminiEmbeddingModelREST(EmbeddingModel):
    """gemini implementation of embedding model"""

    def __init__(self, api_key: str):

        self.api_key = api_key
        self.url = "https://generativelanguage.googleapis.com/v1beta/models/gemini-embedding-001:batchEmbedContents"

    # TODO task tyypin valinta? jos tätä halutaan käyttää
    def generate(self, chunks: TextChunks) -> EmbeddingResponse:
        """generates embeddings from provided chunks"""
        texts = chunks.chunks
        headers = {"Content-Type": "application/json", "x-goog-api-key": self.api_key}
        data = {
            "requests": [
                {
                    "model": "models/gemini-embedding-001",
                    "content": {"parts": [{"text": text}]},
                }
                for text in texts
            ]
        }
        try:
            response = requests.post(self.url, headers=headers, json=data)

        except Exception as e:
            print(f"Error generating embeddings {e}")
            return f"Error generating embeddings {e}"

        embeddings = [x["values"] for x in response.json()["embeddings"]]

        return EmbeddingResponse(embeddings=embeddings)


class OpenAiEmbedREST(EmbeddingModel):
    def __init__(self, api_key: str):

        self.api_key = api_key
        self.url = "https://api.openai.com/v1/embeddings"

    def generate(self, chunks):
        headers = {
            "Authorization": f"Bearer {self.api_key}",
            "Content-Type": "application/json",
        }
        texts = chunks.chunks
        data = {"input": texts, "model": "text-embedding-3-small"}
        response = requests.post(self.url, headers=headers, json=data)
        response = response.json()

        embeddings = [x["embedding"] for x in response["data"]]
        return EmbeddingResponse(embeddings=embeddings)


# TODO tekstin paloitteluun eri vaihtoehtoja
class Indexer:
    def __init__(self, embedding_model: EmbeddingModel):
        self.embedding_model = embedding_model
        # self.text_chunker = text_chunker
        self.data = []

    def chunk_text(self, text, max_chunk_size: int = 600, overlap: int = 100):
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
        return TextChunks(chunks=chunks[0:20])

    def get_page(self, doc_id=None):
        with open("modules/chattim/testidata.txt", "r") as file:
            page = file.read()
        return page

    def create_embeddings(self):
        """generates the data object containing embeddings and corresponding text chunks"""

        text = self.get_page()
        chunks = self.chunk_text(text)

        embeddings = self.embedding_model.generate(chunks)

        ids = list(range(len(chunks.chunks)))

        self.data = [
            EmbeddingData(embedding=embedding, text=text, id=i)
            for (embedding, text, i) in zip(embeddings.embeddings, chunks.chunks, ids)
        ]
        data_dict = [asdict(obj) for obj in self.data]

        try:
            with open("modules/chattim/embeddings2.json", "w") as f:
                json.dump(data_dict, f, indent=2)
        except Exception as e:
            print(f"Error saving embeddings {e}")
            return f"Error saving embeddings {e}"
        return self.data

    def get_embeddings(self):
        try:
            with open("modules/chattim/embeddings2.json", "r") as file:
                page_embeddings = json.load(file)
        except Exception as e:
            print(f"Error retrieving embeddings {e}")
            return f"Error retrieving embeddings {e}"
        return page_embeddings

    def get_context(self, prompt: str, k: int = 5, doc_id: int = None):
        prompt = TextChunks(chunks=[prompt])
        try:
            prompt_embedding = self.embedding_model.generate(prompt)
            prompt_embedding = np.array(prompt_embedding.embeddings[0])
        except Exception as e:

            return f"Prompt embedding error: {e}"
        page_embeddings = self.get_embeddings()

        embeddings = []
        texts = []
        for chunk in page_embeddings:
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

        [context.append(text) for text, similarity in best_chunks]

        return context
