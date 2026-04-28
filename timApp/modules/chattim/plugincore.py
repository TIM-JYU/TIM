import os
import uuid
from dataclasses import dataclass, field
from unicodedata import normalize, category

from timApp.timdb.dbaccess import get_files_path
from timApp.auth.get_user_rights_for_item import UserItemRights
from timApp.item.item import Item
from timApp.document.docinfo import DocInfo
from timApp.modules.chattim.indexer import OpenAiEmbeddingModel, Indexer
from timApp.modules.chattim.database_handler import (
    TimDatabase,
    Document,
)
from timApp.modules.chattim.rag import (
    Rag,
    MessageData,
    RagMode,
    Message,
    Iterable,
    ModelInfo,
)
from typing import Generic, TypeVar, TypedDict, cast

from timApp.modules.chattim.model import (
    ModelResponse,
    Usage,
    GenericApiClient,
    Provider,
    ModelError,
)
from timApp.modules.chattim.conversation import ConversationManager, ChatMessage


@dataclass(frozen=True)
class ChatModel(TypedDict):
    label: str
    value: str


@dataclass()
class InstanceAttributes:
    model_id: str = "gpt-4.1-mini"
    llm_mode: str = "Creative"
    max_tokens: int = 2000
    tim_paths: str = ""

    @classmethod
    def default(cls) -> "InstanceAttributes":
        return cls()


@dataclass
class InstanceSettingsData(InstanceAttributes):
    availableModels: list[ChatModel] = field(kw_only=True)
    availableModes: list[str] = field(kw_only=True)


T = TypeVar("T")
E = TypeVar("E")


class Result(Generic[T, E]):
    def __init__(self, value: T | None = None, error: E | None = None):
        if (value is None) == (error is None):
            raise ValueError("Provide exactly one of value or error")
        self.value: T | None = value
        self.error: E | None = error

    def ok(self) -> bool:
        return self.error is None

    def __repr__(self):
        return f"Ok({self.value})" if self.ok() else f"Err({self.error})"


@dataclass
class GlobalPolicy:
    pass


@dataclass
class StudentPolicy:
    pass


@dataclass(frozen=True)
class PreparedChatRequest:
    caller_id: str
    document_id: str
    user_input: str
    response: Iterable[ModelResponse] | ModelResponse


class PluginCore:
    rag: Rag = Rag()
    history_manager: ConversationManager
    tim_database: TimDatabase = TimDatabase()
    list_of_instance_ids: list[int] = []  # TODO: poista kun db:ssa rulet
    # TODO: a plugin instance specific variable? In global policy?
    max_input_len: int = 1024

    def __init__(self):
        file_path = get_files_path().as_posix()
        self.history_manager = ConversationManager(
            file_path,
            cache_ttl_s=60 * 15,
            cache_tail_len=64,
        )

    def _prepare_chat_request(
        self,
        caller_id: int,
        document_id: int,
        user_input: str,
        *,
        stream: bool = False,
    ) -> Result[PreparedChatRequest, str]:
        """Prepare the chat request.

        :param caller_id: The id of the caller.
        :param document_id: The id of the document.
        :param user_input: User input.
        :param stream: Use streaming for model response.
        :return: Result of the prepared chat request.
        """
        if not self._instance_exists(document_id):
            return Result(error=f"Instance has not been created yet")

        # policy check
        policy_result: Result[str | None, str | None] = self._student_policy_check(
            caller_id, document_id
        )
        if not policy_result.ok():
            return Result(error=policy_result.error)

        # Validate input
        try:
            validated_input = self.validate_input(user_input)
        except ValueError as e:
            return Result(error=str(e))

        document_id_str = str(document_id)
        caller_id_str = str(caller_id)

        # TODO: remember to fetch with timestamps when the time comes
        chat_history = self.get_history(caller_id_str, document_id_str)

        # TODO: fetch mode for instance
        mode: RagMode = RagMode.RETRIEVE
        # TODO: No need for this attribute if we have character limit for input? Maybe keep as is for an option
        max_tokens_for_req = 99999
        response = self.rag.get_context(prompt=validated_input, identifier=document_id)
        context = response.context

        msg_data = MessageData(
            user_prompt=validated_input,
            context=context,
            chat_history=chat_history,
            mode=mode,
            max_tokens=max_tokens_for_req,
        )

        try:
            answer = self.rag.answer(msg_data, identifier=document_id, stream=stream)
        except ModelError as e:
            return Result(error=str(e))
        except Exception as e:
            return Result(error=str(e))

        # TODO: answer post processing
        # Add the citations to the context used
        # Include TIM doc ids etc, and convert to TIM paths
        prepared = PreparedChatRequest(
            caller_id=caller_id_str,
            document_id=document_id_str,
            user_input=validated_input,
            response=answer,
        )
        return Result(value=prepared)

    def _save_messages(
        self,
        *,
        plugin_id: str,
        caller_id: str,
        user_input: str,
        assistant_answer: str,
        timestamp_user: int,
        timestamp_answer: int,
        usage: Usage | None,
    ) -> None:
        """Save the messages.

        :param plugin_id: The id of the plugin or document.
        :param caller_id: The id of the caller.
        :param user_input: The input of the caller.
        :param assistant_answer: The assistant message.
        :param timestamp_user: The timestamp of the user message.
        :param timestamp_answer: The timestamp of the answer.
        :param usage: The usage of the assistant message generation.
        """
        self.history_manager.append_messages(
            plugin_id,
            caller_id,
            messages=[
                ChatMessage(
                    role="user",
                    content=user_input,
                    usage=None,
                    timestamp=timestamp_user,
                ),
                ChatMessage(
                    role="assistant",
                    content=assistant_answer,
                    usage=usage,
                    timestamp=timestamp_answer,
                ),
            ],
        )

    # TODO: palautetaan token usage tätä kautta tai muualta?
    def chat_request(
        self,
        caller_id: int,
        document_id: int,
        user_input: str,
    ) -> Result[str | None, str | None]:
        timestamp_user = ChatMessage.ts_ms()
        # TODO: Do we save user messages to disk if error occurred from some of the checks or just discard?
        prep = self._prepare_chat_request(
            caller_id, document_id, user_input, stream=False
        )
        if not prep.ok() or not prep.value:
            return Result(error=prep.error)
        p = prep.value

        assert isinstance(p.response, ModelResponse)
        response: ModelResponse = p.response
        whole_msg = response.content or ""
        usage = response.usage

        # TODO: viestit arkistoidaan

        # TODO: save user message even if model response fails?
        timestamp_answer = ChatMessage.ts_ms()
        self._save_messages(
            plugin_id=p.document_id,
            caller_id=p.caller_id,
            user_input=p.user_input,
            assistant_answer=whole_msg,
            timestamp_user=timestamp_user,
            timestamp_answer=timestamp_answer,
            usage=usage,
        )

        return Result(value=whole_msg, error=None)

    def chat_request_stream(
        self,
        caller_id: int,
        document_id: int,
        user_input: str,
    ) -> Result[Iterable[ModelResponse], str]:
        timestamp_user = ChatMessage.ts_ms()
        prep = self._prepare_chat_request(
            caller_id, document_id, user_input, stream=True
        )
        if not prep.ok() or not prep.value:
            return Result(error=prep.error)
        p = prep.value

        assert isinstance(p.response, Iterable)
        stream: Iterable[ModelResponse] = p.response

        # TODO: return only the string chunks or the usage as well?
        def gen() -> Iterable[ModelResponse]:
            full_answer: str = ""
            usage: Usage | None = None

            # Collect the chunk message and usage
            def apply_chunk(c: ModelResponse) -> None:
                nonlocal full_answer, usage
                if c.delta:
                    full_answer += c.delta
                if c.usage:
                    usage = c.usage

            try:
                # Yield chunks to the caller
                for chunk in stream:
                    apply_chunk(chunk)
                    yield chunk
            finally:
                # Drain the remaining chunks if the client disconnected mid-stream
                for chunk in stream:
                    apply_chunk(chunk)
                timestamp_answer = ChatMessage.ts_ms()
                self._save_messages(
                    plugin_id=p.document_id,
                    caller_id=p.caller_id,
                    user_input=p.user_input,
                    assistant_answer=full_answer,
                    timestamp_user=timestamp_user,
                    timestamp_answer=timestamp_answer,
                    usage=usage,
                )

        return Result(value=gen(), error=None)

    def get_plugin_settings(
        self, user_id: int, document_id: int
    ) -> Result[InstanceAttributes | None, str | None]:
        """
        Get the settings for the plugin.
        """
        # if no such plugin exists return defaults
        if not self._document_exists(document_id):
            return Result(None, f"Document [{document_id}] does not exist")

        if not self._owns_document(user_id, document_id):
            return Result(None, "Insufficient rights")

        if not self._instance_exists(document_id):
            # TODO: get settings from db
            pass

        data = InstanceSettingsData(
            availableModes=RagMode.supported_modes(),
            availableModels=self._get_supported_chat_models(),
        )

        return Result(value=data)

    def save_instance(
        self, caller_id, document_id: int, instance_settings: InstanceAttributes
    ) -> Result[bool | None, str | None]:
        """
        Create instance if it doesn't exist. New settings are saved if valid.
        Adds a new model instance to RAG and a new llmrule table to database
        :param caller_id:
        :param document_id:
        :param instance_settings:
        :return: On error: Result(None, error_reason) On success (True, None)
        """
        model_id: str = instance_settings.model_id
        llm_mode: str = instance_settings.llm_mode
        max_tokens: int = instance_settings.max_tokens
        tim_paths: str = instance_settings.tim_paths

        if not self._document_exists(document_id):
            return Result(None, f"Document [{document_id}] does not exist")

        if not self._owns_document(caller_id, document_id):
            return Result(None, "Insufficient rights")

        rag_mode = self._parse_rag_mode(llm_mode)
        if rag_mode is None:
            return Result(None, f"Invalid rag mode [{llm_mode}] given")

        supported_models = self.rag.get_supported_models()
        if model_id not in supported_models:
            return Result(None, f"Given model [{model_id}] not supported")

        paths_for_indexing = self._parse_paths(tim_paths)
        if not paths_for_indexing and rag_mode == RagMode.RETRIEVE:
            return Result(None, "Give at least one path when using summarizing mode")

        docs = self._fetch_docs_by_paths(paths_for_indexing)
        if not docs.ok():
            return Result(None, docs.error)
        docs = docs.value

        owns_all = self._owns_all_items(caller_id, docs)
        if owns_all.ok():
            if not owns_all.value:
                return Result(None, owns_all.error)
        else:
            return Result(None, "Internal error")

        # validating max tokens
        if max_tokens < 0:
            return Result(None, "Give non-negative max tokens value")

        # TODO: kun policy saatu niin tässä check niille
        # TODO: if instance exists -> update OTHERIWISE create

        # TODO: remove hard coded api key and model
        api_key = os.getenv("OPENAI_API_KEY")
        kwargs_model = dict(provider="openai", model_id="gpt-4.1-nano", api_key=api_key)
        try:
            self.rag.add_model(identifier=document_id, **kwargs_model)
        except ValueError as e:
            return Result(None, str(e))

        # TODO: indeksoinnit pyörimään

        # TODO:implementation for choosing embedding model provider

        emb_model = OpenAiEmbeddingModel(api_key=api_key)

        file_path = get_files_path().as_posix()
        indexer = Indexer(emb_model, file_path)

        self.rag.add_indexer(indexer, identifier=document_id)
        tokens_used = indexer.create_embeddings(documents=docs)
        # probably better ways to do this
        if tokens_used == 0:
            return Result(None, "Could not create embeddings for given documents")
        print(f"Tokens used for indexing: {tokens_used}")
        self.list_of_instance_ids.append(
            document_id
        )  # TODO: for testing purposes remove when db ok or cache

        return Result(True, None)

    def remove_instance(self, caller_id: str, document_id: int):
        pass

    def modify_instance_pages(
        self, caller_id: str, document_id: int, indexable_paths: list[str]
    ):
        pass

    def get_history(self, caller_id: str, document_id: str) -> list[Message]:
        # TODO: fetch with time window
        history = self.history_manager.get_history(document_id, caller_id, 10)
        return [Message(role=m.role, content=m.content) for m in history]

    def get_messages_tw(
        self,
        caller_id: str,
        document_id: str,
        ts_begin: int | None = None,
        ts_end: int | None = None,
        max_count: int = 128,
    ) -> list[ChatMessage]:
        return self.history_manager.get_history_time_window(
            document_id, caller_id, ts_begin, ts_end, max_count
        )

    def _get_supported_chat_models(self) -> list[ChatModel]:
        chat_models: list[ChatModel] = []
        for model_spec in self.rag.get_supported_models().values():
            chat_models.append(
                ChatModel(label=model_spec.label, value=model_spec.model_id)
            )

        return chat_models

    def change_chatmode(self, caller_id: str, document_id: int, mode: RagMode):
        pass

    def set_globalpolicy(self, caller_id: str, document_id: int, policy: GlobalPolicy):
        pass

    def set_studentpolicy(
        self, caller_id: str, document_id: int, policy: StudentPolicy
    ):
        pass

    def _instance_exists(self, document_id) -> bool:
        # TODO: todnäk pitää muistissa tiedetyt instanssi-idt jottei haeta aina tietokannalta turhaan
        # TODO: korvaa db haulla
        print(
            f"Checking if instance {document_id} exists, list of instances:{self.list_of_instance_ids}"
        )
        if document_id in self.list_of_instance_ids:
            return True
        return False

    def _owns_document(self, caller_id: int, document_id: int) -> bool:
        """Expects that you have checked already that doc and user exist, throws otherwise"""

        tim_rights = self.tim_database.check_rights(caller_id, document_id)

        if tim_rights is None:
            # TODO: proper error?
            raise Exception(
                f"(_owns_document) given user {caller_id} or document {document_id} does not exist"
            )

        is_owner = tim_rights.get("owner")
        if not is_owner:
            return False

        return True

    @staticmethod
    def _parse_paths(paths: str) -> list[str]:
        """
        Gets a string, splits it with separator as "\n", removes empty entries and trims each entry
        :param paths:
        :return: list of paths or an empty list
        """
        return [x.strip() for x in paths.split("\n") if x.strip()]

    def _student_policy_check(
        self, caller_id: int, document_id: int
    ) -> Result[str | None, str | None]:
        """
        Checks that user request is allowed as per set policies
        :param caller_id:  the user that is making the request
        :param document_id:  instance for the plugin
        :return: (can_make_req: bool, reason_for_deny: str)
        """
        # check userpolicy (if exists)

        # check globalpolicy
        # TODO: impl
        return Result(value="ok")

    def _fetch_docs_by_paths(
        self, paths: list[str]
    ) -> Result[list[Document] | None, str | None]:
        """
        Get docs that correspond with given paths.
        If one of paths is a Folder, all the documents within its subfolders are added.
        All Documents within returned list are unique by id.
        :param paths:
        :return: list[Item] | None
        """
        documents = []
        for path in paths:
            found_documents = self.tim_database.get_tim_documents_by_path(path)
            if not found_documents:
                return Result(error=f"Given path [{path}] does not exist")
            documents.extend(found_documents)

        doc_set = list(set(documents))

        return Result(value=doc_set)

    def _document_exists(self, document_id: int) -> bool:
        """Checks if document with given id exists in tim database"""
        if not self.tim_database.get_tim_document_by_id(document_id):
            return False

        return True

    def _owns_all_items(
        self, user_id: int, documents: list[Document]
    ) -> Result[bool | None, str | None]:
        """
        Checks for all documents that the given user owns them. Expects that user and given documents exist.
        :param user_id: User for which the right is checked
        :param documents: Document for which the user has or has no right
        :return: If all documents are owned [True, None]
                 If no documents are provided [True, None]
                 If not all documents are owned [False, msg on item not owned]
                 If error happens:
        """

        for document in documents:
            doc_id = document.id
            right = self.tim_database.check_rights(user_id, doc_id)
            if not right:
                return Result(
                    error=f"Could not get rights for document [{document}] for user [{user_id}]"
                )

            is_owner: bool = right.get("owner")
            if not is_owner:
                return Result(False, f"No owner rights for [{document.docinfo.path}]")

        return Result(value=True)

    @staticmethod
    def _generate_id() -> str:
        return uuid.uuid4().hex

    @staticmethod
    def _parse_rag_mode(mode: str) -> RagMode | None:
        try:
            return RagMode(mode)
        except ValueError:
            print(f"Invalid rag mode given: {mode}")
            return None

    @staticmethod
    def validate_api_key(provider_str: str, api_key: str) -> bool:
        """Check if the api key is valid."""
        if provider_str not in Provider.__args__:
            return False
        provider = cast(Provider, provider_str)
        client = GenericApiClient(provider, api_key)
        try:
            return client.verify_api_key()
        except ModelError:
            return False
        finally:
            client.close()

    @staticmethod
    def _sanitize_input(user_input: str) -> str:
        """Sanitize the user input.

        :param user_input: Input prompt by the user.
        :return: Sanitized user input.
        """
        # Cf = Format, Co = Private Use, Cn = Unassigned
        drop_categories = {"Cf", "Co", "Cn"}
        # Handle composed character sequences
        normalized = normalize("NFKC", user_input)
        # Strip dangerous Unicode property classes
        return "".join(filter(lambda c: category(c) not in drop_categories, normalized))

    def validate_input(self, user_input: str) -> str:
        """Validate the user input.

        :param user_input: Input prompt by the user.
        :raises ValueError: if the user input is invalid.
        :return: Sanitized user input.
        """
        sanitized_input = self._sanitize_input(user_input.strip())
        input_len = len(sanitized_input)
        if input_len < 2 or input_len > self.max_input_len:
            raise ValueError(f"Invalid input length: {input_len}")
        return sanitized_input

    def get_supported_providers(self):
        return self.rag.registry.get_supported_providers()
