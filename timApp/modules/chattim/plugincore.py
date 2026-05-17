import os
import uuid
from dataclasses import dataclass, field
from unicodedata import normalize, category

from timApp.modules.chattim.dbmodels import LLMRule
from timApp.plugin.containerLink import call_plugin_resource
from timApp.util.flask.cache import cache
from timApp.timdb.dbaccess import get_files_path
from timApp.auth.get_user_rights_for_item import UserItemRights
from timApp.item.item import Item
from timApp.document.docinfo import DocInfo
from timApp.user.usergroup import get_groups_by_ids, UserGroup
from timApp.modules.chattim.indexer import (
    OpenAiEmbeddingModel,
    Indexer,
    create_embedder,
)
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
)
from typing import Generic, TypeVar, TypedDict, cast

from timApp.modules.chattim.model import (
    ModelResponse,
    Usage,
    GenericApiClient,
    Provider,
    ModelError,
    ModelErrorKind,
    PROVIDERS,
    ModelErrorKind,
)
from timApp.modules.chattim.conversation import ConversationManager, ChatMessage

DEFAULT_CACHE_TIMEOUT = 60 * 15  # seconds


# TODO: Is this needed if we have no model labels anymore?
@dataclass(frozen=True)
class ChatModel(TypedDict):
    label: str
    value: str


@dataclass()
class Policy:
    token_cap_enabled: bool = False
    token_cap: int | None = 0
    time_window_enabled: bool = False
    window_unit: str = "h"
    window_value: int | None = 0
    token_cap_for_window: int | None = 100


@dataclass()
class InstanceAttributes:
    model_id: str = "gpt-4.1-mini"
    llm_mode: str = "Creative"
    max_tokens: int | None = 2000
    tim_paths: list[str] = field(default_factory=list)
    system_prompt_path: str = ""
    use_streaming: bool = False
    model_temperature: float | None = None
    global_policy: Policy = field(default_factory=Policy)
    embedder_provider: str = "dummy"

    @classmethod
    def default(cls) -> "InstanceAttributes":
        return cls()


@dataclass
class InstanceSettingsData(InstanceAttributes):
    availableModels: list[ChatModel] = field(kw_only=True)
    availableModes: list[str] = field(kw_only=True)
    availableEmbedderProviders: list[str] = field(kw_only=True)


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

    def __repr__(self) -> str:
        return f"Ok({self.value})" if self.ok() else f"Err({self.error})"


@dataclass(frozen=True)
class PreparedChatRequest:
    caller_id: str
    document_id: str
    user_input: str
    response: Iterable[ModelResponse] | ModelResponse


# TODO: maybe a dict or dataclass would be more descriptive
APIKey = tuple[str, str, str, list[UserGroup], list[str]]


class PluginCore:
    rag: Rag = Rag()
    history_manager: ConversationManager
    tim_database: TimDatabase = TimDatabase()
    indexer: Indexer
    list_of_instance_ids: list[int] = []  # TODO: poista kun db:ssa rulet
    # TODO: a plugin instance specific variable? In global policy?
    max_input_len: int = 1024

    def __init__(self) -> None:
        file_path = get_files_path().as_posix()
        self.history_manager = ConversationManager(
            file_path,
            cache_ttl_s=60 * 15,
            cache_tail_len=64,
        )
        self.indexer = Indexer(file_path)

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
        policy_result: Result[str | None, str | None] = self._policy_checks(
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
        # TODO: from the LLMRule table here or bring as arg
        temperature: float | None = 0.2

        # TODO: get this from the LLMRule table
        use_streaming: bool = stream
        # Validate that the user has the correct generate mode
        if stream != use_streaming:
            return Result(error="Bad request. Mismatching generation mode.")

        # response = self.rag.get_context(prompt=validated_input, identifier=document_id)
        response = self.indexer.get_context(
            prompt=validated_input, identifier=document_id
        )
        context = response.context

        system_prompt = self.get_system_prompt(caller_id, document_id)

        msg_data = MessageData(
            user_prompt=validated_input,
            system_prompt=system_prompt,
            context=context,
            chat_history=chat_history,
            mode=mode,
            max_tokens=max_tokens_for_req,
        )

        try:
            answer = self.rag.answer(
                msg_data, identifier=document_id, stream=stream, temperature=temperature
            )
        except ModelError as e:
            return Result(error=e.text())
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
        if usage:
            self.update_usage(int(caller_id), int(plugin_id), usage.total_tokens)

    # TODO: palautetaan token usage tätä kautta tai muualta?
    def chat_request(
        self,
        caller_id: int,
        document_id: int,
        user_input: str,
    ) -> Result[str | None, str | None]:
        """Generate a model response."""
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
        """Generate a model response using streaming.

        :param caller_id: The id of the caller.
        :param document_id: The id of the document.
        :param user_input: The input of the caller.
        :raises ModelError: If an error occurs during the stream.
        :return: A stream of model response chunks.
        """
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
                try:
                    for chunk in stream:
                        apply_chunk(chunk)
                except ModelError:
                    pass
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

        # TODO: Need to get the used API-key from the db
        # If the teacher has not yet saved the settings after setting an API-key alias,
        # the list should be empty.
        # Or should we just disable the model selection in the UI until an API-key alias has been set?
        api_key = os.getenv("OPENAI_API_KEY") or ""
        provider: Provider = "openai"

        data = InstanceSettingsData(
            availableModes=RagMode.supported_modes(),
            availableModels=self._get_supported_chat_models(provider, api_key),
            availableEmbedderProviders=self._get_available_embedder_providers(user_id),
            use_streaming=True,  # TODO: from db
        )

        return Result(value=data)

    def save_instance(
        self, caller_id: int, document_id: int, instance_settings: InstanceAttributes
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
        embedder_provider: str = instance_settings.embedder_provider
        llm_mode: str = instance_settings.llm_mode
        max_tokens: int | None = instance_settings.max_tokens
        tim_paths: list[str] = instance_settings.tim_paths
        system_prompt_path: str = instance_settings.system_prompt_path.strip()
        global_policy: Policy = instance_settings.global_policy
        use_streaming: bool = instance_settings.use_streaming
        temperature: float | None = instance_settings.model_temperature

        # TODO: Remove hard coded API-key, provider and model_id.
        # Fetch from the database.
        api_key = os.getenv("OPENAI_API_KEY")
        provider_str: str = "openai"
        model_id: str = "gpt-4.1-nano"

        if not self._document_exists(document_id):
            return Result(None, f"Document [{document_id}] does not exist")

        if not self._owns_document(caller_id, document_id):
            return Result(None, "Insufficient rights")

        rag_mode = self._parse_rag_mode(llm_mode)
        if rag_mode is None:
            return Result(None, f"Invalid rag mode [{llm_mode}] given")

        if provider_str not in Provider.__args__:
            return Result(None, f"Invalid provider [{provider_str}] given")
        provider = cast(Provider, provider_str)

        supported_models = PluginCore._get_supported_models(provider, api_key)
        if model_id not in supported_models:
            return Result(None, f"Given model [{model_id}] not supported")

        paths_for_indexing = tim_paths
        if not paths_for_indexing and rag_mode == RagMode.RETRIEVE:
            return Result(None, "Give at least one path when using summarizing mode")

        docs_result = self._fetch_docs_by_paths(paths_for_indexing)
        if not docs_result.ok():
            return Result(None, docs_result.error)
        docs = docs_result.value or []

        document_ids = [doc.id for doc in docs]

        owns_all = self._owns_all_items(caller_id, docs)
        if owns_all.ok():
            if not owns_all.value:
                return Result(None, owns_all.error)
        else:
            return Result(None, "Internal error")

        # validating max tokens
        if max_tokens is not None and max_tokens < 0:
            return Result(None, "Give non-negative max tokens value")

        if system_prompt_path:
            prompt_doc = self.tim_database.get_tim_document_by_path(system_prompt_path)
            if not prompt_doc:
                return Result(None, "Invalid system prompt path")
            cache.delete_memoized(PluginCore.get_system_prompt, document_id=document_id)

        if temperature is not None and (temperature < 0 or temperature > 2):
            return Result(None, "Temperature must be between 0 and 2")

        # TODO: update system prompt path in the db row

        if (valid := self._validate_policy(global_policy)) is not None:
            return Result(error=valid)

        # TODO: if instance exists -> update OTHERIWISE create

        kwargs_model = dict(provider=provider, model_id=model_id, api_key=api_key)
        try:
            self.rag.add_model(identifier=document_id, **kwargs_model)
        except ValueError as e:
            return Result(None, str(e))

        # TODO: api key from db, change create_embedder to take api key as parameter
        llm_provider = kwargs_model["provider"]
        available_keys: list[APIKey] = self.get_user_api_keys(owner_id=caller_id)

        for key in available_keys:
            if key[1].lower() == embedder_provider.lower():
                emb_model = create_embedder(
                    embedder_provider=embedder_provider, api_key=key[2]
                )
                break
            # return Result(None, f"Failed to create embedder, No available API key")

        self.indexer.add_embedder(document_id, emb_model)

        tokens_used, failed_embeddings = self.indexer.create_embeddings(
            identifier=document_id, documents=docs
        )
        # probably better ways to do this

        if failed_embeddings > 0:
            return Result(
                None, f"Failed to create embeddings for {failed_embeddings} documents."
            )
        print(f"Tokens used for indexing: {tokens_used}")
        self.list_of_instance_ids.append(
            document_id
        )  # TODO: for testing purposes remove when db ok or cache

        # TODO: hae aliasta vastaava API-avain
        # TODO: tarkista onko aliasta vastaava API-avain olemassa

        rule = self.tim_database.set_llm_rule(
            document_id,
            caller_id,
            "",
            use_streaming,
            temperature,
            [],
            llm_mode,
            0,
            document_ids,
            system_prompt_path,
            model_id,
            0,
            [],
            [],
        )
        self.tim_database.set_global_policy(
            rule,
            global_policy.window_unit,
            global_policy.window_value,
            global_policy.token_cap_for_window,
            global_policy.token_cap,
            max_tokens,
        )

        return Result(True, None)

    def delete_instance(self, owner_id: int, document_id: int) -> None:
        self.tim_database.delete_llm_rule(owner_id, document_id)

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

    @staticmethod
    def get_supported_providers() -> list[Provider]:
        """Get the list of supported API providers."""
        return list(PROVIDERS.keys())

    @staticmethod
    @cache.memoize(timeout=DEFAULT_CACHE_TIMEOUT)
    def _get_supported_models(provider: Provider, api_key: str) -> list[str]:
        """Get all the models the given API-key has access to.
        :param provider: Provider name.
        :param api_key: API key, must match the given provider.
        :return: List of all supported model IDs.
        """
        client = GenericApiClient(provider, api_key)
        try:
            return client.list_models()
        except ModelError:
            return []
        finally:
            client.close()

    @staticmethod
    def _get_supported_chat_models(provider: Provider, api_key: str) -> list[ChatModel]:
        if not api_key:
            return []
        chat_models: list[ChatModel] = []
        for model_id in PluginCore._get_supported_models(provider, api_key):
            chat_models.append(ChatModel(label=model_id, value=model_id))
        return chat_models

    def _get_available_embedder_providers(self, caller_id: int) -> list[str]:
        """Returns a list of available embedding providers based on API keys."""
        print("================================")
        keys = self.get_user_api_keys(owner_id=caller_id)
        providers = []
        for key in keys:
            providers.append(key[1])
        print(f"Providers from DB{providers}")
        print("================================")

        return providers

    @cache.memoize(timeout=DEFAULT_CACHE_TIMEOUT, args_to_ignore=["self", "caller_id"])
    def get_system_prompt(self, caller_id: int, document_id: int) -> str | None:
        # TODO: Fetch from the database. If the caller fetches the whole table for other info as well,
        # then this function should probably only take the prompt path as a parameter.
        prompt_path = ""
        if not prompt_path:
            return None
        prompt_doc = self.tim_database.get_tim_document_by_path(prompt_path)
        if not prompt_doc:
            return None
        content = prompt_doc.export_markdown(export_ids=False).strip()
        return content if len(content) > 0 else None

    def _instance_exists(self, document_id: int) -> bool:
        # TODO: todnäk pitää muistissa tiedetyt instanssi-idt jottei haeta aina tietokannalta turhaan
        # TODO: korvaa db haulla
        if not self.tim_database.get_llm_rule(document_id):
            return False
        return True

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

    def set_user_policy(
        self, caller_id: int, document_id: int, policy_settings: Policy
    ) -> Result[bool | None, str | None]:
        """
        Sets the user's policy for the given user in the given document. Adds new policy row to the database.
        :param caller_id: The user for the policy.
        :param document_id: The document of the plugin.
        :param policy_settings: Settings for the user policy.
        :return: On error: Result(None, error_reason) On success Result(True, None)
        """
        rule = self.tim_database.get_llm_rule(document_id)
        if not rule:
            return Result(None, "No LLMRule found for this document")

        self.tim_database.set_user_policy(
            caller_id,
            rule,
            policy_settings.window_unit,
            policy_settings.window_value,
            policy_settings.token_cap_for_window,
            policy_settings.token_cap,
        )

        return Result(True, None)

    def update_usage(
        self,
        caller_id: int,
        document_id: int,
        used_tokens: int,
    ) -> int | None:
        """
        Updates the usage of the given user in the given document.
        :param caller_id: The user of the tokens.
        :param document_id: The document of the plugin.
        :param used_tokens: Tokens used.
        :return: The complete amount of tokens used by the user.
        """
        rule = self.tim_database.get_llm_rule(document_id)
        if not rule:
            return None
        usage = self.tim_database.get_usage(rule, caller_id)
        if not usage:
            self.tim_database.set_usage(caller_id, rule, used_tokens)
            return used_tokens
        tokens = usage.used_tokens + used_tokens
        self.tim_database.set_usage(caller_id, rule, tokens)
        return tokens

    def _policy_checks(
        self, caller_id: int, document_id: int
    ) -> Result[str | None, str | None]:
        """
        Checks token limits as per global and user policies.
        :param caller_id:  the user that is making the request
        :param document_id:  instance for the plugin
        :return: (can_make_req: bool, reason_for_deny: str)
        """
        # check user policy (if exists)
        rule = self.tim_database.get_llm_rule(document_id)
        if not rule:
            return Result(None, "No LLMRule found for this document")
        usage = self.tim_database.get_usage(rule, caller_id)

        if not usage:
            self.update_usage(caller_id, document_id, 0)
            return Result(value="ok")

        used_tokens = usage.used_tokens
        policy = self.tim_database.get_user_policy(rule, caller_id)

        if policy:
            token_limit = policy.max_tokens_per_user
        else:
            # check global policy
            policy = self.tim_database.get_global_policy(rule)
            if policy:
                token_limit = policy.max_tokens_per_user
            else:
                return Result(value="ok")
        if used_tokens and token_limit:
            if used_tokens >= token_limit:
                return Result(error="No more tokens")

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
                continue  # Might be just empty
                # return Result(error=f"Given path [{path}] does not exist")
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
    def _parse_provider(provider_str: str) -> Provider | None:
        if provider_str not in Provider.__args__:
            return None
        return cast(Provider, provider_str)

    @staticmethod
    def validate_api_key(provider_str: str, api_key: str) -> bool:
        """Check if the api key is valid."""
        provider = PluginCore._parse_provider(provider_str)
        if not provider:
            return False
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

    def update_api_key_permissions(
        self, owner_id: int, public_key: str, groups: list[str], paths: list[str]
    ) -> tuple[list[UserGroup], list[str]]:
        """
        Update the permissions for the API key.
        :param owner_id: Owner of the API key.
        :param public_key: Alias of the API key.
        :param groups: User groups that have access to the API key.
        :param paths: Document paths that this key can be used on.
        """
        f = lambda e: len(e) > 0
        filtered_groups = list(filter(f, groups))
        filtered_paths = list(filter(f, paths))
        key = self.tim_database.update_api_key_permissions(
            owner_id, public_key, filtered_groups, filtered_paths
        )
        groups = get_groups_by_ids(key.groups)
        return groups, key.paths

    def remove_api_key_group(
        self, owner_id: int, public_key: str, group_id: int
    ) -> None:
        self.tim_database.remove_api_key_group(owner_id, public_key, group_id)

    @staticmethod
    def _validate_policy(policy: Policy) -> None | str:
        """Validates policy. None is returned if everything is ok otherwise error string is returned"""
        valid_time_types = ["d", "h", "min"]
        cap_enabled = policy.token_cap_enabled
        token_cap = policy.token_cap

        window_enabled = policy.time_window_enabled
        time_type = policy.window_unit
        time_value = policy.window_value
        window_token_cap = policy.token_cap_for_window

        if cap_enabled and token_cap and token_cap < 0:
            return f"Given token cap [{token_cap}] cannot be negative"

        if window_enabled and time_value and time_value <= 0:
            return f"Given time value [{time_value}] should be greater than 0"

        if window_enabled and (time_type not in valid_time_types):
            return f"Given time type [{time_type}] is not valid"

        if window_enabled and window_token_cap and window_token_cap <= 0:
            return f"Given time cap [{window_token_cap}] should be greater than 0"

        return None

    def add_api_key(
        self,
        userid: int,
        provider: str,
        public_key: str,
        api_key: str,
        *,
        group_names: list[str] | None = None,
        paths: list[str] | None = None,
    ) -> APIKey:
        """Add an API key for the user."""
        alias = public_key.strip()
        if not alias:
            raise ValueError("Alias cannot be empty")
        row = self.tim_database.set_api_key(
            userid, provider, alias, api_key, group_names=group_names, paths=paths
        )
        return self._api_row_to_tuple(row)

    def get_user_api_keys(self, owner_id: int) -> list[APIKey]:
        """Fetch all the API keys the user owns.
        :param owner_id: Owner ID.
        :return: A list of tuples containing the API keys.
        """
        rows = self.tim_database.get_user_api_keys(owner_id)
        keys: list[APIKey] = []
        for row in rows:
            keys.append(self._api_row_to_tuple(row))
        return keys

    def try_access_api_key(self, user_id: int, public_key: str) -> tuple[Provider, str]:
        """
        Try to access the API key. Checks that the user has access to it.
        Used to link an API key to a plugin.

        :param user_id: ID of the user saving the plugin.
        :param public_key: Alias of the API key.
        :raises Exception: If the user has no access to the API key.
                           Or the API key provider is invalid.
        :return: A tuple (provider, api_key)
        """
        key = self.tim_database.access_api_key(user_id, public_key)
        if not key:
            raise Exception(f"No access to the API key.")
        provider = PluginCore._parse_provider(key.provider)
        if not provider:
            raise Exception(f"No provider found for API key.")
        return provider, key.api_key

    def get_api_key(self, public_key: str) -> tuple[Provider, str]:
        """
        Get the API key matching the public key.

        :param public_key: Alias of the API key.
        :raises Exception: If no API key is found.
                           Or the API key provider is invalid.
        :return: A tuple (provider, api_key)
        """
        key = self.tim_database.get_api_key_by_alias(public_key)
        if not key:
            raise Exception(f"API key not found")
        provider = PluginCore._parse_provider(key.provider)
        if not provider:
            raise Exception(f"No provider found for API key.")
        return provider, key.api_key

    def delete_api_key(self, owner_id: int, public_key: str) -> None:
        self.tim_database.delete_api_key(owner_id, public_key)

    def get_llm_rule(self, document_id: int) -> LLMRule:
        return self.tim_database.get_llm_rule(document_id)

    @staticmethod
    def _api_row_to_tuple(rule: LLMRule) -> APIKey:
        """
        Converts the LLMRule table of the API key to a tuple.
        `(alias, provider, api_key, group_names, paths)`
        """
        return (
            str(rule.public_key),
            str(rule.provider),
            str(rule.api_key),
            get_groups_by_ids(rule.groups),
            [str(p) for p in rule.paths],
        )
