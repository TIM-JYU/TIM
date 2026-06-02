import sys
import uuid
from dataclasses import dataclass, field

from unicodedata import normalize, category

from timApp.modules.chattim.dbmodels import LLMRule, Policy, Usage
from timApp.util.flask.cache import cache
from timApp.timdb.dbaccess import get_files_path
from timApp.user.user import User
from timApp.user.usergroup import get_groups_by_ids, UserGroup
from timApp.modules.chattim.indexer import (
    Indexer,
    SUPPORTED_EMBEDDING_PROVIDERS,
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
    ModelUsage,
    GenericApiClient,
    Provider,
    ModelError,
    PROVIDERS,
)
from timApp.modules.chattim.conversation import ConversationManager, ChatMessage

DEFAULT_CACHE_TIMEOUT = 60 * 15  # seconds
HISTORY_MAX_MESSAGES = 64


# TODO: Is this needed if we have no model labels anymore?
@dataclass(frozen=True)
class ChatModel(TypedDict):
    label: str
    value: str


@dataclass()
class GenericPolicy:
    token_cap_enabled: bool = False
    token_cap: int | None = 0
    time_window_enabled: bool = False
    window_unit: str = "h"
    window_value: int | None = None
    token_cap_for_window: int | None = None


@dataclass()
class UserData:
    username: str
    user_id: int
    tokens_spent: int
    hasPolicy: bool
    policy: GenericPolicy


@dataclass()
class InstanceAttributes:
    public_key: str = ""
    # model_id: str = "gpt-4.1-mini"
    model_id: str = ""
    llm_mode: str = "Creative"
    max_tokens: int | None = None
    conv_time_window: int = 0
    tim_paths: list[str] = field(default_factory=list)
    system_prompt_path: str = ""
    use_streaming: bool = False
    model_temperature: float | None = None
    include_citations: bool = False
    similarity_threshold: float | None = None
    top_k_chunks: int = 3
    global_policy: GenericPolicy = field(default_factory=GenericPolicy)

    @classmethod
    def default(cls) -> "InstanceAttributes":
        return cls()


@dataclass
class UserKey:
    provider: str
    public_key: str
    is_selected: bool
    is_shared: bool = False
    shared_by: str = ""


@dataclass
class InstanceSettingsData(InstanceAttributes):
    availableModels: list[ChatModel] = field(kw_only=True, default_factory=list)
    availableModes: list[str] = field(kw_only=True, default_factory=list)
    availableKeys: list[UserKey] = field(kw_only=True, default_factory=list)
    availableEmbedderProviders: list[str] = field(kw_only=True, default_factory=list)
    selectedModel: str | None = None
    allowedItemPaths: list[str] | None = None

    @classmethod
    def default(cls) -> "InstanceSettingsData":
        return cls()


T = TypeVar("T")
E = TypeVar("E")


class SaveInstanceResult(TypedDict):
    supported_models: list[str]
    model_id: str


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
    citations: list[str] | None
    include_citations: bool = False
    verbose: bool = False


# TODO: maybe a dict or dataclass would be more descriptive
APIKey = tuple[str, str, str, list[UserGroup], list[str]]


class PluginCore:
    rag: Rag = Rag()
    history_manager: ConversationManager
    tim_database: TimDatabase = TimDatabase()
    indexer: Indexer
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
        rule = self.tim_database.get_llm_rule(document_id)
        if not rule:
            return Result(error="Instance has not been created yet")

        verbose_errors: bool = caller_id == rule.owner

        # policy checking
        remaining_tokens = 4096  # Looks like this is not obtainable programmatically from the API. These change by model/provider
        if caller_id != rule.owner:
            try:
                usage = self.tim_database.get_usage(rule, caller_id)
                left_tokens_result = self._calculate_remaining_tokens(
                    rule, usage, caller_id
                )
            except (LookupError, TypeError, ValueError) as e:
                return Result(error=str(e))

            tokens_left = left_tokens_result[0]
            message_when_no_tokens = left_tokens_result[1]
            if tokens_left == 0:
                return Result(error=message_when_no_tokens)

            if tokens_left is not None and tokens_left < remaining_tokens:
                remaining_tokens = tokens_left

        # Validate input
        try:
            validated_input = self.validate_input(user_input)
        except ValueError as e:
            return Result(error=str(e))

        document_id_str = str(document_id)
        caller_id_str = str(caller_id)

        # Get conversation history
        chat_history: list[Message]
        conv_tw_s = rule.conv_time_window
        if conv_tw_s > 0:
            ts_now_ms = ChatMessage.ts_ms()
            ts_begin_ms = max(0, ts_now_ms - (conv_tw_s * 1000))
            history = self.get_messages_tw(
                caller_id_str,
                document_id_str,
                ts_begin=ts_begin_ms,
                ts_end=ts_now_ms,
                max_count=HISTORY_MAX_MESSAGES,
            )
            chat_history = [Message(role=m.role, content=m.content) for m in history]
        else:
            history = self.history_manager.get_history(
                document_id_str, caller_id_str, HISTORY_MAX_MESSAGES
            )
            chat_history = [Message(role=m.role, content=m.content) for m in history]

        rag_mode: RagMode | None = self._parse_rag_mode(str(rule.current_mode))
        if rag_mode is None:
            return Result(error=f"Invalid mode '{rag_mode}'")

        temperature: float | None = None
        if rule.temperature is not None:
            temperature = float(rule.temperature)

        similarity_threshold: float | None = None
        if rule.similarity_threshold is not None:
            similarity_threshold = float(rule.similarity_threshold)

        # TODO: fix double cast
        top_k_chunks: int = cast(int, cast(object, rule.top_k_chunks))

        use_streaming: bool = bool(rule.use_streaming)

        include_citations: bool = (
            rule.include_citations and rag_mode == RagMode.RETRIEVE
        )

        # Validate that the user has the correct generate mode
        if stream != use_streaming:
            return Result(error="Bad request. Mismatching generation mode.")

        api_key = self.get_api_key(str(rule.public_key))
        model_id = rule.agent

        if not model_id:
            return Result(error="No model selected.")

        context: str | None = None
        citations: list[str] | None = None

        if rag_mode == RagMode.RETRIEVE:
            provider = api_key[0]
            if provider not in SUPPORTED_EMBEDDING_PROVIDERS:
                # TODO: implement embedding model picking
                return Result(
                    error=f"Can't use summarizing mode on [{provider}] API keys."
                )

            response = self.indexer.get_context(
                prompt=validated_input,
                api_key=api_key,
                k=top_k_chunks,
                threshold=similarity_threshold,
            )
            context = response.context
            citations = self._get_citations(response.used_context)
            self.update_usage(caller_id, document_id, response.tokens_used)

        system_prompt = self.get_system_prompt(str(rule.system_prompt_path))

        msg_data = MessageData(
            user_prompt=validated_input,
            system_prompt=system_prompt,
            context=context,
            chat_history=chat_history,
            mode=rag_mode,
            max_tokens=remaining_tokens,
        )

        try:
            answer = self.rag.answer(
                msg_data,
                api_key=api_key,
                model_id=str(model_id),
                stream=stream,
                temperature=temperature,
            )
        except ModelError as e:
            error = e.text()
            if verbose_errors:
                error += f" {str(e.cause)}"
            return Result(error=error)
        except Exception as e:
            return Result(error=str(e))

        prepared = PreparedChatRequest(
            caller_id=caller_id_str,
            document_id=document_id_str,
            user_input=validated_input,
            response=answer,
            citations=citations,
            include_citations=include_citations,
            verbose=verbose_errors,
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
        usage: ModelUsage | None,
        citations: list[str] | None,
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
                    citations=citations,
                ),
            ],
        )
        if usage:
            self.update_usage(
                int(caller_id), int(plugin_id), usage.total_tokens, timestamp_answer
            )

    def chat_request(
        self,
        caller_id: int,
        document_id: int,
        user_input: str,
    ) -> Result[tuple[str, list[str] | None], str]:
        """Generate a model response."""
        timestamp_user = ChatMessage.ts_ms()
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
        citations = p.citations

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
            citations=citations,
        )

        context = citations if p.include_citations else None
        return Result(value=(whole_msg, context))

    def chat_request_stream(
        self,
        caller_id: int,
        document_id: int,
        user_input: str,
    ) -> Result[tuple[Iterable[str], list[str] | None], str]:
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
        citations = p.citations

        assert isinstance(p.response, Iterable)
        stream: Iterable[ModelResponse] = p.response

        def gen() -> Iterable[str]:
            full_answer: str = ""
            usage: ModelUsage | None = None

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
                    yield chunk.delta or ""
            except ModelError as e:
                error = e.text()
                if p.verbose:
                    error += f" {str(e.cause)}"
                raise Exception(error) from e
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
                    citations=citations,
                )

        context = citations if p.include_citations else None
        return Result(value=(gen(), context))

    def get_plugin_settings(
        self, user_id: int, document_id: int
    ) -> Result[InstanceSettingsData, str]:
        """
        Get the settings for the plugin.
        """
        # if no such plugin exists return defaults
        if not self._document_exists(document_id):
            return Result(None, f"Document [{document_id}] does not exist")

        llm_rule = self.tim_database.get_llm_rule(document_id)
        if llm_rule is None:
            return Result(value=InstanceSettingsData.default())

        if not self._owns_document(user_id, document_id):
            # Return only data the client needs
            data = InstanceSettingsData(
                use_streaming=bool(llm_rule.use_streaming),
                include_citations=bool(llm_rule.include_citations),
                conv_time_window=llm_rule.conv_time_window or 0,
            )
            return Result(data)

        user_keys = self.tim_database.get_user_api_keys(user_id)
        shared_keys = self.tim_database.get_shared_api_keys(user_id)

        # Options for selecting key for plugin, no apikey shown
        available_keys = [
            UserKey(
                provider=str(k.provider),
                public_key=str(k.public_key),
                is_selected=llm_rule is not None
                and k.public_key == llm_rule.public_key,
            )
            for k in user_keys
        ]

        for k in shared_keys:
            owner = User.get_by_id(k.owner)
            owner_name = owner.name if owner else "unknown"
            available_keys.append(
                UserKey(
                    provider=str(k.provider),
                    public_key=str(k.public_key),
                    is_selected=llm_rule is not None
                    and k.public_key == llm_rule.public_key,
                    is_shared=True,
                    shared_by=owner_name,
                )
            )

        provider: Provider | None = None
        api_key: str = ""
        allowed_paths: list[str] = []

        api_key_row = self.tim_database.get_api_key_by_alias(str(llm_rule.public_key))
        if api_key_row is not None:
            provider = self._parse_provider(str(api_key_row.provider))
            api_key = str(api_key_row.api_key)
            allowed_paths = api_key_row.paths

        selected_model = str(llm_rule.agent)
        supported_models = (
            self._get_supported_chat_models(provider, api_key) if provider else []
        )

        global_policy = self.tim_database.get_global_policy(llm_rule)
        global_policy_ui = GenericPolicy()
        max_token_pool = InstanceAttributes().max_tokens
        if global_policy is not None:
            global_policy_ui = self._convert_sql_policy_to_generic_policy(global_policy)
            max_token_pool = global_policy.token_pool

        # Turn selected TIM documents from IDs to paths
        doc_paths: list[str] = []
        for doc_id in llm_rule.indexed_document_ids:
            doc = self.tim_database.get_doc_entry_by_id(doc_id)
            if not doc:
                continue
            doc_paths.append(doc.path)

        data = InstanceSettingsData(
            availableModes=RagMode.supported_modes(),
            availableModels=supported_models,
            availableKeys=available_keys,
            availableEmbedderProviders=self._get_available_embedder_providers(user_id),
            use_streaming=bool(llm_rule.use_streaming),
            selectedModel=selected_model,
            allowedItemPaths=allowed_paths,
            llm_mode=str(llm_rule.current_mode),
            system_prompt_path=llm_rule.system_prompt_path,
            tim_paths=doc_paths,
            model_temperature=llm_rule.temperature,
            include_citations=bool(llm_rule.include_citations),
            similarity_threshold=llm_rule.similarity_threshold,
            top_k_chunks=llm_rule.top_k_chunks,
            conv_time_window=llm_rule.conv_time_window,
            global_policy=global_policy_ui,
            max_tokens=max_token_pool,
        )

        return Result(value=data)

    def check_api_key_doc_access(self, key: LLMRule, document_ids: list[int]) -> None:
        """Check that the API key has access to the given documents."""
        for doc_id in document_ids:
            self.tim_database.api_key_valid_in_doc(key, doc_id)

    def save_instance(
        self, caller_id: int, document_id: int, instance_settings: InstanceAttributes
    ) -> Result[InstanceSettingsData, str]:
        """
        Create instance if it doesn't exist. New settings are saved if valid.
        Adds a new model instance to RAG and a new llmrule table to database
        :param caller_id:
        :param document_id:
        :param instance_settings:
        :return: On error: Result(None, error_reason) On success (True, None)
        """
        public_key: str = instance_settings.public_key
        model_id: str = instance_settings.model_id
        llm_mode: str = instance_settings.llm_mode
        max_tokens: int | None = instance_settings.max_tokens
        conv_time_window: int = instance_settings.conv_time_window
        tim_paths: list[str] = instance_settings.tim_paths
        system_prompt_path: str = instance_settings.system_prompt_path.strip()
        global_policy: GenericPolicy = instance_settings.global_policy
        use_streaming: bool = instance_settings.use_streaming
        temperature: float | None = instance_settings.model_temperature
        include_citations: bool = instance_settings.include_citations
        similarity_threshold: float | None = instance_settings.similarity_threshold
        top_k_chunks: int = instance_settings.top_k_chunks

        if not self._document_exists(document_id):
            return Result(None, f"Document [{document_id}] does not exist")

        if not self._owns_document(caller_id, document_id):
            return Result(None, "Insufficient rights")

        old_plugin_rule = self.tim_database.get_llm_rule(document_id)

        # Need to create before checking any other input
        if old_plugin_rule is None:
            self.tim_database.create_plugin(owner=caller_id, document_id=document_id)
            return self.get_plugin_settings(caller_id, document_id)

        old_provider = None
        if old_plugin_rule.public_key:
            try:
                old_provider, _ = self.get_api_key(str(old_plugin_rule.public_key))
            except Exception:
                old_provider = None

        try:
            api_key_rule, provider_str, api_key = self.try_access_api_key_row(
                caller_id, public_key
            )
        except (ValueError, PermissionError) as e:
            return Result(None, str(e))

        rag_mode = self._parse_rag_mode(llm_mode)
        if rag_mode is None:
            return Result(None, f"Invalid rag mode [{llm_mode}] given")

        provider = self._parse_provider(provider_str)
        if provider is None:
            return Result(None, f"Invalid provider [{provider_str}] given")

        paths_for_indexing = tim_paths

        docs_result = self._fetch_docs_by_paths(paths_for_indexing)
        if not docs_result.ok():
            return Result(None, docs_result.error)
        docs = docs_result.value or []

        document_ids = [doc.id for doc in docs]

        try:
            self._has_rights_one_of(caller_id, docs, ["owner", "manage"])
        except (PermissionError, ValueError) as e:
            return Result(None, str(e))

        # Check if the API key is valid in these documents
        try:
            self.tim_database.api_key_valid_in_doc(api_key_rule, document_id)
            self.check_api_key_doc_access(api_key_rule, document_ids)
        except (PermissionError, ValueError) as e:
            return Result(None, str(e))

        # validating max tokens
        if max_tokens is not None and max_tokens < 0:
            return Result(None, "Give non-negative max tokens value")

        if conv_time_window < 0:
            return Result(None, "Conversation time window must be non-negative")

        if system_prompt_path:
            prompt_doc = self.tim_database.get_tim_document_by_path(system_prompt_path)
            if not prompt_doc:
                return Result(None, "Invalid system prompt path")

        if temperature is not None and (temperature < 0 or temperature > 2):
            return Result(None, "Temperature must be between 0 and 2")

        if similarity_threshold is not None and (
            similarity_threshold < -1 or similarity_threshold > 1
        ):
            return Result(None, "Similarity threshold must be between -1 and 1")

        if top_k_chunks < 1 or top_k_chunks > 20:
            return Result(None, "Top-K must be between 1 and 20")

        if (valid := self._validate_policy(global_policy)) is not None:
            return Result(error=valid)

        provider_changed = old_provider is not None and old_provider != provider_str

        tokens_used, failed_embeddings = 0, 0
        try:
            if provider_changed:
                GenericApiClient.check_access(
                    provider=provider, model_id=model_id, api_key=api_key
                )
            if rag_mode == RagMode.RETRIEVE:
                tokens_used, failed_embeddings = self.indexer.create_embeddings(
                    api_key=(provider_str, api_key),
                    documents=docs,
                )
        except ValueError as e:
            return Result(None, str(e))

        currently_used_tokens = old_plugin_rule.total_tokens_spent + tokens_used
        rule = self.tim_database.set_llm_rule(
            document_id=document_id,
            owner=caller_id,
            public_key=public_key,
            use_streaming=use_streaming,
            temperature=temperature,
            include_citations=include_citations,
            similarity_threshold=similarity_threshold,
            top_k_chunks=top_k_chunks,
            current_mode=llm_mode,
            total_tokens_spent=currently_used_tokens,
            indexed_document_ids=document_ids,
            system_prompt_path=system_prompt_path,
            agent=model_id,
            conv_time_window=conv_time_window,
            policy=[],
            usage=[],
        )

        if not global_policy.token_cap_enabled:
            global_policy.token_cap = -1

        if not global_policy.time_window_enabled:
            global_policy.token_cap_for_window = -1

        self.tim_database.set_global_policy(
            llm_rule=rule,
            token_time_window_type=global_policy.window_unit,
            token_time_window_num=global_policy.window_value,
            time_window_tokens=global_policy.token_cap_for_window,
            max_tokens_per_user=global_policy.token_cap,
            token_pool=max_tokens,
        )

        if failed_embeddings > 0:
            # TODO: bring the error to the ui but still bring the saved data
            error = f"Failed to create embeddings for {failed_embeddings} documents."
            return Result(error=error)

        return self.get_plugin_settings(caller_id, document_id)

    def delete_instance(
        self, caller_id: int, document_id: int, par_id: str
    ) -> Result[str, str]:
        rule = self.tim_database.get_llm_rule(document_id)
        if not rule:
            return Result(error=f"No plugin instance in document [{document_id}]")
        if rule.owner != caller_id:
            return Result(error="Insufficient rights to delete plugin instance")

        document = self.tim_database.get_tim_document_by_id(document_id)
        if document is None:
            return Result(error=f"Document [{document_id}] does not exist")

        if par_id is None:
            return Result(error=f"Plugin paragraph not found")

        par = document.get_paragraph(par_id)
        plugin_type = par.get_attr("plugin")
        if plugin_type is not None and plugin_type == "chattim":
            deleted = document.delete_paragraph(par_id)
            if not deleted:
                return Result(error="Failed to delete plugin paragraph")
            self.tim_database.delete_llm_rule(caller_id, document_id)
            return Result(value="Plugin instance deleted")

        return Result(error="No chattim plugin instance in document")

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

    def get_messages_ui(
        self,
        caller_id: int,
        document_id: int,
        ts_end: int | None = None,
        max_count: int = 128,
    ) -> list[dict]:
        """Retrieve the wanted message window to the UI."""
        rule = self.tim_database.get_llm_rule(document_id)
        if not rule:
            return []
        try:
            rag_mode = self._parse_rag_mode(str(rule.current_mode))
            include_citations = rule.include_citations and rag_mode == RagMode.RETRIEVE
        except ValueError:
            include_citations = False

        messages = self.history_manager.get_history_time_window(
            str(document_id), str(caller_id), None, ts_end, max_count
        )
        return [
            {
                "content": m.content,
                "role": m.role,
                "timestamp_ms": m.timestamp,
                "citations": m.citations if include_citations else None,
            }
            for m in messages
            if m.role in ("user", "assistant")
        ]

    def clear_history(self, caller_id: str, document_id: str) -> None:
        self.history_manager.clear_history(document_id, caller_id)

    @staticmethod
    def get_supported_providers() -> list[Provider]:
        """Get the list of supported API providers."""
        return list(PROVIDERS.keys())

    def get_user_data(self, caller_id: int, document_id: int) -> list[UserData]:
        """
        Returns set policy and token usage for each user that has interacted with the plugin.
        For users that have no policy set, a default GenericPolicy is sent back.
        Owner's data is not sent.
        :param caller_id: The plugin owner that is using the component
        :param document_id: The document id where instance is located
        :return: list of UserData objects
        """

        llm_rule = self.tim_database.get_llm_rule(document_id)
        if not llm_rule:
            raise LookupError("Instance has not been created yet")

        # TODO: unify permissions checking to account for teachers too
        owner = llm_rule.owner
        if owner != caller_id:
            raise PermissionError("You have no access to this resource")

        user_data: list[UserData] = []
        usages = self.tim_database.get_usages(llm_rule)
        for usage in usages:
            user_id = usage.user
            user = User.get_by_id(user_id)
            username = user.name if user is not None else ""

            if user_id == caller_id:
                continue

            user_policy: GenericPolicy
            user_policy_sql = self.tim_database.get_user_policy(llm_rule, user_id)

            if user_policy_sql:
                user_policy = self._convert_sql_policy_to_generic_policy(
                    user_policy_sql
                )
            else:
                user_policy = GenericPolicy()

            policy_enabled: bool = (
                user_policy.time_window_enabled or user_policy.token_cap_enabled
            )

            data = UserData(
                username=username,
                user_id=user_id,
                tokens_spent=usage.used_tokens,
                hasPolicy=policy_enabled,
                policy=user_policy,
            )

            user_data.append(data)

        return user_data

    def save_user_policy(
        self, caller_id: int, document_id: int, user_data: UserData
    ) -> str:
        """
        Saves given user policy for the given user
        :param caller_id: the user who is saving
        :param document_id: document id for the plugin associated with policy
        :param user_data: user data that policy applies for
        :return:
        """

        llm_rule = self.tim_database.get_llm_rule(document_id)
        if not llm_rule:
            raise LookupError("Instance has not been created yet")

        owner = llm_rule.owner
        if owner != caller_id:  # this should never happen since we have UI-limitations
            raise PermissionError("You have no access to this resource")

        user_id = user_data.user_id
        if not User.get_by_id(user_id):
            raise LookupError("This user no longer exists")

        policy: GenericPolicy = user_data.policy
        if (valid := self._validate_policy(policy)) is not None:
            raise ValueError(valid)

        if not policy.token_cap_enabled:
            policy.token_cap = -1

        if not policy.time_window_enabled:
            policy.token_cap_for_window = -1

        self.tim_database.set_user_policy(
            user=user_id,
            llm_rule=llm_rule,
            token_time_window_type=policy.window_unit,
            token_time_window_num=policy.window_value,
            time_window_tokens=policy.token_cap_for_window,
            max_tokens_per_user=policy.token_cap,
        )

        return "Save successful"

    @staticmethod
    def _convert_sql_policy_to_generic_policy(sql_policy: Policy) -> GenericPolicy:
        max_tokens_per_user = sql_policy.max_tokens_per_user
        time_window_tokens = sql_policy.time_window_tokens
        time_window_value = sql_policy.token_time_window_num
        token_cap_enabled = True
        time_window_enabled = True

        if max_tokens_per_user is None or max_tokens_per_user == -1:
            token_cap_enabled = False
            max_tokens_per_user = None

        if time_window_tokens is None or time_window_tokens == -1:
            time_window_enabled = False
            time_window_tokens = None

        return GenericPolicy(
            token_cap_enabled=token_cap_enabled,
            token_cap=max_tokens_per_user,
            time_window_enabled=time_window_enabled,
            window_unit=sql_policy.token_time_window_type or "h",
            window_value=time_window_value,
            token_cap_for_window=time_window_tokens,
        )

    @staticmethod
    def _get_supported_models(provider: Provider, api_key: str) -> list[str]:
        """Get all the models the given API-key has access to.
        :param provider: Provider name.
        :param api_key: API key, must match the given provider.
        :return: List of all supported model IDs.
        """
        try:
            return PluginCore._get_supported_models_cache(provider, api_key)
        except ModelError:
            cache.delete_memoized(
                PluginCore._get_supported_models_cache, provider, api_key
            )
            return []

    @staticmethod
    @cache.memoize(timeout=DEFAULT_CACHE_TIMEOUT)
    def _get_supported_models_cache(provider: Provider, api_key: str) -> list[str]:
        client = GenericApiClient(provider, api_key)
        try:
            return client.list_models()
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
        keys = self.get_user_api_keys(owner_id=caller_id)
        providers = []
        for key in keys:
            providers.append(key[1])

        return providers

    def get_system_prompt(self, system_prompt_path: str) -> str | None:
        if not system_prompt_path:
            return None
        prompt_doc = self.tim_database.get_tim_document_by_path(system_prompt_path)
        if not prompt_doc:
            return None
        content = prompt_doc.export_markdown(export_ids=False).strip()
        return content if len(content) > 0 else None

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
        user_id: int | None,
        document_id: int,
        used_tokens: int,
        timestamp: int | None = None,
    ) -> bool:
        """
        When caller_id is given the Usage table of the user and LLMRule of the
        instance are updated with the used tokens.
        When caller_id None, only the LLMRule tokens are updated.
        :param user_id: The user of the tokens.
        :param document_id: The document of the plugin.
        :param used_tokens: Tokens used by user.
        :param timestamp: The timestamp of the usage. Is "now" when not given.
        :return: The complete amount of tokens used by the user.
        """
        rule = self.tim_database.get_llm_rule(document_id)
        if not rule:
            return False

        self.tim_database.update_instance_usage(rule, used_tokens)

        if user_id is not None:
            if timestamp is None:
                timestamp = ChatMessage.ts_ms()

            self.tim_database.update_usage(user_id, rule, used_tokens, timestamp)

        return True

    def _calculate_remaining_tokens(
        self, llm_rule: LLMRule, usage: Usage | None, caller_id: int
    ) -> tuple[int | None, str]:
        """
        Calculates the tokens left for user to use based on set policies.
        Result is the amount of tokens left for the user to use.
        Per-user policy rules over global settings always when defined.
        This means that if Global Policy has max tokens and User Policy
        does not, the Global Policy will be applied. If both have it defined
        User Policy is applied over Global Policy.
        :param llm_rule: The LLMRule object.
        :param usage: User's usage
        :param caller_id: The user of the tokens.
        :return: The complete amount of tokens left or None in the case of unlimited. If zero, an explanation is added as string.
        """

        user_policy = self.tim_database.get_user_policy(llm_rule, caller_id)
        global_policy = self.tim_database.get_global_policy(llm_rule)

        if global_policy is None:
            raise LookupError("Could not find global policy for this instance")

        up_token_cap_enabled = False
        up_window_enabled = False
        gp_token_cap_enabled = False
        gp_window_enabled = False

        up_max_tokens_per_user = None
        if user_policy is not None:
            up_max_tokens_per_user = user_policy.max_tokens_per_user
            up_time_window_tokens = user_policy.time_window_tokens

            if up_max_tokens_per_user is not None and up_max_tokens_per_user != -1:
                up_token_cap_enabled = True
            if up_time_window_tokens is not None and up_time_window_tokens != -1:
                up_window_enabled = True

        gp_max_tokens_per_user = global_policy.max_tokens_per_user
        gp_time_window_tokens = global_policy.time_window_tokens
        if gp_max_tokens_per_user is not None and gp_max_tokens_per_user != -1:
            gp_token_cap_enabled = True
        if gp_time_window_tokens is not None and gp_time_window_tokens != -1:
            gp_window_enabled = True

        all_used_tokens = 0
        if usage is not None:
            all_used_tokens = usage.used_tokens

        tokens_cap: int | None = None
        if up_token_cap_enabled:
            tokens_cap = up_max_tokens_per_user
        elif gp_token_cap_enabled:
            tokens_cap = gp_max_tokens_per_user

        tokens_allowance_general: int | None = (
            None if tokens_cap is None else (tokens_cap - all_used_tokens)
        )
        window_allowance: int | None = None
        token_pool_cap_allowance: int | None = None

        token_pool_cap = global_policy.token_pool
        if token_pool_cap is not None:
            token_pool_cap_allowance = token_pool_cap - llm_rule.total_tokens_spent

        ts_now_ms = ChatMessage.ts_ms()
        if up_window_enabled:
            window_duration_ms = self._policy_window_as_ms(user_policy)
            ts_begin = ts_now_ms - window_duration_ms
            tokens_used_in_window = self._calculate_usage_in_window(
                usage, ts_begin=ts_begin, ts_end=ts_now_ms
            )
            if user_policy is not None and user_policy.time_window_tokens is not None:
                window_allowance = (
                    user_policy.time_window_tokens - tokens_used_in_window
                )

        elif gp_window_enabled:
            window_duration_ms = self._policy_window_as_ms(global_policy)
            ts_begin = ts_now_ms - window_duration_ms
            tokens_used_in_window = self._calculate_usage_in_window(
                usage, ts_begin=ts_begin, ts_end=ts_now_ms
            )
            if (
                global_policy is not None
                and global_policy.time_window_tokens is not None
            ):
                window_allowance = (
                    global_policy.time_window_tokens - tokens_used_in_window
                )

        if window_allowance is not None and window_allowance < 0:
            window_allowance = 0

        if tokens_allowance_general is not None and tokens_allowance_general < 0:
            tokens_allowance_general = 0

        if token_pool_cap_allowance is not None and token_pool_cap_allowance < 0:
            token_pool_cap_allowance = 0

        message_to_user = ""
        if window_allowance == 0:
            message_to_user = "You have no more chats left for now, come back later"

        if tokens_allowance_general == 0:
            message_to_user = "You have no more chats left"

        if token_pool_cap_allowance == 0:
            message_to_user = "This instance has no more chats left"

        candidates = [
            x
            for x in (
                tokens_allowance_general,
                window_allowance,
                token_pool_cap_allowance,
            )
            if x is not None
        ]

        remaining_tokens = min(candidates) if candidates else None

        return remaining_tokens, message_to_user

    @staticmethod
    def _calculate_usage_in_window(
        usage: Usage,
        ts_begin: int,
        ts_end: int,
    ) -> int:
        """
        Returns the token usage in the table during the given time window.
        Assumes that the timestamps are in rising order within the Usage table.
        Timestamps have to be in milliseconds.
        :param usage: The usage table from which the usage is calculated
        :param ts_begin: the earliest point in time from which to start counting
        :param ts_end: the last point in time to which to stop counting
        :return:
        """
        if usage is None:
            raise ValueError("Given usage is None")

        if ts_begin > ts_end:
            raise ValueError("ts_begin must be less than or equal to ts_end")

        total_tokens = 0
        usage_history = usage.token_usage_history

        for usage_event in reversed(usage_history):
            timestamp = usage_event["timestamp"]

            if timestamp < ts_begin:
                break

            if timestamp <= ts_end:
                total_tokens += usage_event["tokens"]

        return total_tokens

    @staticmethod
    def _policy_window_as_ms(policy: Policy | None) -> int:
        """Gives the duration of the time window of the given policy in milliseconds"""
        if not policy:
            raise TypeError("_policy_window_as_ms: Policy was None")

        time_type = policy.token_time_window_type
        time_value = policy.token_time_window_num

        if time_type is None or time_value is None:
            raise TypeError(
                "_policy_window_as_ms: time_type or/and time_value was None"
            )

        multiplier = 1000
        if time_type == "d":
            multiplier *= 24 * 60 * 60
        if time_type == "h":
            multiplier *= 60 * 60
        if time_type == "min":
            multiplier *= 60

        window_duration_ms = time_value * multiplier

        return window_duration_ms

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

    def _has_rights_one_of(
        self,
        user_id: int,
        documents: list[Document],
        rights: list[str],
    ) -> bool:
        """
        Checks that the user has one of the rights in all the documents.
        Expects that user and given documents exist.

        :param user_id: User for which the right is checked
        :param documents: Document for which the user has or has no right
        :param rights: Rights to check for.
        :raises ValueError: If failed to find rights for documents.
        :raises PermissionError: If insufficient rights for some document.
        :return: True if sufficient rights in all documents.
        """

        for document in documents:
            doc_id = document.id
            right = self.tim_database.check_rights(user_id, doc_id)
            if not right:
                raise ValueError(
                    f"Could not get rights for document [{document}] for user [{user_id}]"
                )

            has_rights = any(right.get(r) is not None for r in rights)
            if document.docinfo is None:
                raise ValueError(
                    f"Could not get rights for document [{document}] for user [{user_id}]"
                )
            if not has_rights:
                raise PermissionError(f"No rights for [{document.docinfo.path}]")

        return True

    @staticmethod
    def _generate_id() -> str:
        return uuid.uuid4().hex

    @staticmethod
    def _parse_rag_mode(mode: str) -> RagMode | None:
        try:
            return RagMode(mode)
        except ValueError:
            return None

    @staticmethod
    def _parse_provider(provider_str: str) -> Provider | None:
        if provider_str not in PluginCore.get_supported_providers():
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
        user_groups = get_groups_by_ids(key.groups)
        return user_groups, key.paths

    def remove_api_key_group(
        self, owner_id: int, public_key: str, group_id: int
    ) -> None:
        self.tim_database.remove_api_key_group(owner_id, public_key, group_id)

    @staticmethod
    def _validate_policy(policy: GenericPolicy) -> None | str:
        """Validates policy. None is returned if everything is ok otherwise error string is returned"""
        valid_time_types = ["d", "h", "min"]
        cap_enabled = policy.token_cap_enabled
        token_cap = policy.token_cap

        window_enabled = policy.time_window_enabled
        time_type = policy.window_unit
        time_value = policy.window_value
        window_token_cap = policy.token_cap_for_window

        if cap_enabled and (token_cap is None or token_cap < 0):
            return f"Given token cap [{token_cap}] cannot be negative"

        if window_enabled and (time_value is None or time_value <= 0):
            return f"Given time value [{time_value}] should be greater than 0"

        if window_enabled and (time_type not in valid_time_types):
            return f"Given time type [{time_type}] is not valid"

        if window_enabled and (window_token_cap is None or window_token_cap <= 0):
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

    def try_access_api_key_row(
        self, user_id: int, public_key: str
    ) -> tuple[LLMRule, Provider, str]:
        key = self.tim_database.access_api_key(user_id, public_key)
        if not key:
            raise PermissionError(f"No access to the API key.")
        provider = PluginCore._parse_provider(str(key.provider))
        if not provider:
            raise ValueError(f"No provider found for API key.")
        return key, provider, str(key.api_key)

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
        row, provider, api_key = self.try_access_api_key_row(user_id, public_key)
        return provider, api_key

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

    def get_models(self, provider_str: str, api_key: str) -> list[ChatModel]:
        provider = self._parse_provider(provider_str)
        if provider is not None:
            return self._get_supported_chat_models(provider, api_key)
        else:
            return []

    def _get_citations(self, blocks: list[tuple[int, int]]) -> list[str]:
        citations: list[str] = []
        for doc_id, block_id in blocks:
            doc = self.tim_database.get_doc_entry_by_id(doc_id)
            if not doc:
                continue
            citations.append(self._get_block_view_addr(doc.path, block_id))
        return list(set(citations))

    @staticmethod
    def _get_block_view_addr(document_path: str, block_id: int) -> str:
        return "/view/" + document_path + "#" + str(block_id)
