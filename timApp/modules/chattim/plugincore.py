import os
import uuid
from dataclasses import dataclass
from unicodedata import normalize, category

from timApp.timdb.dbaccess import get_files_path
from timApp.auth.get_user_rights_for_item import UserItemRights
from timApp.item.item import Item
from timApp.modules.chattim.database_handler import TimDatabase
from timApp.modules.chattim.rag import (
    Rag,
    MessageData,
    RagMode,
    ModelSpec,
    Message,
    Iterable,
    sum_chunks,
    ModelInfo,
)
from typing import Generic, TypeVar, TypedDict

from timApp.modules.chattim.model import ModelResponseChunk, Usage
from timApp.modules.chattim.conversation import ConversationManager, ChatMessage

T = TypeVar("T")
E = TypeVar("E")


@dataclass
class ReqContext:
    input: str
    user_id: int
    document_id: int


@dataclass()
class InstanceAttributes:
    model_id: str
    llm_mode: str
    max_tokens: int
    tim_paths: str


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
    iterable: Iterable[ModelResponseChunk]


class PluginCore:
    rag: Rag = Rag()
    history_manager: ConversationManager
    tim_database: TimDatabase = TimDatabase()
    list_of_instance_ids: list[int] = []  # TODO: poista kun db:ssa rulet
    # TODO: a plugin instance specific variable? In global policy?
    max_input_len: int = 1024

    def __init__(self):
        file_path = get_files_path().as_posix()
        self.history_manager = ConversationManager(file_path)

    def _prepare_chat_request(
        self,
        caller_id: int,
        document_id: int,
        user_input: str,
    ) -> Result[PreparedChatRequest, str]:
        """Prepare the chat request.

        :param caller_id: The id of the caller.
        :param document_id: The id of the document.
        :param user_input: User input.
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

        history = self.get_history(caller_id_str, document_id_str)

        # TODO: remember to fetch with timestamps when the time comes
        chat_history: list[Message] = [
            Message(role=m.role, content=m.content) for m in history
        ]

        # TODO: fetch mode for instance
        mode: RagMode = RagMode.RETRIEVE
        # TODO: No need for this attribute if we have character limit for input? Maybe keep as is for an option
        max_tokens_for_req = 99999

        msg_data = MessageData(
            user_prompt=validated_input,
            context="",
            chat_history=chat_history,
            mode=mode,
            max_tokens=max_tokens_for_req,
        )
        iterable: Iterable[ModelResponseChunk] = self.rag.answer(
            msg_data,
            identifier=document_id,
        )
        prepared = PreparedChatRequest(
            caller_id=caller_id_str,
            document_id=document_id_str,
            user_input=validated_input,
            iterable=iterable,
        )
        return Result(value=prepared)

    def _save_messages(
        self,
        *,
        plugin_id: str,
        caller_id: str,
        user_input: str,
        assistant_msg: str,
        timestamp_user: int,
        timestamp_answer: int,
        usage: Usage | None,
    ) -> None:
        """Save the messages.

        :param plugin_id: The id of the plugin or document.
        :param caller_id: The id of the caller.
        :param user_input: The input of the caller.
        :param assistant_msg: The assistant message.
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
                    content=assistant_msg,
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
        prep = self._prepare_chat_request(caller_id, document_id, user_input)
        if not prep.ok() or not prep.value:
            return Result(error=prep.error)
        p = prep.value

        chunk: ModelResponseChunk = sum_chunks(p.iterable)
        whole_msg = chunk.delta or ""
        usage = chunk.usage

        # TODO: viestit arkistoidaan

        timestamp_answer = ChatMessage.ts_ms()
        self._save_messages(
            plugin_id=p.document_id,
            caller_id=p.caller_id,
            user_input=p.user_input,
            assistant_msg=whole_msg,
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
    ) -> Result[Iterable[ModelResponseChunk], str]:
        timestamp_user = ChatMessage.ts_ms()
        prep = self._prepare_chat_request(caller_id, document_id, user_input)
        if not prep.ok() or not prep.value:
            return Result(error=prep.error)
        p = prep.value

        # TODO: return only the string chunks or the usage as well?
        def gen() -> Iterable[ModelResponseChunk]:
            whole_msg: str = ""
            usage: Usage | None = None

            # Collect the chunk message and usage
            def apply_chunk(c: ModelResponseChunk) -> None:
                nonlocal whole_msg, usage
                if c.delta:
                    whole_msg += c.delta
                if c.usage:
                    usage = c.usage

            try:
                # Yield chunks to the caller
                for chunk in p.iterable:
                    apply_chunk(chunk)
                    yield chunk
            finally:
                # Drain the remaining chunks if the client disconnected mid-stream
                for chunk in p.iterable:
                    apply_chunk(chunk)
                timestamp_answer = ChatMessage.ts_ms()
                self._save_messages(
                    plugin_id=p.document_id,
                    caller_id=p.caller_id,
                    user_input=p.user_input,
                    assistant_msg=whole_msg,
                    timestamp_user=timestamp_user,
                    timestamp_answer=timestamp_answer,
                    usage=usage,
                )

        return Result(value=gen(), error=None)

    def save_instance(
        self, caller_id, document_id: int, instance_settings: InstanceAttributes
    ) -> Result[bool | None, str | None]:
        """
        Create instance if it doesn't exist. New settings are saved if valid
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

        # check that user owns the doc where plugin is being inserted
        if not self._owns_document(caller_id, document_id):
            return Result(None, "Insufficient rights")

        # validate mode
        rag_mode: RagMode | None = self._parse_rag_mode(llm_mode)
        if rag_mode is None:
            return Result(None, f"Invalid rag mode [{llm_mode}] given")

        # validate requested model
        supported_models: dict[str, ModelInfo] = self.rag.get_supported_models()
        if model_id not in supported_models:
            return Result(None, f"Given model [{model_id}] not supported")

        # parse and validate TIM_PATHS
        paths_for_indexing: list[str] = self._parse_paths(tim_paths)
        if not paths_for_indexing and rag_mode == RagMode.RETRIEVE:
            return Result(None, "Give at least one path when using summarizing mode")

        # katotaan oikeudet ja haetaan samalla kaikki doc/folderit indeksointiin myöhemmin (kutsujen minimoimiseksi)
        items = self._fetch_items_by_paths(paths_for_indexing)
        if not items.ok():
            return Result(None, items.error)
        items = items.value

        owns_all = self._owns_all_items(caller_id, items)
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

        api_key = os.getenv("OPENAI_API_KEY")
        spec = ModelSpec(provider="openai", model_id="gpt-4.1-nano", api_key=api_key)
        self.rag.add_model(spec, identifier=document_id)
        # TODO: indeksoinnit pyörimään

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

    def get_history(self, caller_id: str, document_id: str) -> list[ChatMessage]:
        # TODO: fetch with time window
        return self.history_manager.get_history_n(document_id, caller_id, 10)

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
        if document_id in self.list_of_instance_ids:
            return True
        return False

    def _owns_document(self, caller_id: int, document_id: int) -> bool:
        """Expects that you have checked already that doc and user exist, throws otherwise"""

        tim_rights: UserItemRights | None = self.tim_database.check_rights(
            caller_id, document_id
        )

        if tim_rights is None:
            # TODO: proper error?
            raise Exception(
                f"(_owns_document) given user {caller_id} or document {document_id} does not exist"
            )

        is_owner: bool = tim_rights.get("owner")
        if not is_owner:
            return False

        return True

    @staticmethod
    def _owns_item(rights: list[UserItemRights]) -> bool:
        """Expects that you have checked already that doc and user exist, throws otherwise"""

        for right in rights:
            if not right:
                # TODO: proper errors?
                raise Exception(f"(_owns_items) given UserItemRight does not exist")

        return True

    @staticmethod
    def _parse_paths(paths: str) -> list[str]:
        """
        Gets a string, splits it with separator as ",", removes empty entries and trims each entry
        :param paths:
        :return: list of paths or an empty list
        """
        parts = [x.strip() for x in paths.split(",") if x.strip()]

        return parts

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
        return Result(value="ok", error=None)

    def _fetch_items_by_paths(
        self, paths: list[str]
    ) -> Result[list[Item] | None, str | None]:
        """
        Get Items (doc or folder) that correspond with given paths.
        :param paths:
        :return: list[Item] | None
        """
        found_items: list[Item] = []
        for path in paths:
            item = self.tim_database.fetch_item_by_path(path)
            if not item:
                return Result(error=f"Given path [{path}] does not exist")
            found_items.append(item)

        return Result(value=found_items)

    def _owns_all_items(
        self, user_id: int, items: list[Item]
    ) -> Result[bool | None, str | None]:
        """
        Checks for all items that the given user owns them
        :param user_id: User for which the right is checked
        :param items: Item for which the user has or has no right
        :return: If all items are owned [True, None]
                 If no items are provided [True, None]
                 If not all items are owned [False, msg on item not owned]
                 if error happens [None, error_msg]
        """

        for item in items:
            right = self.tim_database.get_rights_per_item(user_id, item)
            if not right:
                return Result(
                    error=f"Could not get rights for item [{item}] for user [{user_id}]"
                )

            is_owner: bool = right.get("owner")
            if not is_owner:
                return Result(False, f"No owner rights for [{item.path}]")

        return Result(value=True)

    @staticmethod
    def _generate_id() -> str:
        return uuid.uuid4().hex

    @staticmethod
    def _parse_tim_paths(paths: str):
        pass

    @staticmethod
    def _parse_rag_mode(mode: str) -> RagMode | None:
        try:
            return RagMode(mode)
        except ValueError:
            print(f"Invalid rag mode given: {mode}")
            return None

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
