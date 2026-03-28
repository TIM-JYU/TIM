import os
import time
import uuid
from dataclasses import dataclass

from timApp.auth.get_user_rights_for_item import UserItemRights
from timApp.modules.chattim.database_handler import TimDatabase
from timApp.modules.chattim.rag import (
    Rag,
    MessageData,
    RagMode,
    ModelSpec,
    Message,
    Iterable,
    sum_chunks,
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


class PluginCore:
    rag: Rag = Rag()
    history_manager: ConversationManager = ConversationManager()
    tim_database: TimDatabase = TimDatabase()

    # TODO: palautetaan token usage tätä kautta tai muualta?
    def chat_request(self, req_context: ReqContext) -> Result[str | None, str | None]:
        document_id = req_context.document_id
        caller_id = req_context.user_id
        user_input = req_context.input

        if not self._instance_exists(document_id):
            return Result(error=f"No instance with id {document_id} exists")

        self.create_instance(caller_id, document_id)  # TODO: remove

        # policy check
        result: Result[str | None, str | None] = self._student_policy_check(
            caller_id, document_id
        )

        if not result.ok():
            return result

        # TODO: luo keskustelu jos ei olemassa, lisää conv_id
        conversation_id = self._generate_id()

        timestamp_before = time.time_ns()
        plugin_id = str(document_id)
        history = self.history_manager.get_history(
            plugin_id, str(caller_id), conversation_id, 10
        )

        # TODO: remember to fetch with timestamps when the time comes
        chat_history: list[Message] = [
            Message(role=m.role, content=m.content) for m in history
        ]
        # TODO: fetch mode for instance
        mode: RagMode = RagMode.RETRIEVE
        # TODO: No need for this attribute if we have character limit for input? Maybe keep as is for an option
        max_tokens_for_req = 99999

        msg_data = MessageData(
            user_prompt=user_input,
            context="",
            chat_history=chat_history,
            mode=mode,
            max_tokens=max_tokens_for_req,
        )
        iterable: Iterable[ModelResponseChunk] = self.rag.answer(
            msg_data,
            identifier=document_id,
        )

        chunk: ModelResponseChunk = sum_chunks(iterable)
        whole_msg = chunk.delta
        usage = chunk.usage

        # TODO: viestit arkistoidaan

        timestamp_after = time.time_ns()
        self.history_manager.append_messages(
            plugin_id,
            str(caller_id),
            conversation_id,
            [
                ChatMessage(
                    role="user",
                    content=user_input,
                    usage=None,
                    timestamp=timestamp_before,
                ),
                ChatMessage(
                    role="assistant",
                    content=whole_msg,
                    usage=usage,
                    timestamp=timestamp_after,
                ),
            ],
        )

        return Result(value=whole_msg, error=None)

    def create_instance(self, caller_id, document_id: int):
        # TODO: tarkista onko teacher jo tietokanssa, jos ei niin lisää
        # TODO: tarkista tässä oikeudet

        # TODO: tälle joku helpompi tapa vetää spec infosta tai jotain (tämä muutenkin väliaikainen)
        supp_models_openai = self.rag.get_supported_models("openai")
        supp_models_dummy = self.rag.get_supported_models("dummy")
        openai_api_key = None
        if openai_api_key is not None:
            model = supp_models_openai.get("gpt-4.1-nano")
            spec = ModelSpec(
                provider=model.provider,
                model_id=model.model_id,
                api_key=openai_api_key,
            )
        else:
            model = supp_models_dummy.get("dummy")
            spec = ModelSpec(
                provider=model.provider,
                model_id=model.model_id,
                api_key="dummy_api_key",
            )
        self.rag.add_model(spec, identifier=document_id)
        # TODO: lisää tietokantaan policyineen
        # TODO: indeksoinnit pyörimään

    def remove_instance(self, caller_id: str, document_id: int):
        pass

    def modify_instance_pages(
        self, caller_id: str, document_id: int, indexable_paths: list[str]
    ):
        pass

    def get_history(self, caller_id: str, document_id: int):
        pass

    def change_chatmode(self, caller_id: str, document_id: int, mode: RagMode):
        pass

    def set_globalpolicy(self, caller_id: str, document_id: int, policy: GlobalPolicy):
        pass

    def set_studentpolicy(
        self, caller_id: str, document_id: int, policy: StudentPolicy
    ):
        pass

    def _instance_exists(self, document_id) -> bool:
        # ideana todnäk pitää muistissa tiedetyt instanssi-idt jottei haeta aina tietokannalta turhaan
        # TODO: impl
        return True

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

    def _generate_id(self) -> str:
        return uuid.uuid4().hex
