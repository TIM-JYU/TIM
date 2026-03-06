from enum import Enum
from dataclasses import dataclass


# Nämä pitää siirtää myöhemmin jonnekkin muualle
@dataclass
class GlobalPolicy:
    pass


@dataclass
class StudentPolicy:
    pass


class ChatMode(Enum):
    Freeform = 0
    Summarizing = 1


class PluginCore:
    def chat_request(self, caller_id: str, document_id: int):
        pass

    def create_instance(self, caller_id, document_id: int):
        pass

    def remove_instance(self, caller_id: str, document_id: int):
        pass

    def modify_instance_pages(
        self, caller_id: str, document_id: int, indexable_paths: list[str]
    ):
        pass

    def get_history(self, caller_id: str, document_id: int):
        pass

    def change_chatmode(self, caller_id: str, document_id: int, mode: ChatMode):
        pass

    def set_globalpolicy(self, caller_id: str, document_id: int, policy: GlobalPolicy):
        pass

    def set_studentpolicy(
        self, caller_id: str, document_id: int, policy: StudentPolicy
    ):
        pass
