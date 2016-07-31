import os

from persistent_queue import PersistentQueue
from typing import Callable, Dict, Optional


class GroupingQueue(PersistentQueue):
    def __init__(self, directory: str, grouping_function: Callable[[Dict, Dict], Optional[Dict]]):
        super().__init__(directory)
        self.grouping_function = grouping_function

    def enqueue(self, element: Dict) -> str:
        for i in range(0, len(self)):
            existing_element = self[i]
            new_element = self.grouping_function(existing_element, element)
            if new_element is not None:
                self[i] = new_element
                return os.path.relpath(self.get_file_at_index(i), self.dir)

        return super().enqueue(element)
