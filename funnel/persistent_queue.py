import json
import shutil

from fileutils import *
from typing import Dict, Optional, Tuple


class PersistenceException(Exception):
    def __init__(self, msg):
        self.msg = msg

    def __str__(self):
        return self.msg


class PersistenceWarning(Exception):
    def __init__(self, msg):
        self.msg = msg

    def __str__(self):
        return self.msg


class PersistentQueue():
    NEXTFILEATTR = '_next_file'
    OLD_NEXTFILEATTR = 'Next-Msg' # for backwards compatibility

    def __init__(self, directory: str):
        self.dir = directory
        os.makedirs(directory, exist_ok=True)

    def assert_dir(self):
        if not os.path.exists(self.dir):
            raise PersistenceException('Queue directory not found')

    def __len__(self):
        return len(listnormalfiles(self.dir))

    def get_first_filename(self) -> str:
        return os.path.join(self.dir, 'first')

    def get_last_filename(self) -> str:
        return os.path.join(self.dir, 'last')

    def get_file_at_index(self, index: int) -> str:
        self.assert_dir()

        if index < 0 or index >= len(self):
            raise IndexError()

        i = 0
        name = self.get_first_filename()

        while i < index:
            name = os.path.join(self.dir, self.get_next(name))
            i += 1

        return name

    def __getitem__(self, index: int):
        self.assert_dir()
        return self.__read_element_dict(self.get_file_at_index(index))

    def __setitem__(self, index: int, value: dict):
        self.assert_dir()
        name = self.get_file_at_index(index)
        next_name = self.get_next(name)
        new_element = value.copy()
        new_element[self.NEXTFILEATTR] = next_name
        self.__write_element_dict(new_element, name)

    def delete(self):
        self.assert_dir()
        shutil.rmtree(self.dir)

    @classmethod
    def __read_element_dict(cls, filename: str) -> Dict[str, str]:
        try:
            with open(filename, 'r') as f_src:
                return json.loads(f_src.read())
        except Exception as e:
            raise PersistenceException('Error reading {} as json: {}'.format(filename, str(e))) from e

    @classmethod
    def __write_element_dict(cls, element: Dict[str, str], filename: str):
        try:
            with open(filename, 'w') as f_dest:
                f_dest.write(json.dumps(element))
        except Exception as e:
            raise PersistenceException('Error writing {} as json: {}'.format(filename, str(e))) from e

    def get_next(self, filename: str) -> Optional[str]:
        self.assert_dir()
        node = self.__read_element_dict(filename)

        # OLD_NEXTFILEATTR is only for backwards compatibility, can be removed later
        return node.get(self.NEXTFILEATTR) or node.get(self.OLD_NEXTFILEATTR)

    def set_next(self, filename: str, next_filename: str):
        self.assert_dir()
        msg = self.__read_element_dict(filename)
        msg[self.NEXTFILEATTR] = next_filename
        self.__write_element_dict(msg, filename)

    def enqueue(self, element: Dict) -> str:
        self.assert_dir()
        this_absfile, this_relfile = get_random_filenames(self.dir)
        element[self.NEXTFILEATTR] = ''
        self.__write_element_dict(element, this_absfile)

        first_file = self.get_first_filename()
        last_file = self.get_last_filename()

        if not os.path.islink(first_file):
            files = listnormalfiles(self.dir)
            if len(files) > 0:
                os.symlink(files[0], first_file)
            else:
                os.symlink(this_relfile, first_file)

        if os.path.islink(last_file):
            self.set_next(last_file, this_relfile)
            os.unlink(last_file)

        os.symlink(this_relfile, last_file)
        return this_relfile

    def dequeue(self) -> Optional[dict]:
        self.assert_dir()
        first_file = self.get_first_filename()
        if not os.path.isfile(first_file):
            return None

        element = self.__read_element_dict(first_file)
        next_file = element.pop(self.NEXTFILEATTR)

        os.unlink(os.path.join(self.dir, os.readlink(first_file)))
        os.unlink(first_file)

        if not next_file:
            os.unlink(self.get_last_filename())
        else:
            os.symlink(next_file, first_file)

        return element

    def is_empty(self) -> bool:
        return not os.path.islink(self.get_first_filename())

