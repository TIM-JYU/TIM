import json

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

    def __len__(self):
        return len(listnormalfiles(self.dir))

    def get_first_filename(self) -> str:
        return os.path.join(self.dir, 'first')

    def get_last_filename(self) -> str:
        return os.path.join(self.dir, 'last')

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
        node = self.__read_element_dict(filename)

        # Next-Msg is only for backwards compatibility, can be removed later
        return node.get(self.NEXTFILEATTR) or node.get('Next-Msg')

    def set_next(self, filename: str, next_filename: str):
        msg = self.__read_element_dict(filename)
        msg[self.NEXTFILEATTR] = next_filename
        self.__write_element_dict(msg, filename)

    def enqueue(self, element: dict) -> str:
        this_absfile, this_relfile = get_random_filenames(self.dir)
        element[self.NEXTFILEATTR] = ''
        self.__write_element_dict(element, this_absfile)

        first_file = self.get_first_filename()
        last_file = self.get_last_filename()

        if not os.path.islink(first_file):
            files = os.listdir(self.dir)
            if len(files) > 1:
                os.symlink(files[0], first_file)
            else:
                os.symlink(this_relfile, first_file)

        if os.path.islink(last_file):
            self.set_next(last_file, this_relfile)
            os.unlink(last_file)

        os.symlink(this_relfile, last_file)
        return this_relfile

    def dequeue(self) -> Optional[dict]:
        first_file = self.get_first_filename()
        if not os.path.isfile(first_file):
            return None

        element = self.__read_element_dict(first_file)
        next_file = element.pop(self.NEXTFILEATTR, '')

        os.unlink(os.path.join(self.dir, os.readlink(first_file)))
        os.unlink(first_file)

        if next_file == '':
            os.unlink(self.get_last_filename())
        else:
            os.symlink(next_file, first_file)

        return element

    def is_empty(self) -> bool:
        return not os.path.islink(self.get_first_filename())

