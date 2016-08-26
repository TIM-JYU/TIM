from typing import List, Tuple

from timdb.timdbbase import TimDbBase, TimDbException, blocktypes
import os
import datetime


class Files(TimDbBase):
    def getFilePath(self, file_id: int, file_filename: str):
        """Gets the path of an file.
        
        :param file_id: The id of the file.
        :param file_filename: The filename of the file.
        :returns: The path of the file.
        """

        return os.path.join(self.blocks_path, str(file_id), file_filename)

    def getFileRelativePath(self, file_id: int, file_filename: str):
        """Gets the relative path of a file.
        
        :param file_id: The id of the file.
        :param file_filename: The filename of the file.
        :returns: The path of the file.
        """

        return os.path.relpath(self.getFilePath(file_id, file_filename), self.blocks_path)

    def saveFile(self, file_data: 'bytes', file_filename: str, owner_group_id: int) -> Tuple[int, str]:
        """Saves a file to the database.
        
        :param file_data: The file data.
        :param file_filename: The filename of the file.
        :param owner_group_id: The owner group of the file.
        :returns: A tuple containing the id of the file and its relative path of the form 'file_id/file_filename'.
        """

        # TODO: Check that the file extension is allowed.
        # TODO: Use imghdr module to do basic validation of the file contents.
        # TODO: Should file name be unique among files?
        cursor = self.db.cursor()
        cursor.execute('INSERT INTO Block (description, UserGroup_id, type_id, created) VALUES (?,?,?,?)',
                       [file_filename, owner_group_id, blocktypes.FILE, datetime.datetime.utcnow()])
        img_id = cursor.lastrowid
        img_path = self.getFilePath(img_id, file_filename)
        os.makedirs(os.path.dirname(img_path))  # TODO: Set mode.

        with open(img_path, 'wb') as f:
            f.write(file_data)

        self.db.commit()
        return img_id, file_filename

    def deleteFile(self, file_id: int):
        """Deletes an file from the database."""

        cursor = self.db.cursor()
        cursor.execute('SELECT description FROM Block WHERE type_id = ? AND id = ?', [blocktypes.FILE, file_id])
        file_filename = cursor.fetchone()[0]
        cursor.execute('DELETE FROM Block WHERE type_id = ? AND id = ?', [blocktypes.FILE, file_id])
        if cursor.rowcount == 0:
            raise TimDbException('The file was not found.')

        img_path = self.getFilePath(file_id, file_filename)
        os.remove(img_path)
        os.rmdir(os.path.dirname(img_path))

        self.db.commit()

    def getFile(self, file_id: int, file_filename: str) -> bytes:
        """Gets the specified file.
        
        :param file_id: The id of the file.
        :param file_filename: The filename of the file.
        :returns: The content of the file.
        """

        with open(self.getFilePath(file_id, file_filename), 'rb') as f:
            return f.read()

    def getFiles(self) -> List[dict]:
        """Gets all the files.
        
        :returns: A list of dictionaries of the form {'id': xx, 'file': 'xx/filename.ext'}.
        """

        cursor = self.db.cursor()
        cursor.execute('SELECT id, id || \'/\' || description AS file FROM Block WHERE type_id = ?', [blocktypes.FILE])
        files = self.resultAsDictionary(cursor)
        return files

    def fileExists(self, file_id: int, file_filename: str):
        """Returns whether the specified file exists.
        
        :param file_id: The id of the file.
        :param file_filename: The filename of the file.
        :returns: True if the file exists, false otherwise.
        """

        if not self.blockExists(file_id, blocktypes.FILE):
            return False

        return os.path.exists(self.getFilePath(file_id, file_filename))
