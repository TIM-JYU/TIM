from contracts import contract, new_contract
from timdb.timdbbase import TimDbBase, TimDbException, blocktypes
import os

new_contract('bytes', bytes)


class Uploads(TimDbBase):
    @contract
    def get_file_path(self, path: 'str', filename: 'str'):
        """Gets the path of for a file.  Creates a new dir by file_id one creater than number of files in dir

        :param path: The path for the file
        :param filename: The filename of the image.
        :returns: The id and path of the image file.
        """
        p = os.path.join(self.blocks_path, path)
        if not os.path.exists(p):
            os.makedirs(p)
        file_id = len(os.listdir(p))+1
        relpath = os.path.join(path, str(file_id))
        p = os.path.join(self.blocks_path, relpath)
        os.makedirs(p)

        return os.path.join(relpath, filename), os.path.join(p, filename)


    @contract
    def save_file(self, file_data: 'bytes', path: 'str', filename: 'str', owner_group_id: 'int') -> 'str':
        """Saves a file to the database.

        :param file_data: The  data.
        :param path: path for the file
        :param filename: The filename
        :param owner_group_id: The owner group of the file.
        :returns: the relative path of the form 'path/file_id/filename'.
        """

        # TODO: Check that the file extension is allowed.
        # TODO: Use imghdr module to do basic validation of the file contents.
        # TODO: Should file name be unique among images?
        cursor = self.db.cursor()
        relfilename, file_path = self.get_file_path(path, filename)
        cursor.execute('INSERT INTO Block (description, UserGroup_id, type_id) VALUES (?,?,?)',
                       [relfilename, owner_group_id, blocktypes.UPLOAD])

        with open(file_path, 'wb') as f:
            f.write(file_data)

        self.db.commit()
        return relfilename

    @contract
    def get_file(self, relfilename: 'str') -> 'bytes':
        """Gets the specified file.

        :param relfilename: The filename.
        :returns: The content of the file.
        """

        p = os.path.join(self.blocks_path, relfilename)
        with open(p, 'rb') as f:
            return f.read()

