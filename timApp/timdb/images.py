from contracts import contract, new_contract
from timdb.timdbbase import TimDbBase, TimDbException, blocktypes
import os

new_contract('bytes', bytes)


class Images(TimDbBase):
    @contract
    def getImagePath(self, image_id: 'int', image_filename: 'str'):
        """Gets the path of an image.
        
        :param image_id: The id of the image.
        :param image_filename: The filename of the image.
        :returns: The path of the image file.
        """

        return os.path.join(self.blocks_path, str(image_id), image_filename)

    @contract
    def getImageRelativePath(self, image_id: 'int', image_filename: 'str'):
        """Gets the relative path of an image.
        
        :param image_id: The id of the image.
        :param image_filename: The filename of the image.
        :returns: The path of the image file.
        """

        return os.path.relpath(self.getImagePath(image_id, image_filename), self.blocks_path)

    @contract
    def saveImage(self, image_data: 'bytes', image_filename: 'str', owner_group_id: 'int') -> 'tuple(int, str)':
        """Saves an image to the database.
        
        :param image_data: The image data.
        :param image_filename: The filename of the image.
        :param owner_group_id: The owner group of the image.
        :returns: A tuple containing the id of the image and its relative path of the form 'image_id/image_filename'.
        """

        # TODO: Check that the file extension is allowed.
        # TODO: Use imghdr module to do basic validation of the file contents.
        # TODO: Should file name be unique among images?
        cursor = self.db.cursor()
        cursor.execute('INSERT INTO Block (description, UserGroup_id, type_id) VALUES (?,?,?)',
                       [image_filename, owner_group_id, blocktypes.IMAGE])
        img_id = cursor.lastrowid
        img_path = self.getImagePath(img_id, image_filename)
        os.makedirs(os.path.dirname(img_path))  # TODO: Set mode.

        with open(img_path, 'wb') as f:
            f.write(image_data)

        self.db.commit()
        return img_id, image_filename

    @contract
    def deleteImage(self, image_id: 'int'):
        """Deletes an image from the database."""

        cursor = self.db.cursor()
        cursor.execute('SELECT description FROM Block WHERE type_id = ? AND id = ?', [blocktypes.IMAGE, image_id])
        image_filename = cursor.fetchone()[0]
        cursor.execute('DELETE FROM Block WHERE type_id = ? AND id = ?', [blocktypes.IMAGE, image_id])
        if cursor.rowcount == 0:
            raise TimDbException('The image was not found.')

        img_path = self.getImagePath(image_id, image_filename)
        os.remove(img_path)
        os.rmdir(os.path.dirname(img_path))

        self.db.commit()

    @contract
    def getImage(self, image_id: 'int', image_filename: 'str') -> 'bytes':
        """Gets the specified image.
        
        :param image_id: The id of the image.
        :param image_filename: The filename of the image.
        :returns: The content of the image.
        """

        with open(self.getImagePath(image_id, image_filename), 'rb') as f:
            return f.read()

    @contract
    def getImages(self) -> 'list(dict)':
        """Gets all the images.
        
        :returns: A list of dictionaries of the form {'id': xx, 'file': 'xx/filename.ext'}.
        """

        cursor = self.db.cursor()
        cursor.execute('SELECT id, id || \'/\' || description AS file FROM Block WHERE type_id = ?', [blocktypes.IMAGE])
        images = self.resultAsDictionary(cursor)
        return images

    def imageExists(self, image_id: 'int', image_filename: 'str'):
        """Returns whether the specified image exists.
        
        :param image_id: The id of the image.
        :param image_filename: The filename of the image.
        :returns: True if the image exists, false otherwise.
        """

        if not self.blockExists(image_id, blocktypes.IMAGE):
            return False

        return os.path.exists(self.getImagePath(image_id, image_filename))
