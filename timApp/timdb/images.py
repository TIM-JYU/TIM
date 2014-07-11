
from contracts import contract, new_contract
from timdb.timdbbase import TimDbBase, TimDbException, blocktypes
import os

new_contract('bytes', bytes)

class Images(TimDbBase):

    @contract
    def __init__(self, db_path : 'Connection', files_root_path : 'str'):
        """Initializes TimDB with the specified database and root path.
        
        :param db_path: The path of the database file.
        :param files_root_path: The root path where all the files will be stored.
        """
        TimDbBase.__init__(self, db_path, files_root_path)
    
    @contract
    def getImagePath(self, image_id : 'int', image_filename : 'str'):
        """Gets the path of an image.
        
        :param image_id: The id of the image.
        :param image_filename: The filename of the image.
        :returns: The path of the image file.
        """
        
        return os.path.join(self.files_root_path, 'img', str(image_id), image_filename)
    
    @contract
    def saveImage(self, image_data : 'bytes', image_filename : 'str'):
        """Saves an image to the database."""
        
        # TODO: Check that the file extension is allowed.
        # TODO: Should file name be unique among images?
        # TODO: User group id should be a parameter.
        cursor = self.db.cursor()
        cursor.execute('insert into Block (description, UserGroup_id, type_id) values (?,?,?)', [image_filename, 0, blocktypes.IMAGE])
        img_id = cursor.lastrowid
        
        with open(self.getImagePath(img_id, image_filename), 'wb') as f:
            f.write(image_data)
        
        self.db.commit()
        #TODO: Return image filename (and id if file names don't have to be unique).
        return

    @contract
    def deleteImage(self, image_id : 'int'):
        """Deletes an image from the database."""
        
        #TODO: Check that the user has right to delete image.
        cursor = self.db.cursor()
        cursor.execute('select description from Block where type_id = ? and id = ?', [blocktypes.IMAGE, image_id])
        image_filename = cursor.fetchone()[0]
        cursor.execute('delete from Block where type_id = ? and id = ?', [blocktypes.IMAGE, image_id])
        if cursor.rowcount == 0:
            raise
            #TODO: Raise error if no image was deleted.
        
        os.remove(self.getImagePath(image_id, image_filename))
        
        self.db.commit()
        