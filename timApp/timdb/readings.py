from contracts import contract
from timdb.timdbbase import TimDbBase


class Readings(TimDbBase):
    @contract
    def __init__(self, db_path : 'Connection', files_root_path : 'str', type_name : 'str', current_user_name : 'str'):
        """Initializes TimDB with the specified database and root path.

        :param db_path: The path of the database file.
        :param files_root_path: The root path where all the files will be stored.
        """
        TimDbBase.__init__(self, db_path, files_root_path, type_name, current_user_name)
        #self.ec = EphemeralClient(EPHEMERAL_URL)

    @contract
    def getReadings(self, user_id : 'int', doc_id : 'int', doc_ver : 'str') -> 'list(dict)':
        """Gets the reading info for a document for a user.
        
        :param user_id: The id of the user whose readings will be fetched.
        :param block_id: The id of the block whose readings will be fetched.
        """
        cursor = self.db.cursor()

        cursor.execute(
            'select par_index, doc_ver from ReadParagraphs where user_id = ? and doc_id = ?',
            [user_id, doc_id])
        readings = self.resultAsDictionary(cursor)

        # Check for modifications
        db_modified = False
        for reading in readings:
            read_ver = reading['doc_ver']
            reading['status'] = 'read'

            if read_ver != doc_ver:
                # Document has been modified, see if the paragraph has changed
                read_par = reading['par_index']
                current_ver = read_ver
                modified = False
                num_links = 0
                print('Paragraph %d refers to old version, trying to find mappings.' % read_par)

                while current_ver != doc_ver:
                    current_par = read_par
                    cursor.execute(
                        """
                        select new_ver, new_index, modified
                        from ParMappings
                        where doc_id = ? and doc_ver = ? and par_index = ?
                        and new_ver is not null and new_index is not null
                        """, [doc_id, current_ver, current_par])
                    mappings = self.resultAsDictionary(cursor)
                    if len(mappings) > 0:
                        print('Found a mapping: %s(%d) -> %s(%d)' %
                              (current_ver, current_par, mappings[0]['new_ver'], mappings[0]['new_index']))

                        current_ver = mappings[0]['new_ver']
                        current_par = mappings[0]['new_index']
                        modified |= mappings[0]['modified'] == 'True'
                        num_links += 1
                    else:
                        print('Loose end: document %d, paragraph %s' % (doc_id, read_par))
                        reading['status'] = 'modified'
                        break
                print('End while.')
                print('num_links = %d, current_ver = %s, doc_ver = %s, modified = %s' %
                      (num_links, current_ver, doc_ver, str(modified)))
                if num_links > 1 and current_ver == doc_ver:
                    # Flatten mappings to speed up future queries
                    # a -> b -> c becomes a -> c
                    print('Updating mapping: %s(%s) -> %s(%s)' %
                          (read_ver, read_par, current_ver, current_par))
                    cursor.execute(
                        """
                        update ParMappings
                        set new_ver = ?, new_index = ?, modified = ?
                        where doc_id = ? and doc_ver = ? and par_index = ?
                        """, [current_ver, current_par, str(modified), doc_id, read_ver, read_par])
                    db_modified = True
                print("")
                reading['par_index'] = current_par
                if modified:
                    reading['status'] = 'modified'
                else:
                    reading['doc_ver'] = current_ver

        if db_modified:
            self.db.commit()

        return readings
        
    @contract
    def setAsRead(self, user_id: 'int', doc_id : 'int', doc_ver : 'str', paragraph_specifier : 'int'):
        cursor = self.db.cursor()

        # Remove previous markings for this paragraph to reduce clutter
        cursor.execute(
            'delete from ReadParagraphs where user_id = ? and doc_id = ? and par_index = ?',
            [user_id, doc_id, paragraph_specifier])

        # Set current version as read
        cursor.execute(
            'insert into ReadParagraphs (user_id, doc_id, doc_ver, par_index, timestamp) values (?, ?, ?, ?, CURRENT_TIMESTAMP)',
            [user_id, doc_id, doc_ver, paragraph_specifier])

        self.db.commit()
