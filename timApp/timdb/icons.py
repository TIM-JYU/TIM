from timdb.timdbbase import TimDbBase, TimDbException


class Icons(TimDbBase):
    def get_file_name(self, icon_id: int) -> str:
        """Retrieves the filename for the icon.

        :param icon_id: The id of the icon.
        :return: A filename for the icon.
        """
        cursor = self.db.cursor()
        cursor.execute('SELECT filename FROM Icon WHERE id = %s', icon_id)
        results = cursor.fetchall()
        if not results:
            raise TimDbException('No icon with id ' + str(icon_id) + ' found.')
        filename = results.pop()
        return filename
