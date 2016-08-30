"""
The module contains the database functions related to icons that are used in velps and annotations.
Icons are be retrieved from the database through this module. The module is not yet used in production.

:authors: Joonas Lattu, Petteri Palojärvi
:copyright: 2016 Timber project members
:version: 1.0.0

"""

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
