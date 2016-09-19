import os
import sqlite3
from timdb.timdb2 import TimDb
from timdb.blocktypes import blocktypes


def create_tables(db):
    c = db.cursor()
    c.execute("SELECT EXISTS(SELECT name from sqlite_master WHERE type = 'table' AND name = 'Folder')")
    if not int(c.fetchone()[0]):
        c.execute(
            """
            CREATE TABLE Folder (
              id        INTEGER       NOT NULL,
              name      VARCHAR(50)   NOT NULL,
              location  VARCHAR(512)  NOT NULL,

              CONSTRAINT Folder_PK
              PRIMARY KEY (id),
              CONSTRAINT Folder_id
              FOREIGN KEY (id)
              REFERENCES Block (id)
              ON DELETE CASCADE
              ON UPDATE CASCADE
            );
            """
        )
        db.commit()

    c.execute("SELECT EXISTS(SELECT NAME from sqlite_master WHERE type = 'table' AND name = 'DocEntry')")
    if not int(c.fetchone()[0]):
        c.execute(
            """
            CREATE TABLE DocEntry (
              id     INTEGER      NOT NULL,
              name   VARCHAR(512) NOT NULL,
              public INTEGER      NOT NULL DEFAULT 1,

              CONSTRAINT DocEntry_PK
              PRIMARY KEY (name),
              CONSTRAINT DocEntry_id
              FOREIGN KEY (id)
              REFERENCES Block (id)
              ON DELETE CASCADE
              ON UPDATE CASCADE
            );
            """
        )
        db.commit()


def update_tables(db):
    cursor = db.cursor()
    timdb = TimDb(db_path='tim_files/tim.db', files_root_path='tim_files')

    cursor.execute("SELECT id, description as name FROM Block WHERE type_id = ?", [blocktypes.FOLDER])
    for folder_id, folder_name in cursor.fetchall():
        print('Folder id {}: {}'.format(folder_id, folder_name))
        if not timdb.folders.exists(folder_id):
            print('...Creating folder(s)')
            owner_id = timdb.users.get_owner_group(folder_id).id
            timdb.folders.create(folder_name, owner_id)

    print()

    for doc in timdb.documents.get_documents():
        print('Doc id {}: {}'.format(doc['id'], doc['name']))
        folder_name, _ = timdb.folders.split_location(doc['name'])
        if folder_name == '':
            continue
        if timdb.folders.get_folder_id(folder_name) is None:
            print('...Creating folder(s)')
            owner_id = timdb.users.get_owner_group(doc['id']).id
            folder_id = timdb.folders.create(folder_name, owner_id)


if __name__ == "__main__":
    db = sqlite3.connect('tim_files/tim.db')
    create_tables(db)
    update_tables(db)
    db.close()
