import sqlite3
import os


def insert_many(conn, count, old_version, new_version):
    c = conn.cursor()
    for i in range(0, count):
        c.execute("""
insert into par_mapping(doc_id, par_index, doc_version, new_index, new_version, certainty) values(?, ?, ?, ?, ?, ?)
    """, [1, i, old_version, i + 1, new_version, 1])


def insert_many_with_commit(conn, count, old_version, new_version):
    insert_many(conn, count, old_version, new_version)
    conn.commit()


def find_rows(conn, doc_id, doc_version):
    c = conn.cursor()
    result = c.execute("""
select new_index, new_version
from par_mapping
where doc_id      = ?
and   doc_version = ?""", [doc_id, doc_version])
    return result.fetchall()


def find_one_row(conn, doc_id, doc_version, par_index):
    c = conn.cursor()
    result = c.execute('''
select new_index, new_version
from par_mapping
where doc_id      = ?
and   doc_version = ?
and   par_index   = ?''', [doc_id, doc_version, par_index])
    return result.fetchall()


if __name__ == '__main__':
    db_name = "sqlite_test.db"
    if os.path.exists(db_name):
        os.remove(db_name)
    conn = sqlite3.connect(db_name)
    cursor = conn.cursor()
    cursor.execute("""
create table par_mapping(
doc_id      INTEGER NOT NULL,
par_index   INTEGER NOT NULL,
doc_version TEXT    NOT NULL,
new_index   INTEGER NOT NULL,
new_version INTEGER NOT NULL,
certainty   INTEGER NOT NULL,

constraint mapping_pk
primary key (doc_id, par_index, doc_version)
)""")
#     cursor.execute("""
# create unique index par_index
# on par_mapping(doc_id, par_index, doc_version)
# """)
    number_of_modifications = 500
    inserts_per_modification = 2000
    for i in range(0, number_of_modifications):
        insert_many(conn, inserts_per_modification, i, i + 1)
        #insert_many_with_commit(conn, 2000, i, i + 1)
        #insert_many_with_commit(conn, 2000, '85136c79cbf9fe36bb9d05d0639c70c265c18d37', '49fdb34f64be0a29af77ae77370a77232c3d6c37')
    conn.commit()
    insert_many_with_commit(conn, inserts_per_modification, number_of_modifications + 1, number_of_modifications + 2)

    find_rows(conn, 1, 123)

    for i in range(0, inserts_per_modification):
        find_one_row(conn, 1, 123, i)
