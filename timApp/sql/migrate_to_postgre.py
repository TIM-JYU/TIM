import argparse
import os

import sqlite3
from typing import Optional, Dict

import psycopg2

from routes.logger import log_info


def perform_migration(sqlite_path: str, postgre_path: str):
    sq3 = sqlite3.connect(sqlite_path)
    pg = psycopg2.connect(postgre_path)
    sq3c = sq3.cursor()
    pgc = pg.cursor()
    log_info('Migration from SQLite3 to PostgreSQL started.')

    migrate_table(sq3c, pgc, 'user', 'useraccount')
    migrate_table(sq3c, pgc, 'accesstype')
    migrate_table(sq3c, pgc, 'answer', placeholders={'valid': 'cast(%s as boolean)'})

    migrate_table(sq3c, pgc, 'answertag')
    migrate_table(sq3c, pgc, 'askedjson', id_column='asked_json_id')
    migrate_table(sq3c, pgc, 'usergroup')
    migrate_table(sq3c, pgc, 'block', placeholders={'created': "coalesce(%s, '2014-06-01 00:00:00'::timestamp)"})

    migrate_table(sq3c, pgc, 'lecture', extra_clause='WHERE doc_id IN (SELECT id FROM block)', id_column='lecture_id')
    migrate_table(sq3c, pgc, 'askedquestion',
                  placeholders={'asked_time': "to_timestamp(%s, 'YYYY-MM-DD HH24:MI:SS:US')"},
                  id_column='asked_id',
                  extra_clause='WHERE lecture_id IN (SELECT lecture_id FROM lecture WHERE doc_id IN (SELECT id FROM block))')

    migrate_table(sq3c, pgc, 'blockaccess', extra_clause='WHERE block_id IN (SELECT id FROM block)', id_column=None)
    migrate_table(sq3c, pgc, 'answerupload', id_column=None)
    migrate_table(sq3c, pgc, 'docentry', id_column=None, placeholders={'public': 'cast(%s as boolean)'})
    migrate_table(sq3c, pgc, 'folder', id_column=None)
    migrate_table(sq3c, pgc, 'lectureusers', id_column=None)
    migrate_table(sq3c, pgc, 'message', id_column='msg_id',
                  extra_clause='WHERE lecture_id IN (SELECT lecture_id FROM lecture WHERE doc_id IN (SELECT id FROM block))')
    migrate_table(sq3c, pgc, 'newuser', id_column=None,
                  placeholders={'created': "coalesce(%s, '2014-06-01 00:00:00'::timestamp)"})
    migrate_table(sq3c, pgc, 'notification', id_column=None,
                  placeholders={'email_doc_modify': 'cast(%s as boolean)',
                                'email_comment_add': 'cast(%s as boolean)',
                                'email_comment_modify': 'cast(%s as boolean)'},
                  extra_clause='WHERE doc_id IN (SELECT id FROM block)')
    migrate_table(sq3c, pgc, 'question', id_column='question_id', extra_clause='WHERE doc_id IN (SELECT id FROM block)')
    migrate_table(sq3c, pgc, 'readparagraphs', id_column=None, extra_clause='WHERE doc_id IN (SELECT id FROM block)')
    migrate_table(sq3c, pgc, 'translation', id_column=None)
    migrate_table(sq3c, pgc, 'useranswer', extra_clause='WHERE user_id IN (SELECT id FROM user)')
    migrate_table(sq3c, pgc, 'usergroupmember', id_column=None)
    migrate_table(sq3c, pgc, 'usernotes')
    migrate_table(sq3c, pgc, 'version')
    migrate_table(sq3c, pgc, 'lectureanswer', id_column='answer_id',
                  placeholders={'answered_on': "to_timestamp(%s, 'YYYY-MM-DD HH24:MI:SS:US')"},
                  extra_clause='WHERE question_id IN (SELECT question_id FROM question WHERE doc_id IN (SELECT id FROM block)) '
                               'AND lecture_id IN (SELECT lecture_id FROM lecture WHERE doc_id IN (SELECT id FROM block))')
    sq3.close()
    os.rename(sqlite_path, sqlite_path + '.bak')
    pg.commit()
    log_info('Migration finished.')


def migrate_table(sq3c, pgc,
                  old_table: str,
                  new_table: Optional[str] = None,
                  placeholders: Optional[Dict[str, str]] = None,
                  id_column: Optional[str] = 'id',
                  extra_clause=''):
    log_info('Migrating table {}...'.format(old_table))
    if new_table is None:
        new_table = old_table
    sq3c.execute("SELECT * FROM {} {}".format(old_table, extra_clause))
    columns = list(map(lambda x: x[0], sq3c.description))
    if placeholders is None:
        placeholders = {}
    for c in columns:
        if c not in placeholders:
            placeholders[c] = '%s'

    column_str = '({})'.format(','.join(columns))
    placeholder_list = [placeholders[c] for c in columns]
    template_str = '({})'.format(','.join(placeholder_list))

    i = 0
    for row in sq3c:
        pgc.execute(
            "INSERT INTO {} {} "
            "VALUES {}".format(new_table, column_str, template_str), row)
        i += 1
    if id_column is not None:
        update_seq_val(pgc, new_table, id_column)
    log_info('Migrated table {} ({} rows)'.format(old_table, i))


def update_seq_val(pgc, tablename, id_col_name='id'):
    pgc.execute('SELECT MAX({}) FROM {}'.format(id_col_name, tablename))
    max_id = pgc.fetchone()[0]
    pgc.execute("SELECT setval('{}_{}_seq', %s)".format(tablename, id_col_name), (max_id,))


if __name__ == '__main__':
    parser = argparse.ArgumentParser('Migrates an SQLite3 database to PostgreSQL.')
    parser.add_argument('-f', help='the path to the SQLite3 database', required=True)
    parser.add_argument('-t', help='the PostgreSQL connection string', required=True)
    opts = parser.parse_args()
    perform_migration(opts.f, opts.t)
