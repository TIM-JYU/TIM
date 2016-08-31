# Python:

import sqlite3
import json

doc_id = input("Doc id: ")

try:
    doc_id = int(doc_id)
except ValueError:
    print("Invalid doc id: %s" % doc_id)
    exit(1)

outfile = "doc_%d_questions.txt" % doc_id

conn = sqlite3.connect('tim.db')
c = conn.cursor()

c.execute('SELECT * FROM Question WHERE doc_id == ?', [doc_id])
questions = c.fetchall()

target = open(outfile, 'w')
par_id = ''
for question in questions:
    question_parsed = json.loads(question[3])
    question_json = json.dumps(question_parsed, indent=4, sort_keys=True)
    lines = ['    ' + line for line in question_json.splitlines()]
    question_json = '\n'.join(lines)
    title = question_parsed['questionText']
    new_par_id = question[2]
    if par_id != new_par_id:
        par_id = new_par_id
        target.write('\n\nPAR_ID: %s\n' % par_id)

    target.write('```{question="%s"}\n' % title)
    target.write('points: %s\n' % question[4])
    target.write('expl: %s\n' % question[5])
    target.write('json:\n')
    target.write(question_json + '\n')
    target.write('```\n\n')

c.close()
conn.close()
