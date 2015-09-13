__author__ = 'vesal'
import json

# -*- coding:utf-8 -*-
__author__ = 'vesal'
'''
Haetaan csPluginin vastaukset yhdeksi tekstiksi

'''
import sqlite3

#document_id = "113471" # ohj1 demo1
#plugin_id = "
# separator = "==================================================================================\n"
separator = "#-\n**Q:** "
separator_end = "\\\n**A:**\n"
print_user = False

# document_id = "113473"  # ohj1 demo 0
# plugin_id = "kysymysLuennoitsijalle"
document_id = "113471"  # ohj1 demo1
plugin_id = "b3alkukysely"
time_limit = "2015-09-12 22:00:00"


# conn = sqlite3.connect('T:/tim/timApp/tim_files/tim.db')
conn = sqlite3.connect('tim.db')
c = conn.cursor()

params = (document_id + "." + plugin_id, time_limit)
c.execute("""
select u.name, a.task_id, a.content, MAX(a.answered_on) as t
from answer as a, userAnswer as ua, user as u
where a.task_id like ? and ua.answer_id = a.id and u.id = ua.user_id and a.answered_on > ?
group by a.task_id, u.id
order by u.id,a.task_id;
""", params)

lines = []
for row in c:
    header = row[0] + ": " + row[1]
    if not print_user: header = ""
    # print(separator + header)
    line = json.loads(row[2])
    if not isinstance(line, list):
        answ = line.get("usercode", "-")
        print(separator + header + answ + separator_end)

conn.close()


students = {}
times = {}

