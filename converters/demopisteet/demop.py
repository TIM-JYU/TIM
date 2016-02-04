# -*- coding:utf-8 -*-
__author__ = 'vesal'
'''
Lasketaan opiskelijoiden pisteet tiedostosta joka on muotoa:

vesal|113073.t0|[true, false, false, false, false, true, true, true]|2015-03-04 08:43:28
vesal|113073.t1|[true, null, true, true, null, null, true, true, true, true,...
vesal|113073.t2|[false, null, null, null, null, null, null, null]|2015-03-06 09:48:08

Tiedosto on tehty SQL-lauseella:

select u.name, a.task_id, a.content, MIN(a.answered_on)
from answer as a, userAnswer as ua, user as u
where a.task_id like "113073%" and ua.answer_id = a.id and u.id = ua.user_id
group by a.task_id, u.id
order by u.id


Oikeiden vastausten tiedosto on muotoa:

113073.t0|[false, true, false, false, false, true, true, true]
113073.t1|[false, false, true, true, false, false, true, false, true, ....
113073.t2|[true, false, true, false, true, true, true, false]

'''
import sqlite3

document_id = "113073%"
answer_file = "arinD1.csv"
correct_file = "arinD1vast.csv"
db = True

if db:
    conn = sqlite3.connect('T:/tim/timApp/tim_files/tim.db')
    c = conn.cursor()

    params = (document_id,)
    c.execute('select u.name, a.task_id, a.content, MIN(a.answered_on) ' +
              'from answer as a, userAnswer as ua, user as u ' +
              'where a.task_id like ? and ua.answer_id = a.id and u.id = ua.user_id ' +
              'group by a.task_id, u.id ' +
              'order by u.id;', params)

    lines = []
    for row in c:
        # print(row)
        line = row[0] + "|" + row[1] + "|" + row[2] + "|" + row[3]
        lines.append(line)
    conn.close()
else:
    f = open(answer_file, "r")
    lines = f.readlines()
    f.close()


students = {}
times = {}

for r in lines:
    name, task, answ, time = r.split("|")
    if name in students:
        students[name][task] = answ
    else:
        students[name] = {task: answ}
    times[name] = time.strip()

f = open(correct_file, "r")
lines = f.readlines()
f.close()

answs = {}
for r in lines:
    task, answ = r.split("|")
    answs[task] = answ

points = {}


def get_answers(task):
    ts = task.replace("[", "")
    ts = ts.replace("]", "")
    ts = ts.strip()
    return ts.split(",")


def calc_points(corr, task):
    p = 0
    corr_t = get_answers(corr)
    task_t = get_answers(task)
    for i in range(0, len(corr_t)):
        if corr_t[i] == task_t[i]: p += 1
    return p


for s in students:
    points[s] = {'n': 0, 'p': 0}
    for t in students[s]:
        task = students[s][t]
        corr = answs[t]
        points[s]['n'] += 1
        points[s]['p'] += calc_points(corr, task)

for p in points:
    print(p + "|" + str(points[p]["n"]) + "|" + str(points[p]["p"]) + "|" + times[p])