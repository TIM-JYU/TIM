# Python:

import hashlib
import sqlite3

realname = input("Real name: ")
email = input("Email: ")
password = input("Password: ")
hash = hashlib.sha256(password.encode()).hexdigest()

conn = sqlite3.connect('tim.db')
c = conn.cursor()

c.execute('INSERT INTO User (name, real_name, email, pass) VALUES (?, ?, ?, ?)', [email, realname, email, hash])
user_id = c.lastrowid
conn.commit()

c.execute('INSERT INTO UserGroup (name) VALUES (?)', [email])
group_id = c.lastrowid
conn.commit()

c.execute('INSERT INTO UserGroupMember (UserGroup_id, User_id) VALUES (?, ?)', [group_id, user_id])
c.execute('INSERT INTO UserGroupMember (UserGroup_id, User_id) VALUES (?, ?)', [4, user_id])
conn.commit()
c.close()
conn.close()

print('User ', email, " created")
print('Use ', email, " and password you entered to login.")
