# Python:

import hashlib
import sqlite3

realname = input("Real name: ")
email = input("Email: ")
password = input("Password: ")
hash = hashlib.sha256(password.encode()).hexdigest()

conn = sqlite3.connect('tim.db')
c = conn.cursor()

c.execute('INSERT INTO UserAccount (name, real_name, email, pass) VALUES (%s, %s, %s, %s)', [email, realname, email, hash])
user_id = c.lastrowid
conn.commit()

c.execute('INSERT INTO UserGroup (name) VALUES (%s)', [email])
group_id = c.lastrowid
conn.commit()

c.execute('INSERT INTO UserGroupMember (UserGroup_id, User_id) VALUES (%s, %s)', [group_id, user_id])
c.execute('INSERT INTO UserGroupMember (UserGroup_id, User_id) VALUES (%s, %s)', [4, user_id])
conn.commit()
c.close()
conn.close()

print('UserAccount ', email, " created")
print('Use ', email, " and password you entered to login.")
