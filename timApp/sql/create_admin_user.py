import hashlib

from tim_app import app
from timdb.timdb2 import TimDb

realname = input("Real name: ")
email = input("Email: ")
password = input("Password: ")
hash = hashlib.sha256(password.encode()).hexdigest()

db = TimDb(app.config['DATABASE'], app.config['FILES_PATH'])
db.users.create_user_with_group(email, realname, email, password, is_admin=True)

print('UserAccount ', email, " created")
print('Use ', email, " and password you entered to login.")
