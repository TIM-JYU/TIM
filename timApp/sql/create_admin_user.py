from tim_app import app
from timdb.timdb2 import TimDb


def query_admin():
    db = TimDb(app.config['FILES_PATH'])
    username = input("Username: ")
    user = db.users.get_user_by_name(username)
    if user is not None:
        if not db.users.has_admin_access(user.id):
            print('A user with this username already exists. Do you want to add this user to administrators?')
            yesno = input('y/n: ')
            if yesno == 'y':
                db.users.addUserToAdmins(user.id)
                print('User {} has been added to administrators.'.format(username))
        else:
            print('User {} is already an administrator.'.format(username))
    else:
        realname = input("Real name: ")
        email = input("Email: ")
        password = input("Password: ")

        db.users.create_user_with_group(email, realname, email, password, is_admin=True)

        print('UserAccount ', email, " created")
        print('Use ', email, " and password you entered to login.")
    db.close()


if __name__ == '__main__':
    query_admin()
