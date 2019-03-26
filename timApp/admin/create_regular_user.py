from timApp.tim_app import app
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.timdb.sqa import db
from timApp.timdb.timdb import TimDb


def query_admin():
    timdb = TimDb(app.config['FILES_PATH'])
    username = input("Username: ")
    user = User.query.filter_by(name=username).first()
    if user is not None:
        if not user.is_admin:
            print('A user with this username already exists. Do you want to add this user to administrators?')
            yesno = input('y/n: ')
            if yesno == 'y':
                user.groups.append(UserGroup.get_admin_group())
                print(f'User {username} has been added to administrators.')
        else:
            print(f'User {username} is already an administrator.')
    else:
        realname = input("Real name: ")
        email = input("Email: ")
        password = input("Password: ")

        User.create_with_group(email, realname, email, password, is_admin=False)

        print('UserAccount ', email, " created")
        print('Use ', email, " and password you entered to login.")
    db.session.commit()
    timdb.close()


if __name__ == '__main__':
    query_admin()
