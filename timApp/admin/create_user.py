from timApp.timdb.dbaccess import get_files_path
from timApp.timdb.sqa import db
from timApp.timdb.timdb import TimDb
from timApp.user.user import User
from timApp.user.usergroup import UserGroup


def query_admin():
    timdb = TimDb(get_files_path())
    while True:
        username = input("Username: ")
        user = User.query.filter_by(name=username).first()
        if user is not None:
            if not user.is_admin:
                print('A user with this username already exists. Do you want to add this user to administrators?')
                yesno = input('y/n: ')
                if yesno == 'y':
                    user.groups.append(UserGroup.get_admin_group())
                    print(f'User {username} has been added to administrators.')
                    break
            else:
                print(f'User {username} is already an administrator.')
        else:
            realname = input("Real name: ")
            email = input("Email: ")
            password = input("Password: ")

            print('Add created user to the administrators?')
            yesno = input('y/n: ')
            isadmin = False
            if yesno == 'y':
                isadmin = True
            print(f"""
Creating following user:            
Username: {username}
Real name: {realname}
Email: {email}
Password: {password} 
Admin: {isadmin}""")
            yesno = input('Is this correct? y/n/quit: ')
            if yesno == 'y':
                User.create_with_group(username, realname, email, password, is_admin=isadmin)
                print('UserAccount ', username, " created")
                print('Use ', username, " and password you entered to login.")
                break
            elif yesno == 'q' or yesno == 'quit':
                exit()
                timdb.close()
    db.session.commit()
    timdb.close()


if __name__ == '__main__':
    query_admin()
