from typing import List

from timApp.timdb.dbaccess import get_files_path
from timApp.timdb.sqa import db
from timApp.timdb.timdb import TimDb
from timApp.user.user import User, UserInfo
from timApp.user.usergroup import UserGroup


def change_email():
    timdb = TimDb(get_files_path())
    # print("You're changing email of every member of given group to [USERNAME]@[GIVEN_EMAIL]")
    print("Changing emails of mallikurssinryhma1")
    while True:
        # groupname = input("Input group to edit: ")
        groupname = "mallikurssinryhma1"
        group = UserGroup.query.filter_by(name="mallikurssinryhma1").first()
        users: List[User] = group.users
        new_email = input("Input new email suffix: ")
        print("New values:")
        for user in users:
            if "@malli" not in user.name:
                continue
            uprefix = str(user.name).replace("@malli", "")
            print(user.name + ": " + uprefix + "@" + new_email)
        yesno = input('Is this correct? y/n/quit: ')
        if yesno == 'y':
            for user in users:
                if "@malli" not in user.name:
                    continue
                uprefix = str(user.name).replace("@malli", "")
                user.update_info(
                    UserInfo(
                        username=user.name,
                        full_name=user.real_name,
                        email=uprefix + "@" + new_email,
                    )
                )
            break
        elif yesno == 'q' or yesno == 'quit':
            timdb.close()
            exit()
    db.session.commit()
    timdb.close()


if __name__ == '__main__':
    change_email()
