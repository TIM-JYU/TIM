from sqlalchemy import select

from timApp.tim_app import app
from timApp.timdb.sqa import db
from timApp.user.user import User, UserInfo
from timApp.user.usergroup import UserGroup


def change_email() -> None:
    with app.app_context():
        # print("You're changing email of every member of given group to [USERNAME]@[GIVEN_EMAIL]")
        print("Changing emails of mallikurssinryhma1")
        while True:
            # groupname = input("Input group to edit: ")
            groupname = "mallikurssinryhma1"
            group = (
                db.session.execute(select(UserGroup).filter_by(name=groupname).limit(1))
                .scalars()
                .first()
            )
            users: list[User] = group.users
            new_email = input("Input new email suffix: ")
            print("New values:")
            for user in users:
                if "@malli" not in user.name:
                    continue
                uprefix = str(user.name).replace("@malli", "")
                print(user.name + ": " + uprefix + "@" + new_email)
            yesno = input("Is this correct? y/n/quit: ")
            if yesno == "y":
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
            elif yesno == "q" or yesno == "quit":
                exit()
        db.session.commit()


if __name__ == "__main__":
    change_email()
