import csv
import itertools
import json
import smtplib
import time
from dataclasses import dataclass
from email.mime.text import MIMEText
from io import TextIOWrapper
from pprint import pprint

import click
from flask import current_app
from flask.cli import AppGroup
from sqlalchemy import func, select

from timApp.admin.import_accounts import import_accounts_impl
from timApp.auth.accesstype import AccessType
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import move_document
from timApp.tim_app import get_home_organization_group
from timApp.timdb.sqa import db, run_sql
from timApp.user.personaluniquecode import SchacPersonalUniqueCode, PersonalUniqueCode
from timApp.user.user import User, UserInfo, deleted_user_suffix
from timApp.user.usercontact import UserContact
from timApp.user.usergroup import UserGroup
from timApp.user.usergroupmember import UserGroupMember
from timApp.user.userutils import check_password_hash
from timApp.util.flask.requesthelper import RouteException, NotExist
from timApp.util.utils import approximate_real_name
from tim_common.timjsonencoder import TimJsonEncoder


def create_user_info_set(u: User) -> set[str]:
    """Returns a set of strings constructed from various parts of user info.
    This set is meant to be intersected with another user to determine whether they have anything in common.
    """
    real_name_ascii = u.real_name.translate(str.maketrans("åöäÅÖÄ", "aoaAOA")).lower()
    return {
        u.name.lower(),
        *u.real_name.lower().split(" "),
        *u.verified_email_name_parts,
        real_name_ascii.replace(" ", ""),
        "".join(real_name_ascii.split(" ")[::-1]),
    }


def has_anything_in_common(u1: User, u2: User) -> bool:
    u1_set = create_user_info_set(u1)
    u2_set = create_user_info_set(u2)
    if u1_set & u2_set:
        return True
    # This allows e.g. testuser1 and testuser2 to be merged.
    return bool({n[:-1] for n in u1_set} & {n[:-1] for n in u2_set})


user_cli = AppGroup("user")


@user_cli.command()
@click.option(
    "--dry-run/--no-dry-run",
    default=True,
    help="Dry run",
)
@click.option(
    "--skip-warnings/--no-skip-warnings",
    default=False,
    help="Skip warnings",
)
def migrate_themes_to_styles(dry_run: bool, skip_warnings: bool) -> None:
    """Migrates old theme-based user preferences to new style-based one"""
    from timApp.timdb.init import create_style_docs

    if dry_run:
        click.echo("Running dry mode, the changes to users will not be saved")

    with db.session.no_autoflush:
        click.echo("Creating style paths")
        folders, docs = create_style_docs()

        for folder in folders:
            click.echo(f"Created folder '{folder.name}' to '{folder.path}'")
        for doc in docs:
            click.echo(f"Created document '{doc.title}' to '{doc.path}'")

        if not dry_run:
            db.session.flush()
        else:
            for i, d in enumerate(docs):  # type: int, DocEntry
                d.id = -i

        style_docs = {d.path.split("/")[-1]: d.id for d in docs}

        click.echo("Updating user styles")

        for u in run_sql(
            select(User).filter(User.prefs != None)
        ).scalars():  # type: User
            prefs_json: dict = json.loads(u.prefs)
            css_combined = prefs_json.pop("css_combined", None)
            css_files: dict[str, bool] = prefs_json.pop("css_files", {})
            if not css_combined and not css_files:
                continue
            enabled_themes = [theme for theme, enabled in css_files.items() if enabled]
            style_list = [
                style_docs[theme] for theme in enabled_themes if theme in style_docs
            ]

            if len(enabled_themes) != len(style_list):
                click.echo(
                    f"User {u.name} ({u.id}) will loose themes: {enabled_themes} => {style_list}"
                )
                if not skip_warnings:
                    click.echo(
                        "Fix the problem manually or skip them with --skip-warnings"
                    )
                    db.session.rollback()
                    return

            prefs_json["style_doc_ids"] = style_list
            u.prefs = json.dumps(prefs_json, cls=TimJsonEncoder)
            click.echo(f"Updated settings for {u.name} ({u.id})")

        if not dry_run:
            db.session.commit()


@user_cli.command()
@click.argument("name")
def addtohomeorg(name: str) -> None:
    """Adds a user to the home organization group."""
    u = User.get_by_name(name)
    if not u:
        click.echo("User not found.")
        return
    if u.is_email_user:
        click.echo("User is email user, so should not be added to home organization.")
        return
    if u.add_to_group(get_home_organization_group(), added_by=None):
        click.echo("Added.")
    else:
        click.echo("User already belongs to home organization.")
    db.session.commit()


@user_cli.command()
@click.argument("primary")
@click.argument("secondary")
def merge(primary: str, secondary: str) -> None:
    """Merges two users by moving data from secondary account to primary account,
    and soft-deletes the secondary account.
    """
    moved_data = find_and_merge_users(primary, secondary)
    find_and_soft_delete(secondary)
    db.session.commit()
    pprint(moved_data)


@dataclass
class MergeResult:
    primary: User
    secondary: User
    owned_lectures: int = 0
    lectureanswers: int = 0
    messages: int = 0
    answers: int = 0
    annotations: int = 0
    velps: int = 0
    readparagraphs: int = 0
    notes: int = 0
    accesses: int = 0
    groups: int = 0
    contacts: int = 0


@user_cli.command()
@click.argument("csvfile", type=click.File())
def mass_merge(csvfile: TextIOWrapper) -> None:
    """Merges multiple users as specified in the given CSV file."""
    reader = csv.reader(csvfile, delimiter=";")
    for row in reader:
        if len(row) != 2:
            raise click.UsageError(
                f"CSV file has a row with wrong number of columns: {row}"
            )
        primary, secondary = row[0], row[1]
        result = find_and_merge_users(primary, secondary)
        click.echo(str(result))
        find_and_soft_delete(secondary)
    db.session.commit()


def find_and_merge_users(primary: str, secondary: str) -> MergeResult:
    u_prim = User.get_by_name(primary)
    u_sec = User.get_by_name(secondary)
    if not u_prim:
        raise NotExist(f"User {primary} not found")
    if not u_sec:
        raise NotExist(f"User {secondary} not found")
    return do_merge_users(u_prim, u_sec)


def do_merge_users(u_prim: User, u_sec: User, force=False) -> MergeResult:
    if u_prim.is_special:
        raise RouteException(f"User {u_prim.name} is a special user")
    if u_sec.is_special:
        raise RouteException(f"User {u_sec.name} is a special user")
    if u_prim == u_sec:
        raise RouteException("Users cannot be the same")
    if not force and not has_anything_in_common(u_prim, u_sec):
        raise RouteException(
            f"Users {u_prim.name} and {u_sec.name} do not appear to be duplicates. "
            f"Merging not allowed to prevent accidental errors."
        )
    moved_data = MergeResult(u_prim, u_sec)
    for a in (
        "owned_lectures",
        "lectureanswers",
        "messages",
        "answers",
        "annotations",
        "velps",
    ):
        a_alt = a + "_alt"
        setattr(moved_data, a, len(getattr(u_sec, a_alt)))
        getattr(u_prim, a_alt).extend(getattr(u_sec, a_alt))
        setattr(u_sec, a_alt, [])
    u_prim_group = u_prim.get_personal_group()
    u_sec_group = u_sec.get_personal_group()
    u_prim_folder = u_prim.get_personal_folder()
    u_sec_folder = u_sec.get_personal_folder()
    docs = u_sec_folder.get_all_documents(include_subdirs=True)

    # Move group memberships.
    curr_prim_memberships = {m.usergroup_id for m in u_prim.memberships}
    for m in u_sec.memberships:
        # Skip personal usergroup.
        if m.usergroup_id == u_sec_group.id:
            continue

        if m.usergroup_id not in curr_prim_memberships:
            ugm = UserGroupMember(
                usergroup_id=m.usergroup_id,
                added_by=m.added_by,
                membership_added=m.membership_added,
                membership_end=m.membership_end,
            )
            u_prim.memberships.append(ugm)
            moved_data.groups += 1
        db.session.delete(m)

    # Move user contacts
    cur_contacts = {(uc.channel, uc.contact): uc for uc in u_prim.contacts}
    for c in u_sec.contacts:
        if (c.channel, c.contact) not in cur_contacts:
            u_prim.contacts.append(
                UserContact(
                    contact=c.contact,
                    contact_origin=c.contact_origin,
                    channel=c.channel,
                    verified=c.verified,
                )
            )
            moved_data.contacts += 1
        # Don't delete primary mail since it is still managed by the main email integration
        if not c.primary:
            db.session.delete(c)

    for d in docs:
        move_document(d, u_prim_folder)
    for a in ("readparagraphs", "notes", "accesses"):
        a_alt = a + "_alt"
        setattr(moved_data, a, len(getattr(u_sec_group, a_alt)))
        if a == "accesses":
            getattr(u_prim_group, a_alt).update(getattr(u_sec_group, a_alt))
            setattr(u_sec_group, a_alt, {})
        else:
            getattr(u_prim_group, a_alt).extend(getattr(u_sec_group, a_alt))
            setattr(u_sec_group, a_alt, [])
    # Restore ownership of secondary's personal folder:
    # * all users are allowed to have at most one personal folder
    # * if we don't restore access for secondary user, a new personal folder would be created when logging in
    for key, a in u_prim_group.accesses_alt.items():
        assert u_sec_folder.block is not None
        if a.block_id == u_sec_folder.block.id and a.type == AccessType.owner.value:
            moved_data.accesses -= 1
            u_prim_group.accesses_alt.pop(key)
            u_sec_group.accesses_alt[key] = a
            break
    return moved_data


@user_cli.command()
@click.argument("name")
def soft_delete(name: str) -> None:
    find_and_soft_delete(name)
    db.session.commit()


def find_and_soft_delete(name: str) -> None:
    u = User.get_by_name(name)
    if not u:
        raise RouteException("User not found.")
    do_soft_delete(u)


def do_soft_delete(u: User) -> None:
    if u.is_deleted:
        raise RouteException("User is already soft-deleted.")

    u.pass_ = None
    u.update_info(
        UserInfo(
            username=f"{u.name}{deleted_user_suffix}_{u.id}",
            email=f"{u.email}{deleted_user_suffix}_{u.id}",
            full_name=u.real_name,
        )
    )


@user_cli.command()
@click.option("--username", prompt="Username")
@click.option("--firstname", prompt="First name", default="")
@click.option("--lastname", prompt="Last name", default="")
@click.option("--email", prompt="Email", default="")
@click.option("--password", prompt="Password", default="")
@click.option(
    "--admin/--no-admin", default=False, prompt="Make this user an administrator?"
)
def create(
    username: str,
    firstname: str,
    lastname: str,
    email: str,
    password: str,
    admin: bool,
) -> None:
    """Creates or updates a user."""

    user = run_sql(select(User).filter_by(name=username).limit(1)).scalars().first()
    info = UserInfo(
        username=username,
        email=email or None,
        full_name=f"{lastname} {firstname}".strip() or None,
        given_name=firstname or None,
        last_name=lastname or None,
        password=password or None,
    )
    if user:
        user.update_info(info)
        if admin:
            user.make_admin()
        click.echo("User updated.")
    else:
        User.create_with_group(info, is_admin=admin)
        click.echo("User created.")
    db.session.commit()


@user_cli.command()
@click.option("--username", prompt="Username prefix", default="mass")
@click.option("--firstname", prompt="First name prefix", default="")
@click.option("--lastname", prompt="Last name prefix", default="")
@click.option("--email", prompt="Email suffix", default="")
@click.option("--password", prompt="Password", default="")
@click.option("--lowerlimit", prompt="First user number", default=1)
@click.option("--higherlimit", prompt="Last user number", default=1)
def create_mass_users(
    username: str,
    firstname: str,
    lastname: str,
    email: str,
    password: str,
    lowerlimit: int,
    higherlimit: int,
) -> None:
    click.echo(
        f"""
Creating/overwriting users: {username}1, {username}2...
Full names: {firstname}1 {lastname}1, {firstname}2 {lastname}2...
Emails: {username}1{email}, {username}2{email}...
With password (same for every user): {password}
From {lowerlimit} to {higherlimit}.
    """
    )
    ok = click.confirm("Ok?")
    if not ok:
        return
    if username != "mass":
        print("Usernames limited to 'mass' for now")
        return
    for i in range(lowerlimit, higherlimit + 1):
        strnum = str(i)
        user = (
            run_sql(select(User).filter_by(name=username + strnum).limit(1))
            .scalars()
            .first()
        )
        # print(i)
        info = UserInfo(
            username=username + strnum,
            email=username + strnum + email or None,
            full_name=f"{lastname + strnum} {firstname + strnum}".strip() or None,
            given_name=firstname + strnum or None,
            last_name=lastname + strnum or None,
            password=password or None,
        )
        if user:
            user.update_info(info)
            click.echo(f"User {username + strnum} updated.")
        else:
            User.create_with_group(info)
            click.echo(f"User {username + strnum} created.")
    db.session.commit()


@user_cli.command()
def fix_aalto_student_ids() -> None:
    users_to_fix: list[User] = (
        run_sql(
            select(User)
            .select_from(UserGroup)
            .filter(UserGroup.name.in_(["aalto19test", "cs-a1141-2017-2018"]))
            .join(User, UserGroup.users)
        )
        .scalars()
        .all()
    )
    for u in users_to_fix:
        u.set_unique_codes(
            [
                SchacPersonalUniqueCode(
                    code=u.name.split(":")[1], codetype="studentID", org="aalto.fi"
                )
            ]
        )
    db.session.commit()
    click.echo(f"Updated {len(users_to_fix)} users.")


@user_cli.command()
@click.option(
    "--csvfile",
    type=click.File(),
    required=True,
    help="Aalto CSV file (A+ format)",
)
def fix_aalto_users(csvfile: TextIOWrapper) -> None:
    for row in csv.reader(csvfile, delimiter=","):
        rowlen = len(row)
        if rowlen != 43:
            click.echo(f"Wrong column count in row: {rowlen} {row}")
            return
        studentid = row[1]
        email = row[2]
        real_name = approximate_real_name(email)
        puc = PersonalUniqueCode.find_by_student_id(studentid, "aalto.fi")
        if not puc:
            click.echo(f"StudentID {studentid} not found")
            continue
        u: User = puc.user
        eu = User.get_by_email_case_insensitive(email)
        if len(eu) == 1:
            puc.user = eu[0]
        elif eu:
            raise Exception(f"Multiple users found for email: {email}")
        else:
            u.update_info(UserInfo(full_name=real_name, email=email, username=email))
    db.session.commit()


@user_cli.command("import")
@click.option(
    "--csvfile",
    type=click.Path(exists=True),
    required=True,
    help="CSV file from which to read user accounts; format: email;full name;username",
)
@click.option(
    "--password",
    help="common password for all accounts",
)
def import_accounts(csvfile: str, password: str | None) -> None:
    added, existing = import_accounts_impl(csvfile, password)
    total = len(added) + len(existing)
    click.echo(f"Processed {total} accounts.")
    if added:
        click.echo(f"Added the following {len(added)} accounts:")
    else:
        click.echo(f"No new accounts were added.")
    for u in added:
        click.echo(u.name)
    if existing:
        click.echo(f"Updated the following {len(existing)} existing accounts:")
    else:
        click.echo(f"No existing accounts were updated.")
    for u in existing:
        click.echo(u.name)


@user_cli.command()
@click.option(
    "--csvfile",
    type=click.File(),
    required=True,
    help="CSV file from which to read passwords; format: email;password;password hash",
)
@click.option(
    "--verify/--no-verify",
    default=True,
    help="whether to verify that the password hash matches the password",
)
def import_passwords(csvfile: TextIOWrapper, verify: bool) -> None:
    return do_import_passwords(csvfile, verify)


def do_import_passwords(csvfile: TextIOWrapper, verify: bool) -> None:
    for row in csv.reader(csvfile, delimiter=";"):
        if len(row) != 3:
            raise click.UsageError(f"Wrong amount of columns in row {row}")
        email = row[0]
        if not email:
            raise click.UsageError(f"Email missing in row")
        password = row[1]
        pw_hash = row[2]
        if password and not pw_hash:
            raise click.UsageError("Password hash must be given if password is given")
        if verify:
            if not password:
                click.echo(
                    f"Cannot verify password for {email} because only hash was provided"
                )
            elif not check_password_hash(password, pw_hash):
                raise click.UsageError(
                    f"Password does not match hash: {password} {pw_hash}"
                )
        u = User.get_by_email(email)
        if not u:
            raise click.UsageError(f"User not found: {email}")
        u.pass_ = pw_hash or None
        if password:
            click.echo(f"Imported password {password} for {email}")
        elif pw_hash:
            click.echo(f"Imported password hash {pw_hash} for {email}")
        else:
            click.echo(f"Cleared password for {email}")

    db.session.commit()


@user_cli.command()
def find_duplicate_accounts() -> None:
    dupes = find_duplicate_accounts_by_email()
    for primary, secondarys in dupes:
        for s in secondarys:
            click.echo(f"{primary.name};{s.name}")


def find_duplicate_accounts_by_email() -> list[tuple[User, set[User]]]:
    email_lwr = func.lower(User.email)
    dupes: list[User] = (
        run_sql(
            select(User)
            .filter(
                email_lwr.in_(
                    select(email_lwr).group_by(email_lwr).having(func.count("*") > 1)
                )
            )
            .order_by(User.email)
        )
        .scalars()
        .all()
    )
    result = []
    dupegroups = [
        list(g) for _, g in (itertools.groupby(dupes, lambda u: u.email.lower()))
    ]

    for group in dupegroups:
        non_email_users = [u for u in group if not u.is_email_user]
        lowercase_email_users = [u for u in group if u.email.islower()]
        rest = set(group)
        if len(non_email_users) == 1:
            primary = non_email_users[0]
        elif len(non_email_users) > 1:
            raise Exception(
                f"Could not determine which account should be the primary one: {group}"
            )
        elif not lowercase_email_users:
            primary = group[0]
        else:
            primary = lowercase_email_users[0]
        rest.remove(primary)
        result.append((primary, rest))
    return result


@user_cli.command("send-email")
@click.option(
    "--template",
    type=click.File(),
    required=True,
    help="Template file for the email. Available placeholders: {password}, {fullname}, {lastname}.",
)
@click.option(
    "--subject",
    required=True,
    help="CSV file with rows email;password",
)
@click.option(
    "--mail-from",
    required=True,
    help="from address for the emails",
)
@click.option(
    "--reply-to",
    help="from address for the emails",
)
@click.option(
    "--mail-host",
    help="mail host to connect to",
)
@click.option(
    "--login",
    help='login info for mail host "user:pass"',
)
@click.option(
    "--passwords",
    type=click.File(),
    required=True,
    help="CSV file with rows email;password",
)
@click.option(
    "--delay",
    type=float,
    required=True,
    help="How much to wait between two emails",
)
@click.option(
    "--dry-run/--no-dry-run",
    default=True,
    help="Dry run",
)
def send_email_cmd(
    template: TextIOWrapper,
    subject: str,
    mail_from: str,
    reply_to: str | None,
    mail_host: str | None,
    login: str | None,
    passwords: TextIOWrapper,
    dry_run: bool,
    delay: float,
) -> None:
    template_text = template.read()
    if mail_host is None:
        mail_host = current_app.config["MAIL_HOST"]
    gmail_host = "smtp.gmail.com"
    port = 587 if mail_host == gmail_host else 0
    s = smtplib.SMTP(mail_host, port) if not dry_run else None
    if s and mail_host == gmail_host:
        if not login:
            raise click.UsageError("Login info is required for gmail")
        s.ehlo()
        s.starttls()
        user, passw = login.split(":")
        s.login(user, passw)
    log_file = open("send_email_log.txt", "w", encoding="utf8", newline="\n")
    error_log_file = open(
        "send_email_error_log.txt", "w", encoding="utf8", newline="\n"
    )
    error_users_file = open(
        "send_email_error_users.txt", "w", encoding="utf8", newline="\n"
    )
    sep = "--------------------------"
    for row in csv.reader(passwords, delimiter=";"):
        email = row[0]
        password = row[1]
        u = User.get_by_email(email)
        if not u:
            email_err = f"Email not found: {email}"
            click.echo(email_err)
            error_log_file.write(f"{email_err}\n")
            continue
        lastname = u.last_name if u.last_name is not None else u.real_name.split(" ")[0]
        msg = (
            template_text.replace("{password}", password)
            .replace("{fullname}", u.pretty_full_name)
            .replace("{lastname}", lastname)
            .replace("{email}", email)
        )
        mime_msg = MIMEText(msg)
        mime_msg["Subject"] = subject
        mime_msg["From"] = mail_from
        mime_msg["To"] = email

        if reply_to:
            mime_msg.add_header("Reply-To", reply_to)

        time.sleep(delay)

        if not s:
            click.echo(f"Email would be sent to {email}")
            log_file.write(f"Email would be sent to {email}:\n{sep}\n{msg}\n{sep}\n")
            continue
        try:
            s.sendmail(mail_from, [email], mime_msg.as_string())
        except (
            smtplib.SMTPSenderRefused,
            smtplib.SMTPRecipientsRefused,
            smtplib.SMTPHeloError,
            smtplib.SMTPDataError,
            smtplib.SMTPNotSupportedError,
        ) as e:
            err = f"Error sending mail to {email}: {e}"
            click.echo(err)
            error_log_file.write(f"{err}\n")
            error_users_file.write(f"{email};{password}\n")
        else:
            click.echo(f"Email sent to {email}")
            log_file.write(f"Email sent to {email}:\n{sep}\n{msg}\n{sep}\n")
    if s:
        s.quit()
    log_file.close()
    error_log_file.close()
    error_users_file.close()
