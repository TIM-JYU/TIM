__authors__ = ["vesal"]
__date__ = "2021-12-07"

import re
from argparse import ArgumentParser
from pathlib import Path
from typing import Optional, Tuple, Dict, Any

from cli.util.errors import CLIError

info = {
    "help": "Analyze IP usage from logs",
    "description": """
Read TIM-logfile and print IPs that has many users and users that has used many IPs.
""",
}


class Arguments:
    log_name: str
    ignore_users: Optional[str]


ipmatcher = re.compile(
    #  d     t    .sec  m   user     ip       prot res  time client processid
    r"^(.*) (.*),(.*) (.*) (.+) \[([0-9.]+)]: (.*) (.*) (.*) (.*) (.+)"
)
rightsmatcher = re.compile(
    #  d     t  .sec                who  op   right    user      doc
    "^(.*) (.*),(.*) INFO: RIGHTS: (.*) (.*) (.*) for ([^ ]*) in (.*)"
)
rightsmatcher2 = re.compile(
    #  d     t    .sec  m   who     ip         prot              op    ip     user       what       res time client processid
    r"^(.*) (.*),(.*) (.*) (.+) \[([0-9.]+)]: GET /permissions/(.*)/([0-9]*)/([^ /?]*)([/?](.*))? (.*) (.*) (.*) (.+)"
)


def get_time_ip_and_user(
    line: str,
) -> Tuple[Optional[str], Optional[str], Optional[str]]:
    match = ipmatcher.search(line)
    if not match:
        return None, None, None
    t = match.group(2)
    return t, match.group(6), match.group(5)


def get_time_user_op_rights(
    line: str,
) -> Tuple[Optional[str], Optional[str], Optional[str]]:
    match = rightsmatcher.search(line)
    if match:
        t = match.group(2)
        return t, match.group(7), match.group(5)
    match = rightsmatcher2.search(line)
    if match:
        t = match.group(2)
        op = match.group(7)
        user = match.group(9)
        if op == "expire":
            op = "removed"
        return t, user, op
    return None, None, None


def run(args: Arguments) -> None:
    log_file_path = Path(args.log_name).resolve()
    if not log_file_path.exists() or not log_file_path.is_file():
        raise CLIError(f"Log does not exist: {log_file_path}")

    ignore_users = []
    if args.ignore_users:
        ignore_users_path = Path(args.ignore_users).resolve()
        if not ignore_users_path.exists() or not ignore_users_path.is_file():
            raise CLIError(f"Ignore users file does not exist: {ignore_users_path}")
        with ignore_users_path.open("r", encoding="utf-8") as f:
            ignore_users = [line.strip() for line in f]

    n = 0
    ips: Dict[str, Dict[str, Dict[str, Any]]] = {}
    users: Dict[str, Dict[str, Any]] = {}
    with log_file_path.open("r", encoding="utf-8") as f:
        for line in f:
            # s = line.rstrip("\n")
            n += 1
            # print(n, s)
            dt, user, op = get_time_user_op_rights(line)
            if dt and user:
                if user in ignore_users:
                    continue
                usr = users.get(user)
                if not usr:
                    usr = {"add": "", "rem": "", "ips": {}}
                    users[user] = usr
                if op == "added":
                    usr["add"] = dt
                if op == "removed":
                    usr["rem"] = dt
                continue

            dt, ip, user = get_time_ip_and_user(line)
            if not dt or not ip or not user:
                continue
            if user in ignore_users:
                continue

            # print(ip, user)
            if not ips.get(ip):
                ips[ip] = {}
            usr = ips[ip].get(user)
            if not usr:
                usr = {"n": 0, "first": dt}
                ips[ip][user] = usr
            usr["n"] += 1
            usr["last"] = dt

            usr = users.get(user)
            if not usr:
                usr = {"add": "", "rem": "", "ips": {}}
                users[user] = usr
            uip = usr["ips"].get(ip)
            if not uip:
                uip = {"n": 0, "first": dt}
                usr["ips"][ip] = uip
            uip["n"] += 1
            uip["last"] = dt

    for ip in ips:
        usrs = ips[ip]
        if len(usrs) <= 1:
            continue
        s = ip + " "
        for user in usrs:
            s += " " + user
        s += ": ("
        sep = ""
        for user in usrs:
            usr = usrs[user]
            s += sep + str(usr["n"])
            sep = ", "
        s += ") ("
        sep = ""
        for user in usrs:
            usr = usrs[user]
            s += sep + usr["first"] + "-" + usr["last"]
            sep = ","
        s += ")"
        print(s)

    for user in users:
        usr = users[user]
        ipsip = usr["ips"]
        if not usr["add"] or not usr["rem"] or user == "nemaaho":
            print(user + " " + usr["add"] + " - " + usr["rem"])
        if len(ipsip) <= 1 or user == "Anonymous":
            continue
        s = user + " "
        for ip in ipsip:
            if ip:
                s += " " + ip
        print(s)
    print(n)


def init(parser: ArgumentParser) -> None:
    parser.add_argument(
        "--ignore_users",
        help="File with users to ignore. One user per line.",
    )
    parser.add_argument(
        "log_name",
        help="Name of the log file to analyze",
    )
