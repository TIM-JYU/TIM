# Read TIM-logfile and print  ip's that has many users
# and users that has used many ip's
# usage: python checkLogIp.py logname [ignoreusers]
# vesal 7.12.2021
import re

import sys

# https://regex101.com/r/S6xh2p/latest
#                        d     t    .sec  m   user     ip          prot res  time client processid
ipmatcher = re.compile("^(.*) (.*),(.*) (.*) (.+) \[([0-9.]+)\]: (.*) (.*) (.*) (.*) (.+)")


# 2021-12-02 00:08:20,484 INFO: aknakka [101.113.93.234]: PUT /read/113113/beEIXz36R3Uc/4 403 0.0307s android_linux_10/chrome/92.0.4515.115 4141
def getTimeIpAndUser(line):
    match = ipmatcher.search(line)
    if not match:
        return None, None, None
    t = match.group(2)
    return t, match.group(6), match.group(5)


# https://regex101.com/r/Sjd9Kd/latest
#                             d     t  .sec                who  op   right    user      doc
rightsmatcher = re.compile("^(.*) (.*),(.*) INFO: RIGHTS: (.*) (.*) (.*) for ([^ ]*) in (.*)")
# 2021-12-02 05:54:12,348 INFO: RIGHTS: roankka added view(duration=4:00:00) for akankka in kurssit/tie/ohj1/2021s/tentti/20211202
# 2021-12-02 14:05:56,112 INFO: RIGHTS: roankka removed view(duration=4:00:00,expired) for akankka in kurssit/tie/ohj1/2021s/tentti/20211202

# https://regex101.com/r/hQ8zOI/latest
#                             d     t    .sec  m   who     ip         prot              op    ip     user       what       res time client processid
rightsmatcher2 = re.compile("^(.*) (.*),(.*) (.*) (.+) \[([0-9.]+)\]: GET /permissions/(.*)/([0-9]*)/([^ /?]*)([/?](.*))? (.*) (.*) (.*) (.+)")
# 2021-12-02 05:54:12,218 INFO: roankka [91.158.181.132]: GET /permissions/add/356445/akankka/?type=view&duration=4 302 0.00128s windows_10/chrome/96.0.4664.45 4132
# 2021-12-02 14:04:41,358 INFO: roankka [84.251.211.181]: GET /permissions/expire/356445/akankka 200 0.048s windows_10/firefox/94.0 4131
def getTimeUserOpRights(line):
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


def main():
    if len(sys.argv) < 2:
        print("Anna logitiedoston nimi")
        exit(1)
    logname = sys.argv[1]
    ignoreusers = []
    if len(sys.argv) >= 3:
        with open(sys.argv[2], "r") as f:
            for line in f:
                ignoreusers.append(line.rstrip("\n"))

    n = 0
    ips = {}
    users = {}
    with open(logname, "r") as f:
        for line in f:
            # s = line.rstrip("\n")
            n += 1
            # print(n, s)
            dt, user, op = getTimeUserOpRights(line)
            if dt:
                if user in ignoreusers:
                    continue;
                usr = users.get(user)
                if not usr:
                    usr = {"add": "", "rem": "", "ips": {}}
                    users[user] = usr
                if op == "added":
                    usr["add"] = dt
                if op == "removed":
                    usr["rem"] = dt
                continue

            dt, ip, user = getTimeIpAndUser(line)
            if not dt:
                continue
            if user in ignoreusers:
                continue;

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
            s += sep + usr["first"] +"-" + usr["last"]
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
            s += " " + ip
        print(s)
    print(n)

if __name__ == "__main__":
    main()