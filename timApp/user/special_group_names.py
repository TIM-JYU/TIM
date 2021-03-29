ANONYMOUS_USERNAME = "Anonymous"
ANONYMOUS_GROUPNAME = "Anonymous users"
LOGGED_IN_GROUPNAME = "Logged-in users"
LOGGED_IN_USERNAME = "Logged-in user"
ADMIN_GROUPNAME = "Administrators"
GROUPADMIN_GROUPNAME = "Group admins"  # Users who can edit groups (but not special or privileged groups)
TEACHERS_GROUPNAME = "teachers"
SPECIAL_GROUPS = {ANONYMOUS_GROUPNAME, LOGGED_IN_GROUPNAME}
SPECIAL_USERNAMES = {ANONYMOUS_USERNAME, LOGGED_IN_USERNAME}
PRIVILEGED_GROUPS = {ADMIN_GROUPNAME, GROUPADMIN_GROUPNAME, TEACHERS_GROUPNAME}  # Only admins can edit these groups

# Users who can schedule functions (in addition to teachers).
FUNCTIONSCHEDULER_GROUPNAME = "Function schedulers"
