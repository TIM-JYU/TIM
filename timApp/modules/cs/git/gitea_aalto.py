
from git.gitea import GiteaLib, sanitize_name
from git.gitlib import LibResponse
from git.util import Options, generate_password, RepoSettings

class GiteaAaltoLib(GiteaLib):
    id = "gitea_aalto"
    url = "courses-git.comnet.aalto.fi"

    def get_user(self, credentials):
        response = self.get(f'users/{sanitize_name(credentials["secret_id"])}')
        return LibResponse(response.status == 200, response.reason, response.data)

    def create_user(self, options):
        defaults = {
            "password": generate_password(),
            "must_change_password": False,
            "send_notify": False,
            "source_id": 0,
        }
        defaults.update(options)

        data = {
            "username": defaults["username"],
            "password": defaults["password"],
            "email": defaults["email"],
            "full_name": defaults["fullname"],
            "must_change_password": defaults["must_change_password"],
            "send_notify": defaults["send_notify"],
            "source_id": 0,
            "secret_id": sanitize_name(defaults["secret_id"])
        }
        response = self.post('admin/users', data)
        user = response.data

        if response.status == 201:
            return LibResponse(True)
        else:
            print(f'Git account creation failed:\nCode: {reason.status}\nReason: {response.reason}\nData: {user}')
            return LibResponse(False, "Unknown error")

    def missing_field(self, fields, dict):
        for field in fields:
            if field not in dict:
                return field
        return None

    def is_registered(self, credentials):
        field = self.missing_field(["secret_id"], credentials)
        if field is not None:
            raise ValueError(f"{field} not present in credentials")
        data = {
            "secret_id": sanitize_name(credentials["secret_id"]),
        }
        response = self.post('admin/users/check', data)
        if response.reason != None:
            raise Exception("Failed to connect to git: " + response["reason"])
        secret_id = response.data.get("secret_id", "")
        if secret_id not in ["ok", "taken"]:
            raise Exception("Internal Error: 0. Please refresh page. If this message persists, please contact course staff.")
        return LibResponse(secret_id == "taken", "")

    def check_credentials(self, credentials):
        data = credentials
        response = self.post('admin/users/check', data)
        if response.reason != None:
            raise Exception("Failed to connect to git: " + response["reason"])

        user = response.data
        err = ""
        if "username" in user:
            if user["username"] == "taken":
                err = err + "Username is taken\n"
            elif user["username"] != "ok":
                err = err + "Username not acceptable\n"
            elif len(credentials["username"]) < 4:
                err = err + "Username must be at least 4 characters long\n"

        if "email" in user:
            if user["email"] == "taken":
                err = err + "Email is already in use\n"
            elif user["email"] != "ok":
                err = err + "Email not acceptable\n"

        if "secret_id" in user:
            if user["secret_id"] != "ok":
                print("check_credentials returned non-ok on secret_id when registering")
                raise Exception("Interal Error: 1. Please refresh page. If this message persists, please contact course staff.")

        if "password" in user and user["password"] != "ok":
                err = err + "Password not acceptable\n"
        elif "password" in credentials and len(credentials["password"]) < 8:
                err = err + "Password must be at least 8 characters long\n"

        return LibResponse(len(err) == 0, err, user)
