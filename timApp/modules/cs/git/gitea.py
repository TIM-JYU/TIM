import os
import json
import subprocess
import shutil
import os.path
import re

from git.gitlib import GitLib, LibResponse
from git.util import RemoteInfo, Settings, generate_password, RepoSettings

def sanitize_name(name):
    return name if len(name) > 255 else name[:255]

class GiteaLib(GitLib):
    id = "gitea"

    def __init__(self, settings: Settings, info: RemoteInfo):
        super().__init__(settings, info)
        self.api_path = self.api_path + "api/v1/"

    def get_headers(self):
        if self.settings.librarySpecific is None or "apiToken" not in self.settings.librarySpecific:
            raise PermissionError("apiToken not specified for GiteaLib")

        token = os.environ.get(self.settings.librarySpecific["apiToken"], None)
        if token is None:
            raise PermissionError("apiToken not found in environment variables")

        return {
            'Authorization': f'token {token}',
            'Content-Type': 'application/json',
        }

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
        }
        response = self.post('admin/users', data)
        user = response.data

        if response.status == 201:
            return LibResponse(True)
        else:
            print(f'Git account creation failed:\nCode: {response.status}\nReason: {response.reason}\nData: {user}')
            return LibResponse(False, "Unknown error")

    def get_user(self, credentials):
        response = self.get(f'users/{sanitize_name(credentials["username"])}')
        return LibResponse(response.status == 200, response.reason, response.data)

    def delete_user(self, username):
        return self.delete(f'admin/users/{username}')

    def create_repository(self, repo_settings: RepoSettings):
        libsettings = repo_settings.librarySpecific
        data = {
            "auto_init": libsettings.get("autoInit", False),
            "description": libsettings.get("description", None),
            "gitignores": libsettings.get("gitignores", None),
            "license": libsettings.get("licence", None),
            "name": self.sanitize_repo_path(repo_settings.name),
            "private": libsettings.get("private", True),
            "readme": libsettings.get("readme", None),
        }
        if repo_settings.owner is None:
            response = self.post('user/repos/', data)
        else:
            response = self.post(f'admin/users/{repo_settings.owner}/repos', data)
        return LibResponse(response.status == 201)

    def delete_repository(self, name, owner):
        return self.delete(f'repos/{owner}/{self.sanitize_repo_path(name)}')

    def get_repository(self, name, owner):
        response = self.get(f'repos/{owner}/{self.sanitize_repo_path(name)}')
        return response

    def repository_exists(self, repo_settings):
        response = self.get_repository(self.sanitize_repo_path(repo_settings.name), repo_settings.owner)
        if response.status == 200:
            return True
        elif response.status != 404:
            print(f"Failed to check if repository exists. Response: {response}")
        return False

    def get_team_id(self, organization, team):
        resp = self.get(f"orgs/{organization}/teams")
        if resp.status != 200:
            raise Exception("Failed to get team id")
        id = [r['id'] for r in resp.data if r['name'] == team]
        if len(id) == 0:
            return None
        return id[0]

    def add_to_team(self, username, team, organization = None):
        if organization is not None:
            team = self.get_team_id(organization, team)

        response = self.put(f"teams/{team}/members/{username}")
        if response.status != 204:
            raise Exception(f"Failed to add user to team: {response.status}")

    def add_to_teams(self, username, teams):
        for team in teams:
            self.add_to_team(username, team["team"], team["organization"])

    def get_token_user(self):
        response = self.get('user')
        if response.status != 200:
            raise Exception(f"Failed to fetch current user: {response.status}")
        return response.data['login']

    def add_collaborators(self, repo, owner=None, teams=None, users=None, default_permission='read'):
        if owner is None:
            owner = self.get_token_user()

        repo = self.sanitize_repo_path(repo)

        if users is None:
            users = []
        elif not isinstance(users, list):
            users = [users]
        if teams is None:
            teams = []
        elif not isinstance(teams, list):
            teams = [teams]
        users = [u if isinstance(u, dict) else {"name":u, "permission":default_permission} for u in users]
        teams = [t if isinstance(t, dict) else {"name":t, "permission":default_permission} for t in teams]

        errors = []
        data = {'permission':default_permission}
        for user in users:
            data["permission"] = user.get("permission", default_permission)
            resp = self.put(f'repos/{owner}/{repo}/collaborators/{user["name"]}', data)
            if resp.status != 204:
                errors.append({'type': 'user', 'id': user, 'error': resp.reason})

        for team in teams:
            if isinstance(team["name"], str) and not team["name"].isdigit():
                tmp = self.get_team_id(owner, team["name"])
                if isinstance(tmp, dict):
                    errors.append({'type': 'team', 'id':team["name"], 'error':tmp})
                    continue
                if tmp is None:
                    errors.append({'type': 'team', 'id':team["name"], 'error':'not found'})
                    continue
                team["name"] = tmp

            resp = self.put(f'teams/{team["name"]}/repos/{owner}/{repo}')
            if resp.status != 204:
                errors.append({'type': 'team', 'id': team["name"], 'error': resp.reason})

        return errors

    def fork(self, settings: RepoSettings):
        if settings.oldOwner is None:
            settings.oldOwner = self.get_token_user()
            if isinstance(settings.oldOwner, dict):
                return LibResponse(False, str(settings.oldOwner))

        data = {'organization': settings.owner, 'name': self.sanitize_repo_path(settings.name)}
        repo_resp = self.post(f'repos/{settings.oldOwner}/{self.sanitize_repo_path(settings.oldName)}/forks', data)
        print(repo_resp)
        return LibResponse(repo_resp.status == 202)

    def library_specific(self, credentials, repo_settings: RepoSettings):
        if repo_settings.librarySpecific is None:
            return LibResponse(True)

        urepos = repo_settings.librarySpecific.get("userRepos", None)
        if urepos is not None:
            default_permission = repo_settings.librarySpecific.get("userPermission", "read")
            for repo in urepos:
                if not isinstance(repo, dict):
                    repo = {"name":repo}
                permission = repo.get("permission", default_permission)
                errors = self.add_collaborators(repo["name"], repo.get("owner", None), [], [credentials["username"]], permission)
                if len(errors) != 0:
                    return LibResponse(False, "Failed to add collaborators: " + ", ".join(errors))

        uteams = repo_settings.librarySpecific.get("userTeams", None)
        if uteams is not None:
            self.add_to_teams(credentials["username"], uteams)

        rusers = repo_settings.librarySpecific.get("repoUsers", None)
        rteams = repo_settings.librarySpecific.get("repoTeams", None)
        if rusers is not None and rteams is not None:
            default_permission = repo_settings.librarySpecific.get("defaultPermission", "read")
            errors = self.add_collaborators(repo_settings.name, repo_settings.owner, rteams, rusers, default_permission)
            if len(errors) != 0:
                return LibResponse(False, "Failed to add collaborators: " + ", ".join(str(error) for error in errors))

        return LibResponse(True)
