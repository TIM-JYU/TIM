import os
import subprocess


def try_fix_safe_dir_config() -> None:
    try:
        subprocess.run(
            [
                "git",
                "config",
                "--global",
                "--add",
                "safe.directory",
                os.path.dirname(os.getcwd()),
            ]
        )
    except:
        print(
            "Could not fix safe.directory config; some git commands will not work. "
            "Make sure global git config is editable."
        )
        pass


# Ensure the main dir is marked as safe
# See https://github.blog/2022-04-12-git-security-vulnerability-announced/
try_fix_safe_dir_config()


def get_latest_commit_timestamp() -> str:
    try:
        return (
            subprocess.run(
                ["git", "log", "-1", "--date=format:%d.%m.%Y %H:%M:%S", "--format=%cd"],
                stdout=subprocess.PIPE,
            )
            .stdout.decode()
            .strip()
        )
    except:
        return "<detached>"


def get_current_branch() -> str:
    try:
        return (
            subprocess.run(
                ["git", "rev-parse", "--abbrev-ref", "HEAD"], stdout=subprocess.PIPE
            )
            .stdout.decode()
            .strip()
        )
    except:
        return "<detached>"
