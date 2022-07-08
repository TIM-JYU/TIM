from pathlib import Path

from cli.util.hash import hash_file


def csplugin_target(profile: str) -> str:
    if profile == "test":
        return "base"
    elif profile == "dev":
        return "sudo"
    return ""


def csplugin_image_tag() -> str:
    cwd = Path.cwd()
    return hash_file(cwd / "timApp" / "modules" / "cs" / "Dockerfile")


def tim_image_tag() -> str:
    cwd = Path.cwd()
    return hash_file(cwd / "timApp" / "Dockerfile", cwd / "poetry.toml")
