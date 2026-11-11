from pathlib import Path

from cli.util.hash import hash_file


def csplugin_target(profile: str) -> str:
    if profile == "test":
        return "base"
    elif profile == "dev":
        return "sudo"
    return "complete"


def csplugin_image_tag() -> str:
    cwd = Path.cwd()
    return hash_file(cwd / "timApp" / "modules" / "cs" / "Dockerfile")


# Name of the main TIM image in the images repository.
# This experimental branch (Angular upgrade) needs a newer Node.js than the original image has.
# It uses its own image name so that the original "tim" image is never built, tagged or pushed from this branch.
TIM_IMAGE_NAME = "angtim"


def tim_image_name() -> str:
    return TIM_IMAGE_NAME


def tim_image_tag() -> str:
    cwd = Path.cwd()
    return hash_file(cwd / "timApp" / "Dockerfile", cwd / "poetry.lock")
