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


def tim_image_tag() -> str:
    cwd = Path.cwd()
    return hash_file(cwd / "timApp" / "Dockerfile", cwd / "poetry.lock")


# Name of the alternative TIM image that has a newer Node.js (see timApp/Dockerfile.angtim).
# The Angular upgrade needs a newer Node.js than the original "tim" image has.
# The alternative image has its own name so that the original "tim" image
# is never built, tagged or pushed because of the upgrade.
ANGTIM_IMAGE_NAME = "angtim"


def angtim_image_name() -> str:
    return ANGTIM_IMAGE_NAME


def angtim_image_tag() -> str:
    # The image is built on top of the original TIM image, so its tag depends on the files of that image too.
    cwd = Path.cwd()
    return hash_file(
        cwd / "timApp" / "Dockerfile.angtim",
        cwd / "timApp" / "Dockerfile",
        cwd / "poetry.lock",
    )
