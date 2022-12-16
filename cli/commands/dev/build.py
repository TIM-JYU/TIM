from argparse import ArgumentParser
from pathlib import Path
from typing import List, Optional, Callable, Dict, NamedTuple

from cli.docker.run import run_docker
from cli.docker.service_variables import tim_image_tag, csplugin_image_tag
from cli.util.logging import log_info

info = {
    "help": "Build TIM components",
    "description": """
Builds different TIM components.
Builds are specified via build tasks that are in format `task[:tag]`.
In other words, you must specify a build task and optionally a tag separate by a colon.

If no tag is specified, the task is built with all tags.
""",
}

# The main build tasks start here
# To add a new build task:
# 1. Create a task method. A task is of type (tag: str | None) -> str | None where the tag is an optional
#    specifier for what image to build. The tag can be used to modify what image to build.
#    If the task does not have tags, the tag will be none.
#    The return value must be a full name of the built image or None if no images were built.
# 2. Specify the task and valid tags in the BUILD_TASKS dictionary.


def build_tim(_: Optional[str], no_cache: bool) -> Optional[str]:
    image_name = f"timimages/tim:{tim_image_tag()}"
    cwd = Path.cwd()
    dockerfile = cwd / "timApp" / "Dockerfile"
    run_docker(
        [
            "build",
            *(["--no-cache"] if no_cache else []),
            "--tag",
            image_name,
            "--file",
            dockerfile.as_posix(),
            cwd.as_posix(),
        ],
    )
    return image_name


def build_csplugin(tag: Optional[str], no_cache: bool) -> Optional[str]:
    assert tag is not None
    image_name = f"timimages/cs3:{tag}-{csplugin_image_tag()}"
    context = Path.cwd() / "timApp" / "modules" / "cs"
    run_docker(
        [
            "build",
            *(["--no-cache"] if no_cache else []),
            "--tag",
            image_name,
            "--target",
            tag,
            context.as_posix(),
        ],
    )
    return image_name


######################################################


class Arguments:
    push: bool
    no_cache: bool
    tasks: List[str]


class BuildTask(NamedTuple):
    tags: Optional[List[str]]
    build: Callable[[Optional[str], bool], Optional[str]]


BUILD_TASKS: Dict[str, BuildTask] = {
    "tim": BuildTask(None, build_tim),
    "csplugin": BuildTask(["base", "complete", "sudo"], build_csplugin),
}


def run(args: Arguments) -> None:
    built_images = []
    for task_name in args.tasks:
        parts = task_name.split(":", 1)
        if len(parts) == 1:
            task, tag = parts[0], None
        else:
            task, tag = parts
        build_task = BUILD_TASKS[task]
        build_tags = [tag] if tag else build_task.tags

        if build_tags:
            for tag in build_tags:
                log_info(f"Building {task}:{tag}")
                image = build_task.build(tag, args.no_cache)
                if image:
                    built_images.append(image)
        else:
            log_info(f"Building {task}")
            image = build_task.build(None, args.no_cache)
            if image:
                built_images.append(image)

    if args.push:
        for image in built_images:
            log_info(f"Pushing {image}")
            run_docker(["push", image])


def init(parser: ArgumentParser) -> None:
    choices = []
    for name, task in BUILD_TASKS.items():
        choices.append(name)
        if task.tags:
            for tag in task.tags:
                choices.append(f"{name}:{tag}")
    parser.add_argument(
        "--push",
        help="Push TIM Docker images to Docker Hub.",
        action="store_true",
    )
    parser.add_argument(
        "--no-cache",
        help="Do not use cache when building the image.",
        action="store_true",
        dest="no_cache",
    )
    parser.add_argument(
        "tasks",
        nargs="*",
        choices=choices,
        help="Tasks to build in format `task:tag`. If not specified, all tasks will be built.",
    )
