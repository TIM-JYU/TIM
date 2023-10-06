import os
from pathlib import Path
from typing import Optional

from cli.config import get_config
from cli.config.config_file import IdeProfile
from cli.util.template import PyTemplate

_compose_refreshed = False


def init_devcontainers(only_templates: bool = False, profile: Optional[str] = None) -> None:
    config = get_config()

    if config.ide_profile != IdeProfile.VSCode:
        return

    devcontainers_path = Path.cwd() / ".devcontainers"
    devcontainers_path.mkdir(parents=True, exist_ok=True)
    templates_folder = Path.cwd() / "cli" / "templates"
    devcontainers_template = templates_folder / "devcontainers"

    ctx = config.var_ctx(profile)

    # Iterate over all files in the template directory and apply the config template
    for root, dirs, files in os.walk(devcontainers_template):
        for file in files:
            file_path = Path(root) / file
            new_file_path = devcontainers_path / file_path.relative_to(devcontainers_template)
            new_file_path.parent.mkdir(parents=True, exist_ok=True)
            file_ext = file_path.suffix
            if file_ext == ".tmpl":
                # Remove .tmpl suffix from new_file_path
                new_file_path = new_file_path.with_suffix("")
                # Then this is a template file, we need to parse it
                template_name = str(file_path.relative_to(templates_folder))
                template_str = PyTemplate(template_name).render(ctx)
                new_file_path.write_text(template_str, encoding="utf-8")
            elif not only_templates:
                # Not a template, just copy
                file_contents = file_path.read_bytes()
                new_file_path.write_bytes(file_contents)


def init_compose(profile: Optional[str] = None) -> None:
    global _compose_refreshed
    if _compose_refreshed:
        return
    config = get_config()

    docker_compose_target = Path.cwd() / "docker-compose.yml"
    dotenv_target = Path.cwd() / ".env"

    ctx = config.var_ctx(profile)
    docker_compose_str = PyTemplate("docker/docker-compose.tmpl.yml").render(ctx)
    docker_compose_target.write_text(docker_compose_str, encoding="utf-8")

    dotenv_str = PyTemplate("docker/.env.tmpl").render(ctx)
    dotenv_target.write_text(dotenv_str, encoding="utf-8")

    init_devcontainers(only_templates=True, profile=profile)

    _compose_refreshed = True
