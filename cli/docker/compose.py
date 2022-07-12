from pathlib import Path
from typing import Optional

from cli.config import get_config
from cli.util.template import PyTemplate


def init_compose(profile: Optional[str] = None) -> None:
    config = get_config()

    docker_path = Path.cwd() / "docker"
    compose_template = docker_path / "docker-compose.tmpl.yml"
    docker_compose_target = Path.cwd() / "docker-compose.yml"
    dotenv_template = docker_path / ".env.tmpl"
    dotenv_target = Path.cwd() / ".env"

    ctx = config.var_ctx(profile)
    docker_compose_str = PyTemplate(compose_template.read_text()).render(ctx)
    docker_compose_target.write_text(docker_compose_str, encoding="utf-8")

    dotenv_str = PyTemplate(dotenv_template.read_text()).render(ctx)
    dotenv_target.write_text(dotenv_str, encoding="utf-8")
