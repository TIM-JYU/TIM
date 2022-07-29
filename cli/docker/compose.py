from pathlib import Path
from typing import Optional

from cli.config import get_config
from cli.util.template import PyTemplate

_compose_refreshed = False


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
    _compose_refreshed = True
