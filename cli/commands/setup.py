import json
import os
import platform
import re
import secrets
import subprocess
from argparse import ArgumentParser
from pathlib import Path
from typing import List, Optional, TypeVar, Callable, Union, Tuple, Any
from urllib.parse import ParseResult, urlparse

from cli.commands.js import js
from cli.commands.npmi import npmi
from cli.commands.rust import build_rust
from cli.commands.up import up
from cli.config import has_config, get_config
from cli.config.config_file import IdeProfile
from cli.docker.compose import init_devcontainers
from cli.docker.run import (
    verify_docker_installed,
    verify_compose_installed,
    run_compose,
)
from cli.npm.run import verify_npm, run_npm, reset_npm_version
from cli.util.errors import CLIError
from cli.util.logging import log_info, log_warning, log_debug
from cli.util.proc import run_cmd

info = {
    "help": "Set up the TIM instance",
    "description": """
Set up the TIM instance and initialize necessary configuration files.
This command is intended to be run once, but can be re-run to re-initialize the instance.
""",
}

# Version of Poetry to install
POETRY_MIN_VERSION = "1.5.1"

# Minimal Python version needed for development
PYTHON_MIN_DEV_VERSION = [3, 10]

VENV_NAME = ".venv"


class Arguments:
    force: bool
    interactive: bool
    install: bool
    up: bool
    http_port: Optional[int]
    https_port: Optional[int]
    profile: Optional[str]
    ide_profile: Optional[str]
    hostname: Optional[str]
    domains: Optional[str]
    is_proxied: Optional[str]


_TOut = TypeVar("_TOut")


def get_str_default(value: Any, default_text: Optional[str]) -> str:
    if default_text is not None:
        return default_text
    if isinstance(value, ParseResult):
        return f"{value.scheme}://{value.netloc}"
    return str(value)


def get_value(
    value: Optional[str],
    prompt: bool,
    prompt_name: str,
    prompt_text: str,
    check: Callable[
        [Union[str, Optional[_TOut]]], Tuple[Optional[_TOut], Optional[str]]
    ],
    default_value: Optional[_TOut] = None,
    default_value_text: Optional[str] = None,
) -> _TOut:
    if value is not None:
        val, _ = check(value)
        if val is not None:
            return val
    if not prompt:
        if default_value is None:
            raise CLIError(f"{prompt_name} is required")
        return default_value
    print(f"{'-'*20}\n{prompt_text.strip()}")
    if default_value is not None:
        print(f"\nDefault value: {get_str_default(default_value, default_value_text)}")
    while True:
        answer = input(f"{prompt_name}= ")
        if not answer and default_value is not None:
            return default_value
        val, err = check(answer)
        if not err:
            return val  # type: ignore
        print(f"Invalid input: {err}. Please input again.")


def check_choices(
    answer: Optional[str], options: List[str]
) -> Tuple[Optional[str], Optional[str]]:
    if answer in options:
        return answer, None
    return (
        None,
        f"{answer} is not a valid choice. Valid choices are: {', '.join(options)}",
    )


def check_port(
    port_val: Union[Optional[str], str]
) -> Tuple[Optional[str], Optional[str]]:
    if port_val is None:
        return None, "Port number is required (specify 'none' to disable)"
    if port_val.lower() == "none":
        return "", None
    try:
        return str(make_port(port_val)), None
    except (ValueError, CLIError):
        return None, f"{port_val} is not a valid port (specify 'none' to disable)"


def make_port(port: str) -> int:
    port_int = int(port)
    if port_int < 1 or port_int > 65535:
        raise CLIError(f"Port must be between 1 and 65535")
    return port_int


def check_hostname(
    hostname: Union[Optional[str], Optional[ParseResult]],
) -> Tuple[Optional[ParseResult], Optional[str]]:
    if hostname is None:
        return None, "Hostname is required"
    if isinstance(hostname, ParseResult):
        return hostname, None
    if not hostname:
        return urlparse("http://localhost"), None
    parsed_url = urlparse(hostname)
    if parsed_url.scheme not in ("http", "https"):
        return None, "Hostname must be a valid URL (http or https)"
    if not parsed_url.netloc:
        return None, "Hostname must be a valid URL of format http[s]://[HOSTNAME]"
    return parsed_url, None


def check_yes_no(
    value: Union[Optional[str], Optional[bool]],
) -> Tuple[Optional[bool], Optional[str]]:
    if isinstance(value, bool):
        return value, None
    if value is None:
        return None, "Value is required"
    if value.lower() in ("yes", "y"):
        return True, None
    if value.lower() in ("no", "n"):
        return False, None
    return None, f"{value} is not a valid choice. Valid choices are: yes, y, no, n"


def check_string(
    value: Optional[str],
) -> Tuple[Optional[str], Optional[str]]:
    if value is None:
        return None, "Value is required"
    return value.strip(), None


def verify_tim_requirements() -> None:
    requirement_checkers = [
        verify_docker_installed,
        verify_compose_installed,
    ]
    if platform.system() == "Windows":

        def check_npm() -> None:
            verify_npm()

        requirement_checkers.append(check_npm)
    errors = []
    for checker in requirement_checkers:
        try:
            checker()
        except CLIError as e:
            errors.append(e)

    if errors:
        error_text = "\n".join(f"* {e}" for e in errors)
        raise CLIError(
            f"""
The following errors were encountered while verifying the requirements:

{error_text}

Install the requirements before installing TIM.

(if you want to run the setup without installing TIM, use the --no-install flag)
"""
        )


def verify_pip(path_prefix: str = "") -> List[str]:
    pip_locations = [
        [path_prefix + "pip"],
        [path_prefix + "pip3"],
        [path_prefix + "python3", "-m", "pip"],
        [path_prefix + "py", "-m", "pip"],
    ]

    for pip_location in pip_locations:
        try:
            run_cmd(
                [*pip_location, "--version"],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
            )
            return pip_location
        except (subprocess.CalledProcessError, FileNotFoundError):
            pass
    raise CLIError(
        "Could not find pip which is needed to setup the local development environment. Make sure pip is installed."
    )


def verify_dev_python(path_prefix: str = "") -> List[str]:
    python_locations = [
        [path_prefix + "python"],
        [path_prefix + "python3"],
        [path_prefix + "py", "-3"],
    ]
    if os.environ.get("PYTHON_CMD"):
        python_locations.insert(0, [path_prefix + os.environ["PYTHON_CMD"]])
    python_version_pattern = re.compile(r"^Python (?P<version>(\d\.?)+)")
    for python_location in python_locations:
        try:
            res = run_cmd(
                [*python_location, "--version"],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                encoding="utf-8",
            )
            stdout = res.stdout.strip()
            if stdout.startswith("Python was not found"):
                raise CLIError(stdout)
            match = python_version_pattern.match(stdout)
            if not match:
                log_debug(f"Could not parse python version from {stdout}")
                continue
            version = match.group("version").split(".")
            for i, part in enumerate(PYTHON_MIN_DEV_VERSION):
                if int(version[i]) < part:
                    raise CLIError("Not supported")
            return python_location
        except (subprocess.CalledProcessError, FileNotFoundError, CLIError) as e:
            pass
    raise CLIError(
        f"Could not find a supported Python version. "
        f"Development requires Python {'.'.join([str(v) for v in PYTHON_MIN_DEV_VERSION])}+."
    )


def verify_poetry(python_cmd: List[str]) -> List[str]:
    poetry_locations = [
        [*python_cmd, "-m", "poetry"],
        ["poetry"],
    ]
    for poetry_location in poetry_locations:
        try:
            run_cmd(
                [*poetry_location, "--version"],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
            )
            return poetry_location
        except (subprocess.CalledProcessError, FileNotFoundError, CLIError) as e:
            pass
    raise CLIError(
        f"Could not find Poetry (package manager used by TIM). Did previous install command work correctly?"
    )


def verify_venv_pip() -> None:
    venv_path = Path.cwd() / ".venv"
    if not venv_path.exists():
        raise CLIError(
            f"Could not find a Python virtual environment at {venv_path}. "
            "Did the previous poetry install command work correctly?"
        )
    pip_commands = [
        str(venv_path / "bin" / "pip"),  # Linux,
        str(venv_path / "Scripts" / "pip.exe"),  # Windows
    ]
    for pip_command in pip_commands:
        try:
            run_cmd(
                [pip_command, "--version"],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
            )
            return
        except (subprocess.CalledProcessError, FileNotFoundError, CLIError) as e:
            pass
    raise CLIError(
        f"Could not find pip in the Python virtual environment at {venv_path}. "
        "Did the previous poetry install command work correctly?"
    )


def init_prod_config() -> None:
    prod_config = Path.cwd() / "timApp" / "prodconfig.py"
    if prod_config.exists():
        log_info("TIM server config already exists, skipping generating one")
        return
    log_info("Generating TIM server config with random secret key")
    secret_key = secrets.token_urlsafe(32)
    with prod_config.open("w", encoding="utf-8") as f:
        f.write(
            f"""
# This is the configuration for the main TIM server
# Refer to defaultconfig.py for the default values

SECRET_KEY = '{secret_key}' 
"""
        )
    log_info(f"Generated default TIM server config to {prod_config}")


def setup_pycharm_dev() -> None:
    log_info("Setting up the development environment for PyCharm")
    python = verify_dev_python()
    venv_path = Path.cwd() / VENV_NAME

    if not venv_path.exists():
        log_info("Creating Python virtual environment")
        run_cmd([*python, "-m", "venv", VENV_NAME])

    plat = platform.system()
    if plat == "Linux":
        venv_bin_path = str(venv_path / "bin")
    elif plat == "Windows":
        venv_bin_path = str(venv_path / "Scripts")
    else:
        raise CLIError(f"Unsupported platform: {platform.system()}")

    python = verify_dev_python(f"{venv_bin_path}{os.path.sep}")
    pip = verify_pip(f"{venv_bin_path}{os.path.sep}")

    log_info("Downloading Poetry")
    run_cmd(
        [
            *pip,
            "install",
            "--upgrade",
            f"poetry=={POETRY_MIN_VERSION}",
        ]
    )
    poetry = verify_poetry(python)

    log_info("Installing Python development dependencies")
    run_cmd([*poetry, "install", "--only=dev"])
    verify_venv_pip()

    if not verify_npm(False):
        log_info("Ensuring npm@6 is installed")
        run_npm(["install", "--global", "npm@6"], "timApp", False)
        reset_npm_version()

    idea_path = Path.cwd() / ".idea"
    if not idea_path.exists():
        log_info("Copying project workspace template")
        idea_template = Path.cwd() / "cli" / "templates" / ".idea"

        # Iterate over all files in the template directory recursively and replace $TIM_DOCKER_COMPOSE$ with
        # path to the compose file. This should make using the template workspace easier.
        docker_compose_path = str(Path.cwd() / "docker-compose.yml")
        prettier_path = json.dumps(
            str(Path.cwd() / "timApp" / "node_modules" / "prettier")
        ).strip('"')
        ts_path = json.dumps(
            str(Path.cwd() / "timApp" / "node_modules" / "typescript" / "lib")
        ).strip('"')
        venv_bin_path = (
            f"{Path.cwd() / VENV_NAME / 'bin'}{os.path.sep}"
            if platform.system() == "Linux"
            else f"{Path.cwd() / VENV_NAME / 'Scripts'}{os.path.sep}"
        )
        for root, dirs, files in os.walk(idea_template):
            for file in files:
                file_path = Path(root) / file
                file_contents = file_path.read_text(encoding="utf-8")
                file_contents = (
                    file_contents.replace("$TIM_DOCKER_COMPOSE$", docker_compose_path)
                    .replace("$TIM_PRETTIER$", prettier_path)
                    .replace("$TIM_VENV_BIN$", venv_bin_path)
                    .replace("$TIM_TS$", ts_path)
                )
                target_path = idea_path / file_path.relative_to(idea_template)
                target_path.parent.mkdir(parents=True, exist_ok=True)
                target_path.write_text(file_contents, encoding="utf-8")
    else:
        log_info("Project workspace already exists, skipping copying template")


def setup_vscode_dev() -> None:
    log_info("Setting up the development environment for Visual Studio Code")
    venv_path = Path.cwd() / VENV_NAME

    def run_tim_cmd(args: List[str]) -> None:
        run_compose([
            "run",
            "-T",
            "--rm",
            "--no-deps",
            "--workdir",
            "/service",
            "tim",
            *args,
        ])

    if not venv_path.exists():
        log_info("Creating Python virtual environment")
        run_tim_cmd(["python3", "-m", "venv", VENV_NAME])

    log_info("Installing Python development dependencies")
    run_tim_cmd(["poetry", "install"])

    init_devcontainers()


def setup_dev() -> None:
    log_info("Setting up the development environment")
    ide_profile = get_config().ide_profile
    if ide_profile == IdeProfile.PyCharm:
        setup_pycharm_dev()
    elif ide_profile == IdeProfile.VSCode:
        setup_vscode_dev()


def run(args: Arguments) -> None:
    if args.install:
        verify_tim_requirements()
    if has_config() and not args.force:
        raise CLIError(
            "TIM is already initialized. Use --force to force re-initialization."
        )

    config = get_config(create_if_not_exist=True)

    profile = get_value(
        args.profile,
        args.interactive,
        "--profile",
        """
Which TIM run profile should be used?
Select one of the following based on your needs:

* prod: Run the TIM instance in production mode. All services will be installed and started.
* dev: Run TIM instance in local development mode. Allows you to develop TIM and related services.
* test: Run TIM in CI testing mode. Starts a minimal number of services to speed up testing.
""",
        lambda x: check_choices(x, ["prod", "dev", "test"]),
    )

    config.set("compose", "profile", profile)

    if profile == "prod":
        http_port = get_value(
            str(args.http_port) if args.http_port else "",
            args.interactive,
            "--http-port",
            """
What port should HTTP requests be served on?
Specify a port number or specify 'none' to not accept HTTP requests.
""",
            check_port,
            "80",
            "80",
        )
        config.set("caddy", "http_port", http_port)

        https_port = get_value(
            str(args.https_port) if args.https_port else "",
            args.interactive,
            "--https-port",
            """
What port should HTTPS requests be served on?
Specify a port number or specify 'none' to not accept HTTPS requests.

Note: TIM will attempt to acquire a HTTPS certificate via Let's Encrypt if HTTPS is enabled.
Disable HTTPS if you are using a custom reverse proxy with custom certificate management.
""",
            check_port,
            "443",
            "443",
        )
        config.set("caddy", "https_port", https_port)

        is_proxy = get_value(
            args.is_proxied,
            args.interactive,
            "--is-proxied",
            """
Will you run TIM behind a (reverse) proxy (yes/no)?

You can run TIM behind a reverse proxy like nginx, Apache or Caddy.
This is sometimes useful if you have a load balancer or a firewall
in front of TIM, or if you want to run multiple instances on the same machine.
            """,
            check_yes_no,
            False,
            "no",
        )
        config.set("caddy", "is_proxied", "yes" if is_proxy else "no")

        hostname = get_value(
            args.hostname,
            args.interactive,
            "--hostname",
            """
What is the primary address that TIM will be accessible from?
Specify the hostname along with the HTTP/HTTPS scheme in format http://hostname or https://hostname.
""",
            check_hostname,
            urlparse("http://localhost"),
        )
        host_url = f"{hostname.scheme}://{hostname.netloc}"
        config.set("tim", "host", host_url)

        domains = get_value(
            args.domains,
            args.interactive,
            "--domains",
            """
Specify any external domains that TIM will be accessible from.
You can specify multiple URLs or domains that TIM will be accessible from.
For possible values, refer to Caddy's documentation:

    https://caddyserver.com/docs/caddyfile/concepts#addresses
    
In most cases, you can use the default value (which is the same as the TIM host).
""",
            check_string,
            host_url,
        )
        config.set("caddy", "domains", domains)

    elif profile == "dev":
        ide_profile = get_value(
            args.ide_profile,
            args.interactive,
            "--ide-profile",
            """
Which IDE profile should be used?
TIM supports development with multiple IDEs.
Select the IDE you are using to develop TIM:

* pycharm: Use JetBrains PyCharm IDE for development. Note that the Community version is not supported.
* vscode: Use Microsoft Visual Studio Code for development.
""",
            lambda x: check_choices(x, IdeProfile.choices()),
        )
        config.set("dev", "ide_profile", ide_profile)

    log_info("Creating tim.conf")
    config.save()
    log_info("Created tim.conf created. Check the config file for more options.")

    if not args.install:
        log_warning(
            "Skipping installation. You will need to install Docker images, NPM packages and build scripts manually."
        )
        return

    log_info("Docker: Pulling TIM images")
    dc_pull_args = ["--quiet"] if not args.interactive else []
    run_compose(["pull", *dc_pull_args])

    if profile == "dev":
        setup_dev()

    if profile == "prod":
        init_prod_config()

    log_info("NPM: Installing TIM dependencies")
    npmi()

    log_info("Building Rust dependencies")
    build_rust()

    if profile != "dev":
        log_info("Building TIM scripts")
        js(False, [])
    if args.up:
        log_info("Docker: Starting containers")
        up()
        log_info("TIM is now up and running!")
    else:
        log_info(
            "TIM is now installed! You can run `./tim up` to start the TIM instance."
        )


def init(parser: ArgumentParser) -> None:
    parser.add_argument(
        "--force",
        help="Force re-initialization of the instance",
        action="store_true",
    )
    parser.add_argument(
        "--no-interactive",
        help="No interactive mode. Prompts are disabled, and some commands are run in quiet mode.",
        action="store_false",
        dest="interactive",
    )
    parser.add_argument(
        "--no-install",
        help="Skip installing TIM services and compiling scripts",
        action="store_false",
        dest="install",
    )
    parser.add_argument(
        "--no-up",
        help="Skip starting TIM services",
        action="store_false",
        dest="up",
    )
    parser.add_argument(
        "--profile",
        help="TIM instance run profile",
        choices=["dev", "prod", "test"],
    )
    parser.add_argument(
        "--ide-profile",
        help="TIM instance run profile",
        choices=IdeProfile.choices(),
        dest="ide_profile",
    )
    parser.add_argument(
        "--http-port",
        help="HTTP port mapping to use for TIM instance",
        metavar="[0-65535]",
        type=make_port,
        dest="http_port",
    )
    parser.add_argument(
        "--https-port",
        help="HTTPS port mapping to use for TIM instance",
        metavar="[0-65535]",
        type=make_port,
        dest="https_port",
    )
    parser.add_argument(
        "--hostname",
        help="Hostname to use for the TIM instance",
        metavar="http[s]://[HOSTNAME]",
    )
    parser.add_argument(
        "--domains",
        help="Caddy domain list to listen to",
        metavar="http[s]://[HOSTNAME]",
    )
    parser.add_argument(
        "--is-proxied",
        help="If specified, the TIM instance will be configured to being able to run behind a reverse proxy.",
        choices=["yes", "no"],
        dest="is_proxied",
    )
