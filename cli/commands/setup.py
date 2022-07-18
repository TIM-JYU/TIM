from argparse import ArgumentParser
from typing import List, Optional, TypeVar, Callable, Union, Tuple, Any
from urllib.parse import ParseResult, urlparse

from cli.config import has_config, get_config
from cli.util.errors import CLIError
from cli.util.logging import log_info

info = {
    "help": "Set up the TIM instance",
    "description": """
Set up the TIM instance and initialize necessary configuration files.
This command is intended to be run once, but can be re-run to re-initialize the instance.
""",
}


class Arguments:
    force: bool
    interactive: bool
    install: bool
    profile: Optional[str]
    hostname: Optional[str]
    ports: Optional[str]
    domains: Optional[str]
    is_proxied: Optional[bool]


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
        answer = input(f"{prompt_name}: ")
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


def check_ports(
    ports_string: Union[Optional[str], List[str]]
) -> Tuple[Optional[List[str]], Optional[str]]:
    if isinstance(ports_string, list):
        return ports_string, None
    if ports_string is None:
        return None, "Ports are required"
    ports = ports_string.split(";")
    ports_result = []
    for port_mapping in ports:
        split_vals = port_mapping.split(":")
        if len(split_vals) != 2:
            return None, f"Invalid port mapping: {port_mapping}"
        from_port, to_port = split_vals
        if not from_port or not to_port:
            return (
                None,
                f"Invalid port mapping: {port_mapping}. Port mapping must be in format 'host_port:tim_port'",
            )
        ports_result.append(port_mapping.strip())
    return ports_result, None


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
    value: Optional[str],
) -> Tuple[Optional[bool], Optional[str]]:
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


def cmd(args: Arguments) -> None:
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
        ports = get_value(
            args.ports,
            args.interactive,
            "--ports",
            """
What ports should be used for the TIM instance?
By default, TIM runs internally on ports 80 and 443 (you may change these defaults later in tim.conf).
Specify a ;-separated list of port mapping between the machine and internal TIM ports.

Examples:
  * Map machine's ports 80 and 443 to TIM's ports 80 and 443 (uses Caddy's HTTPS for 443): 80:80;443:443
  * Map machine's port 50000 to TIM's port 80 (allows running multiple instances with custom HTTPS): 50000:80
""",
            check_ports,
            ["80:80", "443:443"],
            "80:80;443:443",
        )
        config.set("caddy", "port_mapping", "\n".join(ports))

        has_non_common_ports = any(
            True
            for port in ports
            if not port.startswith("80:") and not port.startswith("443:")
        )

        if has_non_common_ports:
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

    log_info("Creating tim.conf")
    config.save()
    log_info("tim.conf created")


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
        "--profile",
        help="TIM instance run profile",
        choices=["dev", "prod", "test"],
    )
    parser.add_argument(
        "--ports",
        help="Port mapping for the TIM instance in format 'host_port:tim_port'. "
        "Specify multiple ports by separating them with `;`.",
        metavar="host_port1:tim_port1[;host_port2:tim_port2[;...]]",
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
        type=lambda x: True if x.lower() in ("yes", "y", "true") else False,
        dest="is_proxied",
        metavar="{yes,no}",
    )
    parser.set_defaults(run=cmd)
