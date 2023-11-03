"""
Default configuration for TIM.
"""

# Update this value every time you change the default configuration!
# This is used to automatically apply migrations to the config file.
from enum import Enum


CURRENT_REVISION = 1

DEFAULT_CONFIG = {
    "__meta__": {
        "revision": (
            f"{CURRENT_REVISION}",
            """
Revision of the configuration. Used for automatic migrations.
DO NOT CHANGE THIS VALUE UNLESS YOU KNOW WHAT YOU ARE DOING.
""",
        ),
    },
    "compose": {
        "profile": (
            "prod",
            """
Possible values: prod, dev, test
Explanations:
* prod: This is a single TIM instance running on a remote machine.
* dev: This is a local development TIM instance.
* test: This instance is used only for running tests. Used mostly in CI.
""",
        ),
        "project_name": (
            "tim",
            """
Name of the Docker Compose project.
Modify only if there are multiple TIM instances in the same host.
""",
        ),
        "images_repository": (
            "ghcr.io/tim-jyu",
            """
Docker images repository to use when pulling images.
With this, you can self-host custom images if need-be.
""",
        ),
    },
    "tim": {
        "host": (
            "http://localhost",
            """
Hostname for the TIM instance.
""",
        ),
        "status_page_url": (
            "",
            """
If main TIM container is down (e.g. during the update),
this URL is shown to the users as the "status page" URL
""",
        ),
        "files_root": (
            "${DIR}/timApp/tim_files",
            """
Root directory for non-db storage (document content, uploaded files, etc.).
""",
        ),
        "log_dir": (
            "${DIR}/tim_logs",
            """
Location of log directory; will be mapped for TIM container at /service/tim_logs.
""",
        ),
        "worker_max_requests": (
            "2000",
            """
Direct value of Gunicorn's max_requests option.
Sets the maximum number of requests a worker will process before restarting.
""",
        ),
        "worker_max_requests_jitter": (
            "200",
            """
Direct value of Gunicorn's max_requests_jitter option.
The maximum jitter to add to the gunicorn_max_requests setting.
""",
        ),
    },
    "caddy": {
        "domains": (
            "http://",
            """
Domain(s) for Caddy server (separate with comma and space).
By default, Caddy domain has no host so that IPs work too (in addition to localhost).
This is important when connecting to local TIM with other devices such as a mobile phone.
For production, replace with your server domain
""",
        ),
        "http_port": (
            "80",
            """
What port to listen on for HTTP requests. If empty, HTTP requests are will not be forwarded to Caddy.
""",
        ),
        "https_port": (
            "443",
            """
What port to listen on for HTTPS requests. If empty, HTTPS requests are will not be forwarded to Caddy.
This may be useful if you are using a custom reverse proxy that will handle HTTPS requests itself.
""",
        ),
        "is_proxied": (
            "no",
            """
Whether the instance will run behind a reverse proxy.
If enabled, the instance will apply fixes to IPs and headers to account for the reverse proxy.
""",
        ),
        "extra_tim_config": (
            "",
            """
Extra configuration to add to main TIM route in Caddyfile.
""",
        ),
        "extra_config": (
            "",
            """
Extra configuration to append to Caddyfile.
""",
        ),
    },
    "postgresql": {
        "__doc__": (
            "",
            """
PostgreSQL configuration.

Any options prefixed with pg_ will be passed to PostgreSQL as configuration options.
""",
        ),
        "max_connections": (
            "200",
            """
Maximum number of allowed connections to PostgreSQL.
""",
        ),
        "shm_size": (
            "64000000",
            """
Size for /dev/shm for PostgreSQL container in bytes.
""",
        ),
    },
    "csplugin": {
        "is_cassandra_enabled": (
            "no",
            """
Whether to enable Cassandra database for use in csplugin programming tasks.
""",
        ),
        "is_mongodb_enabled": (
            "no",
            """
Whether to enable MongoDB database for use in csplugin programming tasks.
""",
        ),
        "cassandra_heap_newsize": (
            "128M",
            """
Initial heap size for Cassandra used for csplugin tasks
""",
        ),
        "cassandra_max_heap_size": (
            "1024M",
            """
Max heap size for Cassandra used for csplugin tasks
""",
        ),
    },
}


class DevServiceBehaviour(Enum):
    StartStopped = "Service is started without an active command.\nThis can be used for starting the server manually."
    Enable = "Service is downloaded and enabled."


DEV_SERVICES = [
    ("csplugin", DevServiceBehaviour.StartStopped),
    ("fields", DevServiceBehaviour.StartStopped),
    ("jsrunner", DevServiceBehaviour.StartStopped),
    ("showfile", DevServiceBehaviour.StartStopped),
    ("pali", DevServiceBehaviour.StartStopped),
    ("feedback", DevServiceBehaviour.StartStopped),
    ("drag", DevServiceBehaviour.StartStopped),
    ("imagex", DevServiceBehaviour.StartStopped),
    ("mailman", DevServiceBehaviour.Enable),
]

for service, behaviour in DEV_SERVICES:
    sec = DEFAULT_CONFIG.get(service, {})
    sec["is_dev"] = (
        "no",
        f"""
Enable development mode for {service} Docker service.
If enabled: {behaviour.value}
""",
    )
    DEFAULT_CONFIG[service] = sec
