"""
Default configuration for TIM.
"""

# Update this value every time you change the default configuration!
# This is used to automatically apply migrations to the config file.

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
        "profiles": (
            "prod",
            """
Possible values: prod, prod_multi, dev, test
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
    },
    "tim": {
        "host": (
            "http://localhost",
            """
Hostname for the TIM instance.
""",
        ),
        "config_file": (
            "prodconfig.py",
            """
Location of config file to use in addition to defaultconfig (relative to timApp directory).
In production, you need to create the prodconfig.py file.
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
        "port_mapping": (
            "80:80\n443:443",
            """
Specify how to map host ports to internal Caddy ports.
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

DEBUGGABLE_SERVICES = [
    "csplugin",
    "fields",
    "jsrunner",
    "showfile",
    "pali",
    "feedback",
    "drag",
    "imagex",
    "mailman",
]

for service in DEBUGGABLE_SERVICES:
    sec = DEFAULT_CONFIG.get(service, {})
    sec["dev"] = (
        "no",
        f"""
Enable development mode for {service} Docker service.
This allows you to manually start the service via an IDE.
""",
    )
    DEFAULT_CONFIG[service] = sec
