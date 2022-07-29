# TIM CLI

This folder contains the TIM command line interface (CLI). The CLI is used to create and manage the active TIM instance.

## Requirements

TIM CLI requires Python 3.6 or higher.
No further external dependencies are required to run the CLI,
however installing and running TIM may require additional software.
The CLI informs of any missing dependencies automatically.

## Usage

The easiest way to use the CLI is to use the shell bootstrap script (`tim`) located in TIM's root directory.

You can always pass `--help` to any of the commands to view the help page. For example:

```bash
# Assuming we're in the TIM root directory (the parent folder of this README)
$ ./tim --help
usage: tim [-h] [--verbose] {dc,js,npmi,restart,run,setup,test,up,dev,pg,tool} ...

Manage the current TIM instance

options:
  -h, --help            show this help message and exit
  --verbose, -v         Enable verbose logging

commands:
  Available commands

  {dc,js,npmi,restart,run,setup,test,up,dev,pg,tool}
                        Additional help
    dc                  Run a docker-compose command on TIM containers
    js                  Compile JavaScript files for production mode
    npmi                Run `npm install` to install TIM dependencies.
    restart             Restart all TIM containers
    run                 Run a command in a TIM container
    setup               Set up the TIM instance
    test                Run unit tests
    up                  (Re)create all containers and start TIM
    dev                 Commands for developing TIM locally
    pg                  Commands for managing the PostgreSQL database
    tool                Various helper commands for managing and developing TIM
```

Alternatively, you can invoke the CLI directly using Python:

```
python3 -m cli.tim
```

> **NOTE**: TIM CLI assumes that the current working directory is the TIM root directory.
> Therefore, if you are running the CLI manually from a different directory, you must first change to the TIM root directory.
> It is *recommended* to use the helper `tim` script as it automatically sets the current working directory correctly.


## Development

TIM CLI is a collection of TIM scripts that are located in the [`commands? ](commands) directory.

For example, invoking `tim dc` will simply run the [`commands/setup.py`](commands/setup.py) script.

To add a new script, create a Python file in the `commands` directory. Boilerplate:

```python
from argparse import ArgumentParser

from cli.util.logging import log_info

info = {"help": "Prints a message to the console"}

class Arguments:
    message: str

def cmd(args: Arguments) -> None:
    log_info(args.message)


def init(parser: ArgumentParser) -> None:
    parser.add_argument(
        "message",
        help="The message to print",
    )
```

Notes:

* The contents of `info` dict will be passed to [`ArgumentParser.add_subparsers`](https://docs.python.org/3/library/argparse.html#argparse.ArgumentParser.add_subparsers). Use the variable to provide any help info.
* `cmd` method *is required* for every command script. It will be called by argparse with the [`Namespace`](https://docs.python.org/3/library/argparse.html#argparse.Namespace) object as parameter.
* `init` method *is optional* for every command script. It will be called with the [`ArgumentParser`](https://docs.python.org/3/library/argparse.html#argparse.ArgumentParser) instance as an argument. Use it to specify the arguments and the behaviour.
* `class Arguments` is not required, but it is useful for strong typing.
* **Don't use Python 3.7+ features!** The CLI tool is targeted to run on Python 3.6.

### Helper methods

The TIM CLI provides some helpers to help with scripts:

* `cli.util.proc.run_cmd` - Runs an external command
* `cli.util.errors.CLIError` - Raising this error will automatically log the error message and exit with a non-zero error code
* `cli.config.loader.get_config` - Get the current TIM configuration (`tim.conf`)
* `cli.docker.run.run_compose` - Run a docker-compose command
* `cli.docker.run.run_docker` - Run a docker command
* `cli.npm.run.run_npm` - Run a npm command
* `cli.util.logging.log_*'` - Logs messages with various levels to the console
  * Note: `log_debug` messages will be visible only if the user passes the `--verbose` flag to TIM CLI 
 
### Updating configuration file

TIM CLI allows to automatically upgrade the configuration file via a migration logic.

If you need to update the configuration file, do the following:

1. Update default values in [`config/default_config.py`](config/default_config.py) if needed
2. Increment `CURRENT_REVISION` in [`config/default_config.py`](config/default_config.py)
3. Create a migration function in [`config/migrations.py`](config/migrations.py) (check `_migrate_variables` for example)
    * The function receives the old configuration as a parameter and modifies it
4. Add the migration function to `MIGRATIONS` list in [`config/migrations.py`](config/migrations.py)