"""
This module provides a class to manage the distribution rights network configuration.

The network is defined in the server config under the key `DIST_RIGHTS_NETWORK`.
"""

from dataclasses import dataclass, field
from functools import cache

from marshmallow import EXCLUDE

from timApp.tim_app import app
from timApp.util.logger import log_warning

from tim_common.marshmallow_dataclass import class_schema


@dataclass(slots=True)
class DistRightTarget:
    """
    Represents a single target for distributing rights.
    """

    target: str
    """
    Host name to which to distribute the rights.
    """
    distribute_group: str | None = None
    """
    If defined, the target will relay the rights to the specified distribution group.
    """


@dataclass(slots=True)
class DistRightNetworkConfig:
    """
    Represents the distribution rights network configuration.

    The configuration defines the different machines in the network and how the rights
    should be distributed between them.
    """

    hosts: dict[str, str] = field(default_factory=dict)
    """
    Mapping of host names to their URLs.
    """
    distribute_groups: dict[str, list[str]] = field(default_factory=dict)
    """
    Mapping of distribution groups. 
    Distribution groups represent machines that can be logically linked together (e.g. they are in the same network).
    """
    distribute_targets: dict[str, list[DistRightTarget]] = field(default_factory=dict)
    """
    Mapping of specific distribution targets. Defines how specifically the right should be distributed.
    """

    def get_host_urls(self, hostnames: list[str]) -> list[str]:
        """
        Get the URLs of the specified host names.
        Invalid host names are ignored.

        :param hostnames: List of host names.
        :return:  List of URLs of the specified host names.
        """
        return [url for h in hostnames if (url := self.hosts.get(h))]

    def get_target_hosts(self, target: str) -> list[str]:
        """
        Get the URLs of the hosts that should receive the rights for the specified target.

        :param target: Target name.
        :return: List of URLs of the hosts that should receive the rights for the specified target.
        """
        target_info = self.distribute_targets.get(target)
        if not target_info:
            return []
        return self.get_host_urls([t.target for t in target_info])

    def get_group_hosts(
        self, group: str, except_hosts: set[str] | None = None
    ) -> list[str]:
        """
        Get the URLs of hosts defined inside a distribution group.

        :param group: Name of the distribution group.
        :param except_hosts: Hosts to exclude from the result.
        :return:  List of URLs of the hosts defined inside the distribution group.
        """
        group_info = set(self.distribute_groups.get(group, []))
        if not group_info:
            return []

        if except_hosts:
            group_info -= except_hosts

        return self.get_host_urls(list(group_info))


_DistRightNetworkConfigSchema = class_schema(DistRightNetworkConfig)(unknown=EXCLUDE)


@cache
def get_dist_right_network() -> DistRightNetworkConfig:
    """
    Get the distribution rights network configuration from the server config.

    :return: Distribution rights network configuration.
    """
    with app.app_context():
        try:
            return _DistRightNetworkConfigSchema.load(app.config["DIST_RIGHTS_NETWORK"])
        except Exception as e:
            log_warning(f"Error loading DIST_RIGHTS_NETWORK: {e}")
            return DistRightNetworkConfig()
