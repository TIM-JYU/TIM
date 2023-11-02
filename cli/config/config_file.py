from configparser import ConfigParser
from typing import Dict, Tuple, List, Any, TYPE_CHECKING, Optional

from cli.docker.service_variables import (
    tim_image_tag,
    csplugin_target,
    csplugin_image_tag,
)

if TYPE_CHECKING:
    from _typeshed import SupportsWrite


class ProxyDict:
    def set(self, key: str, value: Any) -> None:
        self.__dict__[key] = value

    def get(self, key: str, default: Any = None) -> Any:
        return self.__dict__.get(key, default)


class TIMConfig(ConfigParser):
    """
    TIM configuration file handler.
    """

    def __init__(self, save_path: str, *args: Any, **kwargs: Any) -> None:
        super().__init__(*args, **kwargs)
        self._comment_lines: Dict[Tuple[str, str], str] = {}
        self._save_path = save_path

    @property
    def profile(self) -> str:
        return self.get("compose", "profile")

    @property
    def project_name(self) -> str:
        return self.get("compose", "project_name")

    @property
    def mailman_dev(self) -> bool:
        return self.getboolean("mailman", "is_dev")

    @property
    def host(self) -> str:
        return self.get("tim", "host")

    @property
    def images_repository(self) -> str:
        return self.get("compose", "images_repository")

    def add_comment(self, section: str, option: str, comment: str) -> None:
        self._comment_lines[(section, option)] = comment.strip()

    def load_ext_dict(self, ext_dict: Dict[str, Dict[str, Tuple[str, str]]]) -> None:
        for section, options in ext_dict.items():
            self.add_section(section)
            for key, (value, comment) in options.items():
                self.set(section, key, value)
                self.add_comment(section, key, comment)

    def save(self) -> None:
        with open(self._save_path, "w") as fp:
            self.write(fp)

    def var_ctx(self, profile: Optional[str] = None) -> Dict[str, Any]:
        var_dict = {}
        for section in self._sections:  # type: ignore
            var_dict[section] = ProxyDict()
            for key in self._sections[section].keys():  # type: ignore
                if key.startswith("is_"):
                    var_dict[section].set(key, self.getboolean(section, key))
                elif key.startswith("int_"):
                    var_dict[section].set(key, self.getint(section, key))
                elif key.startswith("float_"):
                    var_dict[section].set(key, self.getfloat(section, key))
                else:
                    var_dict[section].set(key, self.get(section, key))
        var_dict["default"] = ProxyDict()
        for key in self._defaults.keys():  # type: ignore
            var_dict["default"].set(key, self._defaults[key])  # type: ignore

        profile = profile or self.get("compose", "profile")
        var_dict["compose"].set("profile", profile)
        var_dict["tim"].set("image_tag", tim_image_tag())
        var_dict["csplugin"].set("image_tag", csplugin_image_tag())
        var_dict["tim"].set("is_dev", profile == "dev")
        var_dict["csplugin"].set("target", csplugin_target(profile))

        var_dict["postgresql"].set(
            "pg_config_ops",
            [
                f"{k[3:]}={v}"
                for k, v in self._sections.get("postgresql").items()  # type: ignore
                if k.startswith("pg_")
            ],
        )

        if var_dict["mailman"].get("is_dev"):
            var_dict["caddy"].set(
                "extra_tim_config",
                f"{var_dict['caddy'].get('extra_tim_config')}\nimport mailman_dev",
            )

        return var_dict

    def write(
        self, fp: "SupportsWrite[str]", space_around_delimiters: bool = True
    ) -> None:
        if space_around_delimiters:
            d = " {} ".format(self._delimiters[0])  # type: ignore
        else:
            d = self._delimiters[0]  # type: ignore
        # Ensure meta is always written first
        meta_section = self._sections.get("__meta__")  # type: ignore
        if meta_section:
            self._write_section(fp, "__meta__", meta_section.items(), d)
        for section in self._sections:  # type: ignore
            if section == "__meta__":
                continue
            self._write_section(fp, section, self._sections[section].items(), d)  # type: ignore

    def _write_section(
        self,
        fp: "SupportsWrite[str]",
        section_name: str,
        section_items: List[Tuple[str, str]],
        delimiter: str,
    ) -> None:
        doc_comment = self._comment_lines.get((section_name, "__doc__"))
        if doc_comment:
            for comment_line in doc_comment.splitlines():
                fp.write("{} {}\n".format(self._comment_prefixes[0], comment_line))  # type: ignore
        fp.write("[{}]\n".format(section_name))
        for key, value in section_items:
            if key == "__doc__":
                continue
            comment = self._comment_lines.get((section_name, key))
            if comment:
                for comment_line in comment.splitlines():
                    fp.write("{} {}\n".format(self._comment_prefixes[0], comment_line))  # type: ignore
            value = self._interpolation.before_write(self, section_name, key, value)  # type: ignore
            if value is not None or not self._allow_no_value:
                value = delimiter + str(value).replace("\n", "\n\t")
            else:
                value = ""
            fp.write("{}{}\n\n".format(key, value))
        fp.write("\n")
