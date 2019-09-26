from dataclasses import dataclass


@dataclass(frozen=True)
class FolderCreationOptions:
    apply_default_rights: bool = False
    get_templates_rights_from_parent: bool = True
