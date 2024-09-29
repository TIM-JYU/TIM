from file_handler import File
from languages import LanguageError
from manager import make_language, make_modifier
from tim_common.utils import type_splitter


class TType:
    def __init__(self, ttype_str, query, sourcefiles=None):
        if sourcefiles is None:
            sourcefiles = [File.default(query)]
        self.success = True
        self.modifiers = []
        self.parts = TType.split(ttype_str)
        if not self.parts:
            self.language = LanguageError(
                query, "", f"Invalid ttype (probably empty): {ttype_str}"
            )
            return

        self.language, lang_class, success = make_language(
            self.parts[0], query, sourcefiles
        )
        self.success = self.success and success
        self.parts[0] = lang_class.get_client_ttype(self.parts[0])

        for i, part in enumerate(self.parts[1:], 1):
            modifier, mod_class, success = make_modifier(part, query)
            self.success = self.success and success
            self.parts[i] = mod_class.get_client_ttype(part)
            self.modifiers.append(modifier)

        self.modifiers.reverse()

    def __contains__(self, item):
        if isinstance(item, str):
            return item.lower() in self.parts
        else:
            return self.has(item)

    def get_language(self):
        return self.language

    def runner_name(self):
        if not self.success:
            return "cs-error"  # TODO: some kind of priority/shadowing system so this isn't necessary

        runner = self.language.runner_name()
        for mod in self.modifiers:
            run = mod.runner_name()
            if run is not None:
                runner = run
        return runner

    def modify_query(self):
        self.language.modify_query()
        for mod in self.modifiers:
            mod.modify_query()

    def has_modifier(self, cls):
        return any(isinstance(mod, cls) for mod in self.modifiers)

    def has_language(self, cls):
        return isinstance(self.language, cls)

    def has(self, cls):
        return self.has_language(cls) or self.has_modifier(cls)

    def has_any_of(self, cls_list):
        if cls_list and isinstance(cls_list[0], str):
            return any(cls in self for cls in cls_list)
        else:
            return any(
                self.has_language(cls) or self.has_modifier(cls) for cls in cls_list
            )

    def has_all_of(self, cls_list):
        if cls_list and isinstance(cls_list[0], str):
            return all(cls in self for cls in cls_list)
        else:
            return all(
                self.has_language(cls) or self.has_modifier(cls) for cls in cls_list
            )

    def __str__(self):
        return "/".join(self.parts)

    @staticmethod
    def split(ttype):
        """Returns a list of the parts of ttype"""
        return list(filter(None, type_splitter.split(ttype.lower())))
