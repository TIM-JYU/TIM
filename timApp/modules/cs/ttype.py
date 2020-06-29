from re import split

from languages import LanguageError
from manager import make_language, make_modifier

class TType:
    def __init__(self, ttype_str, query, usercode = ""):
        self.success = True
        self.modifiers = []
        self.parts = TType.split(ttype_str)
        if not self.parts:
            self.language = LanguageError(query, "", f"Invalid ttype (probably empty): {ttype_str}")
            return
        
        self.language, lang_class, success = make_language(self.parts[0], query, usercode)
        self.success = self.success and success
        self.parts[0] = lang_class.get_client_ttype(self.parts[0])
        
        for i, part in enumerate(self.parts[1:], 1):
            modifier, mod_class, success = make_modifier(part, query)
            self.success = self.success and success
            self.parts[i] = mod_class.get_client_ttype(part)
            self.modifiers.append(modifier)
        
        self.modifiers.reverse()
    
    def get_language(self):
        return self.language
    
    def runner_name(self):
        if not self.success:
            return "cs-error" # TODO: some kind of priority/shadowing system so this isn't necessary
        
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
    
    def has_modifier_or_language(self, cls):
        return self.has_language(cls) or self.has_modifier(cls)
    
    def __str__(self):
        return "/".join(self.parts)
        
    @staticmethod
    def split(ttype):
        """Returns a list of the parts of ttype"""
        return list(filter(None, split(r'[\s,|;\\/]', ttype.lower())))