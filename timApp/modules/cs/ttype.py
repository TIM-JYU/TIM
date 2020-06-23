from re import split

from languages import LanguageError
from manager import make_language

class TType:
    def __init__(self, ttype_str, query, usercode = ""):
        self.success = True
        self.parts = TType.split(ttype_str)
        if not self.parts:
            self.language = LanguageError(query, "", f"Invalid ttype (probably empty): {ttype_str}")
            return
        
        self.language, lang_class, success = make_language(self.parts[0], query, usercode)
        self.success = self.success and success
        self.parts[0] = lang_class.get_client_ttype(self.parts[0])
    
    def get_language(self):
        return self.language
    
    def runner_name(self):
        return self.language.runner_name()
    
    def modify_query(self):
        self.language.modify_query()
    
    def has_language(self, cls):
        return isinstance(self.language, cls)
    
    def __str__(self):
        return "/".join(self.parts)
        
    @staticmethod
    def split(ttype):
        """Returns a list of the parts of ttype"""
        return list(filter(None, split(r'[\s,|;\\/]', ttype.lower())))