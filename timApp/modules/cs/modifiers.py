
class Modifier:
    ttype = "_modifier"
    
    def __init__(self, query):
        self.query = query
    
    def runner_name(self):
        return None
    
    def modify_query(self):
        pass
    
    @staticmethod
    def js_files():
        return []
    
    @staticmethod
    def css_files():
        return []
    
    @classmethod
    def all_subclasses(cls):
        subclasses = cls.__subclasses__()
        return subclasses + [i for sc in subclasses for i in sc.all_subclasses()]
    
    @classmethod
    def get_client_ttype(cls, ttype):
        """Returns the ttype of this class that should be given to client"""
        if isinstance(cls.ttype, list):
            return cls.ttype[0]
        return cls.ttype

class ModifierError(Modifier):
    ttype = "_error"
    def __init__(self, query, error_str):
        try:
            super().__init__(query)
        except Exception as e:
            print("Error:", str(e))
            print_exc()
            self.valid = False
            self.own_error = str(e)
        else:
            self.valid = True
            self.own_error = None
        
        self.query = query
        self.error = error_str
        self.own_error = error_str
    
    def modify_query(self):
        error = None
        own_error = None
        if "error" in self.query.query:
            error = self.query.query["error"][0] + "\n" + self.error
        if "own_error" in self.query.query:
            own_error = self.query.query["own_error"][0] + "\n" + self.own_error
        self.query.query["error"] = [error or self.error]
        self.query.query["own_error"] = [own_error or self.error]
    
    def runner_name(self):
        return "cs-error"
    
    @staticmethod
    def js_files():
        return ["/cs/js/build/language_error.js"]
    
    def is_valid(self):
        return self.valid

class Tiny(Modifier):
    ttype = "tiny"
    def runner_name(self):
        return "cs-text-runner"
    
class Input(Modifier):
    ttype = ["input", "args"]
    @classmethod
    def get_client_ttype(cls, ttype):
        return ttype
        
class Doc(Modifier):
    ttype = "doc"

class CSConsole(Modifier):
    ttype = "csconsole"
    def runner_name(self):
        return "cs-console"
        
class Parsons(Modifier):
    ttype = "parsons"
    def runner_name(self):
        return "cs-parsons-runner"
