from languages import *
from jsframe import *
from stack import *
from geogebra import *
from extcheck import ExtCheck

languages = {}

def populate_languages():
    global languages
    classes =  [Language] + Language.all_subclasses()
    languages = {}
    for cls in classes:
        if not hasattr(cls, "ttype"):
            raise Exception(f"Language {cls.__name__} hasn't defined ttype")
        if cls.ttype is None:
            continue
        cls.ttype = cls.ttype.lower()
        if cls.ttype in languages:
            raise Exception(f"Language {cls.__name__} has a duplicate ttype ({cls.ttype}) with {languages[cls.ttype].__name__}")
        languages[cls.ttype] = cls

populate_languages()
