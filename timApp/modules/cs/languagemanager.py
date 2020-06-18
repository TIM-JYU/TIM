from traceback import print_exc

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

def all_js_files():
    """
    :return: list of needed js-files (maybe copiled from ts-files)
    """
    files = set()
    for language_class in languages.values():  # ask needed js and css files from language
        try:
            files.update(language_class.js_files())
        except:
            print(f"Failed to ask for {language_class.__name__} js files:")
            print_exc()
    return list(files)

def all_css_files():
    """
    :return: list of needed css-files (maybe copiled from scss-files)
    """
    files = set()
    for language_class in languages.values():  # ask needed js and css files from language
        try:
            files.update(language_class.css_files())
        except:
            print(f"Failed to ask for {language_class.__name__} css files:")
            print_exc()
    return list(files)
