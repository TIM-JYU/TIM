from traceback import print_exc

from modifiers import *
from languages import *
from jsframe import *
from stack import *
from geogebra import *
from extcheck import ExtCheck
from gitlang import GitReg, GitCheck


def populated(base_class):
    dictionary = {}
    classes = [base_class] + base_class.all_subclasses()

    def add(cls, ttype):
        if ttype in dictionary:
            raise Exception(f"{base_class.__name__} {cls.__name__} has a duplicate ttype ({ttype}) with {dictionary[ttype].__name__}")
        dictionary[ttype] = cls

    for cls in classes:
        if not hasattr(cls, "ttype"):
            raise Exception(f"{base_class.__name__} {cls.__name__} hasn't defined ttype")
        if cls.ttype is None:
            continue
        if isinstance(cls.ttype, list):
            if len(cls.ttype) == 0:
                raise Exception(f"{base_class.__name__} {cls.__name__} hasn't defined ttype")
            for ttype in cls.ttype:
                add(cls, ttype.lower())
        else:
            add(cls, cls.ttype.lower())
    return dictionary

languages = populated(Language)
modifiers = populated(Modifier)

def all_js_files():
    """
    :return: list of needed js-files (maybe copiled from ts-files)
    """
    files = set()
    def add(dictionary):
        for cls in dictionary.values():  # ask needed js and css files from language
            try:
                files.update(cls.js_files())
            except:
                print(f"Failed to ask for {cls.__name__} js files:")
                print_exc()
    add(languages)
    add(modifiers)
    return list(files)

def all_css_files():
    """
    :return: list of needed css-files (maybe copiled from scss-files)
    """
    files = set()
    def add(dictionary):
        for cls in dictionary.values():  # ask needed js and css files from language
            try:
                files.update(cls.css_files())
            except:
                print(f"Failed to ask for {cls.__name__} css files:")
                print_exc()
    add(languages)
    add(modifiers)
    return list(files)

def make(dictionary, error_cls, desc, name, query, sourcefiles = None):
    kargs = []
    obj = None
    cls = dictionary.get(name)
    if cls is None:
        err_str = f"Error: {desc} {name} not found."
        print_exc()
    else:
        if sourcefiles is not None:
            if cls.supports_multifiles():
                kargs = [sourcefiles]
            elif len(sourcefiles) == 1:
                kargs = [sourcefiles[0].content]
            else:
                # TODO: make all languages support multiple files OR
                # iterate and save all the files before running, as if there were
                # multiple submission boxes
                if name != "stack":
                    raise ValueError(f"Language {name} does not support multiple files")
                else:
                    kargs = [sourcefiles]

        try:
            obj = cls(query, *kargs)
        except Exception as e:
            err_str = f"Error: {str(e)}"
            print_exc()

    if obj is None:
        return error_cls(query, err_str, *kargs), error_cls, False

    return obj, cls, True

def make_language(name, query, usercode = ""):
    """Returns a tuple: (language object of the given class name, bool: whether it succeeded).
    Returns (LanguageError object, false) on failure"""
    return make(languages, LanguageError, "Language", name, query, usercode)

def make_modifier(name, query):
    return make(modifiers, ModifierError, "Modifier", name, query)