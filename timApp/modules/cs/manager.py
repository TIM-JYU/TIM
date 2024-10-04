# TODO: Import and register all languages etc explicitly (= without using star imports).
from modifiers import *
from languages import *

# All languages must be imported for populated even not used in this file
# noinspection PyUnresolvedReferences
from geogebra import *

# noinspection PyUnresolvedReferences
from jsframe import *
from stack import *

# noinspection PyUnresolvedReferences
from extcheck import ExtCheck

# noinspection PyUnresolvedReferences
from gitlang import GitReg, GitCheck
from tim_common.cs_utils import populated

languages = populated(Language)
modifiers = populated(Modifier)


def all_js_files():
    """
    :return: list of needed js-files (maybe copiled from ts-files)
    """
    files = set()

    def add(dictionary):
        for cls in dictionary.values():  # ask needed js and css files from language
            # no time to find what files could throw
            # noinspection PyBroadException
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
            # no time to find what files could throw
            # noinspection PyBroadException
            try:
                files.update(cls.css_files())
            except:
                print(f"Failed to ask for {cls.__name__} css files:")
                print_exc()

    add(languages)
    add(modifiers)
    return list(files)


def make(dictionary, error_cls, desc, name, query, sourcefiles=None):
    kargs = []
    obj = None
    err_str = "unknown"
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


def make_language(name, query, usercode=""):
    """Returns a tuple: (language object of the given class name, bool: whether it succeeded).
    Returns (LanguageError object, false) on failure"""
    return make(languages, LanguageError, "Language", name, query, usercode)


def make_modifier(name, query):
    return make(modifiers, ModifierError, "Modifier", name, query)
