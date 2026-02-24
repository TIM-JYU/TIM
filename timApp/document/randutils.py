import base64
import random
import string

import mmh3

_rng = random.Random()

"""
Utilities for generating random data.
This utils uses local _rng random object and it should NOT!!!
be seeded, because it is used for generating 
random IDs and they should be different each time.
"""

alphanum = string.digits + string.ascii_lowercase + string.ascii_uppercase
n_alphanum = len(alphanum)
empty_hash = mmh3.hash("{}")


def seed_for_tests(seed: int) -> None:
    """
    Seeds the random generator for
    tests. This should not be used in production code.
    If you need this for some more test functions,
    add the function names to the allowed list in this function.
    """
    import inspect

    stack = inspect.stack()
    allowed = any(frame.function == "test_parsing" for frame in stack)
    if not allowed:
        raise RuntimeError(
            "This function must be called from DocumentParserTest.test_parsing"
        )
    _rng.seed(seed)


def hashfunc(text: str, attrs: dict | None = None) -> str:
    text_hash = mmh3.hash(text)
    attr_hash = empty_hash if not attrs else mmh3.hash(str(attrs))
    full_hash = text_hash ^ attr_hash
    return base64.b64encode(hex(full_hash).encode()).decode()


def __id_checksum(idstr):
    # Luhn checksum modified to alphanumeric digits
    acc = 0
    for i in range(len(idstr) - 1, -1, -1):
        value = alphanum.find(idstr[i])
        acc += value * 2 if i % 2 == 0 else value
    return acc % n_alphanum


def is_valid_id(randid) -> bool:
    return __id_checksum(randid) == 0


def idchecksum(randid):
    check_digit = __id_checksum(randid + alphanum[0])
    return alphanum[0] if check_digit == 0 else alphanum[n_alphanum - check_digit]


def random_id() -> str:
    randid = "".join(_rng.choice(alphanum) for _ in range(11))
    return randid + idchecksum(randid)


def random_word(min_len=2, max_len=12):
    n = _rng.randint(min_len, max_len)
    return "".join(_rng.choice(string.ascii_lowercase) for _ in range(n))


def random_sentence():
    n = _rng.randint(2, 5)
    s = " ".join(random_word() for _ in range(n))
    return s.capitalize()


def random_sentences():
    n = _rng.randint(1, 3)
    return ", ".join(random_sentence() for _ in range(n))


def random_paragraph():
    n = _rng.randint(3, 6)
    return ". ".join(random_sentences() for _ in range(n)) + "."


def random_jsonpar(par_id):
    content = random_paragraph()
    chash = hashfunc(content, {})
    return [{"id": par_id, "t": chash, "md": content, "html": content}]
