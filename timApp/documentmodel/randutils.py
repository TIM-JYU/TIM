import base64
import json
import mmh3
import random
import string

alphanum = string.digits + string.ascii_lowercase + string.ascii_uppercase
n_alphanum = len(alphanum)
empty_hash = mmh3.hash('{}')

def hashfunc(text, attrs=None):
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

def is_valid_id(randid):
    return __id_checksum(randid) == 0

def idchecksum(randid):
    check_digit = __id_checksum(randid + alphanum[0])
    return alphanum[0] if check_digit == 0 else alphanum[n_alphanum - check_digit]

def random_id():
    randid = ''.join(random.choice(alphanum) for _ in range(11))
    return randid + idchecksum(randid)

def random_word(min_len = 2, max_len = 12):
    n = random.randint(min_len, max_len)
    return ''.join(random.choice(string.ascii_lowercase) for _ in range(n))

def random_sentence():
    n = random.randint(2, 5)
    s = ' '.join(random_word() for _ in range(n))
    return s.capitalize()

def random_sentences():
    n = random.randint(1, 3)
    return ', '.join(random_sentence() for _ in range(n))

def random_paragraph():
    n = random.randint(3, 6)
    return '. '.join(random_sentences() for _ in range(n)) + '.'

def random_jsonpar(par_id):
    content = random_paragraph()
    chash = hashfunc(content, [])
    return [{'id': par_id, 't': chash, 'md': content, 'html': content}]

