import json
import numbers
import random
import time


def get_sample_list(ranges: str):
    ret = []
    ranges = ranges.split(":")
    n = int(ranges[0])
    if len(ranges) == 1:  # s10
        ints = list(range(0, n, 1))
        random.shuffle(ints)
        return ints
    r = json.loads(ranges[1])

    if isinstance(r, int):  # s10:50
        r = [r]
    if len(r) < 2:  # s10:[50]
        r.insert(0, 0)
    step = 1
    if len(r) > 2:
        step = r[2]

    ints = list(range(r[0], r[1] + 1, step))
    i = n
    while i >= len(ints):
        random.shuffle(ints)
        ret.extend(ints)
        i -= len(ints)
    random.shuffle(ints)
    ret.extend(ints[0:i])
    return ret


def get_int_list(jso):
    ranges = json.loads(jso)
    if isinstance(ranges, int):  # only on item, rnd=6
        return [random.randint(0, ranges)]
    ret = []
    for r in ranges:
        if isinstance(r, int):  # only on item, rnd=[6, 4]
            ret.append(random.randint(0, r))
        else:
            if len(r) < 2:
                r.insert(0, 0)
            step = 1
            if len(r) > 2:
                step = r[2]
            ret.append(random.randrange(r[0], r[1] + 1, step))
    return ret


def get_uniform_list(jso):
    ranges = json.loads(jso)
    if isinstance(ranges, numbers.Number):  # only on item, rnd=6
        return [random.uniform(0, ranges)]
    ret = []
    for r in ranges:
        if isinstance(r, numbers.Number):  # only on item, rnd=[6, 4]
            ret.append(random.uniform(0, r))
        else:
            if len(r) < 2:
                r.insert(0, 0)
            ret.append(random.uniform(r[0], r[1]))
    return ret


def get_rnds(attrs, name="rnd", rnd_seed=None):
    """ Returns list of random numbers based to attribute name (def: rnd)  and rnd_seed """
    if attrs is None:
        return None, rnd_seed
    rnd_seed = attrs.get('seed', rnd_seed)
    if rnd_seed is None:
        rnd_seed = time.clock()*1000

    # noinspection PyBroadException
    try:
        rnd_seed = int(rnd_seed)
    except:
        rnd_seed = int(time.clock()*1000)

    jso = attrs.get(name, None)
    if jso is None:
        return None, rnd_seed
    random.seed(a=rnd_seed)
    ret = None

    # noinspection PyBroadException
    try:
        if jso.startswith('s'):  # s10:[1,7,2], s10, s10:50, s10:[0,50]
            return get_sample_list(jso[1:]), rnd_seed
        if jso.startswith('u'):  # s10:[1,7,2], s10, s10:50, s10:[0,50]
            return get_uniform_list(jso[1:]), rnd_seed

        ret = get_int_list(jso)
    except:
        ret = None
    return ret, rnd_seed


def get_rands_as_dict(attrs, rnd_seed):
    if attrs is None:
        return None, rnd_seed
    names = attrs.get('rndnames', 'rnd').split(',')
    ret = {}
    for name in names:
        rnds, rnd_seed = get_rnds(attrs, name, rnd_seed)
        if rnds is None:
            continue
        ret[name] = rnds
    if not ret:
        return None, rnd_seed
    ret['seed'] = rnd_seed
    return ret, rnd_seed


def get_rands_as_str(attrs, rnd_seed):
    if attrs is None:
        return '', rnd_seed
    rands, rnd_seed = get_rands_as_dict(attrs, rnd_seed)
    if rands is None:
        return '', rnd_seed
    ret = '', rnd_seed
    for name, rnds in rands.items():
        if rnds is None:
            continue
        ret += '{% set ' + name + '=' + str(rnds) + ' %}\n'
    return ret, rnd_seed


def myhash(s):
    csum = 0
    for c in s:
        csum += ord(c)
    return csum


def get_seed_from_par_and_user(block, user):
    h = str(block.get_id()) + str(block.get_doc_id())
    if user:
        h += user.name
    rnd_seed = myhash(h) & 0xffffffff
    return rnd_seed
