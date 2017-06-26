import re


def give_points(points_rule, rule, default=0):
    if not points_rule:
        return
    if rule in points_rule or default != 0:
        points_rule["valid"] = True  # rule found
    p = points_rule.get(rule, default)
    if not points_rule.get("cumulative", True):
        points_rule["result"] = max(points_rule.get("result", 0), p)
        return
    print("rule: ", rule)
    ptstype = "run"
    if "test" in rule:
        ptstype = "test"
    if "doc" in rule:
        ptstype = "doc"
    # if "code" in rule: ptstype = "code"
    pts = points_rule.get("points", None)
    if pts:
        ptype = pts.get(ptstype, 0)
        print(ptstype, "===", pts[ptstype], p)
        pts[ptstype] = ptype + p
    else:
        pts = dict()
        points_rule["points"] = pts
        pts[ptstype] = p
    points_rule["result"] = pts.get("run", 0) + pts.get("test", 0) + pts.get("doc", 0) + pts.get("code", 0)


def check_number_rule(s, number_rule):
    try:
        val = float(s.replace(",", "."))
    except ValueError:
        return 0
    points = 0
    if not isinstance(number_rule, list) or (len(number_rule) >= 1 and isinstance(number_rule[0], float)):
        number_rule = [number_rule]

    for rule in number_rule:
        r = rule
        if not isinstance(r, list):
            r = re.findall(r"-?[0-9.]+", r)
        if len(r) < 2:
            continue
        if len(r) < 3:
            r.append(r[1])
        try:
            if float(r[1]) <= val <= float(r[2]):
                points = max(float(r[0]), points)
        except ValueError:
            continue
    return points


def get_points_rule(points_rule, key, default):
    if not points_rule:
        return default
    return points_rule.get(key, default)


def return_points(points_rule, result):
    if not points_rule:
        return
    if "save" not in result:
        return
    # if not points_rule.get("valid", False) and "points" not in points_rule: return # no rule found
    if "result" in points_rule:
        tim_info = {"points": points_rule["result"]}
        result["tim_info"] = tim_info
    if "points" in points_rule:
        result["save"]["points"] = points_rule["points"]
