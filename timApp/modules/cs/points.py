import re


def give_points(points_rule, rule, default=0):
    if not isinstance(points_rule, dict):
        return
    if rule in points_rule or default != 0:
        points_rule["valid"] = True  # rule found
    p = points_rule.get(rule, default)
    if not points_rule.get("cumulative", True):
        points_rule["result"] = max(points_rule.get("result", 0), p)
        return
    # print("rule: ", rule)
    ptstype = "run"
    pts = points_rule.get("points", None)
    doc_limit = points_rule.get("doc_limit", 0.5)
    if "test" in rule:
        ptstype = "test"
    if "doc" in rule:
        ptstype = "doc"
        other = 0
        if pts:
            other = pts.get("run", 0) + pts.get("test", 0) + pts.get("code", 0)
        if other < doc_limit:  # if not enough other points, no doc points
            p = 0
    # if "code" in rule: ptstype = "code"
    if pts:
        ptype = pts.get(ptstype, 0)
        # print(ptstype, "===", pts[ptstype], p)
        pts[ptstype] = ptype + p
    else:
        pts = dict()
        points_rule["points"] = pts
        pts[ptstype] = p

    other = pts.get("run", 0) + pts.get("test", 0) + pts.get("code", 0)
    docpts = pts.get("doc", 0)
    if other < doc_limit:  # if not enough other points, no doc points
        docpts = 0
    points_rule["result"] = other + docpts


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
    if not isinstance(points_rule, dict):
        return default
    return points_rule.get(key, default)


def return_points(points_rule, result):
    if not points_rule:
        return
    if "save" not in result:
        return
    # if not points_rule.get("valid", False) and "points" not in points_rule: return # no rule found
    min_points = points_rule.get("min_points", 0)
    max_points = points_rule.get("max_points", 1e100)
    if "result" in points_rule:
        points = points_rule["result"]
        tim_info = {"points": max(min(points,max_points), min_points)}
        result["tim_info"] = tim_info
    if "points" in points_rule:
        points = points_rule["points"]
        result["save"]["points"] = points
