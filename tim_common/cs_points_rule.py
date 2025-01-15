"""
Functions to handle cs points_rule rules.
TODO: change points_rule to proper class
"""
import re

from marshmallow import EXCLUDE

from tim_common.markupmodels import HiddenFieldsMixin
from tim_common.marshmallow_dataclass import dataclass, class_schema


@dataclass
class CsPluginPointsRule:
    """
    Points rules used in csPlugin.
    """

    class Meta:
        unknown = (
            EXCLUDE  # Ignore general TIM fields for pointsRule not used by csPlugin
        )

    # TODO: Document all csPlugin pointsRule fields as they are typed
    cumulative: bool = True
    """Whether the points are cumulative or not."""
    compile: float | None = None
    """Points for successful code compilation."""
    run: float | None = None
    """Points for code running without errors."""
    test: float | None = None
    """Points for successful tests."""
    doc: float | None = None
    """Points for documentation being generated successfully."""
    code: float | None = None
    """Points for code being correct (see expectCode and expectCodePlain)."""
    output: float | None = None
    """Points for correct output (see expectOutput and expectOutputPlain)."""
    numberRule: str | list[str] | None = None
    """Rule for points based on a number."""
    expectCode: str | None = None
    """RegEx that the user's input must match to get code points.."""
    expectCodePlain: str | None = None
    """Contents that the user's input muse be to get code points."""
    expectOutput: str | None = None
    """RegEx that the program's output must match to get output points."""
    expectOutputPlain: str | None = None
    """Contents that the program's output must be to get output points."""
    multiplier: float = 1.0
    """Multiplier for the points."""


@dataclass
class CsPluginMarkup(HiddenFieldsMixin):
    """
    Markup values used in csPlugin.
    """

    # TODO: Document more csPlugin markup fields as they are typed
    pointsRule: CsPluginPointsRule | None = None


CsPluginMarkupSchema = class_schema(CsPluginMarkup)()


def count_max_points(markup: dict, details: bool) -> str | None:
    """
    Counts max possible points from rule
    :param points_rule: rule to us
    :param details: if true give details, otherwise only the max
    :return: max points axplained
    """
    markup_obj = CsPluginMarkupSchema.load(markup, unknown=EXCLUDE)
    points_rule = markup_obj.pointsRule

    if not points_rule:
        return None

    points_sum: float = 0
    details_text = ""
    found = 0
    plus = ""

    def add_points_value(item: str, p: float) -> None:
        nonlocal points_sum
        nonlocal found
        nonlocal details_text
        nonlocal plus
        if points_rule.cumulative:
            points_sum += p
        else:
            if p > points_sum:
                points_sum = p
        if details:
            details_text += f"{plus}{item}:{p:.2g}"
            plus = " + "
        found += 1

    def add_points(item: str, val: float | None, default: float | None = None) -> None:
        if val is not None:
            add_points_value(item, val)
        elif default is not None:
            add_points_value(item, default)

    if points_rule.expectCode is not None or points_rule.expectCodePlain is not None:
        add_points("code", points_rule.code, 1)

    if (
        points_rule.expectOutput is not None
        or points_rule.expectOutputPlain is not None
    ):
        add_points("output", points_rule.output, 1)

    add_points("compile", points_rule.compile)
    add_points("run", points_rule.run)
    add_points("test", points_rule.test)
    add_points("doc", points_rule.doc)

    if points_rule.numberRule is not None:
        add_points_value("number", get_max_number_rule(points_rule.numberRule))

    if found == 0:
        return None

    if details_text != "":
        if points_rule.multiplier != 1:
            details_text = f"({details_text})  *{points_rule.multiplier:.2g}"
        details_text += " = "

    return f"{details_text}{points_rule.multiplier * points_sum:.2g}"


# TODO: This function should probably use types from the CsPluginPointsRule class
def give_points(points_rule: dict, rule: str, default: float = 0) -> None:
    """
    Count poinst to points_rule["result"]
    :param points_rule: rule set to use
    :param rule: rule to check
    :param default: points to use if rule do not have points
    """
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


def get_rule_values(rule: str | list[str]) -> list[str] | None:
    """
    Takes number string out from rule like "give 1 point if more than 3 and less than 5"
    :param rule: rule to use, if only to 2 numbers like  "1 point if equal 2" returns ["1", "2", "2"]
    :return: at least 3 item long list of number strings
    """
    r: list[str]
    if isinstance(rule, list):
        r = rule
    else:
        r = re.findall(r"-?[0-9.]+", rule)
    if len(r) < 2:
        return None
    if len(r) < 3:
        r.append(r[1])
    return r


def get_max_number_rule(number_rule: str | list[str]) -> float:
    """
    count max possible points from rule
    :param number_rule: rule to use, like 1 3 4 giving one point if 3 <= s <= 4
    :return: max points
    """
    min_points = -1000000.0
    points = min_points
    if not isinstance(number_rule, list) or (
        len(number_rule) >= 1 and isinstance(number_rule[0], float)
    ):
        number_rule = [number_rule]

    for rule in number_rule:
        if (r := get_rule_values(rule)) is None:
            continue
        try:
            points = max(float(r[0]), points)
        except ValueError:
            continue
    if points == min_points:
        return 0
    return points


def check_number_rule(s: str, number_rule: str | list[str]) -> float:
    """
    check how many points is given from answer s by rule
    :param s: value to check
    :param number_rule: rule to use, like 1 3 4 giving one point if 3 <= s <= 4
    :return: points for s
    """
    try:
        val = float(s.replace(",", "."))
    except ValueError:
        return 0
    min_points = -1000000.0
    points = min_points
    if not isinstance(number_rule, list) or (
        len(number_rule) >= 1 and isinstance(number_rule[0], float)
    ):
        number_rule = [number_rule]

    for rule in number_rule:
        if (r := get_rule_values(rule)) is None:
            continue
        try:
            if float(r[1]) <= val <= float(r[2]):
                points = max(float(r[0]), points)
        except ValueError:
            continue
    if points == min_points:
        return 0
    return points


def get_points_rule(
    points_rule: dict, key: str, default: float | str | None
) -> str | float | None:
    if not isinstance(points_rule, dict):
        return default
    return points_rule.get(key, default)


def return_points(points_rule: dict, result: dict) -> None:
    if not points_rule:
        return
    if "save" not in result:
        return
    # if not points_rule.get("valid", False) and "points" not in points_rule: return # no rule found
    min_points = points_rule.get("min_points", 0)
    max_points = points_rule.get("max_points", 1e100)
    if "result" in points_rule:
        points = points_rule["result"]
        tim_info = result.get("tim_info", None)
        if tim_info is None:
            result["tim_info"] = {}
            tim_info = result.get("tim_info", None)
        tim_info["points"] = max(min(points, max_points), min_points)
    if "points" in points_rule:
        points = points_rule["points"]
        result["save"]["points"] = points
