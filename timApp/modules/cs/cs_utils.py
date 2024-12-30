import re
from tim_common.fileParams import get_2_items, get_param, QueryClass


def replace_code(rules: list, s: str) -> str:
    """
    Use all rules and replace them in s
    :param rules: list of {"replace": "at", "by": "cc"} type pairs
    :param s: string where to replace
    :return: edited string
    """
    result = s
    if not rules:
        return result

    for rule in rules:
        cut_replace, cut_by = get_2_items(rule, "replace", "by", None, "")
        if cut_replace:
            try:
                p = re.compile(cut_replace, flags=re.S)
                result = p.sub(cut_by, result)
                """ # Who could this ever work?
                while True:
                    m = re.search(cut_replace, result, flags=re.S)
                    if not m:
                        break
                    result = result.replace(m.group(1), cut_by)
                """
            except Exception as e:
                msg = str(e)
                if isinstance(e, IndexError):
                    msg = "group () missing"
                result = (
                    "replace pattern error: "
                    + msg
                    + "\n"
                    + "Pattern: "
                    + cut_replace
                    + "\n\n"
                    + result
                )
    return result


def check_parsons(
    expect_code: str,
    usercode: str,
    maxn: int,
    notordermatters: bool = False,
    edit_code_rules: str | None = None,
) -> tuple[int, list]:
    """
    Checks if order of usercode matches expect_code order
    :param expect_code: where to compare
    :param usercode: what to compare
    :param maxn: how many lines max to compare
    :param notordermatters: if true, then order does not matter
    :param edit_code_rules: rules to edit code
    :return: (1 if matches; 0 otherwise, list of corretcs)
    """
    p = 0
    if edit_code_rules:
        expect_code = replace_code(edit_code_rules, expect_code)
        usercode = replace_code(edit_code_rules, usercode)
    exlines = expect_code.splitlines()[:maxn]
    usrlines = usercode.splitlines()[:maxn]

    n = max(len(usrlines), len(exlines))
    correct = [-1] * n

    if notordermatters:
        for j, usrline in enumerate(usrlines):
            for i, exline in enumerate(exlines):
                if usrline == exline:
                    exlines[i] = "XXXXXXXXXXXX"
                    p += 1
                    correct[j] = 1
                    break
    else:
        for i, exline in enumerate(exlines):
            if i >= len(usrlines):
                break
            if exline == usrlines[i]:
                p += 1
                correct[i] = 1
    if p == len(exlines) and p == len(usrlines):
        return 1, correct
    return 0, correct


def text_value_replace(
    query: QueryClass, text: str, replace_key: str, by_key: str
) -> str:
    text_replace = get_param(query, replace_key, None)
    text_by = get_param(query, by_key, "")
    if text_replace:
        if isinstance(text_replace, list):
            for rep in text_replace:
                replace = rep.get("replace", "")
                if replace:
                    text = re.sub(replace, rep.get("by", text_by), text, flags=re.M)
        else:
            text = re.sub(text_replace, text_by, text, flags=re.M)
    return text
