"""
Module for serving TIM imagex plugin.
See: https://tim.jyu.fi/view/tim/TIMin%20kehitys/Plugin%20development
Serving from local port 5000
"""
import json

from geometry import is_inside

# Library for checking if a point is inside a shape.
from tim_common.fileParams import (
    encode_json_data,
    make_lazy,
    NOLAZY,
    replace_template_params,
    is_review,
    get_all_templates,
    do_headers,
    QueryClass,
    get_param,
    get_json_param,
    query_params_to_map,
    get_query_from_json,
)

# ImagexServer is inherited from this. Contains methods like do_GET, do_PUT, etc generic server stuff.
from tim_common.tim_server import TimServer, start_server

PORT = 5000
PROGDIR = "."


def get_lazy_imagex_html(query: QueryClass) -> str:
    """Returns the lazy version of html before the real template is given by javascript.

    :param query: query params where lazy options can be read
    :return: lazy version of imagex-plugins html

    """
    s = (
        '<div class="csRunDiv no-popup-menu'
        + (" cs-has-header" if get_param(query, "header", None) else "")
        + '">'
    )
    s += replace_template_params(query, "<h4>{{header}}</h4>", "header")
    if not get_param(query, "stem", None) and not get_param(query, "header", None):
        s += '<p class="stem">Open plugin</p>'
    else:
        s += replace_template_params(query, '<p class="stem">{{stem}}</p>', "stem")
    s += "</div>"
    return s


class ImagexServer(TimServer):
    """Class for imagex server that can handle the TIM routes."""

    def get_html(self, query: QueryClass) -> str:
        """Return the html for this query. Params are dumbed as hexstring to avoid problems with html input and so on.

        :param query: get or put params
        :return : html string for this markup

        """
        # print("--Query--"+ query.dump() + "--Query--" ) # uncomment to see query
        # Get user id from session
        user_id = get_param(query, "user_id", "--")
        preview = get_param(query, "preview", False)
        hiddentargets = get_param(query, "-targets", None)

        if is_review(query):
            usercode = "image"
            s = ""
            result = (
                NOLAZY
                + '<div class="review" ng-non-bindable><pre>'
                + usercode
                + "</pre>"
                + s
                + "</div>"
            )
            print("REVIEW: ", result)
            return result

        # Check if this is in preview. If it is, set targets as visible.
        if preview and hiddentargets:
            jso2 = query_params_to_map(query.query)
            jso2["state"] = query.jso["state"]
            jso2["markup"]["targets"] = hiddentargets
            query = get_query_from_json(jso2)

        # do the next if Anonymoys is not allowed to use plugins
        if user_id == "Anonymous":
            allow_anonymous = str(get_param(query, "anonymous", "false")).lower()
            # SANITOIDAAN markupista tuleva syöte
            jump = get_param(query, "taskID", "")
            # print("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX jump: ", jump)
            if allow_anonymous != "true":
                return (
                    NOLAZY
                    + '<p class="pluginError"><a href="/login?anchor='
                    + jump
                    + '">Please login to interact with this component</a></p><pre class="csRunDiv">'
                    + get_param(query, "byCode", "")
                    + "</pre>"
                )  # imageX does not have byCode???

        jso = query_params_to_map(query.query)
        jso["state"] = query.jso["state"]
        runner = "imagex-runner"
        attrs = json.dumps(jso)
        s = f'<{runner} json="{encode_json_data(attrs)}"></{runner}>'
        s = make_lazy(s, query, get_lazy_imagex_html)
        return s

    # gets reqs
    def get_reqs_result(self) -> dict:
        """
        :return: reqs result as json
        """
        # Get templates for plugin
        templs = get_all_templates("templates")
        # print("--templates--" + str(templs))
        ret = {
            "js": ["tim/plugin/imagex"],
            "multihtml": True,
        }
        # Add templates to reqs.
        ret.update(templs)
        return ret

    def do_answer(self, query: QueryClass):
        """Do answer route. Check if images are dragged to correct targets. Award points on whether they are or aren't.

        :param query: post and get params
        :return: nothing

        """

        # print("--QUERYANSWER--" + str(query))
        # Setup for answers
        do_headers(self, "application/json")
        result = {}
        web = {}
        result["web"] = web
        err = ""

        previous_value = {}
        defaults = get_param(query, "defaults", {})

        # Find value for key from value, previous or defaults. If nowhere, return default_value.
        def get_value_def(value, key, default_value, keep_empty_as_none=False):
            keys = key.split(".")
            ret = default_value
            if len(keys) < 1:
                return ret

            k_last = keys.pop()

            v = value
            p = previous_value
            d = defaults
            # Loop v and p to same level
            for k in keys:
                if not k in p:
                    p[k] = {}  # if not on p, add
                p = p[k]  # for next round
                if v and k in v:
                    v = v[k]
                else:
                    v = None
                if d and k in d:
                    d = d[k]
                else:
                    d = None

            k = k_last
            if v and k in v:
                if v[k] and (not keep_empty_as_none or v[k] != ""):
                    ret = v[k]
                    p[k] = ret
                else:
                    p[k] = None
                return ret

            if p and (k in p) and p[k] != None:
                return p[k]
            if d and (k in d) and d[k] != None:
                return d[k]
            return ret

        # print("--state--" + str(get_param(query, "state",None)))
        # Student points
        points = 0
        # get default values for targets. If values arent told for targets get them from here or from previous
        # target.
        # If all tries have been used just return.
        max_infinity = 1000000
        max_tries = int(get_param(query, "answerLimit", max_infinity))
        tries = int(get_json_param(query.jso, "info", "earlier_answers", 0))

        # Targets dict
        try:
            targets = list(get_param(query, "targets", None))
        except:
            targets = []
        drags = get_json_param(query.jso, "input", "drags", None)
        gottenpoints = {}
        gottenpointsobj = {}
        # For tracking indexes
        # Uncomment to see student answers
        # print("---drags---" + str(drags))
        # No points are awarded if all tries have been given or the final answer has been given to the student.
        tnr = 0
        if targets != None:
            for target in targets:
                # Find object name from user input.
                target["points"] = get_value_def(target, "points", {})
                target["type"] = get_value_def(target, "type", "rectangle")
                target["a"] = get_value_def(target, "a", 0)
                target["size"] = get_value_def(target, "size", [10])
                target["position"] = get_value_def(target, "position", [0, 0])
                target["max"] = get_value_def(target, "max", 100000)
                target["snapOffset"] = get_value_def(target, "snapOffset", [0, 0])
                target["n"] = 0
                tnr += 1
                print(target)
                if tries < max_tries:
                    for selectkey in target["points"].keys():
                        for drag in drags:
                            if drag["id"] == selectkey:
                                # Check if needed values exist for target. If they dont, read them from defaults
                                # or first target. # TODO: NOT FROM FIRST!!!
                                # Check if image is inside target, award points.
                                if target["n"] < target["max"] and is_inside(
                                    target["type"],
                                    target["size"],
                                    -target["a"],
                                    target["position"],
                                    drag["position"],
                                ):
                                    target["n"] += 1
                                    drag["td"] = "trg" + str(tnr)
                                    if "id" in target:
                                        drag["tid"] = target["id"]
                                    pts = target["points"][selectkey]
                                    drag["points"] = pts
                                    points += pts
                                    gottenpointsobj[selectkey] = target["points"][
                                        selectkey
                                    ]
                                    gottenpoints.update(gottenpointsobj)
                                    gottenpointsobj = {}

        answer = {}
        # Check if getting finalanswer from exercise is allowed and if client asked for it.
        finalanswer = get_param(query, "finalanswer", False)
        finalanswerquery = get_json_param(query.jso, "input", "finalanswerquery", False)

        if tries >= max_tries and (finalanswer == False or finalanswerquery == False):
            out = "You have exceeded the answering limit or have seen the answer"
            web["tries"] = tries
            web["result"] = out
            web["error"] = out
            sresult = json.dumps(result)
            # Write results to site.
            self.wout(sresult)
            return

        if (
            finalanswer
            and finalanswerquery
            and (tries >= max_tries or max_tries == max_infinity)
        ):
            print("--final answer--")
            obj = {}
            answertable = []
            for target in targets:
                for k in target["points"].keys():
                    if target["points"][k] > 0:
                        obj["id"] = k
                        obj["position"] = [
                            target["position"][0] + target["snapOffset"][0],
                            target["position"][1] + target["snapOffset"][1],
                        ]
                        # Empty dict between loops.
                answertable.append(obj)
                obj = {}

            answer["rightanswers"] = answertable
            answer["studentanswers"] = gottenpoints
            # print(answer)
        tries = tries + 1

        # Save user input and points to markup
        save = {"userAnswer": {"drags": drags}}
        drawings = get_json_param(query.jso, "input", "drawings", None)
        if drawings:
            save["drawings"] = drawings
        result["save"] = save
        out = "saved"
        result["tim_info"] = {"points": points}

        # Send stuff over to tim.
        web["tries"] = tries
        web["result"] = out
        web["error"] = err
        web["answer"] = answer
        # print(web)
        sresult = json.dumps(result)
        # Write results to site.
        self.wout(sresult)


if __name__ == "__main__":
    start_server(ImagexServer)
