# -*- coding: utf-8 -*-
__author__ = 'vesal,iltapeur,mikkalle'
"""
Module for serving TIM imagex plugin.
See: https://tim.it.jyu.fi/view/tim/TIMin%20kehitys/Plugin%20development
Serving from local port 5000
"""

import sys
sys.path.insert(0, '/py')  # /py on mountattu docker kontissa /opt/tim/timApp/modules/py -hakemistoon

# yaml templates and other methods imported from here.
from http_params import *
# ImagexServer is inherited from this. Contains methods like do_GET, do_PUT, etc generic server stuff.
import tim_server
# Library for checking if a point is inside a shape.
from geometry import*
from xml.sax.saxutils import quoteattr

PORT = 5000
PROGDIR = "."


def get_lazy_imagex_html(query: QueryParams) -> str:
    """Returns the lazy version of html before the real template is given by javascript.

    :param query: query params where lazy options can be read
    :return: lazy version of imagex-plugins html

    """
    s = '<div class="csRunDiv no-popup-menu">'
    s += replace_template_params(query, "<h4>{{header}}</h4>", "header")
    s += replace_template_params(query, '<p class="stem" >{{stem}}</p>', "stem")
    s += '</div>'
    return s


class ImagexServer(tim_server.TimServer):
    """Class for imagex server that can handle the TIM routes."""

    def get_html(self, query: QueryParams) -> str:
        print("--QUERYHTML--" + str(query))
        """Return the html for this query. Params are dumbed as hexstring to avoid problems with html input and so on.

        :param query: get or put params
        :return : html string for this markup

        """
        # print("--Query--"+ query.dump() + "--Query--" ) # uncomment to see query
        # Get user id from session
        user_id = query.get_param("user_id", "--")
        preview = query.get_param("preview", False)
        hiddentargets = query.get_param("-targets", None)

        if is_review(query):
            usercode = "image"
            s = ""
            result = NOLAZY + '<div class="review" ng-non-bindable><pre>' + usercode + '</pre>' + s + '</div>'
            print("REVIEW: ", result)
            return result

        # Check if this is in preview. If it is, set targets as visible.
        if preview and hiddentargets:
            jso2 = query.to_json(accept_nonhyphen)
            jso2['markup']['targets'] = hiddentargets
            query = QueryParams(jso2)

        # do the next if Anonymoys is not allowed to use plugins
        if user_id == "Anonymous":
            allow_anonymous = str(query.get_param("anonymous", "false")).lower()
            # SANITOIDAAN markupista tuleva syöte
            jump = query.get_param("taskID", "")
            # print("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX jump: ", jump)
            if allow_anonymous != "true":
                return NOLAZY + '<p class="pluginError"><a href="/login?anchor=' + jump +\
                       '">Please login to interact with this component</a></p><pre class="csRunDiv">' + \
                       query.get_param("byCode", "") + '</pre>' # imageX does not have byCode???

        jso = query.to_json(accept_nonhyphen)
        runner = 'imagex-runner'
        attrs = json.dumps(jso)
        s = f'<{runner} json={quoteattr(attrs)}></{runner}>'
        s = make_lazy(s, query, get_lazy_imagex_html)
        return s

    # gets reqs
    def get_reqs_result(self) -> dict:
        """
        :return: reqs result as json
        """
        # Get templates for plugin
        templs = get_all_templates('templates')
        # print("--templates--" + str(templs))
        ret = {
            "js": ["tim/plugin/imagex", "angular-bootstrap-colorpicker"],
            "angularModule": ["imagexApp", "colorpicker.module"],
            "css": [
                "/static/css/imagex.css",
                "/static/scripts/jspm_packages/npm/angular-bootstrap-colorpicker@3.0.26/css/colorpicker.min.css",
            ],
            "multihtml": True}
        # Add templates to reqs.
        ret.update(templs)
        return ret

    def gettargetattr(self, prevtarget, defaults, attr, default):
        """Get attr for target.

        First checked from default, then from the previous target. if neither is found return default.

        """
        if str(attr) in defaults:
            return defaults[str(attr)]
        if str(attr) in prevtarget:
            return prevtarget[str(attr)]
        return default

    def do_answer(self, query: QueryParams):
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
        defaults = query.get_param("defaults", {})

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

        # print("--state--" + str(query.get_param("state",None)))
        # Student points
        points = 0
        # get default values for targets. If values arent told for targets get them from here or from previous
        # target.
        # If all tries have been used just return.
        max_tries = int(query.get_param("answerLimit", 1000000))
        tries = int(query.get_json_param("info", "earlier_answers", 0))

        # Targets dict
        try:
            targets = list(query.get_param("targets", None))
        except:
            targets = []
        drags = query.get_json_param("input", "drags", None)
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
                target['points'] = get_value_def(target, "points", {})
                target['type'] = get_value_def(target, "type", "rectangle")
                target['a'] = get_value_def(target, "a", 0)
                target['size'] = get_value_def(target, "size", [10])
                target['position'] = get_value_def(target, "position", [0, 0])
                target['max'] = get_value_def(target, "max", 100000)
                target['snapOffset'] = get_value_def(target, "snapOffset", [0, 0])
                target['n'] = 0
                tnr += 1
                print(target)
                if tries < max_tries:
                    for selectkey in target['points'].keys():
                        for drag in drags:
                            if drag['id'] == selectkey:
                                # Check if needed values exist for target. If they dont, read them from defaults
                                # or first target. # TODO: NOT FROM FIRST!!!
                                # Check if image is inside target, award points.
                                if target["n"] < target["max"] and \
                                        is_inside(target['type'], target['size'], -target['a'], target['position'], drag["position"]):
                                    target["n"] += 1
                                    drag["td"] = "trg" + str(tnr)
                                    if "id" in target:
                                        drag["tid"] = target["id"]
                                    pts = (target['points'][selectkey])
                                    drag["points"] = pts
                                    points += pts
                                    gottenpointsobj[selectkey] = target['points'][selectkey]
                                    gottenpoints.update(gottenpointsobj)
                                    gottenpointsobj = {}

        answer = {}
        # Check if getting finalanswer from excercise is allowed and if client asked for it.
        finalanswer = query.get_param("finalanswer", False)
        finalanswerquery = query.get_json_param("input", "finalanswerquery", False)

        if tries >= max_tries and (finalanswer == False or finalanswerquery == False):
            out = "You have exceeded the answering limit or have seen the answer"
            web["tries"] = tries
            web["result"] = out
            web["error"] = out
            sresult = json.dumps(result)
            # Write results to site.
            self.wout(sresult)
            return

        if finalanswer and finalanswerquery and tries >= max_tries:
            print("--final answer--")
            obj = {}
            answertable = []
            for target in targets:
                for k in target['points'].keys():
                    if target['points'][k] > 0:
                        obj['id'] = k
                        obj['position'] = [target['position'][0] + target['snapOffset']
                                           [0], target['position'][1] + target['snapOffset'][1]]
                        # Empty dict between loops.
                answertable.append(obj)
                obj = {}

            answer['rightanswers'] = answertable
            answer['studentanswers'] = gottenpoints
            print(answer)
        tries = tries + 1
        free_hand_data = query.get_json_param("input", "freeHandData", None)

        # Save user input and points to markup
        save = {"userAnswer": {"drags": drags}}
        if free_hand_data:
            save['freeHandData'] = free_hand_data
        result["save"] = save
        out = "saved"
        result["tim_info"] = {"points": points}

        # Send stuff over to tim.
        web["tries"] = tries
        web["result"] = out
        web["error"] = err
        web["answer"] = answer
        print(web)
        sresult = json.dumps(result)
        # Write results to site.
        self.wout(sresult)


# Start plugin.
if __name__ == '__main__':
    tim_server.start_server(ImagexServer, 'imagex')
