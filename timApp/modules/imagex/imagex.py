# -*- coding: utf-8 -*-
__author__ = 'vesal,iltapeur'
"""
Module for serving TIM imagex plugin.
See: https://tim.it.jyu.fi/view/tim/TIMin%20kehitys/Plugin%20development
Serving from local port 5000
"""

import binascii
import sys
sys.path.insert(0, '/py') # /py on mountattu docker kontissa /opt/tim/timApp/modules/py -hakemistoon

# yaml templates and other methods imported from here.
from http_params import *
# ImagexServer is inherited from this. Contains methods like do_GET, do_PUT, etc generic server stuff.
import tim_server
# Library for checking if a point is inside a shape.
from geometry import*
# For copying dicts.
from copy import deepcopy
# Methods for checking contents of JSON.
from fileParams3 import get_all_templates
PORT = 5000
PROGDIR = "."



def get_lazy_imagex_html(query: QueryParams) -> str:
    """
    Returns the lazy version of html before the real template is given by javascript.
    :param query: query params where lazy options can be read 
    :return: lazy version of imagex-plugins html
    """
    s = '<div class="csRunDiv no-popup-menu">'
    s += replace_template_params(query, "<h4>{{header}}</h4>","header")
    s += replace_template_params(query, '<p class="stem" >{{stem}}</p>',"stem")
    s += '</div>'
    return s              
    

class ImagexServer(tim_server.TimServer):
    """
    Class for imagex server that can handle the TIM routes
    """
    def get_html(self, query: QueryParams) -> str:
        #print("--QUERYHTML--" + str(query))
        """
        Return the html for this query. Params are dumbed as hexstring to avoid problems
        with html input and so on.
        :param query: get or put params
        :return : html string for this markup
        """
        #print("--Query--"+ query.dump() + "--Query--" ) # uncomment to see query
        #Get user id from session
        user_id = query.get_param("user_id", "--")
        preview = query.get_param("preview",False)
        targets = {"targets":query.get_param("-targets",{})}
        query2 = ""

        # Check if this is in preview. If it is set targets as visible. and query2 is used instead of query.
        if preview:
            jso2 = query.to_json(accept_nonhyphen)
            jso2['markup'].update(targets) #+= targets
            print("--..--" + str(jso2))
            jso2['markup']['preview'] = True
            if jso2['markup']['targets'] == {}:
                jso2['markup'].update({"targets": query.get_param("targets", {})})
                print("jso" + str(jso2))
            query2 = QueryParams(jso2)



        # do the next if Anonymoys is not allowed to use plugins
        if user_id == "Anonymous":
            allow_anonymous = str(query.get_param("anonymous", "false")).lower()
            # SANITOIDAAN markupista tuleva syöte
            jump = query.get_param("taskID", "")
            # print("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX jump: ", jump)
            if allow_anonymous != "true":
                return NOLAZY + '<p class="pluginError"><a href="/login?anchor=' + jump + '">Please login to interact with this component</a></p><pre class="csRunDiv">' + get_param(
                    query, "byCode", "") + '</pre>'

        # Send query 2 instead of normal query if it exists.
        if query2:
            jso = query2.to_json(accept_nonhyphen)
        else:
            jso = query.to_json(accept_nonhyphen)
        runner = 'imagex-runner'
        attrs = json.dumps(jso)

        # print(attrs) # uncomment this to look what is in outgoing js

        if query.get_param("nohex", False): # as a default do hex
            attrs = "xxxJSONxxx" + attrs
        else:  # this is on by default, but for debug can be put off to see the json better
            hx = 'xxxHEXJSONxxx'+binascii.hexlify(attrs.encode("UTF8")).decode()
            attrs = hx
        s = '<' + runner + '>' + attrs + '</' + runner + '>'

        #Put query2 into s if it exists, otherwise send query.
        if query2:
            s = make_lazy(s, query2, get_lazy_imagex_html)
        else:
            s = make_lazy(s, query, get_lazy_imagex_html)
        return s


    # Creates accurate state for answer, ie. changes the object positions.
    def create_state_imagex(self, markup, drags):
        dict = {"objects":drags}
        return dict

    #gets reqs
    def get_reqs_result(self) -> dict:
        """
        :return: reqs result as json
        """
        #Get templates for plugin
        templs = get_all_templates('templates')
        #print("--templates--" + str(templs))
        ret = {"js": ["/static/scripts/timHelper.js","/static/scripts/imagex.js","/static/scripts/bower_components/angular-bootstrap-colorpicker/js/bootstrap-colorpicker-module.min.js"], "angularModule": ["imagexApp","colorpicker.module"],#["/static/scripts/timHelper.js","js/imagex.js"], "angularModule": ["imagexApp"],
                       "css": ["static/css/imagex.css","/static/scripts/bower_components/angular-bootstrap-colorpicker/css/colorpicker.css"], "multihtml": True}
        # Add templates to reqs.
        ret.update(templs)
        return ret


    def gettargetattr(self,prevtarget,defaults,attr,default):
        """
        Get attr for target. First checked from default, then from
        the previous target. if neither is found return default.
        """
        if str(attr) in defaults:
            return defaults[str(attr)]
        if str(attr) in prevtarget:
            return prevtarget[str(attr)]
        return default


    def do_answer(self, query: QueryParams):
        """
        Do answer route.
        Check if images are dragged to correct targets. Award points on whether they are or arent.
        :param query: post and get params
        :return: nothing
        """

        #print("--QUERYANSWER--" + str(query))
        #Setup for answers
        do_headers(self, "application/json")
        result = {}
        web = {}
        result["web"] = web
        out = ""
        err = ""
        tries = 0

        try:
            #print("--state--" + str(query.get_param("state",None)))
            #Student points
            points = 0
            #get default values for targets. If values arent told for targets get them from here or from previous
            # target.
            defaults = query.get_param("defaults","")
            #If all tries have been used just return.
            max_tries = int(query.get_param("max_tries", 1000000))
            tries = int(query.get_json_param("state", "tries", 0))
            if tries >= max_tries:
                return
            finalanswergiven = query.get_json_param("state","finalanswergiven",False)
            #Targets dict.
            targets = list(query.get_param("targets",None))
            drags = query.get_json_param("input", "drags", None)
            gottenpoints = {}
            gottenpointsobj = {}
            #For tracking indexes
            i = 0
            #Uncomment to see student answers
            #print("---drags---" + str(drags))
            #No points are awarded if all tries have been given or the final answer has been given to the student.
            if tries < max_tries and finalanswergiven == False:
                for target in targets:
                    #Find object name from user input.
                    for selectkey in target['points'].keys():
                        for drag in drags:
                            if drag['id'] == selectkey:
                                #Check if needed values exist for target. If they dont, read them from defaults
                                #or first target. # TODO: NOT FROM FIRST!!!
                                if 'type' not in target:
                                    target['type'] = self.gettargetattr(targets[i], defaults, "type", "") # TODO: vieläkin väärin, pitää ottaa edellisestä ei 0:sta!
                                if 'a' not in target:
                                    target['a'] = self.gettargetattr(targets[i], defaults, "a", 0)
                                if 'size' not in target:
                                    target['size'] = self.gettargetattr(targets[i],defaults,"size",[10])
                                if 'position' not in target:
                                    target['position'] = self.gettargetattr(targets[i], defaults, "position", [0,0])
                                # Check if image is inside target, award points.
                                if isInside(target['type'],target['size'],target['a'],target['position'],drag["position"]):
                                    #print("--targetpoints--" + str(target['target']['points']))
                                    #print(target['target']['points'][selectkey])
                                    #Add points for objects being inside shape.
                                    points += (target['points'][selectkey])

                                    gottenpointsobj[selectkey] = target['points'][selectkey]
                                    gottenpoints.update(gottenpointsobj)
                                    gottenpointsobj = {}

                    i = i + 1

            if tries >= max_tries or finalanswergiven == True:
                out = out[0:20000]
                web["tries"] = tries
                web["error"] = "You have exceeded the answering limit or made a request for the correct answer"
                sresult = json.dumps(result)
                # Write results to site.
                self.wout(sresult)
                return

            tries = tries + 1

            answer = {}
            # Check if getting finalanswer from excercise is allowed and if client asked for it.
            finalanswer = query.get_param("finalanswer",False)
            finalanswerquery = query.get_json_param("input","finalanswerquery", False)

            if finalanswer == True and (tries >= max_tries or finalanswerquery == True):
                print("--final answer--")
                #Set tries to be max_tries so that this cannot be exploited.
                tries = max_tries
                obj = {}
                answertable = []
                for target in targets:
                    for key in target['points'].keys():
                        if target['points'][key] > 0:
                            obj['id'] = key
                            obj['position'] = target['position']
                            #Empty dict between loops.
                    answertable.append(obj)
                    obj = {}
                    ##

                answer['rightanswers'] = answertable
                answer['studentanswers'] = gottenpoints
                answer['targets'] = targets
                #print(answer)
                finalanswergiven = True




            markup = {}
            #Create state to be saved for this excercise.
            markup["objects"] = self.create_state_imagex(query.get_param("markup",None),drags) # TODO: stateen menee ihan liian paljon tavaraa.  Vain se käyttäjän vastaus!!!
            markup["tries"] = tries
            #Save if finalanswer was given to student.
            markup['finalanswergiven'] = finalanswergiven
            freeHandData =  query.get_json_param("input", "freeHandData", None)
            # markup["targets"] = targets
            # Return correct answer if the answer table isnt empty.
            if len(answer) != 0:
                markup['correctanswer'] = answer

            #Save user input and points to markup
            tim_info = {"points":points}
            save = {"markup": markup,"tries":tries, 'freeHandData':freeHandData} #{"drags":drags,"tries":tries}
            result["save"] = save
            result["tim_info"] = tim_info
            out = "saved"
        #Print exception and error.
        except Exception as e:
            err = str(e)
            print("---Virhe---")
            print(err)
        #Send stuff over to tim.
        out = out[0:20000]
        web["tries"] = tries
        web["result"] = out
        web["error"] = err
        sresult = json.dumps(result)
        #Write results to site.
        self.wout(sresult)

#Start plugin.
if __name__ == '__main__':
    tim_server.start_server(ImagexServer,'imagex')

