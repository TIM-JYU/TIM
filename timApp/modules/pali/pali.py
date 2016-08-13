# -*- coding: utf-8 -*-
__author__ = 'vesal'
"""
Module for serving TIM example pali plugin.
See: https://tim.it.jyu.fi/view/tim/TIMin%20kehitys/Plugin%20development
Serving from local port 5000
"""

import binascii
import sys
sys.path.insert(0, '/py') # /py on mountattu docker kontissa /opt/tim/timApp/modules/py -hakemistoon


from http_params import *
import tim_server

PORT = 5000
PROGDIR = "."


def check_letters(word: str, needed_len: int) -> bool:
    """
    checks if word has needed_amount of chars
    :param word: word to check
    :param needed_len: how many letters needed
    :return: true if len match
    """
    s = word.upper()
    return len(re.sub("[^[A-ZÅÄÖ]","",s)) == needed_len

    
def get_lazy_pali_html(query: QueryParams) -> str:
    """
    Returns a lazy version of plugins html
    :param query: query params where lazy options can be read 
    :return: lazy version of pali-plugins html
    """
    initword = str(query.get_param("initword",""))
    s = '<div class="csRunDiv no-popup-menu">'
    s += replace_template_params(query, "<h4>{{header}}</h4>","header")
    s += replace_template_params(query, '<p class="stem" >{{stem}}</p>',"stem")
    s += replace_template_params(query, '<div><label>{{inputstem}} <span><input type ="text" class="paliInput"  value="{{state.userword}}" size="{{cols}}"></span></label>',
         "", ["inputstem", "state.userword:"+initword, "cols"])
    s += '</div>'
    s += replace_template_params(query, '&nbsp;&nbsp;<span class="tries"> Tries: {{tries}}/{{max_tries}}</span>',"max_tries",["tries:0"])
    s += replace_template_params(query, '<p class="plgfooter">{{footer}}</p>',"footer")
    s += '</div>'
    return s              
    
    
class PaliServer(tim_server.TimServer):
    """
    Class for palindrome server that can handle the TIM routes
    """

    
    
    def get_html(self, query: QueryParams) -> str:
        """
        Return the html for this query. Params are dumbed as hexstring to avoid problems
        with html input and so on.
        :param query: get or put params
        :return : html string for this markup
        """
        # print(query.dump()) # uncomment to see query
        user_id = query.get_param("user_id", "--")

        # do the next if Anonymoys is not allowed to use plugins
        if user_id == "Anonymous":
            # SANITOIDAAN markupista tuleva syöte
            return NOLAZY + '<p class="pluginError">The interactive plugin works only for users who are logged in</p><pre class="csRunDiv">' \
                   + query.get_sanitized_param("initword", "") + '</pre>'

        # check if points array is 2x2 matrix
        points_array = query.get_param("points_array", None)
        if points_array and not check_array(points_array,2,2):
            return NOLAZY + '<p class="pluginError">points_array must be an 2x2 array, f.ex [[0, 0.1], [0.6, 1]]</p>'

        jso = query.to_json(accept_nonhyphen)
        runner = 'pali-runner'

        attrs = json.dumps(jso)
        # print(attrs) # uncomment this to look what is in outgoing js
        if query.get_param("nohex", False): # as a default do hex
            attrs = "xxxJSONxxx" + attrs
        else:  # this is on by default, but for debug can be put off to see the json better
            hx = 'xxxHEXJSONxxx'+binascii.hexlify(attrs.encode("UTF8")).decode()
            attrs = hx
        s = '<' + runner + '>' + attrs + '</' + runner + '>'
        s = make_lazy(s, query, get_lazy_pali_html)
        return s


    def get_reqs_result(self) -> dict:
        """
        :return: reqs result as json
        """
        #Get templates for plugin
        templs = {}
        # templs = get_all_templates('/pali/templates') # uncoment this to test how templates works
        ret = {"js": ["/static/scripts/timHelper.js","js/pali.js"], "angularModule": ["paliApp"],
                       "css": ["css/pali.css"], "multihtml": True}
        ret.update(templs)
        return ret


    def do_answer(self, query: QueryParams):
        """
        Do answer route.
        Check if userword is different than the old word.  If it is save the
        new word if max_tires is not passed.  Count points based pali ok and length ok.
        :param query: post and get params
        :return: nothing
        """
        do_headers(self, "application/json")
        result = {}
        web = {}
        result["web"] = web
        out = ""
        err = ""
        tries = 0
        try:
            print(query.dump()) # uncomment this to see the query

            userword = str(query.get_json_param("input", "userword", None))
            oldword = str(query.get_json_param("state", "userword", ""))
            max_tries = int(query.get_param("max_tries", 1000000))
            tries = int(query.get_json_param("state", "tries", 0))
            if userword:
                pali_ok = query.get_json_param("input", "paliOK", False)
                needed_len = int(query.get_param("needed_len", 0))
                len_ok = True
                if needed_len: len_ok = check_letters(userword, needed_len)
                if not len_ok: err = "Wrong length"
                if not needed_len and not pali_ok: len_ok = False
                points_array = query.get_param("points_array",[[0,0.25],[0.5,1]])
                points = points_array[pali_ok][len_ok]

                # plugin can ask not to save the word
                nosave = query.get_json_param("input", "nosave", None)
                if (not nosave) and tries < max_tries and  userword != oldword :
                    tries += 1
                    tim_info = {"points": points}
                    save = {"userword": userword, "tries": tries}
                    result["save"] = save
                    result["tim_info"] = tim_info
                    out = "saved"
                    # print(tries,max_tries)
        except Exception as e:
            err = str(e)

        out = out[0:20000]
        web["tries"] = tries
        web["result"] = out
        web["error"] = err

        sresult = json.dumps(result)
        self.wout(sresult)
        print(sresult)


if __name__ == '__main__':
    tim_server.start_server(PaliServer,'pali')