# -*- coding: utf-8 -*-
__author__ = 'vesal'
"""
Module for serving TIM example pali plugin.
Serving from local port 5000
"""

import binascii
import sys
sys.path.insert(0, '../py')


from http_params import *
import tim_server

PORT = 5000
PROGDIR = "."


class PaliServer(tim_server.TimServer):
    """
    Class for palindrome server that can handle the TIM routes
    """

    def get_html(self, query: QueryParams) -> str:
        """
        Return the html for this query. Params are dumbed as hexstring to avoid problems
        with html input and so on.
        :type query: QueryParams
        :rtype : str
        :param query: get or put params
        :return : html string for this markup
        """
        # print(query) # uncomment to see query
        user_id = query.get_param("user_id", "--")
        # do the next if Anonymoys is not allowed to use plugins
        if user_id == "Anonymous":
            # SANITOIDAAN markupista tuleva syöte
            return '<p class="pluginError">The interactive plugin works only for users who are logged in</p><pre class="csRunDiv">' \
                   + query.get_sanitized_param("initword", "") + '</pre>'

        jso = query.to_json(accept_nonhyphen)
        runner = 'pali-runner'
        js = json.dumps(jso)
        # print(js) # uncomment this to look what is in js
        hx = binascii.hexlify(js.encode("UTF8"))
        s = '<' + runner + '>xxxHEXJSONxxx' + hx.decode() + '</' + runner + '>'
        return s

    def get_reqs_result(self) -> dict:
        """
        :return: reqs result as json
        """
        return {"js": ["js/pali.js"], "angularModule": ["paliApp"],
                       "css": ["css/pali.css"], "multihtml": True}

    def do_answer(self, query: QueryParams):
        """
        Do answer route
        :type query: QueryParams
        :param query: post and get params
        :return: nothing
        """
        do_headers(self, "application/json")
        result = {}
        save = {}
        web = {}
        result["web"] = web
        err = ""

        # userinput = get_json_param(query.jso, "state", "userinput", None)
        # if userinput: query.query["userinput"] = [userinput]

        userword = query.get_json_param("input", "userword", None)
        if userword: save["userword"] = userword

        # plugin can ask not to save the word
        nosave = query.get_json_param("input", "nosave", None)
        if not nosave: result["save"] = save
        out = "saved"

        out = out[0:20000]
        web["console"] = out
        web["error"] = err

        sresult = json.dumps(result)
        self.wout(sresult)
        # print("Result ========")
        print(sresult)


if __name__ == '__main__':
    tim_server.start_server(PaliServer)