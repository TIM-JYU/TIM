# -*- coding: utf-8 -*-
__author__ = 'vesal'
"""
Module for serving TIM example pali plugin.
See: https://tim.it.jyu.fi/view/tim/TIMin%20kehitys/Plugin%20development
Serving from local port 5000
"""

import binascii
import sys
import re
sys.path.insert(0, '/py')


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
            return '<p class="pluginError">The interactive plugin works only for users who are logged in</p><pre class="csRunDiv">' \
                   + query.get_sanitized_param("initword", "") + '</pre>'

        jso = query.to_json(accept_nonhyphen)
        runner = 'pali-runner'
        js = json.dumps(jso)
        print(js) # uncomment this to look what is in outgoing js
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
        out = ""
        err = ""
        try:
            # print(query.dump()) # uncomment this to see the query

            userword = query.get_json_param("input", "userword", None)
            max_tries = query.get_param("max_tries", 1000000)
            oldword = query.get_json_param("state", "userword", "")
            tries = query.get_json_param("state", "tries", 0)
            if userword:
                save["userword"] = userword
                pali_ok = query.get_json_param("input", "paliOK", False)
                needed_len = query.get_param("needed_len", None)
                len_ok = True
                if needed_len: len_ok = check_letters(userword,needed_len)
                if not len_ok: err = "Wrong length"
                if not needed_len and not pali_ok: len_ok = False
                tim_info = {}
                points_array = query.get_param("points_array",[[0,0.25],[0.5,1]])
                points = points_array[pali_ok][len_ok]
                tim_info["points"] = points

                # plugin can ask not to save the word
                nosave = query.get_json_param("input", "nosave", None)
                if (not nosave) and tries < max_tries and  userword != oldword :
                    tries += 1
                    save["tries"] = tries
                    result["save"] = save
                    result["tim_info"] = tim_info
                    out = "saved"
                    print(tries,max_tries)
        except Exception as e:
            err = str(e)

        out = out[0:20000]
        web["tries"] = tries
        web["result"] = out
        web["error"] = err

        sresult = json.dumps(result)
        self.wout(sresult)
        # print("Result ========")
        print(sresult)


if __name__ == '__main__':
    tim_server.start_server(PaliServer,'pali')