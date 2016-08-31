# -*- coding: utf-8 -*-
import binascii
import sys

sys.path.insert(0, '/py')  # /py on mountattu docker kontissa /opt/tim/timApp/modules/py -hakemistoon

from http_params import *
import tim_server

from CC.consumer_class.consumer import *

from pprint import pprint as pretty_print

__author__ = 'vesal; pealkasa; josakepp'
"""
Module (starting point) for serving TIM example pali plugin.
See: https://tim.it.jyu.fi/view/tim/TIMin%20kehitys/Plugin%20development
Serving from local port 5000
"""

PORT = 5000
PROGDIR = "."

users = {}  # Dict of all users (keys are user IDs)

# LTI param set that stays constant regardless of question, course
# (Commented out ones are variable)
fixed_lti_params = {
    'lti_message_type': 'basic-lti-launch-request',
    'lti_version': 'LTI-1p0',
    # 'tool_url': "http://timstack.it.jyu.fi:8080/moodle/local/ltiprovider/tool.php?id=8",
    'consumer_secret': '__lti_secret__',
    'consumer_key': '__consumer_key__',
    'resource_link_id': 'tim.jyu.fi',
    'lis_outcome_service_url': "http://timstack.it.jyu.fi/grades/",
    'lis_person_sourcedid': 'sis:111',
    'lis_result_sourcedid': '__sourcedid__',
    'lis_course_offering_sourcedid': 'JYU-MATA123',
    'lis_course_section_sourcedid': 'JYU-MATA123-Kappale1-Teht15',
    'roles': 'Learner',
    'ext_ims_lis_basic_outcome_url': 'http://timstack.it.jyu.fi/lti/grades/',
    'ext_ims_lis_resultvalue_sourcedids': 'decimal'
}


def get_lazy_lti_html(query: QueryParams) -> str:
    """
    Returns a lazy version of plugins html
    :param query: query params where lazy options can be read
    :return: lazy version of pali-plugins html
    """

    # print(str(query.get_param("markup", "")).encode('utf-8'))

    initword = str(query.get_param("initword", ""))
    s = '<div class="csRunDiv no-popup-menu">'
    s += replace_template_params(query, "<h4>{{header}}</h4>", "header")
    s += replace_template_params(query, '<p class="stem" >{{stem}}</p>', "stem")
    s += str(query.get_param("form", ""))
    s += str(query.get_param("iframe", ""))
    s += replace_template_params(query, '<p class="plgfooter">{{footer}}</p>', "footer")
    s += '</div>'
    return s

# Question IDs mapped to their respective URLs
qid_lookup = {
    'http://timstack.it.jyu.fi:8080/moodle/mod/quiz/view.php?id=6':
        'http://timstack.it.jyu.fi:8080/moodle/local/ltiprovider/tool.php?id=7',  # Järjestelmän käyttöharjoitteluteht.
    'http://timstack.it.jyu.fi:8080/moodle/mod/quiz/view.php?id=16':
        'http://timstack.it.jyu.fi:8080/moodle/local/ltiprovider/tool.php?id=8',  # Polynomin derivaatta
    'http://timstack.it.jyu.fi:8080/moodle/mod/quiz/view.php?id=2':
        'http://timstack.it.jyu.fi:8080/moodle/local/ltiprovider/tool.php?id=9',  # Kolme derivaattaa
    'http://timstack.it.jyu.fi:8080/moodle/mod/quiz/view.php?id=17':
        'http://timstack.it.jyu.fi:8080/moodle/local/ltiprovider/tool.php?id=11'  # Testaus 3
}


# Get question ID from URL
def getQID(url):
    for qid in qid_lookup:
        if qid_lookup[qid] == url:
            return qid
    print("getQID returned None!")
    return None


def CreateConsumer(query: QueryParams = None):

    if query:
        # key = query.get_param('consumer_key', default='Query exists but no KEY')
        # secret = query.get_param('consumer_secret', default='Query exists but no SECRET')
        # TODO: placeholders
        key = '__consumer_key__'
        secret = '__lti_secret__'
        tim_id = query.get_param('user_id', 'Query exists but no USER_ID')
        tool_url = query.get_param('tool_url', 'DEFAULT_tool_url')
        view_url = query.get_param('view_url', 'DEFAULT_view_url')
    else:
        key = '__consumer_key__'
        secret = '__lti_secret__'
        tim_id = 'Query does not exist: using default ID'
        tool_url = 'http://timstack.it.jyu.fi' # Should actually be some 404 page...
        view_url = 'http://timstack.it.jyu.fi' # Should actually be some 404 page...

    # Get question ID that matches given Moodle question URL
    # qid = getQID(question_url)
    # qid = question_url
    qid = tool_url

    LTI_params = fixed_lti_params
    LTI_params['tool_url'] = tool_url
    LTI_params['user_id'] = tim_id
    LTI_params['lis_person_contact_email_primary'] = tim_id + "@jyu.fi"
    LTI_params['lis_person_sourcedid'] = 'PERSONSOURCEDID_' + tim_id
    LTI_params['lis_person_name_given'] = 'TIMUSER_' + tim_id
    LTI_params['lis_person_name_family'] = 'Learner'
    LTI_params['lis_person_name_full'] = 'user_id: ' + tim_id
    LTI_params['lis_result_sourcedid'] = tim_id + '_' + qid

    tool_url = LTI_params['tool_url']
    LTI_credentials = {'consumer_key': key, 'consumer_secret': secret}

    NewConsumer = Consumer(credentials=LTI_credentials, lti_params=LTI_params, tp_url=tool_url)
    return NewConsumer


List_of_Consumers = {}


class LtiServer(tim_server.TimServer):
    """
    Class for LTI server that can handle the TIM routes
    """

    def do_getform(self):

        do_headers(self, 'text/html')

        GET_params = get_params(self)
        hash_in_GET = str(GET_params.get_param('hash', "DEFAULT HASH"))

        pretty_print("GET_query.get_param('hash'): " + hash_in_GET)

        try:
            Found_Consumer = List_of_Consumers[hash_in_GET]
            found_consumer_hash = str(hash(Found_Consumer))
            oauth_params = Found_Consumer.authorize()
            form_html = Found_Consumer.make_form(js=True, hidden=True, target_iframe=False)
            print("OAuth'd Consumer (hash: " + hash_in_GET + ") with the following params:")
            print(oauth_params)
            return self.wout(form_html)

        except KeyError as e:
            return self.wout("Did not find any consumer with hash_in_GET: " + hash_in_GET)

    def do_getlink(self):

        do_headers(self, 'text/html')
        link = str(get_params(self).get_param('page', "DEFAULT LINK"))
        page_html = '<script>document.location.href="' + link + '";</script>'
        return self.wout(page_html)

    def do_new_consumer(self, query: QueryParams):

        self.send_response(303)
        self.send_header('Content-type', 'text/html')
        # response.send_header('Access-Control-Allow-Origin', '*')
        # response.send_header('Access-Control-Allow-Methods', 'GET, PUT, POST, OPTIONS')
        # response.send_header("Access-Control-Allow-Headers", "version, X-Requested-With, Content-Type")
        self.end_headers()

        NewConsumer = CreateConsumer(query)
        List_of_Consumers.update({str(hash(NewConsumer)):NewConsumer})
        return "We created a new consumer : " + str(NewConsumer.lti_params)

    # This function is called whenever Angular calls GET on /lti/grades/ with a Consumer-specific hash
    # i.e. "GET /lti/grades?hash=1234567890"
    def do_get_grade(self, query: QueryParams):
        do_headers(self, 'text/html')

        userhash = query.get_param('hash', 'None')

        # Holy computational complexity, Batman!
        for user in users:
            for qid in users[user]:
                cons = users[user][qid][0]
                if cons['hash'] == userhash:
                    print("hash got!")
                    print(cons['grade'])
                    if isinstance(cons['grade'], str) or isinstance(cons['grade'], int):
                        return self.wout(users[user][qid][0]['grade'])
                    else:
                        return self.wout("ei pisteitä")
        return self.wout("0")

    def do_getconsumers(self):
        hash_for_consumer = get_params(self).get_param('hash', None)

        if hash_for_consumer:
            hash_for_consumer = str(hash_for_consumer)

        pretty_print(hash_for_consumer)

        do_headers(self, "text/html")

        s = "<!DOCTYPE html><html><head></head><body>"

        if not hash_for_consumer:

            s += "<h1>*** List of ALL Consumers with hashes (Count: " + str(len(List_of_Consumers)) + ") ***</h1><br>"
            s += "<form action=\"new_consumer\" method=\"get\">" \
                    \
                 + "user_id: <input type=\"text\" name=\"user_id\" value=\"consumeri\" size=\"50\">" \
                 + "<input type=\"submit\" value=\"New Consumer\"><br>" \
                 \
                 + "tool_url: <input type=\"text\" name=\"tool_url\" size=\"100\"" \
                 + "value=\"http://timstack.it.jyu.fi:8080/moodle/local/ltiprovider/tool.php?id=1\">" \
                 \
                 + "</form>"

            from pprint import pformat

            for consumer_tuple in List_of_Consumers.items():

                consumer_hash = str(consumer_tuple[0])
                consumer_obj  = consumer_tuple[1]

                link_to_getform = "../lti/getform?hash=" + consumer_hash
                a_href_to_getform = "<a href=\"" + link_to_getform + "\">" + consumer_hash  +"</a>"

                id_string = "  <h2>" +\
                            "Hash: " \
                            + a_href_to_getform \
                            + "</h2>"\
                            \
                            + "<h3>"\
                            + pformat(consumer_obj.credentials, indent=4, width=80) + "<br>" * 2 \
                            + pformat(consumer_obj.lti_params, indent=4, width=80)  \
                            + "</h3>"

                id_string = id_string.replace('\n', '<br>')
                # print(id_string)
                s += id_string

            s += "</body></html>\n"

            return self.wout(s)

        else:
            try:
                consumer_tuple = List_of_Consumers[hash_for_consumer]

                s = "<h2>Hash " + hash_for_consumer + "</h2><h3>" + str(
                    consumer_tuple.credentials) + '<br>' * 2 + str(
                    consumer_tuple.lti_params) + "</h3>"

                return self.wout(s)

            except KeyError as not_found:
                return self.wout("KeyError: " + str(not_found.args))

    def do_getusers(self):

        do_headers(self, "text/html")

        s = "<!DOCTYPE html><html><head></head><body>"
        s += "<h1>Users</h1>"
        s += '<div class="users">'

        for user in users:
            s += '<div class="user-info">'
            s += "<h3>" + user + "</h3>"
            s += '<ul>'
            for qid in users[user]:
                s += '<li><p>Question: ' + qid + '</p>'
                for idx, answer in enumerate(users[user][qid]):
                    s += '<ul><p><b>Answer #' + str(idx + 1) + '</b></p>'
                    # None is not sensible for HTML (default value)
                    grade = 'not submitted'
                    if answer['grade'] != None:
                        grade = answer['grade']
                    s += '<li>Grade: ' + grade + '</li>'
                    s += '<li>Hash: ' + answer['hash'] + '</li>'
                    s += '</ul>'
                s += '</li>'
            s += '</ul>'
            s += "</div>"

        s += '</div>'
        return self.wout(s)

    def do_GET(self):
        """
        Do needed things for GET request
        :return: nothing
        """
        print("do_GET ==================================================")

        if self.path.find('/getform') >= 0:
            return self.do_getform()

        if self.path.find('/getlink') >= 0:
            return self.do_getlink()

        if self.path.find('/getconsumers') >= 0:
            return self.do_getconsumers()

        if self.path.find('/getusers') >= 0:
            return self.do_getusers()

        if self.path.find('/new_consumer') >= 0:
            return self.do_new_consumer(get_params(self))

        if self.path.find('/reqs') >= 0:
            return self.do_reqs()

        if self.path.find('/grades') >= 0:
            return self.do_get_grade(get_params(self))

        if self.path.find('/favicon.ico') >= 0: return self.send_response(404)
        if self.path.find('/template') >= 0:    return self.send_text(self.do_template(get_params(self)), "text/plain")
        fname = self.path.split("?")[0]
        if fname.find('.css') >= 0:  return self.send_text_file(fname, "css", "text/css")
        if fname.find('.js') >= 0:   return self.send_text_file(fname, "js", "application/javascript")
        if fname.find('.html') >= 0: return self.send_text_file(fname, "html", "text/html")

        return self.do_all(get_params(self))

    def do_POST(self):
        """
        Do needed things for POST request
        This may be a f.ex a request single html-plugin or multiple plugins
        :return: nothing
        """

        print("do_POST =================================================")
        if self.path.find('/multihtml') < 0:
            if self.path.find('/grades') >= 0:
                bytecount = int(self.headers['Content-Length'])  # Size of request body in bytes
                req_body = self.rfile.read(bytecount).decode('UTF8')  # XML-formatted request body

                outcomes = Outcomes(xml=req_body)
                req_data = outcomes.parse_request()
                print(req_data)

                sourcedid = req_data['lis_result_sourcedid'].split('_')
                user = sourcedid[0]
                question_url = sourcedid[1]

                qid = qid_lookup[question_url]

                users[user][qid][0]['grade'] = req_data['score']
                print(users[user])

                do_headers(self, 'text/xml')
                res_body = outcomes.make_response() # Output XML response body
                return self.wout(res_body + '\n')
            else:
                return self.do_all(post_params(self))

        print("do_POST MULTIHML ==========================================")
        querys = multi_post_params(self)
        do_headers(self, "application/json")
        htmls = []
        self.user_id = querys[0].get_param("user_id", "--")
        print("UserId:", self.user_id)
        tim_server.log(self)
        # print(querys)

        for query in querys:
            # print(query.jso)
            # print(str(query))
            s = self.get_html(query)
            # print(s)
            htmls.append(s)

        # print(htmls)
        sresult = json.dumps(htmls)
        self.wout(sresult + "\n")
        tim_server.log(self)  # to measure time spent in doing all the html

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
            return NOLAZY \
                   + '<p class="pluginError">The interactive plugin works only for users who are logged in</p><pre class="csRunDiv">' \
                   + query.get_sanitized_param("initword", "") + '</pre>'

        JSO_from_YAML_without_hyphens = query.to_json(accept_nonhyphen)
        AngularJS_directive_name = 'lti-runner'  # REMEMBER: Angular.JS treats lti-runner <=> ltiRunner

        use_lti = query.get_param("use_lti", True)

        try:
            # Create our LTI Consumer (don't authorize yet!)
            New_Consumer = CreateConsumer(query)

            consumer_hash = str(hash(New_Consumer))
            consumer_hash_dict = {consumer_hash: New_Consumer}

            List_of_Consumers.update(consumer_hash_dict)

            # Store New_Consumer data into global users dict under the question id (qid)
            # qid = JSO_from_YAML_without_hyphens['markup']['tool_url']
            qid = query.get_param('tool_url', '')
            users[user_id] = users.get(user_id) or {}
            users[user_id][qid] = users[user_id].get(qid) or {}
            users[user_id][qid] = [{'hash': consumer_hash, 'consumer': New_Consumer, 'grade': None}]

            JSO_from_YAML_without_hyphens['markup']['hash'] = consumer_hash

            AngularJS_directive_attributes = json.dumps(JSO_from_YAML_without_hyphens)

            if query.get_param("nohex", False):  # as a default do hex
                AngularJS_directive_attributes = "xxxJSONxxx" + AngularJS_directive_attributes
            else:
                AngularJS_directive_attributes = 'xxxHEXJSONxxx' + binascii.hexlify(
                    AngularJS_directive_attributes.encode("UTF8")).decode()

            s = '<' + AngularJS_directive_name + '>' + AngularJS_directive_attributes + '</' + AngularJS_directive_name + '>'
            s = make_lazy(s, query, get_lazy_lti_html)

            return s

        except Exception as e:
            import traceback
            error_str = "<div><p>" + traceback.format_exc() + "</p></div>"
            return error_str

    def get_reqs_result(self) -> dict:
        """
        :return: reqs result as json
        """
        return {"js": ["/static/scripts/timHelper.js", "js/lti.js"], "angularModule": ["ltiApp"],
                "css": ["css/lti.css"], "multihtml": True}

    def do_grades(self, query: QueryParams) -> str:
        result = query.dump()
        self.wout(result)

    def do_answer(self, query: QueryParams):  # TODO: currently using only grade --> use answer!
        """
        Do answer route.
        Check if grade is different than the old word.  If it is save the
        new word if max_tires is not passed.  Count points based pali ok and length ok.
        :param query: post and get params
        :return: nothing
        """

        print("do_answer called! =================")
        print(query.dump())

        do_headers(self, "application/json")

        result = {}
        web = {}
        result["web"] = web

        out = ""
        err = ""
        tries = 0

        try:

            grade = float(query.get_json_param("input", "grade", 0))
            # oldgrade = float(query.get_json_param("state", "oldgrade", -1))
            # max_grade = float(query.get_json_param("max_grade", 1))
            max_tries = int(query.get_param("max_tries", 1000000))
            tries = int(query.get_json_param("state", "tries", 0))

            if grade is not None:

                # TODO: add max_grade as upper bound (if present)
                grade_ok = True if grade >= 0 else False

                # TODO: what is points_array and what is points?
                points_array = query.get_param("points_array", [[0, 0.25], [0.5, 1]])
                points = points_array[grade_ok]

                # plugin can ask not to save the word
                nosave = query.get_json_param("input", "nosave", None)

                # if (not nosave) and tries < max_tries and grade != oldgrade:
                if (not nosave) and tries < max_tries:
                    tries += 1
                    tim_info = {"points": points}
                    save = {"grade": grade, "tries": tries}
                    result["save"] = save
                    result["tim_info"] = tim_info
                    out = "Tallennettu"
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
    tim_server.start_server(LtiServer, 'lti')
