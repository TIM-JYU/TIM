from json import loads

from lxml import html

from timApp.auth.accesstype import AccessType
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.util.utils import static_tim_doc


class QuestionTest(TimRouteTest):
    def test_question_html(self):
        with self.internal_container_ctx():
            self.login_test1()
            d = self.create_doc(from_file=static_tim_doc("questions.md"))
            pars = d.document.get_paragraphs()
            data = self.get(d.url_relative, as_tree=True)
            first_id = pars[0].get_id()
            self.assert_plugin_json(
                data.cssselect(".parContent tim-qst")[0],
                self.create_plugin_json(
                    d,
                    "test1",
                    par_id=first_id,
                    toplevel={"show_result": False},
                    markup={
                        "answerFieldType": "radio",
                        "headers": [],
                        "isTask": False,
                        "questionText": "What day is it today?",
                        "questionTitle": "Today",
                        "questionType": "radio-vertical",
                        "rows": ["Monday", "Wednesday", "Friday"],
                        "timeLimit": 90,
                    },
                ),
            )

            second_id = pars[1].get_id()
            result = data.cssselect(f"#{second_id} .parContent tim-qst")
            self.assertEqual(1, len(result))

            expected_element = html.fromstring(
                f"""
                <div class="par"
                     id="{pars[2].get_id()}"
                     t="{pars[2].get_hash()}"
                     attrs="{{&#34;question&#34;: &#34;true&#34;, &#34;taskId&#34;: &#34;test3&#34;}}">
                    <span class="headerlink">
                    <a href="#{'test3'}" title="Permanent link to paragraph">
                        <span class="header-anchor">#</span>
                    </a>
                    <span title="Copy reference" class="header-name">{'test3'}</span>
                    </span>
                    <div ng-non-bindable tabindex="0" class="parContent" id="{'test3'}">
                    <pre><code>
json:
  answerFieldType: radio
  headers: []
  questionText: What day is it today?
  questionTitle: Today
  questionType: radio-vertical
  rows:
  - Monday
  - Wednesday
  - Friday
  timeLimit: 90
points: '2:1'</code></pre>
                    </div>
                    <div class="editline" tabindex="0" title="Click to edit this paragraph"></div>
                    <div class="readline"
                         title="Click to mark this paragraph as read"></div>
                </div>
                        """
            )
            self.assert_elements_equal(
                expected_element, data.cssselect("#" + pars[2].get_id())[0]
            )

            self.get(
                "/getQuestionByParId",
                query_string={"doc_id": d.id, "par_id": first_id},
                expect_content={
                    "docId": d.id,
                    "markup": {
                        "answerFieldType": "radio",
                        "defaultPoints": 0.5,
                        "headers": [],
                        "points": "2:1",
                        "questionText": "What day is it today?",
                        "questionTitle": "Today",
                        "questionType": "radio-vertical",
                        "rows": ["Monday", "Wednesday", "Friday"],
                        "timeLimit": 90,
                    },
                    "parId": first_id,
                    "qst": True,
                    "isPreamble": False,
                    "taskId": "test1",
                },
            )

            self.get(
                "/getQuestionByParId",
                query_string={"doc_id": d.id, "par_id": second_id},
                expect_content={
                    "docId": d.id,
                    "markup": {
                        "answerFieldType": "radio",
                        "headers": [],
                        "points": "2:1",
                        "questionText": "What day is it today?",
                        "questionTitle": "Today",
                        "questionType": "radio-vertical",
                        "rows": ["Monday", "Wednesday", "Friday"],
                        "timeLimit": 1,
                    },
                    "parId": second_id,
                    "qst": False,
                    "isPreamble": False,
                    "taskId": "test2",
                },
            )

            self.get(
                "/getQuestionByParId",
                query_string={"doc_id": d.id, "par_id": pars[2].get_id()},
                expect_status=400,
                expect_content={
                    "error": f"Paragraph is not a plugin: {pars[2].get_id()}"
                },
            )

            normal_par_id = pars[3].get_id()
            self.get(
                "/getQuestionByParId",
                query_string={"doc_id": d.id, "par_id": normal_par_id},
                expect_status=400,
                expect_content={"error": f"Paragraph is not a plugin: {normal_par_id}"},
            )

    def test_question_invalid_numeric_keys(self):
        with self.internal_container_ctx():
            self.login_test1()
            d = self.create_doc(
                initial_par="""
#- {plugin=qst}
1:1|
#- {plugin=qst}
1:[]
"""
            )
            self.get(d.url)

    def test_hidden_points(self):
        with self.internal_container_ctx():
            self.login_test1()
            d = self.create_doc(
                initial_par="""
#- {#t plugin=qst question="false"}
answerFieldType: radio
answerLimit: 1
defaultPoints: -0.5
expl:
  '1': def
  '2': def
headers:
- a
- b
matrixType: radiobutton-horizontal
points: '2:1|2:1'
questionText: test
questionTitle: test
questionType: matrix
rows:
- x
- def
"""
            )
            d.document.set_settings(
                {"global_plugin_attrs": {"qst": {"showPoints": False}}}
            )
            self.test_user_2.grant_access(d, AccessType.view)
            db.session.commit()
            db.session.refresh(d)
            r = self.post_answer(
                "qst", f"{d.id}.t", user_input={"answers": [["2"], ["1"]]}
            )
            self.assertEqual(
                {
                    "markup": {
                        "answerFieldType": "radio",
                        "answerLimit": 1,
                        "headers": ["a", "b"],
                        "matrixType": "radiobutton-horizontal",
                        "questionText": "test",
                        "questionTitle": "test",
                        "questionType": "matrix",
                        "rows": ["x", "def"],
                        "showPoints": False,
                    },
                    "result": "Saved",
                    "show_result": True,
                    "state": [["2"], ["1"]],
                },
                r["web"],
            )
            self.assertTrue("error" not in r)
            answers = self.get_task_answers(f"{d.id}.t", self.current_user)
            self.assertEqual(0.5, answers[0]["points"])
            r = self.get(d.url, as_tree=True)
            # TODO: showPoints: false hides explanations and points from teachers too
            plugjson = self.get_plugin_json(r.cssselect(".parContent tim-qst")[0])
            self.assertIsNone(plugjson["markup"].get("points"))
            self.assertIsNone(plugjson["markup"].get("expl"))
            self.login_test2()
            r = self.post_answer(
                "qst", f"{d.id}.t", user_input={"answers": [["2"], ["1"]]}
            )
            self.assertEqual(
                {
                    "markup": {
                        "answerFieldType": "radio",
                        "answerLimit": 1,
                        "headers": ["a", "b"],
                        "matrixType": "radiobutton-horizontal",
                        "questionText": "test",
                        "questionTitle": "test",
                        "questionType": "matrix",
                        "rows": ["x", "def"],
                        "showPoints": False,
                    },
                    "result": "Saved",
                    "show_result": True,
                    "state": [["2"], ["1"]],
                },
                r["web"],
            )
            self.assertTrue("error" not in r)
            answers = self.get_task_answers(f"{d.id}.t", self.current_user)
            self.assertEqual(None, answers[0]["points"])  # Should be hidden.
            r = self.get(d.url, as_tree=True)
            plugjson = self.get_plugin_json(r.cssselect(".parContent tim-qst")[0])
            self.assertIsNone(plugjson["markup"].get("points"))
            self.assertIsNone(plugjson["markup"].get("expl"))

    def test_question_shuffle(self):
        """
        make shuffled qst, answer with user2, remove shuffle, answer with user3, answer again with user2
        user2 points should be based on original shuffle even after shuffle removal
        user3 points should be based on non-shuffled rows
        """
        self.login_test1()
        d = self.create_doc()
        pars = d.document.add_text(
            """
#- {#t plugin="qst" question="false"}
answerFieldType: checkbox
expl: {}
headers:
- '1'
- '2'
- '3'
matrixType: checkbox
points: 1:1;2:-0.2;3:-0.2|1:-0.2;2:1;3:-0.2|1:-0.2;2:-0.2;3:1
default_points: -0.4
questionText: Testi
questionTitle: Testi3
questionType: matrix
rows:
- First
- Second
- Third
randomizedRows: 2
"""
        )
        self.test_user_2.grant_access(d, AccessType.view)
        self.test_user_3.grant_access(d, AccessType.view)
        db.session.commit()
        db.session.refresh(d)
        self.login_test2()
        self.post_answer(
            "qst", f"{d.id}.t", user_input={"answers": [["2"], ["1", "2"]]}
        )
        answers = self.get_task_answers(f"{d.id}.t", self.current_user)
        order_with_shuffle = loads(answers[0].get("content", {})).get("order")
        self.login_test1()
        d.document.delete_paragraph(pars[0].get_id())
        d.document.add_text(
            """
#- {#t plugin="qst" question="false"}
answerFieldType: checkbox
expl: {}
headers:
- '1'
- '2'
- '3'
matrixType: checkbox
points: 1:1;2:-0.2;3:-0.2|1:-0.2;2:1;3:-0.2|1:-0.2;2:-0.2;3:1
questionText: Testi
questionTitle: Testi3
questionType: matrix
rows:
- First
- Second
- Third
        """
        )
        db.session.commit()
        db.session.add(d)
        db.session.refresh(d)
        self.login_test3()
        self.post_answer(
            "qst", f"{d.id}.t", user_input={"answers": [["1"], ["1", "2"], ["2", "3"]]}
        )
        answers = self.get_task_answers(f"{d.id}.t", self.current_user)
        self.assertEqual(2.6, answers[0]["points"])
        self.login_test2()
        self.post_answer(
            "qst",
            f"{d.id}.t",
            user_input={
                "answers": [[str(order_with_shuffle[0])], [str(order_with_shuffle[1])]]
            },
        )
        answers = self.get_task_answers(f"{d.id}.t", self.current_user)
        order_without_shuffle = loads(answers[0].get("content", {})).get("order")
        self.assertEqual(order_with_shuffle, order_without_shuffle)
        self.assertEqual(2, answers[0]["points"])

    def test_question_default_points(self):
        self.login_test1()
        d = self.create_doc()
        d.document.add_text(
            """
#- {#checkbox plugin="qst" question="false"}
answerFieldType: checkbox
expl: {}
headers:
- '1'
- '2'
- '3'
matrixType: checkbox
points: 1:1;2:0|2:1|3:1;2:0
defaultPoints: -2
questionText: Testi
questionTitle: Testi3
questionType: matrix
rows:
- First
- Second
- Third

#- {#radio question="false" plugin="qst"}
answerFieldType: radio
expl: {}
headers: []
points: 1:1;3:0
questionText: test
questionTitle: test
questionType: radio-vertical
defaultPoints: -2
rows:
- Right
- Wrong
- No answer

#- {#onlydefault question="false" plugin="qst"}
answerFieldType: radio
expl: {}
headers: []
questionText: Answer the survey
questionTitle: Earn free points
questionType: radio-vertical
defaultPoints: 3
rows:
- Good
- Neutral
- Negative

        """
        )
        # defaultPoints vs checkbox point format, also explicit 0 overrides defaultpoints
        self.post_answer(
            "qst",
            f"{d.id}.checkbox",
            user_input={"answers": [["1", "2"], ["1", "2"], ["2", "3"]]},
        )
        answers = self.get_task_answers(f"{d.id}.checkbox", self.current_user)
        self.assertEqual(1, answers[0]["points"])
        # defaultPoints vs radio point format
        self.post_answer("qst", f"{d.id}.radio", user_input={"answers": [["3"]]})
        answers = self.get_task_answers(f"{d.id}.radio", self.current_user)
        self.assertEqual(0, answers[0]["points"])
        self.post_answer("qst", f"{d.id}.radio", user_input={"answers": [["2"]]})
        answers = self.get_task_answers(f"{d.id}.radio", self.current_user)
        self.assertEqual(-2, answers[0]["points"])
        # default points work even without regular points
        self.post_answer("qst", f"{d.id}.onlydefault", user_input={"answers": [["3"]]})
        answers = self.get_task_answers(f"{d.id}.onlydefault", self.current_user)
        self.assertEqual(3, answers[0]["points"])

    def test_question_answer_markdown_html(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
``` {#t plugin="qst"}
answerFieldType: radio
answerLimit: 1
expl:
 1: '**wrong**'
headers:
 - '*h1*'
 - '*h2*'
points: '2:1'
questionText: '*test*'
questionTitle: test
stem: '*hello*'
questionType: matrix
matrixType: radiobutton-horizontal
rows:
- '*a*'
- '*b*'
```"""
        )
        r = self.post_answer("qst", f"{d.id}.t", user_input={"answers": [["2"], []]})
        ans_id = r["savedNew"]
        self.assertEqual(
            {
                "savedNew": ans_id,
                "valid": True,
                "web": {
                    "markup": {
                        "answerFieldType": "radio",
                        "answerLimit": 1,
                        "expl": {"1": "<strong>wrong</strong>"},
                        "headers": ["<em>h1</em>", "<em>h2</em>"],
                        "matrixType": "radiobutton-horizontal",
                        "points": "2:1",
                        "questionText": "<em>test</em>",
                        "questionTitle": "test",
                        "questionType": "matrix",
                        "rows": ["<em>a</em>", "<em>b</em>"],
                        "stem": "<em>hello</em>",
                    },
                    "result": "Saved",
                    "show_result": True,
                    "state": [["2"], []],
                },
            },
            r,
        )
