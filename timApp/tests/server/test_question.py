from lxml import html

from timApp.tests.server.timroutetest import TimRouteTest
from timApp.tests.timliveserver import TimLiveServer
from timApp.util.utils import EXAMPLE_DOCS_PATH


class QuestionTest(TimLiveServer, TimRouteTest):
    def test_question_html(self):
        self.login_test1()
        d = self.create_doc(from_file=f'{EXAMPLE_DOCS_PATH}/questions.md')
        pars = d.document.get_paragraphs()
        data = self.get(d.url_relative, as_tree=True)
        first_id = pars[0].get_id()
        expected_element = html.fromstring(
            f"""
    <div class="par questionPar"
         id="{first_id}"
         t="{pars[0].get_hash()}"
         attrs="{{&#34;plugin&#34;: &#34;qst&#34;, &#34;question&#34;: &#34;true&#34;, &#34;taskId&#34;: &#34;test1&#34;}}">
        <a href="#test1" title="Permlink" class="headerlink">#</a>
        <div tabindex="0" class="parContent" id="test1">
            <tim-plugin-loader type="full" task-id="{d.id}.test1" class="pluginqst" answer-id="">
            <div id="{d.id}.test1.{first_id}" data-plugin="/qst">
                <qst-runner json='{{"anonymous": true, "current_user_id": "testuser1", "doLazy": false, "info": null, "markup": {{"answerFieldType": "radio", "headers": [], "isTask": false, "questionText": "What day is it today?", "questionTitle": "Today", "questionType": "radio-vertical", "rows": ["Monday", "Wednesday", "Friday"], "timeLimit": 90}}, "preview": false, "review": false, "show_result": false, "state": null, "targetFormat": "latex", "taskID": "{d.id}.test1", "taskIDExt": "{d.id}.test1.{first_id}", "userPrint": false, "user_id": "testuser1", "viewmode": true}}'></qst-runner>
            </div>
            </tim-plugin-loader>
        </div>
        <div class="editline" tabindex="0" title="Click to edit this paragraph"></div>
        <div class="readline"
             title="Click to mark this paragraph as read"></div>
    </div>
            """)
        par = data.cssselect('#' + first_id)[0]
        self.assert_elements_equal(expected_element, par)

        second_id = pars[1].get_id()
        result = data.cssselect(f'#{second_id} .parContent qst-runner')
        self.assertEqual(1, len(result))

        expected_element = html.fromstring(
            f"""
            <div class="par"
                 id="{pars[2].get_id()}"
                 t="{pars[2].get_hash()}"
                 attrs="{{&#34;question&#34;: &#34;true&#34;, &#34;taskId&#34;: &#34;test3&#34;}}" ng-non-bindable>
                <a href="#{'test3'}" title="Permlink" class="headerlink">#</a>
                <div tabindex="0" class="parContent" id="{'test3'}">
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
                    """)
        self.assert_elements_equal(expected_element, data.cssselect('#' + pars[2].get_id())[0])

        self.get('/getQuestionByParId',
                 query_string={'doc_id': d.id, 'par_id': first_id},
                 expect_content={
                     'docId': d.id,
                     'markup': {
                         'answerFieldType': 'radio',
                         'headers': [],
                         'points': '2:1',
                         'questionText': 'What day is it today?',
                         'questionTitle': 'Today',
                         'questionType': 'radio-vertical',
                         'rows': ['Monday', 'Wednesday', 'Friday'],
                         'timeLimit': 90},
                     'parId': first_id,
                     'qst': False,
                     'taskId': 'test1'
                 })

        self.get('/getQuestionByParId',
                 query_string={'doc_id': d.id, 'par_id': second_id},
                 expect_content={
                     'docId': d.id,
                     'markup': {
                         'answerFieldType': 'radio',
                         'headers': [],
                         'points': '2:1',
                         'questionText': 'What day is it today?',
                         'questionTitle': 'Today',
                         'questionType': 'radio-vertical',
                         'rows': ['Monday', 'Wednesday', 'Friday'],
                         'timeLimit': 1},
                     'parId': second_id,
                     'qst': True,
                     'taskId': 'test2'})

        self.get('/getQuestionByParId',
                 query_string={'doc_id': d.id, 'par_id': pars[2].get_id()},
                 expect_status=400,
                 expect_content={'error': f'Paragraph is not a plugin: {pars[2].get_id()}'})

        normal_par_id = pars[3].get_id()
        self.get('/getQuestionByParId',
                 query_string={'doc_id': d.id, 'par_id': normal_par_id},
                 expect_status=400,
                 expect_content={'error': f'Paragraph is not a plugin: {normal_par_id}'})
