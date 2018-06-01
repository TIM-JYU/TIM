from lxml import html

from timApp.tests.server.timroutetest import TimRouteTest
from timApp.tests.timliveserver import TimLiveServer


class QuestionTest(TimLiveServer, TimRouteTest):
    def test_question_html(self):
        self.login_test1()
        d = self.create_doc(from_file='example_docs/questions.md')
        pars = d.document.get_paragraphs()
        data = self.get(d.url_relative, as_tree=True)
        first_id = pars[0].get_id()
        expected_element = html.fromstring(
            f"""
    <div class="par questionPar"
         id="{first_id}"
         t="{pars[0].get_hash()}"
         attrs="{{&#34;plugin&#34;: &#34;qst&#34;, &#34;question&#34;: &#34;true&#34;, &#34;taskId&#34;: &#34;test1&#34;}}">
        <answerbrowserlazy task-id="{d.id}.test1"/>
        <a href="#test1" title="Permlink" class="headerlink">#</a>
        <div class="parContent" id="test1">
            <div id="{d.id}.test1.{first_id}" data-plugin="/qst">
                <qst-runner
                        json="{{&quot;markup&quot;: {{&quot;timeLimit&quot;: 90, &quot;questionText&quot;: &quot;What day is it today?&quot;, &quot;questionType&quot;: &quot;radio-vertical&quot;, &quot;questionTitle&quot;: &quot;Today&quot;, &quot;rows&quot;: [&quot;Monday&quot;, &quot;Wednesday&quot;, &quot;Friday&quot;], &quot;headers&quot;: [], &quot;matrixType&quot;: &quot;&quot;, &quot;answerFieldType&quot;: &quot;radio&quot;, &quot;isTask&quot;: false}}, &quot;state&quot;: null, &quot;taskID&quot;: &quot;3.test1&quot;, &quot;taskIDExt&quot;: &quot;{d.id}.test1.{first_id}&quot;, &quot;doLazy&quot;: false, &quot;userPrint&quot;: false, &quot;preview&quot;: false, &quot;anonymous&quot;: true, &quot;info&quot;: null, &quot;user_id&quot;: &quot;testuser1&quot;, &quot;targetFormat&quot;: &quot;latex&quot;, &quot;review&quot;: false, &quot;show_result&quot;: false}}"/>
            </div>
        </div>
        <div class="editline" title="Click to edit this paragraph"></div>
        <div class="readline"
             title="Click to mark this paragraph as read"></div>
    </div>
            """)
        self.assert_elements_equal(expected_element, data.cssselect('#' + first_id)[0])

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
                <div class="parContent" id="{'test3'}">
                    <pre><code>
json:
  answerFieldType: radio
  headers: []
  matrixType: ''
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
                <div class="editline" title="Click to edit this paragraph"></div>
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
                         'matrixType': '',
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
                         'matrixType': '',
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
