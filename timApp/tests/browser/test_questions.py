from selenium.webdriver.remote.webelement import WebElement
from selenium.webdriver.support.select import Select

from timApp.documentmodel.yamlblock import YamlBlock
from timApp.tests.browser.browsertest import BrowserTest, find_button_by_text, find_by_ngmodel, find_all_by_ngmodel
from timApp.timdb.tim_models import Answer


class QuestionTest(BrowserTest):
    def test_questions(self):
        """Create a document question and answer it."""

        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par='test')
        self.goto_document(d, view='lecture')
        par: WebElement = self.drv.find_element_by_css_selector('.par')
        par.click()
        par.click()
        find_button_by_text(par, 'Add question above').click()
        dialog = self.drv.find_element_by_css_selector('tim-edit-question')
        questiontext = find_by_ngmodel(dialog, 'qctrl.question.questionText')
        questiontext.send_keys('Is Moon made of cheese?')
        questiontitle = find_by_ngmodel(dialog, 'qctrl.question.questionTitle')
        questiontitle.click()
        questiontitle.send_keys('Moon problem')
        questiontype = Select(find_by_ngmodel(dialog, 'qctrl.question.questionType'))
        questiontype.select_by_visible_text('Multiple choice (radio button)')
        choice_elems = find_all_by_ngmodel(dialog, 'row.text')
        reason_elems = find_all_by_ngmodel(dialog, 'row.expl')
        point_elems = find_all_by_ngmodel(dialog, 'column.points')
        self.assertEqual(4, len(choice_elems))
        texts = [
            ('Yes', 'reason for yes', '3'),
            ('No', 'reason for no', '1'),
            ('Partially', 'reason for partially', ''),
            ('No idea', 'reason for no idea', '0'),
        ]
        for (choice, reason, point), choice_elem, reason_elem, point_elem in zip(texts, choice_elems, reason_elems,
                                                                                 point_elems):
            choice_elem.send_keys(choice)
            reason_elem.send_keys(reason)
            point_elem.send_keys(point)
        matrix = self.drv.find_element_by_css_selector('tim-question-matrix')
        answersheet = self.drv.find_element_by_css_selector('dynamic-answer-sheet')
        self.assert_same_screenshot(matrix, 'questions/question_matrix_radio')
        self.assert_same_screenshot(answersheet, 'questions/answer_sheet_radio', move_to_element=True)
        find_button_by_text(dialog, 'Save').click()
        self.wait_until_hidden('tim-edit-question')
        qst = self.drv.find_element_by_css_selector('qst-runner')
        self.assert_same_screenshot(qst, 'questions/qst_radio')
        d.document.clear_mem_cache()
        qst_par = d.document.get_paragraphs()[0]
        qst_md = qst_par.get_markdown()

        # TODO: timeLimitFields is not used for qst
        # TODO: matrixType useless?
        expected = YamlBlock.from_markdown("""
json:
  answerFieldType: radio
  headers: []
  matrixType: ''
  questionText: Is Moon made of cheese?
  questionTitle: Moon problem
  questionType: radio-vertical
  rows:
  - 'Yes'
  - 'No'
  - Partially
  - No idea
  timeLimitFields:
    hours: 0
    minutes: 0
    seconds: 30
points: 1:3;2:1
xpl:
  '1': reason for yes
  '2': reason for no
  '3': reason for partially
  '4': reason for no idea
        """)
        self.assertEqual(expected, YamlBlock.from_markdown(qst_md))
        labels = qst.find_elements_by_css_selector('label')
        labels[1].click()
        savebtn = find_button_by_text(qst, 'Save')
        savebtn.click()
        self.wait_until_present('answerbrowser')
        self.assert_same_screenshot(qst, 'questions/qst_radio_answered')

        # check answer format is correct
        a = Answer.query.filter_by(task_id=f'{d.id}.{qst_par.get_attr("taskId")}').one()
        self.assertEqual('[["2"]]', a.content)
        self.assertEqual(1, a.points)
