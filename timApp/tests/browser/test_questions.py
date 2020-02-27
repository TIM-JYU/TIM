from time import sleep
from typing import List, Tuple, Dict, Union, Optional

from selenium.common.exceptions import StaleElementReferenceException
from selenium.webdriver.remote.webelement import WebElement
from selenium.webdriver.support.select import Select

from timApp.answer.answer import Answer
from timApp.document.yamlblock import YamlBlock
from timApp.tests.browser.browsertest import BrowserTest, find_button_by_text, find_by_ngmodel, find_all_by_ngmodel, \
    find_by_ngclick

ChoiceList = List[Tuple[str, str]]
ElementList = List[WebElement]


def create_yaml(field_type: str,
                question_type: str,
                choices: ChoiceList,
                points_str: Optional[str] = None,
                headers=None,
                matrix_type=None):
    headers = headers or []
    # TODO: timeLimit missing (for lecture question)
    # TODO: matrixType useless?
    return {
        'answerFieldType': field_type,
        'expl': {str(i + 1): choices[i][1] for i in range(len(choices))},
        'headers': headers,
        **({'matrixType': matrix_type} if matrix_type else {}),
        'questionText': 'Is Moon made of cheese?',
        'questionTitle': 'Moon problem',
        'questionType': question_type,
        'answerLimit': 1,
        'rows': [c[0] for c in choices],
        **({'points': points_str} if points_str else {}),
    }


def get_matrix_fields(dialog: WebElement) -> Tuple[ElementList, ElementList, ElementList, ElementList]:
    choice_elems = find_all_by_ngmodel(dialog, 'row.text')
    reason_elems = find_all_by_ngmodel(dialog, 'row.expl')
    point_elems = find_all_by_ngmodel(dialog, 'column.points')
    header_elems = find_all_by_ngmodel(dialog, '$ctrl.qctrl.columnHeaders[$index].text')
    return choice_elems, header_elems, point_elems, reason_elems


def adjust_matrix_size(dialog: WebElement, missing_choices: int, rowcol: str):
    addbutton = find_by_ngclick(dialog, f'$ctrl.qctrl.add{rowcol}(-1)')
    if missing_choices > 0:
        for i in range(missing_choices):
            addbutton.click()
    elif missing_choices < 0:
        for i in range(-missing_choices):
            delbutton = find_by_ngclick(dialog, f'$ctrl.qctrl.del{rowcol}($index)')
            delbutton.click()


class QuestionTest(BrowserTest):
    def test_questions(self):
        """Create document questions and answer them."""
        self.login_browser_quick_test1()
        self.login_test1()

        points = ['3', '1', '', '0']
        choices = [
            ('Yes', 'reason for yes'),
            ('No', 'reason for no'),
            ('Partially', 'reason for partially'),
            ('No idea', 'reason for no idea'),
        ]
        self.do_question_test(
            answer_choices=[1],
            choices=choices,
            expected_answer='[["2"]]',
            expected_points=1,
            expected_yaml=create_yaml('radio', 'radio-vertical', choices, points_str='1:3;2:1'),
            headers=[],
            points=points,
            questiontype='radio',
            type_choice='Multiple choice (radio button)',
        )
        self.do_question_test(
            answer_choices=[0, 2, 3],
            choices=choices,
            expected_answer='[["1", "3", "4"]]',
            expected_points=3,
            expected_yaml=create_yaml('checkbox', 'checkbox-vertical', choices, points_str='1:3;2:1'),
            headers=[],
            points=points,
            questiontype='checkbox',
            type_choice='Multiple choice (checkbox)',
        )
        truefalseheaders = ['Correct', 'Wrong']
        self.do_question_test(
            answer_choices=[0, 3],
            choices=choices[0:2],
            expected_answer='[["1"], ["2"]]',
            expected_points=4,
            expected_yaml=create_yaml('radio', 'true-false', choices[0:2], headers=truefalseheaders,
                                      points_str='1:3|2:1'),
            headers=truefalseheaders,
            points=['3', '', '0', '1'],
            questiontype='true-false',
            type_choice='True/False',
            adjust_matrix=['Row'],
        )
        matrixheaders = ['h1', 'h2', 'h3', 'h4']
        choices.append(('Maybe', 'reason for maybe'), )
        self.do_question_test(
            answer_choices=[0, 1, 3, 5, 12, 17, 18],
            choices=choices,
            expected_answer='[["1", "2", "4"], ["2"], [], ["1"], ["2", "3"]]',
            expected_points=-2,
            expected_yaml=create_yaml('checkbox', 'matrix', choices, headers=matrixheaders,
                                      points_str='1:3;4:-1|2:-5|4:2|1:1|', matrix_type='checkbox'),
            headers=matrixheaders,
            points=['3', '', '0', '-1',
                    '', '-5', '', '',
                    '', '', '', '2',
                    '1', '', '', '',
                    '', '', '0', '', ],
            questiontype='matrix-checkbox',
            type_choice='Many rows and columns',
            answer_type_choice='Checkbox',
            adjust_matrix=['Row', 'Col'],
        )
        self.do_question_test(
            answer_choices=[5, 11, 18],
            choices=choices,
            expected_answer='[[], ["2"], ["4"], [], ["3"]]',
            expected_points=-3,
            expected_yaml=create_yaml('radio', 'matrix', choices, headers=matrixheaders,
                                      points_str='1:3;4:-1|2:-5|4:2|1:1|', matrix_type='radiobutton-horizontal'),
            headers=matrixheaders,
            points=['3', '', '0', '-1',
                    '', '-5', '', '',
                    '', '', '', '2',
                    '1', '', '', '',
                    '', '', '0', '', ],
            questiontype='matrix-radio',
            type_choice='Many rows and columns',
            answer_type_choice='Radio Button horizontal',
            adjust_matrix=['Row', 'Col'],
        )
        matrixheaders = ['h1', 'h2', 'h3']
        self.do_question_test(
            answer_choices=['1st', '2nd', '', '4th'],
            choices=choices,
            expected_answer='[["1st", "2nd", ""], ["4th", "", ""], ["", "", ""], ["", "", ""], ["", "", ""]]',
            expected_points=None,
            expected_yaml=create_yaml('text', 'matrix', choices, headers=matrixheaders, matrix_type='textArea'),
            headers=matrixheaders,
            points=[],
            questiontype='matrix-textarea',
            type_choice='Many rows and columns',
            answer_type_choice='Text area',
            adjust_matrix=['Row', 'Col'],
        )

    def do_question_test(self,
                         answer_choices: Union[List[int], List[str]],
                         choices: ChoiceList,
                         expected_answer: str,
                         expected_points: Optional[float],
                         expected_yaml: Dict,
                         headers: List[str],
                         points: List[str],
                         questiontype: str,
                         type_choice: str,
                         answer_type_choice=None,
                         adjust_matrix=None,
                         ):
        if adjust_matrix is None:
            adjust_matrix = []
        d = self.create_doc(initial_par='test')
        self.goto_document(d, view='lecture')
        self.find_element('.glyphicon-menu-hamburger').click()
        par = self.drv.find_elements_by_css_selector('.editline')[1]
        par.click()
        find_button_by_text(par, 'Add question above').click()
        sleep(0.5)
        dialog = self.drv.find_element_by_css_selector('tim-edit-question')
        questiontext = find_by_ngmodel(dialog, 'qctrl.question.questionText')
        questiontext.send_keys('Is Moon made of cheese?')
        questiontitle = find_by_ngmodel(dialog, 'qctrl.question.questionTitle')
        questiontitle.click()
        questiontitle.send_keys('Moon problem')
        questionselect = Select(find_by_ngmodel(dialog, 'qctrl.question.questionType'))
        questionselect.select_by_visible_text(type_choice)
        if answer_type_choice:
            answertypeselect = Select(find_by_ngmodel(dialog, 'qctrl.question.matrixType'))
            answertypeselect.select_by_visible_text(answer_type_choice)
        choice_elems, header_elems, point_elems, reason_elems = get_matrix_fields(dialog)
        diffs = {
            'Row': len(choices) - len(choice_elems),
            'Col': len(headers) - len(header_elems),
        }
        for x in adjust_matrix:
            adjust_matrix_size(dialog, diffs[x], x)
        if adjust_matrix:
            choice_elems, header_elems, point_elems, reason_elems = get_matrix_fields(dialog)

        self.assertEqual(len(reason_elems), len(choice_elems))
        self.assertEqual(len(choices), len(choice_elems))
        self.assertEqual(len(headers), len(header_elems))

        for (choice, reason), choice_elem, reason_elem in zip(choices, choice_elems, reason_elems):
            choice_elem.send_keys(choice)
            reason_elem.send_keys(reason)
        for point, point_elem in zip(points, point_elems):
            point_elem.send_keys(point)
        for header, elem in zip(headers, header_elems):
            elem.clear()
            elem.send_keys(header)
        matrix = self.drv.find_element_by_css_selector('tim-question-matrix')
        answersheet = self.drv.find_element_by_css_selector('dynamic-answer-sheet')
        questiontext.click()  # move focus out of matrix to get consistent screenshots
        self.assert_same_screenshot(matrix, f'questions/question_matrix_{questiontype}',
                                    move_to_element=True, attempts=2)
        self.assert_same_screenshot(answersheet, f'questions/answer_sheet_{questiontype}',
                                    move_to_element=True, attempts=2)
        find_button_by_text(dialog, 'Save').click()
        self.wait_until_hidden('tim-edit-question')
        qst = self.find_element_and_move_to('qst-runner')
        self.assert_same_screenshot(qst, f'questions/qst_{questiontype}', attempts=2)
        d.document.clear_mem_cache()
        qst_par = d.document.get_paragraphs()[0]
        qst_md = qst_par.get_markdown()

        self.assertEqual(expected_yaml, YamlBlock.from_markdown(qst_md))
        if answer_type_choice == 'Text area':
            textareas = qst.find_elements_by_css_selector('textarea')
            textareas[0].click()
            for answer, area in zip(answer_choices, textareas):
                area.send_keys(answer)
        else:
            labels = qst.find_elements_by_css_selector('label')
            for i in answer_choices:
                labels[i].click()
        savebtn = find_button_by_text(qst, 'Save')
        savebtn.click()
        self.wait_until_text_present('qst-runner', 'Vastattu')

        # Move mouse out of button to prevent flaky screenshot.
        header = self.drv.find_element_by_css_selector('qst-runner h5')
        header.click()

        while True:
            qst = self.drv.find_element_by_css_selector('qst-runner')
            try:
                self.assert_same_screenshot(qst, f'questions/qst_{questiontype}_answered')
                break
            except StaleElementReferenceException:
                continue

        # check answer format is correct
        a = Answer.query.filter_by(task_id=f'{d.id}.{qst_par.get_attr("taskId")}').one()
        self.assertEqual(expected_answer, a.content)
        self.assertEqual(expected_points, a.points)
