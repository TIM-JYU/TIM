from timApp.auth.accesstype import AccessType
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.user.userutils import grant_access


class FeedbackReportTest(TimRouteTest):

    def test_empty_report(self):
        self.login_test1()
        d = self.create_doc()
        self.get(f'/feedback/report/{d.path}', expect_content="Full Name,"
                                                              "Username,"
                                                              "Result,"
                                                              "Item,"
                                                              "Selected option,"
                                                              "Feedback,"
                                                              "Time spent on item(sec),"
                                                              "Time spent on feedback(sec)\r\n")

    def test_data_report(self):
        self.login_test1()
        d = self.create_doc(initial_par="""
``` {#fb1 plugin="feedback"}
nextTask: ""
questionItems:
- pluginNames: [dropdown1]
  words: []
  choices:
    - match: ["is cooking"]
      correct: true
      levels: &rightmatch
        - "**Correct!** You answered: *|answer|*"
    - match: []  # Empty brackets for default feedback.
      levels: &defaultmatch
        - "*Level 1 default feedback* in italics with *"
```              
        """)
        e = self.get(d.url, as_tree=True).cssselect('feedback-runner')
        self.assertTrue(e)
        empty = self.post_answer(plugin_type='feedback', task_id=f'{d.id}.fb1',
                                 user_input={'correct': False,
                                             'user_answer': '',
                                             'correct_answer': '',
                                             'feedback': ''})
        self.assertEqual({'result': 'saved'}, empty['web'])
        answer = self.post_answer(plugin_type='feedback', task_id=f'{d.id}.fb1',
                                  user_input={'correct': True,
                                              'user_answer': 'aaaaaa',
                                              'correct_answer': 'aaaaaa',
                                              'feedback': 'correct!'})
        self.assertEqual({'result': 'saved'}, answer['web'])

        exp_results = [
            f"""Full Name,Username,Result,Item,Selected option,Feedback,Time spent on item(sec),Time spent on feedback(sec)

Test user 1,testuser1,right,aaaaaa,aaaaaa,correct!,0.0,0.{d}
""".replace('\n', '\r\n') for d in (0, 1, 2)]

        r = self.get(f'/feedback/report/{d.path}')
        self.assertIn(r, exp_results)

    def test_no_permissions(self):
        self.login_test3()
        d = self.create_doc()
        self.login_test1()
        self.get(f'/feedback/report/{d.path}', expect_status=403)

    def test_grant_permission(self):
        self.login_test3()
        d = self.create_doc()
        grant_access(self.test_user_1.get_personal_group(), d, AccessType.teacher)
        d_path = d.path
        self.login_test1()
        self.get(f'/feedback/report/{d_path}')

    def test_missing_fields(self):
        self.login_test1()
        d = self.create_doc(initial_par="""
``` {#fb1 plugin="feedback"}
nextTask: ""
questionItems:
- pluginNames: [dropdown1]
  words: []
  choices:
    - match: ["is cooking"]
      correct: true
      levels: &rightmatch
        - "**Correct!** You answered: *|answer|*"
    - match: []  # Empty brackets for default feedback.
      levels: &defaultmatch
        - "*Level 1 default feedback* in italics with *"
```              
                """)
        e = self.get(d.url, as_tree=True).cssselect('feedback-runner')
        self.assertTrue(e)

        missing_user_answer = self.post_answer(plugin_type='feedback', task_id=f'{d.id}.fb1',
                                 user_input={'correct': False,
                                             'correct_answer': '',
                                             'feedback': ''})

        self.assertEqual({'error': '<div class="pluginError">\n'
          'The following fields have invalid values:\n'
          '<ul><li>user_answer: Missing data for required field.</li></ul>\n'
          '</div>'}, missing_user_answer['web'])

        missing_correct = self.post_answer(plugin_type='feedback', task_id=f'{d.id}.fb1',
                                 user_input={'user_answer' : '',
                                             'correct_answer': '',
                                             'feedback': ''})

        self.assertEqual({'error': '<div class="pluginError">\n'
                                   'The following fields have invalid values:\n'
                                   '<ul><li>correct: Missing data for required field.</li></ul>\n'
                                   '</div>'}, missing_correct['web'])

        missing_correct_answer = self.post_answer(plugin_type='feedback', task_id=f'{d.id}.fb1',
                                           user_input={'correct': False,
                                                        'user_answer': '',
                                                       'feedback': ''})

        self.assertEqual({'error': '<div class="pluginError">\n'
                                   'The following fields have invalid values:\n'
                                   '<ul><li>correct_answer: Missing data for required field.</li></ul>\n'
                                   '</div>'}, missing_correct_answer['web'])

        missing_feedback = self.post_answer(plugin_type='feedback', task_id=f'{d.id}.fb1',
                                           user_input={'correct': False,
                                                        'user_answer': '',
                                                       'correct_answer': ''})

        self.assertEqual({'error': '<div class="pluginError">\n'
                                   'The following fields have invalid values:\n'
                                   '<ul><li>feedback: Missing data for required field.</li></ul>\n'
                                   '</div>'}, missing_feedback['web'])

        empty = self.post_answer(plugin_type='feedback', task_id=f'{d.id}.fb1',
                                 user_input={'correct': False,
                                             'user_answer': '',
                                             'correct_answer': '',
                                             'feedback': ''})
        self.assertEqual({'result': 'saved'}, empty['web'])



