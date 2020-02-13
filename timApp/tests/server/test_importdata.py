"""Server tests for importData plugin."""
from timApp.tests.browser.browsertest import BrowserTest


class ImportDataTest(BrowserTest):
    def test_importdata(self):
        self.login_test1()
        d = self.create_doc(initial_par="""
#- {defaultplugin=textfield}
{#a#} {#b#}
#- {plugin=importData #t}
#- {plugin=importData #t2}
prefilter: |!!
return ["x;y;z", "a;b;c"];
!!
#- {plugin=importData #t3}
prefilter: |!!
data.push("u;x;1");
return data;
!!
        """)

        def imp_200(data, expect_web=None, fields=None, expect=None, task=None):
            import_input = {'data': data}
            if fields:
                import_input['fields'] = fields
            imp(import_input, expect if expect else {'web': expect_web}, 200, task)

        def imp_400(data, expect, fields=None, task=None):
            import_input = {'data': data}
            if fields:
                import_input['fields'] = fields
            imp(import_input, expect, 400, task)

        def imp(data, expect, status: int, task=None):
            if not task:
                task = 't'
            self.post_answer(
                'importData',
                f'{d.id}.{task}',
                data,
                expect_content=expect,
                expect_status=status,
            )

        imp_400('testuser1;x;x', 'Task not found in the document: x')
        imp_400('testuser1;;x', 'Invalid task name: ')
        imp_400('testuser1;a;x;z;y', 'Task not found in the document: z')
        imp_200('testuser1;a;x;z;y;w', {
            'result': 'Imported 0 values for 0 users\nWrong lines: 1\n\ntestuser1;a;x;z;y;w: odd number of field-value pairs'})
        imp_400('testuser1;1;2', 'Task not found in the document: z', fields=['z'])
        imp_400('testuser1;a;2', 'Task not found in the document: z', fields=['a=z'])
        imp_400('testuser1;x;1;y;2\ntestuser1;z;3', 'Task not found in the document: z',
                fields=['x=a', 'y=b', '*'])

        imp_200('', {'result': 'Imported 0 values for 0 users'})
        imp_200('x', {'result': 'Imported 0 values for 0 users\nWrong lines: 1\n\nx: too few parts'})
        imp_200('x;x;x', {'result': 'Imported 0 values for 0 users\nWrong lines: 1\n\nx: user not found'})
        imp_200('x;x', {'result': 'Imported 0 values for 0 users\nWrong lines: 1\n\nx;x: too few parts'})
        imp_200('testuser1;a;x', {'result': 'Imported 1 values for 1 users'})
        imp_200('testuser1;a;x;b;y', {'result': 'Imported 2 values for 1 users'})
        imp_200('testuser1;a;x\ntestuser1;b;y', {'result': 'Imported 2 values for 1 users'})
        imp_200('testuser1;x;x', expect={'savedNew': 3, 'web': {'result': 'Imported 1 values for 1 users'}},
                fields=['a'])
        imp_200('testuser1;x;x', expect={'savedNew': 5, 'web': {'result': 'Imported 2 values for 1 users'}},
                fields=['a', 'b'])
        imp_200('testuser1;x', expect={'savedNew': None, 'web': {'result': 'Imported 1 values for 1 users'}},
                fields=['a', 'b'])
        imp_200('testuser1', expect={'savedNew': None, 'web': {'result': 'Imported 0 values for 0 users'}},
                fields=['a', 'b'])
        imp_200('testuser1',
                {'result': 'Imported 0 values for 0 users\n'
                           'Wrong lines: 2\n'
                           '\n'
                           'x: user not found\n'
                           'a: user not found'}, task='t2')
        imp_200('testuser1;x;1;y;2\ntestuser1;z;3\ntestuser1',
                expect={'savedNew': 8, 'web': {'result': 'Imported 2 values for 1 users'}},
                fields=['x=a', 'y=b'])
        imp_200('x;a;1',
                {'result': 'Imported 0 values for 0 users\n'
                           'Wrong lines: 2\n'
                           '\n'
                           'x: user not found\n'
                           'u: user not found'}, task='t3')
