"""Server tests for importData plugin."""
from timApp.tests.browser.browsertest import BrowserTest
from timApp.timdb.sqa import db
from timApp.user.personaluniquecode import SchacPersonalUniqueCode, PersonalUniqueCode
from timApp.user.usergroup import UserGroup


def field_result(
        missing_users=None,
        created_users=None,
        user_key='username',
        changed=0,
        unchanged=0,
        result_text='',
        codes=None,
):
    if not missing_users:
        missing_users = []
    if not created_users:
        created_users = []
    if not codes:
        codes = [[]] * len(missing_users)

    empty_user = {
        'email': None,
        'full_name': None,
        'given_name': None,
        'last_name': None,
        'origin': None,
        'password': None,
        'password_hash': None,
        'unique_codes': [],
        'username': None,
    }
    return {
        'result': result_text,
        'fieldresult': {
            'fields_changed': changed,
            'fields_ignored': 0,
            'fields_unchanged': unchanged,
            'users_created': created_users,
            'users_missing': [
                {
                    **empty_user,
                    user_key: u,
                    'unique_codes': c,
                } for u, c in zip(missing_users, codes)],
        },
    }


class ImportDataTestBase(BrowserTest):
    def imp(self, d, data, expect, status: int, task=None):
        if not task:
            task = 't'
        self.post_answer(
            'importData',
            f'{d.id}.{task}',
            data,
            expect_content=expect,
            expect_status=status,
        )

    def grant_user_creation_right(self):
        self.current_user.add_to_group(UserGroup.get_user_creator_group(), added_by=None)
        db.session.commit()
        db.session.refresh(self.current_user)


class ImportDataTest(ImportDataTestBase):
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
#- {plugin=importData #t4}
joinProperty: x
#- {plugin=importData #t5}
joinProperty: studentID
#- {plugin=importData #t6}
joinProperty: id
#- {plugin=importData #t7}
joinProperty: email
#- {plugin=importData #t8}
joinProperty: username
#- {plugin=importData #t9}
joinProperty: studentID(jyu.fi)
        """)

        def imp_200(data, expect_web=None, fields=None, expect=None, task=None):
            import_input = {'data': data}
            if fields:
                import_input['fields'] = fields
            self.imp(d, import_input, expect if expect else {'web': expect_web}, 200, task)

        def imp_400(data, expect, fields=None, task=None):
            import_input = {'data': data}
            if fields:
                import_input['fields'] = fields
            self.imp(d, import_input, expect, 400, task)

        imp_400('testuser1;x;x', 'Task not found in the document: x')
        imp_400('testuser1;;x', 'Invalid task name: ')
        imp_400('testuser1;a;x;z;y', 'Task not found in the document: z')

        imp_200(
            'testuser1;a;x;z;y;w',
            field_result(result_text='Wrong lines: 1\n\ntestuser1;a;x;z;y;w: odd number of field-value pairs'),
        )
        imp_400('testuser1;1;2', 'Task not found in the document: z', fields=['z'])
        imp_400('testuser1;a;2', 'Task not found in the document: z', fields=['a=z'])
        imp_400(
            'testuser1;x;1;y;2\ntestuser1;z;3', 'Task not found in the document: z',
            fields=['x=a', 'y=b', '*'])
        imp_200('', field_result())
        imp_200('x', field_result(result_text='Wrong lines: 1\n\nx: too few parts'))
        imp_200('x;x;x', field_result(missing_users=['x']))
        imp_200('x;x', field_result(result_text='Wrong lines: 1\n\nx;x: too few parts'))
        imp_200('testuser1;a;x', field_result(changed=1))
        imp_200('testuser1;a;x;b;y', field_result(changed=1, unchanged=1))
        imp_200('testuser1;a;x\ntestuser1;b;y', field_result(unchanged=2))
        imp_200(
            'testuser1;x;x', expect={'savedNew': 3, 'web': field_result(unchanged=1)},
            fields=['a'])
        imp_200(
            'testuser1;x;x', expect={'savedNew': 5, 'web': field_result(changed=1, unchanged=1)},
            fields=['a', 'b'])
        imp_200(
            'testuser1;x', expect={'savedNew': None, 'web': field_result(unchanged=1)},
            fields=['a', 'b'])
        imp_200(
            'testuser1', expect={'savedNew': None, 'web': field_result()},
            fields=['a', 'b'])
        imp_200(
            'testuser1',
            field_result(missing_users=['x', 'a']), task='t2')
        imp_200(
            'testuser1;x;1;y;2\ntestuser1;z;3\ntestuser1',
            expect={'savedNew': 8, 'web': field_result(changed=2)},
            fields=['x=a', 'y=b'])
        imp_200(
            'x;a;1',
            field_result(missing_users=['x', 'u']), task='t3')
        imp_200('x;a;1', {'error': 'Invalid joinProperty: x'}, task='t4')
        imp_200('x;a;1', {'error': 'Invalid joinProperty: studentID'}, task='t5')
        imp_200('x;a;1', {'error': 'User ids must be ints (invalid literal for int() with base '
                                   "10: 'x')"}, task='t6')
        imp_200(f'{self.current_user.id};a;1',
                field_result(unchanged=1), task='t6')

        imp_200(f'{self.current_user.email};a;1', field_result(unchanged=1), task='t7')
        imp_200(
            f'{self.current_user.id};a;1', field_result(
                missing_users=[str(self.current_user.id)],
                user_key='email',
            ), task='t7')
        imp_200(f'{self.current_user.name};a;1', field_result(unchanged=1), task='t8')
        imp_200(f'{self.current_user.email};a;1', field_result(missing_users=[self.current_user.email]),
                task='t8')
        imp_200(f'x;a;1', field_result(
            missing_users=['imported_studentid_x'],
            codes=[[{'code': 'x',
                     'codetype': 'studentID',
                     'org': 'jyu.fi'}]], ),
                task='t9')
        self.current_user.set_unique_codes([
            SchacPersonalUniqueCode(code='x', codetype='studentID', org='jyu.fi'),
        ])
        db.session.commit()
        imp_200(f'x;a;1', field_result(unchanged=1), task='t9')


class AaltoImportTest(ImportDataTestBase):
    def test_aalto_import(self):
        self.login_test1()
        d = self.create_doc(initial_par="""
``` {#t plugin="importData"}
allowMissing: true
useurl: true
useurltoken: true
joinProperty: studentID(aalto.fi)
prefilter: |!!
res = [];
let nr = 0;
let fields = data[0];
let useridfield = 1;

fields = fields.toLowerCase(fields);
fields = fields.split(",");
for (let i = 0; i < fields.length; i++) {
   let fn = fields[i];
   let parts = fn.split(' ');
   if (parts.length < 2) continue;
   fields[i] = parts[1] + parts[0];
}

for (let i = 1; i < data.length; i++) {
    let r = data[i];
    let parts = r.split(",");
    let userid = parts[useridfield];
    let fieldcount = 0;
    for (let fi = useridfield+1; fi < fields.length; fi++) {
        if (fi >= parts.length) continue;
        value = parts[fi].trim();
        if (value === "") continue;
        if (value === "0") continue;
        fieldcount++;
        let r2 = "" + userid + ";" + fields[fi] + ";" + value;
        res.push(r2);
    }
    if (fieldcount < 1) continue;
    nr++;
}
return res;
!!
```
        """)

        self.imp(
            d,

            {
                'data': """
        UserID,StudentID,Email,Tags,1 Count,1 Total,1 Ratio,2 Count,2 Total,2 Ratio
        123,12345X,matti.meikalainen@aalto.fi,aalto,5,200,1.0,4,700,1.0
                """.strip(),
                'url': 'https://plus.cs.aalto.fi/api/v2/courses/12345/aggregatedata/',
                'createMissingUsers': True,
            },
            'You do not have permission to create users.',
            403,
        )
        self.grant_user_creation_right()
        self.imp(
            d,

            {
                'data': """
UserID,StudentID,Email,Tags,1 Count,1 Total,1 Ratio,2 Count,2 Total,2 Ratio
123,12345X,matti.meikalainen@aalto.fi,aalto,5,200,1.0,4,700,1.0
        """.strip(),
                'url': 'https://plus.cs.aalto.fi/api/v2/courses/12345/aggregatedata/',
                'createMissingUsers': True,
            },
            {'savedNew': 9,
             'web': field_result(
                 changed=8,
                 created_users=[
                     {'email': 'matti.meikalainen@aalto.fi',
                      'id': 5,
                      'name': 'matti.meikalainen@aalto.fi',
                      'real_name': 'Meikalainen Matti'},
                 ]),
             },
            200,
        )
        self.imp(
            d,

            {
                'data': """
        UserID,StudentID,Email,Tags,1 Count,1 Total,1 Ratio,2 Count,2 Total,2 Ratio
        123,12345X,matti.meikalainen@aalto.fi,aalto,5,200,1.0,4,700,1.0
                """.strip(),
                'url': 'https://plus.cs.aalto.fi/api/v2/courses/12345/aggregatedata/',
            },
            {
                'savedNew': None,
                'web': field_result(unchanged=8),
            }
            ,
            200,
        )

        puc = PersonalUniqueCode.find_by_student_id('12345X', 'aalto.fi')
        self.assertIsNotNone(puc)
        u = puc.user
        self.assertEqual('matti.meikalainen@aalto.fi', u.email)
        self.assertEqual('matti.meikalainen@aalto.fi', u.name)
        self.assertEqual('Meikalainen Matti', u.real_name)

        self.imp(
            d,

            {
                'data': """
        UserID,StudentID,Email,Tags,1 Count,1 Total,1 Ratio,2 Count,2 Total,2 Ratio
        123,12345Z,,aalto,5,200,1.0,4,700,1.0
                """.strip(),
                'url': 'https://plus.cs.aalto.fi/api/v2/courses/12345/aggregatedata/',
                'createMissingUsers': True,
            },
            'Invalid email: ""'
            ,
            400,
        )
