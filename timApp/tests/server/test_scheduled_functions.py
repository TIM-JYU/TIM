import json
from contextlib import contextmanager
from datetime import timedelta

from isodate import Duration, duration_isoformat

from timApp.answer.routes import post_answer_impl
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.tim_app import app
from timApp.tim_celery import do_run_user_function
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.user.usergroup import UserGroup
from timApp.util.utils import get_current_time


class ScheduledFunctionTest(TimRouteTest):
    def test_scheduled_function(self):
        self.login_test1()
        d = self.create_doc(initial_par="#- {plugin=textfield #t}")
        self.json_post(
            f"/scheduling/functions",
            {
                "doc_id": d.id,
                "plugin_name": "t",
                "expires": get_current_time() + timedelta(seconds=10),
                "interval": duration_isoformat(Duration(seconds=1)),
            },
            expect_status=403,
        )
        u = self.test_user_1
        u.groups.append(UserGroup.get_teachers_group())
        db.session.commit()
        r = self.json_post(
            f"/scheduling/functions",
            {
                "doc_id": d.id,
                "plugin_name": "t",
                "expires": get_current_time() + timedelta(seconds=10),
                "interval": duration_isoformat(Duration(seconds=1)),
            },
        )
        pt_id = r["block_id"]
        self.json_post(
            f"/scheduling/functions",
            {
                "doc_id": d.id,
                "plugin_name": "t",
                "expires": get_current_time() + timedelta(seconds=10),
                "interval": duration_isoformat(Duration(seconds=1)),
            },
            expect_status=400,
            expect_content="A scheduled function for this plugin already exists. Remove the existing function first "
            "before adding a new one.",
        )
        self.json_delete(f"/scheduling/functions/{pt_id}", {})
        self.json_delete(f"/scheduling/functions/{pt_id}", {}, expect_status=404)

    def test_function_listing(self):
        self.login_test2()
        d = self.create_doc(
            initial_par="""
#- {#t plugin=textfield}
#- {#t2 plugin=textfield}
"""
        )
        u = self.test_user_2
        u.groups.append(UserGroup.get_teachers_group())
        db.session.commit()
        self.json_post(
            f"/scheduling/functions",
            {
                "doc_id": d.id,
                "plugin_name": "t",
                "expires": get_current_time() + timedelta(seconds=10),
                "interval": duration_isoformat(Duration(seconds=1)),
            },
        )
        self.json_post(
            f"/scheduling/functions",
            {
                "doc_id": d.id,
                "plugin_name": "t2",
                "expires": get_current_time() + timedelta(seconds=10),
                "interval": duration_isoformat(Duration(seconds=1)),
            },
        )
        r = self.get("/scheduling/functions")
        self.assertEqual(2, len(r))
        self.login_test3()
        r = self.get("/scheduling/functions")
        self.assertEqual(0, len(r))
        r = self.get(
            "/scheduling/functions", query_string={"all_users": True}, expect_status=403
        )
        self.make_admin(self.test_user_3)
        r = self.get("/scheduling/functions", query_string={"all_users": True})
        self.assertEqual(2, len(r))


class ScheduledFunctionRunTest(TimRouteTest):
    @contextmanager
    def no_request_context(self):
        self.client.__exit__(None, None, None)
        with app.app_context():
            yield
        self.client.__enter__()
        self.get("/")

    def test_scheduled_function(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#- {plugin=textfield #t}

``` {#runner plugin=jsrunner}
groups:
 - testuser1
fields:
 - t
program: |!!
const x = tools.getInt("t", 0);
tools.setInt("t", x + 1);
!!
```
"""
        )
        # Make sure the following post_answer_impl call works without a request context.
        with self.no_request_context():
            post_answer_impl(f"{d.id}.t", {}, {}, {}, self.test_user_1, (), [], None)
            post_answer_impl(
                f"{d.id}.runner", {}, {}, {}, self.test_user_1, (), [], None
            )

    def test_scheduled_function_chain(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#- {plugin=textfield #t}
#- {plugin=textfield #t2}
#- {plugin=textfield #sum}

``` {#runner plugin=jsrunner}
groups:
 - testuser1
fields:
 - t
 - t2
program: |!!
for (const f of ["t", "t2"]) {
    const x = tools.getInt(f, 0);
    tools.setInt(f, x + 1);
}
!!
nextRunner: sumrunner
```

``` {#sumrunner plugin=jsrunner}
groups:
 - testuser1
fields:
 - t
 - t2
program: |!!
let sum = 0;
for (const f of ["t", "t2"]) {
    sum += tools.getInt(f, 0);
}
tools.setInt("sum", sum);
!!
```
        """
        )
        with self.no_request_context():
            do_run_user_function(self.test_user_1.id, f"{d.id}.runner", {})
            self.verify_answer_content(
                f"{d.id}.sum", "c", 2, self.test_user_1, expected_count=1
            )
            do_run_user_function(self.test_user_1.id, f"{d.id}.runner", {})
            self.verify_answer_content(
                f"{d.id}.sum", "c", 4, self.test_user_1, expected_count=2
            )

    def test_scheduled_function_exportdata(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
``` {#GLO_stat plugin="timTable"}
task: true
```

``` {plugin="csPlugin" #GLO_DemoN}
type: chartjs
data:
  title: Demot
```

#- {#runner plugin=jsrunner}
groups: []
fields: []
program: ""
postprogram: |!!
gtools.outdata.exportdata = [
{
  plugin: 'GLO_stat',
  data: {
        "headers": ["", "n", "sum", "avg", "min", "max", "sd", "%"],
        "matrix": [
            ["d1", 1, 1, 1, 1, 1, 0, 100],
            ["d2", 1, 2, 2, 2, 2, 0, 100]
        ]
    },
  save: true,
},
{
  plugin: 'GLO_DemoN',
  data: {
    "labels": ["d1", "d2", "d3"],
    "data": [306, 300, 294]
  },
  save: true,
},
];
!!
        """
        )
        with self.no_request_context():
            with self.internal_container_ctx():
                do_run_user_function(self.test_user_1.id, f"{d.id}.runner", {})
        self.verify_answer_content(
            f"{d.id}.GLO_stat",
            None,
            {
                "headers": ["", "n", "sum", "avg", "min", "max", "sd", "%"],
                "userdata": {
                    "cells": {
                        "A1": "d1",
                        "A2": "d2",
                        "B1": 1,
                        "B2": 1,
                        "C1": 1,
                        "C2": 2,
                        "D1": 1,
                        "D2": 2,
                        "E1": 1,
                        "E2": 2,
                        "F1": 1,
                        "F2": 2,
                        "G1": 0,
                        "G2": 0,
                        "H1": 100,
                        "H2": 100,
                    },
                    "type": "Relative",
                },
            },
            self.test_user_1,
            expected_count=1,
        )
        self.verify_answer_content(
            f"{d.id}.GLO_DemoN",
            "c",
            {
                "labels": [
                    "d1",
                    "d2",
                    "d3",
                ],
                "data": [
                    306,
                    300,
                    294,
                ],
            },
            self.test_user_1,
            expected_count=1,
        )

    def test_import_function_with_create_users(self):
        self.login_test2()
        d = self.create_doc(
            initial_par="""
#- {plugin=jsrunner #runner}
includeUsers: all
fields: []
groups: []
program: ''

``` {#import plugin="importData"}
ignoreMissing: true
allowMissing: true
aplus:
  course: 1234
addUsersToGroup: testgroup1
nextRunner: runner
```
        """
        )
        ug = UserGroup.create("testgroup1")
        db.session.add(ug)
        ug.admin_doc = self.create_doc().block
        u = self.test_user_2
        u.groups.append(UserGroup.get_user_creator_group())
        db.session.commit()
        with self.no_request_context():
            with self.internal_container_ctx() as m:
                m.add(
                    "GET",
                    "https://plus.cs.aalto.fi/api/v2/courses/1234/aggregatedata/?format=json",
                    body=json.dumps(
                        [
                            {
                                "UserID": 123,
                                "StudentID": "12345X",
                                "Email": "matti.meikalainen@aalto.fi",
                                "Tags": "aalto",
                                "1 Count": 2,
                                "1 Total": 100,
                                "1 Ratio": 0.125,
                                "2 Count": 0,
                                "2 Total": 0,
                                "2 Ratio": 0.0,
                            }
                        ]
                    ),
                    status=200,
                )
                do_run_user_function(
                    self.test_user_2.id, f"{d.id}.import", {"token": "abc"}
                )
        self.verify_answer_content(
            f"{d.id}.ratio1",
            "c",
            "0.125",
            User.get_by_email("matti.meikalainen@aalto.fi"),
            expected_count=1,
        )
