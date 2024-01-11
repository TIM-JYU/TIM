from timApp.auth.accesstype import AccessType
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.user import User


class InlinePluginTest(TimRouteTest):
    # Previously server wrapped inlineplugins in a span element, but now the client-side does the wrapping
    def test_inline_plugins(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#- {defaultplugin=pali id=Lm7y6R7n5XIb}
*Hello* {#t1#}, {#tx:nonexistent#} and {#t2#} $x$

#- {defaultplugin=pali math_type=svg id=spOMcE20X2aX}
Hi {#t3#} $x$

#- {defaultplugin=pali id=Se0s8FDLbhOp}
{#t4 header: hi, footer: ho#}
            """
        )
        r = self.get(d.url, as_tree=True)
        e = r.cssselect(".par")[0]
        expected_json = self.create_plugin_json(
            d,
            "t1",
            "Lm7y6R7n5XIb",
        )
        expected_json2 = self.create_plugin_json(
            d,
            "t2",
            "Lm7y6R7n5XIb",
        )
        self.assert_same_html(
            e,
            rf"""
<div class="par" id="Lm7y6R7n5XIb" t="MHgyN2U5NDhhMA==" attrs='{{"defaultplugin": "pali"}}'>
<div tabindex="0" class="parContent">
            <p>
                <em>Hello</em> <tim-plugin-loader type="full" answer-id="" class="pluginpali inlineplugin" wrapper="span" id="{d.id}.t1.Lm7y6R7n5XIb" plugin-type="/pali" task-id="{d.id}.t1">
                <pali-runner json="{self.make_base64(
        expected_json)}"></pali-runner></tim-plugin-loader>, <span class="error" ng-non-bindable>Plugin nonexistent error: Plugin does not exist.</span> and <tim-plugin-loader type="full" answer-id="" class="pluginpali inlineplugin" wrapper="span" id="{d.id}.t2.Lm7y6R7n5XIb" plugin-type="/pali" task-id="{d.id}.t2"><pali-runner json="{self.make_base64(
        expected_json2)}"></pali-runner></tim-plugin-loader>
            <span class="math inline">\(x\)</span>
            </p>
            </div>
<div class="editline" tabindex="0" title="Click to edit this paragraph"></div>
<div class="readline" title="Click to mark this paragraph as read"></div>
</div>
    """,
        )
        a = self.post_answer_no_abdata(
            plugin_type="pali",
            task_id=f"{d.id}.t2",
            user_input={"userword": "aaaaaa"},
        )
        aid = a["savedNew"]
        self.assertEqual(
            {"savedNew": aid, "valid": True, "web": {"result": "saved"}}, a
        )
        self.assertIsInstance(aid, int)

        r = self.get(d.url, as_tree=True)
        e = r.cssselect(".par")[0]
        s = {"userword": "aaaaaa"}
        expected_json2 = self.create_plugin_json(
            d,
            "t2",
            "Lm7y6R7n5XIb",
            info={
                "earlier_answers": 1,
                "look_answer": False,
                "max_answers": None,
                "user_id": "testuser1",
                "valid": True,
                "show_points": True,
            },
            state=s,
        )
        self.assert_same_html(
            e,
            rf"""
<div class="par" id="Lm7y6R7n5XIb" t="MHgyN2U5NDhhMA==" attrs='{{"defaultplugin": "pali"}}'>
<div tabindex="0" class="parContent">
<p>
<em>Hello</em>
<tim-plugin-loader type="full" answer-id="" class="pluginpali inlineplugin" wrapper="span" id="{d.id}.t1.Lm7y6R7n5XIb" plugin-type="/pali" task-id="{d.id}.t1">
<pali-runner json="{self.make_base64(expected_json)}"></pali-runner></tim-plugin-loader>,
<span class="error" ng-non-bindable>Plugin nonexistent error: Plugin does not exist.</span> and 
<tim-plugin-loader type="full" answer-id="{aid}" class="pluginpali inlineplugin" wrapper="span" id="{d.id}.t2.Lm7y6R7n5XIb" plugin-type="/pali" task-id="{d.id}.t2">
<pali-runner json="{self.make_base64(
        expected_json2)}"></pali-runner></tim-plugin-loader>
<span class="math inline">\(x\)</span>
</p>
            </div>
<div class="editline" tabindex="0" title="Click to edit this paragraph"></div>
<div class="readline" title="Click to mark this paragraph as read"></div>
</div>
            """,
        )
        expected_json = self.create_plugin_json(
            d,
            "t3",
            "spOMcE20X2aX",
        )
        self.assert_same_html(
            r.cssselect(".par")[1],
            f"""
<div class="par" id="spOMcE20X2aX" t="LTB4MTk4ZmYxOTQ=" attrs='{{"defaultplugin": "pali", "math_type": "svg"}}'>
<div tabindex="0" class="parContent">
<p>
                Hi
<tim-plugin-loader type="full" answer-id="" class="pluginpali inlineplugin" wrapper="span" id="{d.id}.t3.spOMcE20X2aX" plugin-type="/pali" task-id="{d.id}.t3">
<pali-runner json="{self.make_base64(expected_json)}"></pali-runner>
</tim-plugin-loader>
<span class="mathp inline"><img style="width:0.80327em; vertical-align:-0.06000em" src="data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0nMS4wJyBlbmNvZGluZz0nVVRGLTgnPz4KPCEtLSBUaGlzIGZpbGUgd2FzIGdlbmVyYXRlZCBieSBkdmlzdmdtIDIuMTQgLS0+CjxzdmcgdmVyc2lvbj0nMS4xJyB4bWxucz0naHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmcnIHhtbG5zOnhsaW5rPSdodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rJyB3aWR0aD0nNi42OTM5MjJwdCcgaGVpZ2h0PSc1LjI4OTQ2cHQnIHZpZXdCb3g9Jy0uNTAwMDAyIC00Ljc4OTQ1OCA2LjY5MzkyMiA1LjI4OTQ2Jz4KPGRlZnM+CjxzdHlsZSB0eXBlPSd0ZXh0L2Nzcyc+CjwhW0NEQVRBW3BhdGgge3N0cm9rZTogY3VycmVudENvbG9yO3N0cm9rZS13aWR0aDogMC4wNXB0O31dXT4KPC9zdHlsZT4KPHBhdGggaWQ9J2cwLTEyMCcgZD0nTTMuMzI3NTIyLTMuMDA4NzE3QzMuMzg3Mjk4LTMuMjY3NzQ2IDMuNjE2NDM4LTQuMTg0MzA5IDQuMzEzODIzLTQuMTg0MzA5QzQuMzYzNjM2LTQuMTg0MzA5IDQuNjAyNzQtNC4xODQzMDkgNC44MTE5NTUtNC4wNTQ3OTVDNC41MzMwMDEtNC4wMDQ5ODEgNC4zMzM3NDgtMy43NTU5MTUgNC4zMzM3NDgtMy41MTY4MTJDNC4zMzM3NDgtMy4zNTc0MSA0LjQ0MzMzNy0zLjE2ODEyIDQuNzEyMzI5LTMuMTY4MTJDNC45MzE1MDctMy4xNjgxMiA1LjI1MDMxMS0zLjM0NzQ0NyA1LjI1MDMxMS0zLjc0NTk1M0M1LjI1MDMxMS00LjI2NDAxIDQuNjYyNTE2LTQuNDAzNDg3IDQuMzIzNzg2LTQuNDAzNDg3QzMuNzQ1OTUzLTQuNDAzNDg3IDMuMzk3MjYtMy44NzU0NjcgMy4yNzc3MDktMy42NDYzMjZDMy4wMjg2NDMtNC4zMDM4NjEgMi40OTA2Ni00LjQwMzQ4NyAyLjIwMTc0My00LjQwMzQ4N0MxLjE2NTYyOS00LjQwMzQ4NyAuNTk3NzU4LTMuMTE4MzA2IC41OTc3NTgtMi44NjkyNEMuNTk3NzU4LTIuNzY5NjE0IC42OTczODUtMi43Njk2MTQgLjcxNzMxLTIuNzY5NjE0Qy43OTcwMTEtMi43Njk2MTQgLjgyNjg5OS0yLjc4OTUzOSAuODQ2ODI0LTIuODc5MjAzQzEuMTg1NTU0LTMuOTM1MjQzIDEuODQzMDg4LTQuMTg0MzA5IDIuMTgxODE4LTQuMTg0MzA5QzIuMzcxMTA4LTQuMTg0MzA5IDIuNzE5ODAxLTQuMDk0NjQ1IDIuNzE5ODAxLTMuNTE2ODEyQzIuNzE5ODAxLTMuMjA3OTcgMi41NTA0MzYtMi41NDA0NzMgMi4xODE4MTgtMS4xNDU3MDRDMi4wMjI0MTYtLjUyODAyIDEuNjczNzI0LS4xMDk1ODkgMS4yMzUzNjctLjEwOTU4OUMxLjE3NTU5Mi0uMTA5NTg5IC45NDY0NTEtLjEwOTU4OSAuNzM3MjM1LS4yMzkxMDNDLjk4NjMwMS0uMjg4OTE3IDEuMjA1NDc5LS40OTgxMzIgMS4yMDU0NzktLjc3NzA4NkMxLjIwNTQ3OS0xLjA0NjA3NyAuOTg2MzAxLTEuMTI1Nzc4IC44MzY4NjItMS4xMjU3NzhDLjUzNzk4My0xLjEyNTc3OCAuMjg4OTE3LS44NjY3NSAuMjg4OTE3LS41NDc5NDVDLjI4ODkxNy0uMDg5NjY0IC43ODcwNDkgLjEwOTU4OSAxLjIyNTQwNSAuMTA5NTg5QzEuODgyOTM5IC4xMDk1ODkgMi4yNDE1OTQtLjU4Nzc5NiAyLjI3MTQ4Mi0uNjQ3NTcyQzIuMzkxMDM0LS4yNzg5NTQgMi43NDk2ODkgLjEwOTU4OSAzLjM0NzQ0NyAuMTA5NTg5QzQuMzczNTk5IC4xMDk1ODkgNC45NDE0NjktMS4xNzU1OTIgNC45NDE0NjktMS40MjQ2NThDNC45NDE0NjktMS41MjQyODQgNC44NTE4MDYtMS41MjQyODQgNC44MjE5MTgtMS41MjQyODRDNC43MzIyNTQtMS41MjQyODQgNC43MTIzMjktMS40ODQ0MzMgNC42OTI0MDMtMS40MTQ2OTVDNC4zNjM2MzYtLjM0ODY5MiAzLjY4NjE3Ny0uMTA5NTg5IDMuMzY3MzcyLS4xMDk1ODlDMi45Nzg4MjktLjEwOTU4OSAyLjgxOTQyNy0uNDI4Mzk0IDIuODE5NDI3LS43NjcxMjNDMi44MTk0MjctLjk4NjMwMSAyLjg3OTIwMy0xLjIwNTQ3OSAyLjk4ODc5Mi0xLjY0MzgzNkwzLjMyNzUyMi0zLjAwODcxN1onLz4KPC9kZWZzPgo8ZyBpZD0ncGFnZTEnPgo8dXNlIHg9JzAnIHk9JzAnIHhsaW5rOmhyZWY9JyNnMC0xMjAnLz4KPC9nPgo8L3N2Zz4=" title="x"></span>
</p>
</div>

<div class="editline" tabindex="0" title="Click to edit this paragraph"></div>
<div class="readline" title="Click to mark this paragraph as read"></div>
</div>
            """,
        )

        expected_json = self.create_plugin_json(
            d,
            "t4",
            "Se0s8FDLbhOp",
            markup={
                "header": "hi",
                "footer": "ho",
            },
        )
        self.assert_same_html(
            r.cssselect(".par")[2],
            f"""
<div class="par" id="Se0s8FDLbhOp" t="LTB4NGU3YzFkYWM=" attrs='{{"defaultplugin": "pali"}}'>
<div tabindex="0" class="parContent">
<p>
<tim-plugin-loader type="full" answer-id="" class="pluginpali inlineplugin" wrapper="span" id="{d.id}.t4.Se0s8FDLbhOp" plugin-type="/pali" task-id="{d.id}.t4">
<pali-runner json="{self.make_base64(expected_json)}"></pali-runner>
</tim-plugin-loader>
</p>
</div>
<div class="editline" tabindex="0" title="Click to edit this paragraph"></div>
<div class="readline" title="Click to mark this paragraph as read"></div>
</div>
                    """,
        )

        a = self.post_answer_no_abdata(
            plugin_type="pali",
            task_id=f"{d.id}.t5",
            user_input={"userword": "aaaaaa"},
            expect_status=400,
            expect_content="Task not found in the document: t5",
        )

    def test_inline_plugin_no_html_escape(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#- {defaultplugin=pali id=a3Xuyg1PF1l1}
{#t5 initword: #}
        """
        )
        r = self.get(d.url, as_tree=True)
        # Make sure Dumbo won't escape plugin's error HTML.
        self.assert_same_html(
            r.cssselect(".parContent")[0],
            f"""
<div tabindex="0" class="parContent">
    <p>
        <tim-plugin-loader type="full" answer-id="" class="pluginpali inlineplugin" wrapper="span" id="{d.id}.t5.a3Xuyg1PF1l1" plugin-type="/pali" task-id="{d.id}.t5">
            <div class="pluginError">
                The following fields have invalid values:
                <ul>
                    <li>
                        initword: Field may not be null.
                    </li>
                </ul>
            </div>
        </tim-plugin-loader>
    </p>
</div>
        """,
        )

    def test_inline_plugin_sanitize(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#- {defaultplugin=pali}
<script>alert('hello')</script>
        """
        )
        r = self.get(d.url, as_tree=True)
        self.assertFalse(r.cssselect(".parContent script"))

    def test_multiline_inlineplugin(self):
        """Multiline markup in inlineplugins works."""
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#- {defaultplugin=pali id=SSYigUyqdb7p}
{#t
initword: hi
#}
        """
        )
        r = self.get(d.url, as_tree=True)
        e = r.cssselect(
            f'.parContent > p > tim-plugin-loader[task-id="{d.id}.t"] > pali-runner'
        )
        self.assertTrue(e)
        self.assert_plugin_json(
            e[0],
            self.create_plugin_json(
                d,
                "t",
                par_id="SSYigUyqdb7p",
                markup={"initword": "hi"},
            ),
        )
        d = self.create_doc(
            initial_par="""
#- {defaultplugin=pali id=SSYigUyqdb7p}
{#t
initword: hi
inputplaceholder: test
#}
            """
        )
        r = self.get(d.url, as_tree=True)
        e = r.cssselect(
            f'.parContent > p > tim-plugin-loader[task-id="{d.id}.t"] > pali-runner'
        )
        self.assertTrue(e)
        self.assert_plugin_json(
            e[0],
            self.create_plugin_json(
                d,
                "t",
                par_id="SSYigUyqdb7p",
                markup={
                    "initword": "hi",
                    "inputplaceholder": "test",
                },
            ),
        )

    def test_inline_plugin_error_html_no_p(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#- {defaultplugin="pali" id=a3Xuyg1PF1l1}
a {#x initword: #} b
        """
        )
        r = self.get(d.url, as_tree=True)
        self.assert_same_html(
            r.cssselect(".parContent")[0],
            f"""
<div tabindex="0" class="parContent">
    <p>
        a
        <tim-plugin-loader type="full" answer-id="" class="pluginpali inlineplugin" wrapper="span" id="{d.id}.x.a3Xuyg1PF1l1" plugin-type="/pali" task-id="{d.id}.x">
            <div class="pluginError">
                The following fields have invalid values:
                <ul>
                    <li>
                        initword: Field may not be null.
                    </li>
                </ul>
            </div>
        </tim-plugin-loader>
        b
    </p>
</div>""",
        )

    def test_inline_plugin_ref(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#-  {defaultplugin=pali id=SSYigUyqdb7p}
{#t}
        """
        )
        d2 = self.create_doc()
        d2.document.add_paragraph_obj(
            d.document.get_paragraphs()[0].create_reference(d2.document)
        )
        self.get(d2.url)

    def test_inline_plugin_login(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#- {defaultplugin=pali}
{#t#}
"""
        )
        u = d.url
        User.get_anon().grant_access(d, AccessType.view)
        db.session.commit()
        self.logout()
        r = self.get(u, as_tree=True).cssselect(".parContent tim-login-menu")
        self.assertTrue(r)

    def test_taskinfo_invalid_inline_plugin(self):
        """Check that invalid inline plugin doesn't affect task info of other plugins"""
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#- {defaultplugin="pali"}
{#.invalid#}

``` {#pali1 plugin="pali"}
```
        """
        )

        self.json_req(
            f"/taskinfo/{d.id}.pali1",
            json_data={
                "maxPoints": None,
                "userMin": None,
                "userMax": None,
                "showPoints": {},
                "deadline": None,
                "starttime": None,
                "answerLimit": None,
                "triesText": None,
                "pointsText": None,
                "buttonNewTask": None,
                "modelAnswer": None,
            },
        )
