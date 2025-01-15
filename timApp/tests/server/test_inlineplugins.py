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
<span class="mathp inline"><img style="width:0.80327em; vertical-align:-0.06000em" src="data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0nMS4wJyBlbmNvZGluZz0nVVRGLTgnPz4KPCEtLSBUaGlzIGZpbGUgd2FzIGdlbmVyYXRlZCBieSBkdmlzdmdtIDMuMi4xIC0tPgo8c3ZnIHZlcnNpb249JzEuMScgeG1sbnM9J2h0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnJyB4bWxuczp4bGluaz0naHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluaycgd2lkdGg9JzYuNjkzOTIycHQnIGhlaWdodD0nNS4yODk0NnB0JyB2aWV3Qm94PSctLjUwMDAwMiAtNC43ODk0NTggNi42OTM5MjIgNS4yODk0Nic+CjxkZWZzPgo8c3R5bGUgdHlwZT0ndGV4dC9jc3MnPgo8IVtDREFUQVtwYXRoIHtzdHJva2U6IGN1cnJlbnRDb2xvcjtzdHJva2Utd2lkdGg6IDAuMDVwdDt9XV0+Cjwvc3R5bGU+CjxwYXRoIGlkPSdnMC0xMjAnIGQ9J00zLjMyNzUyMi0zLjAwODcxN0MzLjM4NzI5OC0zLjI2Nzc0NiAzLjYxNjQzOC00LjE4NDMwOSA0LjMxMzgyMy00LjE4NDMwOUM0LjM2MzYzNi00LjE4NDMwOSA0LjYwMjc0LTQuMTg0MzA5IDQuODExOTU1LTQuMDU0Nzk1QzQuNTMzMDAxLTQuMDA0OTgxIDQuMzMzNzQ4LTMuNzU1OTE1IDQuMzMzNzQ4LTMuNTE2ODEyQzQuMzMzNzQ4LTMuMzU3NDEgNC40NDMzMzctMy4xNjgxMiA0LjcxMjMyOS0zLjE2ODEyQzQuOTMxNTA3LTMuMTY4MTIgNS4yNTAzMTEtMy4zNDc0NDcgNS4yNTAzMTEtMy43NDU5NTNDNS4yNTAzMTEtNC4yNjQwMSA0LjY2MjUxNi00LjQwMzQ4NyA0LjMyMzc4Ni00LjQwMzQ4N0MzLjc0NTk1My00LjQwMzQ4NyAzLjM5NzI2LTMuODc1NDY3IDMuMjc3NzA5LTMuNjQ2MzI2QzMuMDI4NjQzLTQuMzAzODYxIDIuNDkwNjYtNC40MDM0ODcgMi4yMDE3NDMtNC40MDM0ODdDMS4xNjU2MjktNC40MDM0ODcgLjU5Nzc1OC0zLjExODMwNiAuNTk3NzU4LTIuODY5MjRDLjU5Nzc1OC0yLjc2OTYxNCAuNjk3Mzg1LTIuNzY5NjE0IC43MTczMS0yLjc2OTYxNEMuNzk3MDExLTIuNzY5NjE0IC44MjY4OTktMi43ODk1MzkgLjg0NjgyNC0yLjg3OTIwM0MxLjE4NTU1NC0zLjkzNTI0MyAxLjg0MzA4OC00LjE4NDMwOSAyLjE4MTgxOC00LjE4NDMwOUMyLjM3MTEwOC00LjE4NDMwOSAyLjcxOTgwMS00LjA5NDY0NSAyLjcxOTgwMS0zLjUxNjgxMkMyLjcxOTgwMS0zLjIwNzk3IDIuNTUwNDM2LTIuNTQwNDczIDIuMTgxODE4LTEuMTQ1NzA0QzIuMDIyNDE2LS41MjgwMiAxLjY3MzcyNC0uMTA5NTg5IDEuMjM1MzY3LS4xMDk1ODlDMS4xNzU1OTItLjEwOTU4OSAuOTQ2NDUxLS4xMDk1ODkgLjczNzIzNS0uMjM5MTAzQy45ODYzMDEtLjI4ODkxNyAxLjIwNTQ3OS0uNDk4MTMyIDEuMjA1NDc5LS43NzcwODZDMS4yMDU0NzktMS4wNDYwNzcgLjk4NjMwMS0xLjEyNTc3OCAuODM2ODYyLTEuMTI1Nzc4Qy41Mzc5ODMtMS4xMjU3NzggLjI4ODkxNy0uODY2NzUgLjI4ODkxNy0uNTQ3OTQ1Qy4yODg5MTctLjA4OTY2NCAuNzg3MDQ5IC4xMDk1ODkgMS4yMjU0MDUgLjEwOTU4OUMxLjg4MjkzOSAuMTA5NTg5IDIuMjQxNTk0LS41ODc3OTYgMi4yNzE0ODItLjY0NzU3MkMyLjM5MTAzNC0uMjc4OTU0IDIuNzQ5Njg5IC4xMDk1ODkgMy4zNDc0NDcgLjEwOTU4OUM0LjM3MzU5OSAuMTA5NTg5IDQuOTQxNDY5LTEuMTc1NTkyIDQuOTQxNDY5LTEuNDI0NjU4QzQuOTQxNDY5LTEuNTI0Mjg0IDQuODUxODA2LTEuNTI0Mjg0IDQuODIxOTE4LTEuNTI0Mjg0QzQuNzMyMjU0LTEuNTI0Mjg0IDQuNzEyMzI5LTEuNDg0NDMzIDQuNjkyNDAzLTEuNDE0Njk1QzQuMzYzNjM2LS4zNDg2OTIgMy42ODYxNzctLjEwOTU4OSAzLjM2NzM3Mi0uMTA5NTg5QzIuOTc4ODI5LS4xMDk1ODkgMi44MTk0MjctLjQyODM5NCAyLjgxOTQyNy0uNzY3MTIzQzIuODE5NDI3LS45ODYzMDEgMi44NzkyMDMtMS4yMDU0NzkgMi45ODg3OTItMS42NDM4MzZMMy4zMjc1MjItMy4wMDg3MTdaJy8+CjwvZGVmcz4KPGcgaWQ9J3BhZ2UxJz4KPHVzZSB4PScwJyB5PScwJyB4bGluazpocmVmPScjZzAtMTIwJy8+CjwvZz4KPC9zdmc+" title="x"></span>
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
