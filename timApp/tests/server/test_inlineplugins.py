from timApp.tests.server.timroutetest import TimRouteTest
from timApp.user.usergroup import UserGroup
from timApp.user.userutils import grant_view_access


class InlinePluginTest(TimRouteTest):

    def test_inline_plugins(self):
        self.login_test1()
        d = self.create_doc(initial_par="""
#- {defaultplugin=pali id=Lm7y6R7n5XIb}
*Hello* {#t1#}, {#tx:nonexistent#} and {#t2#} $x$

#- {defaultplugin=pali math_type=svg id=spOMcE20X2aX}
Hi {#t3#} $x$

#- {defaultplugin=pali id=Se0s8FDLbhOp}
{#t4 header: hi, footer: ho#}
            """)
        r = self.get(d.url, as_tree=True)
        e = r.cssselect('.par')[0]
        expected_json = self.create_plugin_json(
            d,
            't1',
            'Lm7y6R7n5XIb',
        )
        expected_json2 = self.create_plugin_json(
            d,
            't2',
            'Lm7y6R7n5XIb',
        )
        self.assert_same_html(e, f"""
<div class="par" id="Lm7y6R7n5XIb" t="MHgyN2U5NDhhMA==" attrs='{{"defaultplugin": "pali"}}'>
<div tabindex="0" class="parContent">
                <em>Hello</em> <tim-plugin-loader type="full" answer-id="" class="pluginpali inlineplugin" task-id="{d.id}.t1"><span id="{d.id}.t1.Lm7y6R7n5XIb" data-plugin="/pali">
                <pali-runner json="{self.make_base64(
        expected_json)}"></pali-runner></span></tim-plugin-loader>, <span class="error">Plugin nonexistent error: Plugin does not exist.</span> and <tim-plugin-loader type="full" answer-id="" class="pluginpali inlineplugin" task-id="{d.id}.t2"><span id="{d.id}.t2.Lm7y6R7n5XIb" data-plugin="/pali"><pali-runner json="{self.make_base64(
        expected_json2)}"></pali-runner></span></tim-plugin-loader>
            <span class="math inline">\(x\)</span>
            </div>
<div class="editline" tabindex="0" title="Click to edit this paragraph"></div>
<div class="readline" title="Click to mark this paragraph as read"></div>
</div>
    """)
        a = self.post_answer_no_abdata(
            plugin_type='pali', task_id=f'{d.id}.t2',
            user_input={'userword': 'aaaaaa'},
        )
        aid = a['savedNew']
        self.assertEqual({'savedNew': aid, 'web': {'result': 'saved'}}, a)
        self.assertIsInstance(aid, int)

        r = self.get(d.url, as_tree=True)
        e = r.cssselect('.par')[0]
        s = {'userword': 'aaaaaa'}
        expected_json2 = self.create_plugin_json(
            d,
            't2',
            'Lm7y6R7n5XIb',
            info={
                "earlier_answers": 1,
                "look_answer": False,
                "max_answers": None,
                "user_id": "testuser1",
                "valid": True,
            },
            state=s,
        )
        self.assert_same_html(e, f"""
<div class="par" id="Lm7y6R7n5XIb" t="MHgyN2U5NDhhMA==" attrs='{{"defaultplugin": "pali"}}'>
<div tabindex="0" class="parContent">
<em>Hello</em>
<tim-plugin-loader type="full" answer-id="" class="pluginpali inlineplugin" task-id="{d.id}.t1">
<span id="{d.id}.t1.Lm7y6R7n5XIb" data-plugin="/pali">
<pali-runner json="{self.make_base64(expected_json)}"></pali-runner></span></tim-plugin-loader>,
<span class="error">Plugin nonexistent error: Plugin does not exist.</span> and 
<tim-plugin-loader type="full" answer-id="{aid}" class="pluginpali inlineplugin" task-id="{d.id}.t2">
<span id="{d.id}.t2.Lm7y6R7n5XIb" data-plugin="/pali"><pali-runner json="{self.make_base64(
        expected_json2)}"></pali-runner></span></tim-plugin-loader>
<span class="math inline">\(x\)</span>
            </div>
<div class="editline" tabindex="0" title="Click to edit this paragraph"></div>
<div class="readline" title="Click to mark this paragraph as read"></div>
</div>
            """)
        expected_json = self.create_plugin_json(
            d,
            't3',
            'spOMcE20X2aX',
        )
        self.assert_same_html(r.cssselect('.par')[1], f"""
<div class="par" id="spOMcE20X2aX" t="LTB4MTk4ZmYxOTQ=" attrs='{{"defaultplugin": "pali", "math_type": "svg"}}'>
<div tabindex="0" class="parContent">
                Hi
<tim-plugin-loader type="full" answer-id="" class="pluginpali inlineplugin" task-id="{d.id}.t3">
<span id="{d.id}.t3.spOMcE20X2aX" data-plugin="/pali">
<pali-runner json="{self.make_base64(expected_json)}"></pali-runner>
</span>
</tim-plugin-loader>
<span class="mathp inline"><img style="width:0.80327em; vertical-align:-0.06000em" src="data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0nMS4wJyBlbmNvZGluZz0nVVRGLTgnPz4KPCEtLSBUaGlzIGZpbGUgd2FzIGdlbmVyYXRlZCBieSBkdmlzdmdtIDIuNCAtLT4KPHN2ZyBoZWlnaHQ9JzUuMjg5NDZwdCcgdmVyc2lvbj0nMS4xJyB2aWV3Qm94PSctMC41MDAwMDIgLTQuNzg5NDU4IDYuNjkzOTIyIDUuMjg5NDYnIHdpZHRoPSc2LjY5MzkyMnB0JyB4bWxucz0naHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmcnIHhtbG5zOnhsaW5rPSdodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rJz4KPGRlZnM+PHN0eWxlIHR5cGU9InRleHQvY3NzIj48IVtDREFUQVtwYXRoIHtzdHJva2U6IGN1cnJlbnRDb2xvcjtzdHJva2Utd2lkdGg6IDAuMDVwdDt9XV0+PC9zdHlsZT48cGF0aCBkPSdNMy4zMjc1MjIgLTMuMDA4NzE3QzMuMzg3Mjk4IC0zLjI2Nzc0NiAzLjYxNjQzOCAtNC4xODQzMDkgNC4zMTM4MjMgLTQuMTg0MzA5QzQuMzYzNjM2IC00LjE4NDMwOSA0LjYwMjc0IC00LjE4NDMwOSA0LjgxMTk1NSAtNC4wNTQ3OTVDNC41MzMwMDEgLTQuMDA0OTgxIDQuMzMzNzQ4IC0zLjc1NTkxNSA0LjMzMzc0OCAtMy41MTY4MTJDNC4zMzM3NDggLTMuMzU3NDEgNC40NDMzMzcgLTMuMTY4MTIgNC43MTIzMjkgLTMuMTY4MTJDNC45MzE1MDcgLTMuMTY4MTIgNS4yNTAzMTEgLTMuMzQ3NDQ3IDUuMjUwMzExIC0zLjc0NTk1M0M1LjI1MDMxMSAtNC4yNjQwMSA0LjY2MjUxNiAtNC40MDM0ODcgNC4zMjM3ODYgLTQuNDAzNDg3QzMuNzQ1OTUzIC00LjQwMzQ4NyAzLjM5NzI2IC0zLjg3NTQ2NyAzLjI3NzcwOSAtMy42NDYzMjZDMy4wMjg2NDMgLTQuMzAzODYxIDIuNDkwNjYgLTQuNDAzNDg3IDIuMjAxNzQzIC00LjQwMzQ4N0MxLjE2NTYyOSAtNC40MDM0ODcgMC41OTc3NTggLTMuMTE4MzA2IDAuNTk3NzU4IC0yLjg2OTI0QzAuNTk3NzU4IC0yLjc2OTYxNCAwLjY5NzM4NSAtMi43Njk2MTQgMC43MTczMSAtMi43Njk2MTRDMC43OTcwMTEgLTIuNzY5NjE0IDAuODI2ODk5IC0yLjc4OTUzOSAwLjg0NjgyNCAtMi44NzkyMDNDMS4xODU1NTQgLTMuOTM1MjQzIDEuODQzMDg4IC00LjE4NDMwOSAyLjE4MTgxOCAtNC4xODQzMDlDMi4zNzExMDggLTQuMTg0MzA5IDIuNzE5ODAxIC00LjA5NDY0NSAyLjcxOTgwMSAtMy41MTY4MTJDMi43MTk4MDEgLTMuMjA3OTcgMi41NTA0MzYgLTIuNTQwNDczIDIuMTgxODE4IC0xLjE0NTcwNEMyLjAyMjQxNiAtMC41MjgwMiAxLjY3MzcyNCAtMC4xMDk1ODkgMS4yMzUzNjcgLTAuMTA5NTg5QzEuMTc1NTkyIC0wLjEwOTU4OSAwLjk0NjQ1MSAtMC4xMDk1ODkgMC43MzcyMzUgLTAuMjM5MTAzQzAuOTg2MzAxIC0wLjI4ODkxNyAxLjIwNTQ3OSAtMC40OTgxMzIgMS4yMDU0NzkgLTAuNzc3MDg2QzEuMjA1NDc5IC0xLjA0NjA3NyAwLjk4NjMwMSAtMS4xMjU3NzggMC44MzY4NjIgLTEuMTI1Nzc4QzAuNTM3OTgzIC0xLjEyNTc3OCAwLjI4ODkxNyAtMC44NjY3NSAwLjI4ODkxNyAtMC41NDc5NDVDMC4yODg5MTcgLTAuMDg5NjY0IDAuNzg3MDQ5IDAuMTA5NTg5IDEuMjI1NDA1IDAuMTA5NTg5QzEuODgyOTM5IDAuMTA5NTg5IDIuMjQxNTk0IC0wLjU4Nzc5NiAyLjI3MTQ4MiAtMC42NDc1NzJDMi4zOTEwMzQgLTAuMjc4OTU0IDIuNzQ5Njg5IDAuMTA5NTg5IDMuMzQ3NDQ3IDAuMTA5NTg5QzQuMzczNTk5IDAuMTA5NTg5IDQuOTQxNDY5IC0xLjE3NTU5MiA0Ljk0MTQ2OSAtMS40MjQ2NThDNC45NDE0NjkgLTEuNTI0Mjg0IDQuODUxODA2IC0xLjUyNDI4NCA0LjgyMTkxOCAtMS41MjQyODRDNC43MzIyNTQgLTEuNTI0Mjg0IDQuNzEyMzI5IC0xLjQ4NDQzMyA0LjY5MjQwMyAtMS40MTQ2OTVDNC4zNjM2MzYgLTAuMzQ4NjkyIDMuNjg2MTc3IC0wLjEwOTU4OSAzLjM2NzM3MiAtMC4xMDk1ODlDMi45Nzg4MjkgLTAuMTA5NTg5IDIuODE5NDI3IC0wLjQyODM5NCAyLjgxOTQyNyAtMC43NjcxMjNDMi44MTk0MjcgLTAuOTg2MzAxIDIuODc5MjAzIC0xLjIwNTQ3OSAyLjk4ODc5MiAtMS42NDM4MzZMMy4zMjc1MjIgLTMuMDA4NzE3WicgaWQ9J2cwLTEyMCcvPgo8L2RlZnM+CjxnIGlkPSdwYWdlMSc+Cjx1c2UgeD0nMCcgeGxpbms6aHJlZj0nI2cwLTEyMCcgeT0nMCcvPgo8L2c+Cjwvc3ZnPg==" title="x"></span></div>

<div class="editline" tabindex="0" title="Click to edit this paragraph"></div>
<div class="readline" title="Click to mark this paragraph as read"></div>
</div>
            """)

        expected_json = self.create_plugin_json(
            d,
            't4',
            'Se0s8FDLbhOp',
            markup={"header": "hi",
                    "footer": "ho", }
        )
        self.assert_same_html(r.cssselect('.par')[2], f"""
<div class="par" id="Se0s8FDLbhOp" t="LTB4NGU3YzFkYWM=" attrs='{{"defaultplugin": "pali"}}'>
<div tabindex="0" class="parContent">
<tim-plugin-loader type="full" answer-id="" class="pluginpali inlineplugin" task-id="{d.id}.t4">
<span id="{d.id}.t4.Se0s8FDLbhOp" data-plugin="/pali">
<pali-runner json="{self.make_base64(expected_json)}"></pali-runner>
</span>
</tim-plugin-loader>
</div>
<div class="editline" tabindex="0" title="Click to edit this paragraph"></div>
<div class="readline" title="Click to mark this paragraph as read"></div>
</div>
                    """)

        a = self.post_answer_no_abdata(
            plugin_type='pali',
            task_id=f'{d.id}.t5',
            user_input={'userword': 'aaaaaa'},
            expect_status=400,
            expect_content='Task not found in the document: t5',
        )

    def test_inline_plugin_no_html_escape(self):
        self.login_test1()
        d = self.create_doc(initial_par="""
#- {defaultplugin=pali id=a3Xuyg1PF1l1}
{#t5 initword: #}
        """)
        r = self.get(d.url, as_tree=True)
        # Make sure Dumbo won't escape plugin's error HTML.
        self.assert_same_html(r.cssselect('.parContent')[0], f"""
<div tabindex="0" class="parContent">
    <tim-plugin-loader type="full" answer-id="" class="pluginpali inlineplugin" task-id="{d.id}.t5">
    <span id="{d.id}.t5.a3Xuyg1PF1l1" data-plugin="/pali">
        <div class="pluginError">
            The following fields have invalid values:
            <ul>
                <li>
                    initword: Field may not be null.
                </li>
            </ul>
        </div>
    </span>
    </tim-plugin-loader>
</div>
        """)

    def test_inline_plugin_sanitize(self):
        self.login_test1()
        d = self.create_doc(initial_par="""
#- {defaultplugin=pali}
<script>alert('hello')</script>
        """)
        r = self.get(d.url, as_tree=True)
        self.assertFalse(r.cssselect('.parContent script'))

    def test_multiline_inlineplugin(self):
        """Multiline markup in inlineplugins works."""
        self.login_test1()
        d = self.create_doc(initial_par="""
#- {defaultplugin=pali id=SSYigUyqdb7p}
{#t
initword: hi
#}
        """)
        r = self.get(d.url, as_tree=True)
        e = r.cssselect(f'.parContent > tim-plugin-loader[task-id="{d.id}.t"] > span > pali-runner')
        self.assertTrue(e)
        self.assert_plugin_json(
            e[0],
            self.create_plugin_json(
                d,
                't',
                par_id='SSYigUyqdb7p',
                markup={'initword': 'hi'},
            ),
        )

    def test_inline_plugin_error_html_no_p(self):
        self.login_test1()
        d = self.create_doc(initial_par="""
#- {defaultplugin="pali" id=a3Xuyg1PF1l1}
a {#x initword: #} b
        """)
        r = self.get(d.url, as_tree=True)
        self.assert_same_html(r.cssselect('.parContent')[0], f"""
<div tabindex="0" class="parContent">
    a
    <tim-plugin-loader type="full" answer-id="" class="pluginpali inlineplugin" task-id="{d.id}.x">
    <span id="{d.id}.x.a3Xuyg1PF1l1" data-plugin="/pali">
        <div class="pluginError">
            The following fields have invalid values:
            <ul>
                <li>
                    initword: Field may not be null.
                </li>
            </ul>
        </div>
    </span>
    </tim-plugin-loader>
    b
</div>""")

    def test_inline_plugin_ref(self):
        self.login_test1()
        d = self.create_doc(initial_par="""
#-  {defaultplugin=pali id=SSYigUyqdb7p}
{#t}
        """)
        d2 = self.create_doc()
        d2.document.add_paragraph_obj(d.document.get_paragraphs()[0].create_reference(d2.document))
        self.get(d2.url)

    def test_inline_plugin_login(self):
        self.login_test1()
        d = self.create_doc(initial_par="""
#- {defaultplugin=pali}
{#t#}
""")
        u = d.url
        grant_view_access(UserGroup.get_anonymous_group(), d)
        self.logout()
        r = self.get(u, as_tree=True).cssselect('.parContent login-menu')
        self.assertTrue(r)
