"""Tests for the embed mode (?embed=true) and the ?task= parameter of the document view."""
import json

from timApp.auth.accesstype import AccessType
from timApp.document.embed import (
    parse_embed_allowed_origins,
    find_par_id_by_task,
    get_embed_frame_ancestors,
)
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.timdb.sqa import db
from timApp.user.user import User
from timApp.util.flask.requesthelper import NotExist

ORIGINS = ["https://book.example.com", "https://other.example.com"]
CSP = "frame-ancestors 'self' https://book.example.com https://other.example.com"

TASK_DOC = """
# Intro

Some text before the task.

``` {#t1 plugin="textfield"}
header: First task
```

Text between.

``` {#t2 plugin="textfield"}
header: Second task
```
"""


def task_ids(tree) -> list[str | None]:
    """Returns the task ids of the rendered paragraphs in document order.

    Settings paragraphs are skipped: they are always included in a restricted view.
    """
    attrs = [json.loads(p.get("attrs") or "{}") for p in tree.cssselect("#pars .par")]
    return [a.get("taskId") for a in attrs if "settings" not in a]


class EmbedTest(TimRouteTest):
    def test_parse_allowed_origins(self):
        self.assertEqual([], parse_embed_allowed_origins(None))
        self.assertEqual([], parse_embed_allowed_origins([]))
        self.assertEqual([], parse_embed_allowed_origins(""))
        self.assertEqual(ORIGINS, parse_embed_allowed_origins(ORIGINS))
        self.assertEqual(
            ORIGINS,
            parse_embed_allowed_origins(
                "https://book.example.com, https://other.example.com/"
            ),
        )
        self.assertEqual(
            ORIGINS,
            parse_embed_allowed_origins(
                "https://book.example.com https://other.example.com https://book.example.com"
            ),
        )
        self.assertEqual(
            ["http://localhost:8080"],
            parse_embed_allowed_origins(["http://localhost:8080/"]),
        )
        with self.assertRaises(ValueError):
            parse_embed_allowed_origins(["book.example.com"])
        with self.assertRaises(ValueError):
            parse_embed_allowed_origins({"a": 1})

    def test_frame_ancestors_from_config(self):
        with self.temp_config({"EMBED_ALLOWED_ORIGINS": []}):
            self.assertEqual("frame-ancestors 'self'", get_embed_frame_ancestors())
        with self.temp_config({"EMBED_ALLOWED_ORIGINS": ORIGINS}):
            self.assertEqual(CSP, get_embed_frame_ancestors())
        with self.temp_config({"EMBED_ALLOWED_ORIGINS": ", ".join(ORIGINS)}):
            self.assertEqual(CSP, get_embed_frame_ancestors())

    def test_find_par_id_by_task(self):
        self.login_test1()
        d = self.create_doc(initial_par=TASK_DOC)
        pars = d.document.get_paragraphs()
        t1_par = next(p for p in pars if p.get_attr("taskId") == "t1")
        t2_par = next(p for p in pars if p.get_attr("taskId") == "t2")
        doc = d.document
        self.assertEqual(t1_par.get_id(), find_par_id_by_task(doc, "t1"))
        self.assertEqual(t2_par.get_id(), find_par_id_by_task(doc, "t2"))
        self.assertEqual(t1_par.get_id(), find_par_id_by_task(doc, f"{d.id}.t1"))
        with self.assertRaises(NotExist):
            find_par_id_by_task(doc, "missing")
        # Wrong document id
        with self.assertRaises(NotExist):
            find_par_id_by_task(doc, f"{d.id + 1}.t1")
        # Not a valid task id (only checked in embed mode, see test_task_param_without_embed)
        self.get(
            f"/view/{d.id}",
            query_string={"task": "a.b.c.d", "embed": True},
            expect_status=400,
        )

    def test_find_par_id_by_task_in_translation(self):
        """A task referenced from another document (e.g. a translation) is found via the referencing paragraph."""
        self.login_test1()
        d = self.create_doc(initial_par=TASK_DOC)
        tr = self.create_translation(d)
        tr_pars = tr.document.get_paragraphs()
        t2_src = next(
            p for p in d.document.get_paragraphs() if p.get_attr("taskId") == "t2"
        )
        tr_par = next(p for p in tr_pars if p.get_attr("rp") == t2_src.get_id())
        self.assertEqual(tr_par.get_id(), find_par_id_by_task(tr.document, "t2"))
        self.assertEqual(
            tr_par.get_id(), find_par_id_by_task(tr.document, f"{d.id}.t2")
        )
        with self.assertRaises(NotExist):
            find_par_id_by_task(tr.document, f"{tr.id}.t2")

    def test_task_param_renders_only_that_task(self):
        self.login_test1()
        d = self.create_doc(initial_par=TASK_DOC)
        tree = self.get(
            f"/view/{d.id}", as_tree=True, query_string={"task": "t2", "embed": True}
        )
        self.assertEqual(["t2"], task_ids(tree))
        self.assertNotIn("Some text before the task", tree.text_content())

        # Same result as b=<par_id>&size=1
        t2_par = next(
            p for p in d.document.get_paragraphs() if p.get_attr("taskId") == "t2"
        )
        tree2 = self.get(
            f"/view/{d.id}",
            as_tree=True,
            query_string={"b": t2_par.get_id(), "size": 1, "embed": True},
        )
        self.assertEqual(
            [p.get("id") for p in tree.cssselect("#pars .par")],
            [p.get("id") for p in tree2.cssselect("#pars .par")],
        )

        # Full task id with doc id
        tree = self.get(
            f"/view/{d.id}",
            as_tree=True,
            query_string={"task": f"{d.id}.t1", "embed": True},
        )
        self.assertEqual(["t1"], task_ids(tree))

        # Not found -> 404 with a clear message
        self.get(
            f"/view/{d.id}",
            query_string={"task": "nosuchtask", "embed": True},
            expect_status=404,
            expect_contains="Task 'nosuchtask' was not found in document",
        )
        # Wrong document
        self.get(
            f"/view/{d.id}",
            query_string={"task": f"{d.id + 1}.t1", "embed": True},
            expect_status=404,
        )
        # With b/e the range comes from them; task only selects the answer on the client
        # (the "only" link of the answer browser: answerNumber, task, user, b, size).
        tree = self.get(
            f"/view/{d.id}",
            as_tree=True,
            query_string={
                "answerNumber": 1,
                "task": "t1",
                "user": "testuser1",
                "b": t2_par.get_id(),
                "size": 1,
            },
        )
        self.assertEqual(["t2"], task_ids(tree))

    def test_task_param_without_embed(self):
        """Without embed=true, task does not restrict the view (answer links use it on the client)."""
        self.login_test1()
        d = self.create_doc(initial_par=TASK_DOC)
        for task in ("t2", "nosuchtask", "a.b.c.d"):
            tree = self.get(f"/view/{d.id}", as_tree=True, query_string={"task": task})
            # Text paragraphs are rendered too (None = paragraph without a task)
            self.assertEqual([None, "t1", None, "t2"], task_ids(tree))
            self.assertIn("Some text before the task", tree.text_content())
        # The plain answer link of the answer browser shows the whole document
        tree = self.get(
            f"/answers/{d.id}",
            as_tree=True,
            query_string={"answerNumber": 1, "task": "t2", "user": "testuser1"},
        )
        self.assertEqual([None, "t1", None, "t2"], task_ids(tree))

    def test_embed_mode(self):
        self.login_test1()
        d = self.create_doc(initial_par=TASK_DOC)
        d.document.add_setting("show_task_summary", True)
        # The summary is shown once the user has answered something
        self.post_answer("textfield", f"{d.id}.t1", user_input={"c": "x"})
        with self.temp_config({"EMBED_ALLOWED_ORIGINS": ORIGINS}):
            # Normal view: no CSP header, task summary shown, no embed globals
            r = self.get(f"/view/{d.id}", as_response=True)
            self.assertIsNone(r.headers.get("Content-Security-Policy"))
            html = r.get_data(as_text=True)
            self.assertIn("var embedMode = false;", html)
            self.assertIn('id="task-point-summary"', html)
            self.assertNotIn("embed.css", html)

            # pars_only alone must not change: no CSP, task summary still shown
            r = self.get(
                f"/view/{d.id}", as_response=True, query_string={"pars_only": True}
            )
            self.assertIsNone(r.headers.get("Content-Security-Policy"))
            html = r.get_data(as_text=True)
            self.assertIn("var parsOnly = true;", html)
            self.assertIn("var embedMode = false;", html)
            self.assertIn('id="task-point-summary"', html)

            # Embed mode
            r = self.get(
                f"/view/{d.id}",
                as_response=True,
                query_string={"task": "t1", "embed": True},
            )
            self.assertEqual(CSP, r.headers.get("Content-Security-Policy"))
            html = r.get_data(as_text=True)
            self.assertIn("var parsOnly = true;", html)
            self.assertIn("var embedMode = true;", html)
            self.assertIn(
                'var embedAllowedOrigins = ["https://book.example.com", "https://other.example.com"];',
                html,
            )
            self.assertIn("viewhide.css", html)
            self.assertIn("embed.css", html)
            self.assertNotIn('id="task-point-summary"', html)
            tree = self.get(
                f"/view/{d.id}",
                as_tree=True,
                query_string={"task": "t1", "embed": True},
            )
            self.assertEqual(["t1"], task_ids(tree))
            # Full-width layout: the content column has no offset and spans the frame
            self.assertEqual(
                0, len(tree.cssselect(".content-container.col-lg-offset-2"))
            )
            self.assertEqual(
                0, len(tree.cssselect(".content-container > .col-lg-offset-2"))
            )
            self.assertEqual(
                1,
                len(tree.cssselect(".content-container > .col-lg-12.col-lg-offset-0")),
            )

        # Empty allowed origins: only 'self'
        with self.temp_config({"EMBED_ALLOWED_ORIGINS": []}):
            r = self.get(
                f"/view/{d.id}", as_response=True, query_string={"embed": True}
            )
            self.assertEqual(
                "frame-ancestors 'self'", r.headers.get("Content-Security-Policy")
            )

    def test_embed_not_logged_in(self):
        self.login_test1()
        d = self.create_doc(initial_par=TASK_DOC)
        self.logout()
        with self.temp_config({"EMBED_ALLOWED_ORIGINS": ORIGINS}):
            # No access: compact login notice with 403, login link opens outside the frame
            r = self.get(
                f"/view/{d.id}",
                as_response=True,
                expect_status=403,
                query_string={"task": "t1", "embed": True},
            )
            self.assertEqual(CSP, r.headers.get("Content-Security-Policy"))
            html = r.get_data(as_text=True)
            self.assertIn("Log in to TIM to answer this task.", html)
            self.assertIn(f'href="/embed/login/{d.path}" target="_blank"', html)
            self.assertNotIn("<tim-root>", html)
            self.assertNotIn("tim-login-menu", html)

            # Anonymous view access: still the notice (answering requires a login), 200
            User.get_anon().grant_access(d, AccessType.view)
            db.session.commit()
            r = self.get(
                f"/view/{d.id}",
                as_response=True,
                query_string={"task": "t1", "embed": True},
            )
            self.assertEqual(CSP, r.headers.get("Content-Security-Policy"))
            self.assertIn(
                "Log in to TIM to answer this task.", r.get_data(as_text=True)
            )

            # Non-embed anonymous view is unchanged
            r = self.get(f"/view/{d.id}", as_response=True)
            self.assertIsNone(r.headers.get("Content-Security-Policy"))
            self.assertNotIn("Log in to TIM to answer", r.get_data(as_text=True))

    def test_embed_login_page(self):
        self.login_test1()
        d = self.create_doc(initial_par=TASK_DOC)
        self.logout()

        # Logged out: the normal TIM login page (not the embed notice, not the document)
        r = self.get(f"/embed/login/{d.path}", as_response=True, expect_status=403)
        html = r.get_data(as_text=True)
        self.assertIn("<tim-root>", html)
        self.assertIn("requires_login = true", html)
        self.assertNotIn("Log in to TIM to answer this task.", html)
        self.assertNotIn('class="par"', html)
        with self.client.session_transaction() as s:
            self.assertEqual(f"http://localhost/embed/login/{d.path}", s["came_from"])

        self.get("/embed/login/no/such/document", expect_status=404)

        # Logged in (the login reloads the page): only the "close this tab" page
        self.login_test1()
        r = self.get(f"/embed/login/{d.path}", as_response=True)
        html = r.get_data(as_text=True)
        self.assertIn("You are now logged in to TIM.", html)
        self.assertIn("You can close this tab", html)
        self.assertIn('new BroadcastChannel("tim-embed-login").postMessage("reload")', html)
        self.assertNotIn("<tim-root>", html)
        self.assertNotIn('class="par"', html)
        self.assertEqual("no-store, must-revalidate", r.headers.get("Cache-Control"))

    def test_embed_does_not_save_last_page(self):
        """An embedded frame must not become the page the user returns to after logging in."""
        self.login_test1()
        d = self.create_doc(initial_par=TASK_DOC)
        self.get(f"/view/{d.path}")
        with self.client.session_transaction() as s:
            self.assertEqual(f"/view/{d.path}?", s["last_doc"])
        self.get(f"/view/{d.path}", query_string={"task": "t1", "embed": True})
        with self.client.session_transaction() as s:
            self.assertEqual(f"/view/{d.path}?", s["last_doc"])
        self.logout()
        self.get(
            f"/view/{d.path}",
            query_string={"task": "t1", "embed": True},
            expect_status=403,
        )
        with self.client.session_transaction() as s:
            self.assertNotIn("embed", s.get("last_doc", ""))

    def test_embed_no_access(self):
        self.login_test1()
        d = self.create_doc(initial_par=TASK_DOC)
        self.login_test2()
        with self.temp_config({"EMBED_ALLOWED_ORIGINS": ORIGINS}):
            # Logged in without access: compact notice that names the user, link opens TIM outside the frame
            r = self.get(
                f"/view/{d.id}",
                as_response=True,
                expect_status=403,
                query_string={"task": "t1", "embed": True},
            )
            self.assertEqual(CSP, r.headers.get("Content-Security-Policy"))
            html = r.get_data(as_text=True)
            self.assertIn("You do not have permission to view this task.", html)
            self.assertIn("You are logged in as testuser2.", html)
            self.assertIn(f'href="/view/{d.path}" target="_blank"', html)
            self.assertNotIn("<tim-root>", html)

            # Non-embed view is unchanged
            r = self.get(f"/view/{d.id}", as_response=True, expect_status=403)
            self.assertIsNone(r.headers.get("Content-Security-Policy"))

    def test_embed_error_is_compact(self):
        self.login_test1()
        d = self.create_doc(initial_par=TASK_DOC)
        with self.temp_config({"EMBED_ALLOWED_ORIGINS": ORIGINS}):
            # An error page in the frame is the compact notice, not the full TIM error page
            r = self.get(
                f"/view/{d.id}",
                as_response=True,
                expect_status=404,
                query_string={"task": "nosuchtask", "embed": True},
                headers=[("Accept", "text/html")],
            )
            self.assertEqual(CSP, r.headers.get("Content-Security-Policy"))
            html = r.get_data(as_text=True)
            self.assertIn("was not found in document", html)
            self.assertIn(f'href="/view/{d.id}" target="_blank"', html)
            self.assertNotIn("<tim-root>", html)

            # Without embed=true the full error page is used
            r = self.get(
                "/view/no/such/document",
                as_response=True,
                expect_status=404,
                headers=[("Accept", "text/html")],
            )
            self.assertIn("<tim-root>", r.get_data(as_text=True))
