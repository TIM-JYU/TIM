"""Browser tests for answer tracking:

- imagex `track`: drags recorded per session, unsaved moves survive a reload via
  localStorage, save markers, `init` positions.
- csplugin `track`: typing / paste / run recorded in the log file and the answer summary,
  in both the textarea and the ACE editor.
"""

import json
import time
from pathlib import Path

from selenium.webdriver import ActionChains, Keys
from selenium.webdriver.remote.webelement import WebElement

from timApp.tests.browser.browsertest import BrowserTest

LOG_NAME = "test_edit_track_browser.log"

IMAGEX_DOC = """
``` {#t plugin="imagex"}
track: true
canvaswidth: 400
canvasheight: 300
objects:
  - id: apple
    type: rectangle
    position: [50, 50]
    size: [60, 60]
    color: red
targets:
  - id: box
    type: rectangle
    position: [300, 200]
    size: [80, 80]
    points:
      apple: 1
```
"""


class SavedAnswer:
    def __init__(self, a: dict):
        self.content_as_json = json.loads(a["content"])


class TrackingBrowserTestBase(BrowserTest):
    def wait_for_answers(
        self, task_id: str, count: int, timeout=15
    ) -> list[SavedAnswer]:
        """Polls the user's answers (newest first) through the browser.

        Reading them via the live server avoids querying the database from the test
        thread while the server thread is handling the save request.
        """
        end = time.time() + timeout
        while True:
            answers = self.drv.execute_async_script(
                """
                const done = arguments[arguments.length - 1];
                fetch('/getAnswers/' + arguments[0] + '/' + arguments[1])
                    .then(r => r.json()).then(done).catch(() => done([]));
                """,
                task_id,
                self.test_user_1_id,
            )
            if len(answers) >= count or time.time() > end:
                self.assertEqual(count, len(answers), "unexpected answer count")
                return [SavedAnswer(a) for a in answers]
            time.sleep(0.5)

    @property
    def test_user_1_id(self) -> int:
        return self.drv.execute_script("return current_user.id")

    def local_storage_keys(self, prefix: str) -> list[str]:
        return self.drv.execute_script(
            "return Object.keys(window.localStorage).filter(k => k.startsWith(arguments[0]));",
            prefix,
        )

    def clear_local_storage(self):
        self.drv.execute_script("window.localStorage.clear();")


class ImagexTrackingTest(TrackingBrowserTestBase):
    def canvas(self) -> WebElement:
        # Both canvases have the same size and position; events go to the topmost one.
        return self.find_element_avoid_staleness("imagex-runner canvas")

    def drag(self, from_xy: tuple[int, int], to_xy: tuple[int, int]):
        """Drags on the canvas between canvas coordinates (offsets are from the element center)."""
        c = self.canvas()
        w, h = c.size["width"], c.size["height"]
        ActionChains(self.drv).move_to_element_with_offset(
            c, from_xy[0] - w // 2, from_xy[1] - h // 2
        ).click_and_hold().move_by_offset(10, 10).move_to_element_with_offset(
            c, to_xy[0] - w // 2, to_xy[1] - h // 2
        ).release().perform()
        time.sleep(0.3)

    def click_save(self):
        self.find_element(
            xpath="//imagex-runner//button[normalize-space()='Save']"
        ).click()

    def test_imagex_track_reload_and_save(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par=IMAGEX_DOC)
        task_id = f"{d.id}.t"
        prefix = f"imagexTrack_{self.test_user_1.id}_{task_id}_"

        self.goto_document(d)
        self.canvas()
        self.clear_local_storage()
        self.refresh()
        self.canvas()
        time.sleep(1)

        # 1. Drag and save -> session 1 with one drag and a save marker
        self.drag((90, 90), (150, 100))
        self.assertEqual(1, len(self.local_storage_keys(prefix)))
        self.click_save()
        answers = self.wait_for_answers(task_id, 1)
        track = answers[0].content_as_json["track"]
        self.assertEqual(1, len(track))
        s1 = track[0]
        self.assertEqual([], s1["init"])
        self.assertEqual(2, len(s1["e"]))
        drag1, save1 = s1["e"]
        self.assertEqual("apple", drag1[0])
        self.assertEqual({"type": "save"}, {"type": save1["type"]})
        saved_pos = (drag1[1], drag1[2])
        self.assertEqual([], self.local_storage_keys(prefix))

        # 2. Unsaved drag, then reload: the drag must survive via localStorage
        # The grabbed point follows the pointer, so (150, 100) is on the object.
        self.drag((150, 100), (250, 150))
        self.assertEqual(1, len(self.local_storage_keys(prefix)))
        self.refresh()
        self.canvas()
        time.sleep(1)

        # 3. New page load: drag and save -> answer has both sessions
        self.drag((150, 100), (120, 220))
        self.click_save()
        answers = self.wait_for_answers(task_id, 2)
        track = answers[0].content_as_json["track"]
        self.assertEqual(2, len(track))
        s1, s2 = track  # sorted by start time
        self.assertNotEqual(s1["id"], s2["id"])
        # Session 1: drag, save, unsaved drag (recovered from localStorage)
        self.assertEqual(3, len(s1["e"]))
        self.assertEqual("apple", s1["e"][2][0])
        # Session 2 started from the saved board, not from the unsaved position
        self.assertEqual(1, len(s2["init"]))
        init_id, init_x, init_y = s2["init"][0]
        self.assertEqual("apple", init_id)
        self.assertAlmostEqual(saved_pos[0], init_x, delta=1)
        self.assertAlmostEqual(saved_pos[1], init_y, delta=1)
        self.assertEqual(2, len(s2["e"]))
        self.assertEqual("save", s2["e"][1]["type"])

        # Timing: dt values are non-negative numbers with 0.1 s precision
        for s in track:
            for e in s["e"]:
                dt = e[3] if isinstance(e, list) else e["dt"]
                self.assertGreaterEqual(dt, 0)
                self.assertAlmostEqual(dt * 10, round(dt * 10), places=6)

        self.assertEqual([], self.local_storage_keys(prefix))

    def test_imagex_track_reset_is_mass_move(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par=IMAGEX_DOC)
        task_id = f"{d.id}.t"
        self.goto_document(d)
        self.canvas()
        time.sleep(1)
        self.drag((90, 90), (150, 100))
        self.find_element(xpath="//imagex-runner//a[normalize-space()='Reset']").click()
        time.sleep(0.3)
        self.click_save()
        answers = self.wait_for_answers(task_id, 1)
        events = answers[0].content_as_json["track"][0]["e"]
        self.assertEqual("apple", events[0][0])
        self.assertEqual("reset", events[1]["type"])
        self.assertEqual([["apple", 50, 50]], events[1]["moves"])
        self.assertEqual("save", events[2]["type"])

    def test_imagex_unchanged_save_creates_no_new_answer(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par=IMAGEX_DOC)
        task_id = f"{d.id}.t"
        prefix = f"imagexTrack_{self.test_user_1.id}_{task_id}_"
        self.goto_document(d)
        self.canvas()
        self.clear_local_storage()
        time.sleep(1)
        self.drag((90, 90), (150, 100))
        self.click_save()
        self.wait_for_answers(task_id, 1)
        # Save again without changes: no new answer, the save marker isn't kept
        self.click_save()
        time.sleep(2)
        answers = self.wait_for_answers(task_id, 1)
        self.assertEqual(
            ["apple", "save"],
            [
                e[0] if isinstance(e, list) else e["type"]
                for e in answers[0].content_as_json["track"][0]["e"]
            ],
        )
        # A real change is saved, with the log continuing from the first save
        self.drag((150, 100), (250, 150))
        self.click_save()
        answers = self.wait_for_answers(task_id, 2)
        self.assertEqual(
            ["apple", "save", "apple", "save"],
            [
                e[0] if isinstance(e, list) else e["type"]
                for e in answers[0].content_as_json["track"][0]["e"]
            ],
        )
        self.assertEqual([], self.local_storage_keys(prefix))

    def test_imagex_no_tracking_when_disabled(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par=IMAGEX_DOC.replace("track: true", ""))
        task_id = f"{d.id}.t"
        self.goto_document(d)
        self.canvas()
        time.sleep(1)
        self.drag((90, 90), (150, 100))
        self.assertEqual([], self.local_storage_keys("imagexTrack_"))
        self.click_save()
        answers = self.wait_for_answers(task_id, 1)
        self.assertNotIn("track", answers[0].content_as_json)


CS_DOC = """
``` {{#t plugin="csPlugin"}}
type: text
button: Save
track: true
editorMode: {mode}
editorModes: "01"
```
"""


class CsPluginTrackingTest(TrackingBrowserTestBase):
    def setUp(self):
        super().setUp()
        self.log_path = Path(self.app.config["FILES_PATH"]) / LOG_NAME
        self.log_path.unlink(missing_ok=True)

    def tearDown(self):
        self.log_path.unlink(missing_ok=True)
        super().tearDown()

    def read_log(self) -> list[dict]:
        if not self.log_path.exists():
            return []
        return [json.loads(l) for l in self.log_path.read_text().splitlines()]

    def wait_for_log_event(self, event_type: str, timeout=15) -> list[list]:
        end = time.time() + timeout
        while True:
            events = [e for line in self.read_log() for e in line["ev"]]
            if any(e[0] == event_type for e in events) or time.time() > end:
                self.assertIn(event_type, [e[0] for e in events])
                return events
            time.sleep(0.5)

    def dispatch_paste(self, selector: str, text: str):
        """Fires a paste event like the browser does (clipboard access isn't available in tests)."""
        self.drv.execute_script(
            """
            const el = document.querySelector(arguments[0]);
            const dt = new DataTransfer();
            dt.setData('text', arguments[1]);
            el.dispatchEvent(new ClipboardEvent('paste', {clipboardData: dt, bubbles: true}));
            """,
            selector,
            text,
        )

    def click_save(self):
        self.find_element(xpath="//cs-runner//button[normalize-space()='Save']").click()

    def run_case(self, mode: int, editor_css: str, typing_css: str):
        with self.temp_config({"EDIT_TRACK_LOG_FILE": LOG_NAME}):
            self.login_browser_quick_test1()
            self.login_test1()
            d = self.create_doc(initial_par=CS_DOC.format(mode=mode))
            task_id = f"{d.id}.t"
            self.goto_document(d)
            editor = self.find_element_avoid_staleness(editor_css)
            time.sleep(1)

            # Typing (click to focus; ACE types through a hidden textarea)
            editor.click()
            ActionChains(self.drv).send_keys("hello world").perform()
            time.sleep(0.3)
            # Paste (event only; the paste listener records it before the browser inserts text)
            self.dispatch_paste(typing_css, "pasted text 123")
            # Leave and come back to the page
            self.drv.execute_script(
                "window.dispatchEvent(new Event('blur')); window.dispatchEvent(new Event('focus'));"
            )
            self.click_save()

            answers = self.wait_for_answers(task_id, 1)
            summaries = answers[0].content_as_json["trackSummary"]
            self.assertEqual(1, len(summaries))
            s = next(iter(summaries.values()))
            self.assertGreaterEqual(s["typed"], len("hello world"))
            self.assertEqual(1, s["paste"])
            self.assertEqual(len("pasted text 123"), s["pasteChars"])
            self.assertEqual(1, s["away"])
            self.assertIsNotNone(s.get("firstEdit"))

            events = self.wait_for_log_event("run")
            types = [e[0] for e in events]
            self.assertEqual("init", types[0])
            self.assertIn("k", types)
            self.assertIn("p", types)
            self.assertIn("away", types)
            self.assertIn("back", types)
            typed = sum(e[2] for e in events if e[0] == "k") + sum(
                e[2] for e in events if e[0] == "big"
            )
            self.assertGreaterEqual(typed, len("hello world"))
            # The pasted text itself is never logged
            self.assertNotIn("pasted text 123", self.log_path.read_text())
            lines = self.read_log()
            self.assertEqual(task_id, lines[0]["task"])
            self.assertEqual(list(range(len(lines))), sorted(l["seq"] for l in lines))
            return editor

    def test_textarea_tracking(self):
        self.run_case(0, "cs-normal-editor textarea", "cs-normal-editor textarea")

    def test_ace_tracking(self):
        self.run_case(1, "cs-ace-editor .ace_editor", "cs-ace-editor textarea")

    def test_reset_and_load_not_counted_as_typing(self):
        with self.temp_config({"EDIT_TRACK_LOG_FILE": LOG_NAME}):
            self.login_browser_quick_test1()
            self.login_test1()
            d = self.create_doc(
                initial_par=CS_DOC.format(mode=1).replace(
                    "track: true", "track: true\nbyCode: |!!\nsome initial code\n!!"
                )
            )
            self.goto_document(d)
            self.find_element_avoid_staleness("cs-ace-editor .ace_editor")
            time.sleep(1)
            self.click_save()
            events = self.wait_for_log_event("run")
            self.assertNotIn("k", [e[0] for e in events])
            self.assertNotIn("big", [e[0] for e in events])
            init = events[0]
            self.assertEqual("init", init[0])
            self.assertEqual(len("some initial code\n"), init[2])

    def open_doc(self, markup: str, editor_css: str):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par=markup)
        self.goto_document(d)
        editor = self.find_element_avoid_staleness(editor_css)
        time.sleep(1)
        return d, editor

    def test_ace_paste_with_crlf_counted_once(self):
        with self.temp_config({"EDIT_TRACK_LOG_FILE": LOG_NAME}):
            _, editor = self.open_doc(
                CS_DOC.format(mode=1), "cs-ace-editor .ace_editor"
            )
            editor.click()
            text = "def f(x):\r\n    return x * 2\r\nprint(f(21))"
            self.dispatch_paste("cs-ace-editor textarea", text)
            time.sleep(0.3)
            self.click_save()
            events = self.wait_for_log_event("run")
            pastes = [e for e in events if e[0] == "p"]
            self.assertEqual(1, len(pastes))
            self.assertEqual(len(text.replace("\r\n", "\n")), pastes[0][2])
            self.assertNotIn("big", [e[0] for e in events])

    def test_ace_undo_redo_not_counted_as_typing(self):
        with self.temp_config({"EDIT_TRACK_LOG_FILE": LOG_NAME}):
            d, editor = self.open_doc(
                CS_DOC.format(mode=1), "cs-ace-editor .ace_editor"
            )
            editor.click()
            pasted = "a pasted line that is long enough"
            self.dispatch_paste("cs-ace-editor textarea", pasted)
            time.sleep(0.3)
            undo = (
                ActionChains(self.drv)
                .key_down(Keys.CONTROL)
                .send_keys("z")
                .key_up(Keys.CONTROL)
            )
            undo.perform()
            time.sleep(0.3)
            ActionChains(self.drv).key_down(Keys.CONTROL).key_down(
                Keys.SHIFT
            ).send_keys("z").key_up(Keys.SHIFT).key_up(Keys.CONTROL).perform()
            time.sleep(0.3)
            self.click_save()
            events = self.wait_for_log_event("run")
            types = [e[0] for e in events]
            self.assertEqual(["init", "p", "u", "r", "run"], types)
            self.assertEqual([0, len(pasted)], events[2][2:])
            self.assertEqual([len(pasted), 0], events[3][2:])
            s = next(
                iter(
                    self.wait_for_answers(f"{d.id}.t", 1)[0]
                    .content_as_json["trackSummary"]
                    .values()
                )
            )
            self.assertEqual(
                (0, 0, 0, 2), (s["typed"], s["deleted"], s["big"], s["undo"])
            )

    def test_undo_link_is_tracked(self):
        with self.temp_config({"EDIT_TRACK_LOG_FILE": LOG_NAME}):
            markup = CS_DOC.format(mode=0).replace(
                "track: true", "track: true\nundo:\n  button: Undo"
            )
            d, editor = self.open_doc(markup, "cs-normal-editor textarea")
            editor.send_keys("abc")
            self.click_save()
            self.wait_for_answers(f"{d.id}.t", 1)
            editor.send_keys("defgh")
            time.sleep(0.3)
            self.find_element(xpath="//cs-runner//a[contains(., 'Undo')]").click()
            time.sleep(0.3)
            self.assertEqual("abc", editor.get_attribute("value"))
            editor.send_keys("x")
            self.click_save()
            answers = self.wait_for_answers(f"{d.id}.t", 2)
            reverts = [e for e in self.wait_for_log_event("revert") if e[0] == "revert"]
            self.assertEqual([[len("abcdefgh"), len("abc")]], [e[2:] for e in reverts])
            s = next(iter(answers[0].content_as_json["trackSummary"].values()))
            self.assertEqual(1, s["undo"])

    def test_no_tracking_when_disabled(self):
        with self.temp_config({"EDIT_TRACK_LOG_FILE": LOG_NAME}):
            self.login_browser_quick_test1()
            self.login_test1()
            d = self.create_doc(
                initial_par=CS_DOC.format(mode=0).replace("track: true", "")
            )
            task_id = f"{d.id}.t"
            self.goto_document(d)
            self.find_element_avoid_staleness("cs-normal-editor textarea").send_keys(
                "abc"
            )
            self.click_save()
            answers = self.wait_for_answers(task_id, 1)
            self.assertNotIn("trackSummary", answers[0].content_as_json)
            time.sleep(2)
            self.assertEqual([], self.read_log())
