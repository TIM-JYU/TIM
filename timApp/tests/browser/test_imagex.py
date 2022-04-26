from selenium.webdriver import ActionChains
from selenium.webdriver.common.by import By

from timApp.answer.answers import save_answer
from timApp.plugin.taskid import TaskId
from timApp.tests.browser.browsertest import BrowserTest
from timApp.timdb.sqa import db


class ImagexTest(BrowserTest):
    def test_imagex_freehand_legacy(self):
        """Test imagex drawing and legacy freehand loading"""

        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(
            initial_par="""
``` {plugin="imagex" #Plugin1}
header: Piirrä tähän mitä tarvitsee
canvasheight: 420
freeHand: true
freeHandColor: red
freeHandWidth: 3
fixedobjects:
  -
      type: "vector"
      cornerradius: 0
      position: [50, 400]
      size: [400, 2]
      vectorproperties:
        color: "black"
        arrowheadwidth: 20
        arrowheadlength: 20
  -
      a: 90
      position: [50, 400]
      size: [400, 2]
```

    """
        )
        # directly input a legacy formatted drawing
        save_answer(
            [self.test_user_1],
            TaskId.parse(f"{d.id}.Plugin1"),
            content={
                "userAnswer": {"drags": []},
                "freeHandData": [
                    {"lines": [[229, 236], [368, 191]], "color": "red", "w": 3},
                    {
                        "lines": [[304, 161], [304, 162], [305, 167], [305, 169]],
                        "color": "red",
                        "w": 3,
                    },
                    {
                        "lines": [
                            [522, 149],
                            [521, 150],
                            [521, 172],
                            [521, 179],
                            [521, 190],
                            [521, 197],
                            [521, 205],
                        ],
                        "color": "#00f",
                        "w": 5,
                    },
                ],
            },
            points=0,
        )
        db.session.commit()
        self.goto_document(d)
        canvas = self.find_element_avoid_staleness("imagex-runner canvas")
        # legacy format loads correctly
        self.assert_same_screenshot(canvas, ["imagex/canvas_legacy_init"])
        ActionChains(self.drv).drag_and_drop_by_offset(canvas, 50, 100).perform()
        # drawing does not mess with dragTask objects
        self.assert_same_screenshot(canvas, ["imagex/canvas_draw"])
        save = self.drv.find_elements(By.CSS_SELECTOR, "imagex-runner button")[0]
        save.click()
        self.wait_until_present_and_vis("imagex-runner pre")
        self.refresh()
        canvas = self.find_element_avoid_staleness("imagex-runner canvas")
        # new format loads correctly
        self.assert_same_screenshot(canvas, ["imagex/canvas_modern_init"])
        ans = self.get_task_answers(f"{d.id}.Plugin1", self.test_user_1)
        # new saves are made in new format
        self.assertEqual(
            """{"userAnswer": {"drags": []}, "drawings": [{"type": "freehand", "drawData": {"w": 3, "lines": [[229, 236], [368, 191]], "color": "red"}}, {"type": "freehand", "drawData": {"w": 3, "lines": [[304, 161], [304, 162], [305, 167], [305, 169]], "color": "red"}}, {"type": "freehand", "drawData": {"w": 5, "lines": [[522, 149], [521, 150], [521, 172], [521, 179], [521, 190], [521, 197], [521, 205]], "color": "#00f"}}, {"type": "freehand", "drawData": {"lines": [[400, 209], [450, 309]], "color": "red", "w": 3}}]}""",
            ans[0]["content"],
        )
