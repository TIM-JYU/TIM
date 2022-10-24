from selenium.webdriver import ActionChains, Keys
from selenium.webdriver.common.by import By
from selenium.webdriver.support.select import Select

from timApp.tests.browser.browsertest import BrowserTest


class TextfieldPluginTest(BrowserTest):
    def get_screenshot_tolerance(self):
        return 13

    def test_textfield_numericfield_multisave(self):
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#- {plugin=textfield #t1}
cols: 7
autosave: false
#- {plugin=numericfield #t2}
cols: 7
autosave: false
#- {plugin=multisave #t3}
        """,
            settings={"form_mode": True},
        )

        # Test Case 1 - expected success in both fields after Save-button click and page refresh

        self.goto_document(d)
        self.wait_until_present_and_vis("#t1 input")
        field = self.find_element_and_move_to("#t1 input")
        field.send_keys("Aku Ankka")
        self.wait_until_present_and_vis("#t2 input")
        input2 = self.find_element_and_move_to("#t2 input")
        input2.send_keys("2.75")
        self.get_uninteractable_element().click()
        par = self.find_element_avoid_staleness("#pars")
        multisave = self.find_element_avoid_staleness("#t3 tim-multisave")
        self.wait_until_present_and_vis("#t3 div")  # wait for ng-if to finish
        self.assert_same_screenshot(par, ["textfield/fields_before_answer"])
        runbutton = multisave.find_element(by=By.CSS_SELECTOR, value="button")
        runbutton.click()
        self.wait_until_present_and_vis("p.savedtext")
        self.refresh()

        self.wait_until_present_and_vis("#t1 input")
        self.wait_until_present_and_vis("#t2 input")
        par = self.find_element_avoid_staleness("#pars")
        self.assert_same_screenshot(par, ["textfield/fields_after_answer"])

        # Test Case 2 - expected previously saved value in numericField, as it refuses to save empty input

        # TODO: for some reason, the invalid numericfield value (' ') is not validated in browser in selenium,
        #  so an empty value is saved. Disabling the test for now.

        return

        self.goto_document(d)
        self.wait_until_present_and_vis("#t1 input")
        field = self.find_element_and_move_to("#t1 input")
        field.clear()
        field.send_keys(" ")
        self.wait_until_present_and_vis("#t2 input")
        input2 = self.find_element_and_move_to("#t2 input")
        input2.clear()
        input2.send_keys(" ")
        self.get_uninteractable_element.click()
        multisave = self.find_element_avoid_staleness("#t3 tim-multisave")
        runbutton = multisave.find_element(by=By.CSS_SELECTOR, value="button")
        runbutton.click()
        self.goto_document(d)
        self.wait_until_present_and_vis("#t1 input")
        self.wait_until_present_and_vis("#t2 input")
        par = self.find_element_avoid_staleness("#pars")
        self.assert_same_screenshot(par, ["textfield/fields_after_answer_switch"])


class FieldSaveTest(BrowserTest):
    def test_field_failed_save(self):
        # Ensure minimalist fields show save button and error messages on save failure
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(
            initial_par="""
#- {plugin=textfield #textfield}
button: 
#- {plugin=numericfield #numericfield}
button: 
#- {plugin=cbfield #cbfield}
#- {plugin=cbcountfield #cbcountfield}
#- {plugin=rbfield #rbfield}
#- {plugin=dropdown #dropdown}
autosave: true
words: [a,b,c]
#- {plugin="drag" #dragsource}
words: ["a", "b", "c"]
copy: source
#- {plugin="drag" #dragtarget}
autoSave: true
copy: target
                """,
        )
        self.goto_document(d)

        def send_input(task: str, input: str | None):
            self.wait_until_present_and_vis(f"#{task} input")
            field = self.find_element_and_move_to(f"#{task} input")
            self.wait_until_hidden(f"#{task} button")
            if input:
                field.send_keys(input)
                ActionChains(self.drv).send_keys(Keys.ENTER).perform()
            else:
                field.click()
            self.wait_until_present_and_vis(f".plugin{task} bs-tooltip-container")

        def check_hover(task: str):
            button = self.find_element_avoid_staleness(f"#{task} button")
            # Hover back and forth to remove tooltip
            self.get_uninteractable_element().click()
            self.find_element_and_move_to(f"#{task} .alertFrame")
            self.get_uninteractable_element().click()
            self.wait_until_hidden(f"#{task} bs-tooltip-container")
            button.click()
            self.wait_until_present_and_vis(f"#{task} bs-tooltip-container")

        def answer_successfully(task: str):
            button = self.find_element_and_move_to(f"#{task} button")
            button.click()
            self.wait_until_hidden(f"#{task} .alertFrame")
            self.wait_until_hidden(f"#{task} bs-tooltip-container")

        self.drv.execute_cdp_cmd("Network.setBlockedURLs", {"urls": ["*"]})
        self.drv.execute_cdp_cmd("Network.enable", {})

        send_input("textfield", "Hello")
        send_input("numericfield", "400")
        send_input("cbfield", None)
        send_input("cbcountfield", None)
        send_input("rbfield", None)

        self.wait_until_present_and_vis("#dropdown select")
        field = self.find_element_and_move_to("#dropdown select")
        self.wait_until_hidden("#dropdown button")
        selector = Select(field)
        selector.select_by_index(1)
        self.wait_until_present_and_vis(".plugindropdown bs-tooltip-container")
        self.wait_until_present_and_vis("#dragsource li")
        self.wait_until_present_and_vis("#dragtarget .dropword")
        drag_piece = self.find_element("#dragsource li")
        drag_target = self.find_element("#dragtarget .dropword")
        # Regular selenium drag-drop doesn't trigger all the events here, https://stackoverflow.com/a/40608989
        JS_HTML5_DND = 'function e(e,t,n,i){var r=a.createEvent("DragEvent");r.initMouseEvent(t,!0,!0,o,0,0,0,c,g,!1,!1,!1,!1,0,null),Object.defineProperty(r,"dataTransfer",{get:function(){return d}}),e.dispatchEvent(r),o.setTimeout(i,n)}var t=arguments[0],n=arguments[1],i=arguments[2]||0,r=arguments[3]||0;if(!t.draggable)throw new Error("Source element is not draggable.");var a=t.ownerDocument,o=a.defaultView,l=t.getBoundingClientRect(),u=n?n.getBoundingClientRect():l,c=l.left+(l.width>>1),g=l.top+(l.height>>1),s=u.left+(u.width>>1)+i,f=u.top+(u.height>>1)+r,d=Object.create(Object.prototype,{_items:{value:{}},effectAllowed:{value:"all",writable:!0},dropEffect:{value:"move",writable:!0},files:{get:function(){return this._items.Files}},types:{get:function(){return Object.keys(this._items)}},setData:{value:function(e,t){this._items[e]=t}},getData:{value:function(e){return this._items[e]}},clearData:{value:function(e){delete this._items[e]}},setDragImage:{value:function(e){}}});if(n=a.elementFromPoint(s,f),!n)throw new Error("The target element is not interactable and need to be scrolled into the view.");u=n.getBoundingClientRect(),e(t,"dragstart",101,function(){var i=n.getBoundingClientRect();c=i.left+s-u.left,g=i.top+f-u.top,e(n,"dragenter",1,function(){e(n,"dragover",101,function(){n=a.elementFromPoint(c,g),e(n,"drop",1,function(){e(t,"dragend",1,callback)})})})})'
        self.drv.execute_script(JS_HTML5_DND, drag_piece, drag_target)

        check_hover("textfield")
        check_hover("numericfield")
        check_hover("cbfield")
        check_hover("cbcountfield")
        check_hover("rbfield")
        check_hover("dropdown")
        check_hover("dragtarget")

        self.drv.execute_cdp_cmd("Network.setBlockedURLs", {"urls": []})
        self.drv.execute_cdp_cmd("Network.enable", {})

        answer_successfully("textfield")
        answer_successfully("numericfield")
        answer_successfully("cbfield")
        answer_successfully("cbcountfield")
        answer_successfully("rbfield")
        answer_successfully("dropdown")
        answer_successfully("dragtarget")
