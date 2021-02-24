from selenium.webdriver import ActionChains
from selenium.webdriver.remote.webelement import WebElement
from selenium.webdriver.support.select import Select

from timApp.tests.browser.browsertest import BrowserTest, find_button_by_text


class VelpTest(BrowserTest):
    def test_velps(self):
        """Ensures:

        * Velp selection component is visible in /velp route
        * New velp can be created
        * The created velp appears in the list of velps
        """
        self.login_browser_quick_test1()
        self.login_test1()
        d = self.create_doc(initial_par='This is a velp test.')
        self.goto_document(d, view='velp')

        velp_selection_element = self.drv.find_element_by_css_selector('#velpSelection')
        self.assert_same_screenshot(velp_selection_element, ['velps/velp_selection_empty'])
        create_velp_btn = find_button_by_text(velp_selection_element, 'Create new velp')
        create_velp_btn.click()
        new_velp_selector = '.velp-data.new.edit'
        new_velp_element: WebElement = velp_selection_element.find_element_by_css_selector(new_velp_selector)
        self.assert_same_screenshot(new_velp_element, 'velps/create_new_velp_empty')

        velp_content_input: WebElement = new_velp_element.find_element_by_css_selector('input[ng-model="$ctrl.velp.content"]')
        velp_content_input.send_keys('first velp')

        velp_points_input: WebElement = new_velp_element.find_element_by_css_selector('input[ng-model="$ctrl.velp.points"]')
        velp_points_input.send_keys('2')

        velp_comment_input: WebElement = new_velp_element.find_element_by_css_selector(
            'textarea[ng-model="$ctrl.velp.default_comment"]')
        velp_comment_input.send_keys('Just a default comment.')

        # Setting the color does not work well because SystemJS does not find the module for some reason.
        # velp_color_input: WebElement = new_velp_element.find_element_by_css_selector('input[ng-model="velp.color"]')
        # self.drv.execute_script(
        #     f"""
        #     arguments[0].value = '#00FF00';
        #     SystemJS.registry.get("http://tim/static/scripts/tim/ngimport.ts").$rootScope.$apply();
        #     """,
        #     velp_color_input)

        velp_visible_input = Select(new_velp_element.find_element_by_css_selector(
            'select[ng-model="$ctrl.velp.visible_to"]'))
        velp_visible_input.select_by_visible_text('Just me')
        self.assert_same_screenshot(new_velp_element, 'velps/create_new_velp_filled')

        save_button: WebElement = new_velp_element.find_element_by_css_selector('input[type="submit"]')
        save_button.click()
        self.wait_until_hidden(new_velp_selector)

        # get mouse out of the newly created velp so that the velp is not highlighted
        ActionChains(self.drv).move_to_element(create_velp_btn).perform()

        self.assert_same_screenshot(velp_selection_element, ['velps/velp_selection_one_velp',
                                                             ])

        # Selecting text using these styles does not work for some reason:
        # par: WebElement = self.drv.find_element_by_css_selector('.parContent > p')
        # ActionChains(self.drv).key_down(Keys.LEFT_SHIFT).send_keys(Keys.ARROW_RIGHT,
        #                                                            Keys.ARROW_RIGHT).key_up(
        #     Keys.LEFT_SHIFT).perform()
        # ActionChains(self.drv).move_to_element_with_offset(par, 5, 5).click_and_hold().move_by_offset(70, 0).release().perform()
