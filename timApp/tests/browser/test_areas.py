from selenium.webdriver.common.by import By

from timApp.tests.browser.browsertest import BrowserTest


class AreasTest(BrowserTest):

    def test_areas(self):
        self.login_browser_quick_test1()
        self.login_test1()
        ref = self.create_doc(initial_par="""
#- {id=hP7jT8DiV9al}
par 13

#- {area=b1}
#-
par 15
#- {area_end=b1}

#- {area=b2 collapse=true}
par 17
#-
par 18
#- {area_end=b2}

#- {id=6JG9R9LAg4AT}
par 21
        """)
        d = self.create_doc(initial_par="""
#-
par 1

#- {area=a1}
par 2
#-
par 3
#- {area_end=a1}

#-
par 5

#- {area=a2}
par 6
#-
par 7
#-
par 8
#- {area_end=a2}

#- {area=a3 collapse=true}
par 10
#-
par 11
#- {area_end=a3}
        """)
        d.document.add_text(f"""
#- {{rd={ref.id} rp=hP7jT8DiV9al}}
#- {{rd={ref.id} ra=b1}}
#- {{rd={ref.id} ra=b2}}

#- {{area=a4}}
par 20

#- {{rd={ref.id} rp=6JG9R9LAg4AT}}

#-
par 22

#- {{area_end=a4}}
""")
        self.goto_document(d)

        # open collapsed areas
        self.find_element_by_text('par 10').click()
        self.find_element_by_text('par 17').click()

        pars = self.drv.find_elements(By.CSS_SELECTOR, '.par')
        norm_par_choices = [
            'View source',
            'Comment/note',
            'Add paragraph above',
            'Edit',
            'Copy',
            'Add question above',
            'Close menu',
        ]
        ref_par_choices = [
            'View source',
            'Comment/note',
            'Follow reference',
            'Add paragraph above',
            'Edit',
            'Copy',
            'Add question above',
            'Close menu',
        ]
        ref_area_choices = [
            'View source',
            'Comment/note',
            'Follow reference',
            'Add paragraph above',
            'Add question above',
            'Close menu',
        ]
        ref_area_choices_no_add = [
            'View source',
            'Comment/note',
            'Follow reference',
            'Close menu',
        ]
        ref_area_choices_ext = [
            'View source',
            'Comment/note',
            'Follow reference',
            'Add paragraph above',
            'Paste...',
            'Add question above',
            'Start selection',
            'Close menu',
        ]
        ref_area_choices_ext_no_add = [
            'View source',
            'Comment/note',
            'Follow reference',
            'Paste...',
            'Start selection',
            'Close menu',
        ]

        def area_choices(name):
            return [
                'View source',
                'Comment/note',
                'Add paragraph above',
                f"Edit area '{name}'",
                f"Cut area '{name}'",
                f"Copy area '{name}'",
                'Add question above',
                'Close menu',
            ]

        def area_choices_ext(name):
            return [
                'View source',
                'Comment/note',
                'Add paragraph above',
                f"Edit area '{name}'",
                f"Cut area '{name}'",
                f"Copy area '{name}'",
                'Paste...',
                'Add question above',
                'Start selection',
                'Close menu',
            ]

        # To make this test easier to follow, let's make sure that the indexes correspond to the paragraph texts.
        for i in (1, 2, 3, 5, 6, 7, 8, 10, 11, 13, 15, 17, 18, 20, 21, 22):
            self.assertEqual(f'par {i}', pars[i].text)

        self.check_menu(pars[1], norm_par_choices)
        self.check_menu(pars[2], area_choices('a1'))
        self.check_menu(pars[3], norm_par_choices)
        self.check_menu(pars[5], norm_par_choices)
        self.check_menu(pars[6], area_choices('a2'))
        self.check_menu(pars[7], norm_par_choices)
        self.check_menu(pars[8], norm_par_choices)
        self.check_menu(pars[10], area_choices('a3'))
        self.check_menu(pars[11], norm_par_choices)
        self.check_menu(pars[13], ref_par_choices)
        self.check_menu(pars[15], ref_area_choices_no_add)
        self.check_menu(pars[17], ref_area_choices)
        self.check_menu(pars[18], ref_area_choices_no_add)
        self.check_menu(pars[20], area_choices('a4'))
        self.check_menu(pars[21], ref_par_choices)
        self.check_menu(pars[22], norm_par_choices)

        menu = self.open_menu(pars[1])
        self.find_element('.parEditButton', parent=menu).click()
        self.close_menu(menu)

        # These are clickable only in edit mode.
        self.check_menu(pars[4], area_choices_ext('a1'))
        self.check_menu(pars[9], area_choices_ext('a2'))
        self.check_menu(pars[12], area_choices_ext('a3'))
        self.check_menu(pars[14], ref_area_choices_ext)
        self.check_menu(pars[16], ref_area_choices_ext_no_add)
        self.check_menu(pars[23], area_choices_ext('a4'))

        # Make sure viewing source works for a reference paragraph that is inside an area.
        menu = self.open_menu(pars[21])
        self.find_element_by_text('View source', parent=menu).click()
        self.wait_until_text_present('tim-diff-dialog', f'#- {{rd="{ref.id}" rp="6JG9R9LAg4AT"}}')

    def check_menu(self, p, expected):
        menu = self.open_menu(p)
        self.assertEqual(expected, menu.text.splitlines())
        self.close_menu(menu)

    def close_menu(self, menu):
        self.find_element('tim-close-button', parent=menu).click()

    def open_menu(self, p):
        self.find_element('.editline', parent=p).click()
        menu = self.find_element('tim-popup-menu-dialog')
        return menu

    def test_area_delete_par(self):
        self.login_test1()
        d = self.create_doc(initial_par="""
#- {area="a"}

#-
par 1

#-
par 2

#- {area_end="a"}
        """)
        self.login_browser_quick_test1()
        self.goto_document(d)
        pars = self.drv.find_elements(By.CSS_SELECTOR, '.par:not(#HELP_PAR)')
        menu = self.open_menu(pars[2])
        self.find_element_by_text('Edit', parent=menu).click()
        self.wait_until_present_and_vis('pareditor')
        old_len = len(pars)
        self.wait_for_editor_load()
        self.find_element_by_text('Delete').click()
        self.drv.switch_to.alert.accept()
        self.wait_until_hidden('pareditor')
        pars = self.drv.find_elements(By.CSS_SELECTOR, '.par:not(#HELP_PAR)')
        self.assertEqual(old_len - 1, len(pars))
