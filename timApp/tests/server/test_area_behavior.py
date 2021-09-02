from timApp.tests.server.timroutetest import TimRouteTest


class AreaTest(TimRouteTest):

    def test_name_area(self):
        self.login_test1()
        d = self.create_doc(initial_par='test')
        pars = d.document.get_paragraphs()
        par_id = pars[0].get_id()
        self.json_post(f'/name_area/{d.id}/{"testarea"}',
                       {"area_start": par_id, "area_end": par_id,
                        "options": {"collapse": True, "hlevel": 0}})
        d.document.clear_mem_cache()
        pars_new = d.document.get_paragraphs()
        self.assertEqual(3, len(pars_new))
        self.assertDictEqual({'area': 'testarea'}, pars_new[0].get_attrs())
        self.assertEqual('', pars_new[0].get_markdown())
        self.assertEqual(pars[0], pars_new[1])
        self.assertDictEqual({'area_end': 'testarea'}, pars_new[2].get_attrs())
        self.assertEqual('', pars_new[2].get_markdown())

    def test_area_classed(self):
        self.login_test1()
        d = self.create_doc(initial_par="""
#- {area=a .myclass}

#-
test

#- {area_end=a}
        """)
        doc = self.get(d.url, as_tree=True)
        areacontent = doc.cssselect('.areaContent.myclass')
        self.assertEqual(len(areacontent), 1)
        myclass = doc.cssselect('.myclass')
        self.assertEqual(len(myclass), 1)

    def test_area_duplicate_warning(self):
        self.login_test1()
        d = self.create_doc()
        d.document.add_text(f"""
#- {{area=a}}

#-
test

#- {{area_end=a}}

#- {{rd={d.id} ra=a}}
        """)
        r = self.get(d.url, as_tree=True)
        self.assertEqual(0, len(r.cssselect('.alert.alert-info')))
        d.document.add_text(f"""
#- {{area=a}}

#-
test

#- {{area_end=a}}
        """)
        r = self.get(d.url, as_tree=True)
        self.assertEqual(
            'Area a appears more than once in this document. Fix this to get rid of this warning.',
            r.cssselect('.alert.alert-info')[0].text_content().strip(),
        )

    def test_area_no_wrong_warning(self):
        """If preview has a broken area temporarily, TIM shouldn't complain about it."""
        self.login_test1()
        d = self.create_doc()
        self.post_preview(d, '#- {area=a}')
        r = self.get(d.url, as_tree=True)
        self.assertFalse(r.cssselect('.alert.alert-info'))
