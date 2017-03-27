from lxml import html

from tests.server.timroutetest import TimRouteTest


class SlideTest(TimRouteTest):

    def test_slide_html(self):
        self.login_test1()
        d = self.create_doc(initial_par="""
# First

First text
#-
# Second

Second text
#-
# Third

Third text

<§

* §§first list item§§
* §§second list item§§

Separate fragment
§>

Outside fragment
#-
New paragraph
        """)
        self.check_slide_content(d)
        d.document.set_settings({'test': 'test'})
        self.check_slide_content(d)

    def check_slide_content(self, d):
        t = self.get('/show_slide/{}'.format(d.id), as_tree=True)
        slidesection = t.cssselect('.reveal')
        self.assertEqual(1, len(slidesection))
        pars = [p for p in d.document.get_paragraphs() if not p.is_setting()]
        # TODO get rid of the empty <p></p> tags
        expected_html = """
<div class="reveal" ng-controller="ViewCtrl">
    <div class="slides">
        <section>
            <div class="par" id="{}" t="{}" attrs="{{}}">
                <h1 id="first">First</h1>
                <p>First text</p>
            </div>
        </section>
        <section>
            <div class="par" id="{}" t="{}" attrs="{{}}">
                <h1 id="second">Second</h1>
                <p>Second text</p>
            </div>
        </section>
        <section>
            <div class="par" id="{}" t="{}" attrs="{{}}">
                <h1 id="third">Third</h1>
                <p>Third text</p>
                <p></p>
                <div class="fragment"><p></p>
                    <ul>
                        <li class="fragment">first list item</li>
                        <li class="fragment">second list item</li>
                    </ul>
                    <p>Separate fragment </p>
                </div>
                <p></p>
                <p>Outside fragment</p>
            </div>
            <div class="par" id="{}" t="{}" attrs="{{}}">
                <p>New paragraph</p>
            </div>
        </section>
    </div>
</div>
""".format(pars[0].get_id(), pars[0].get_hash(),
           pars[1].get_id(), pars[1].get_hash(),
           pars[2].get_id(), pars[2].get_hash(),
           pars[3].get_id(), pars[3].get_hash())
        expected_element = html.fromstring(expected_html)
        self.assert_elements_equal(expected_element, slidesection[0])
