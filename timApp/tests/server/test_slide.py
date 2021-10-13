from timApp.tests.server.timroutetest import TimRouteTest


class SlideTest(TimRouteTest):
    def test_slide_html(self):
        self.login_test1()
        d = self.create_doc(
            initial_par="""
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

#- {slide_break="sub"}
New subslide

#- {slide_break="sub"}
---
#-
Forced subslide

#-
---
#-
New slide
        """
        )
        self.check_slide_content(d)
        d.document.set_settings({"test": "test"})
        self.check_slide_content(d)

    def check_slide_content(self, d):
        t = self.get(f"/show_slide/{d.id}", as_tree=True)
        slidesection = t.cssselect(".reveal")
        self.assertEqual(1, len(slidesection))
        pars = [p for p in d.document.get_paragraphs() if not p.is_setting()]
        # TODO get rid of the empty <p></p> tags
        expected_html = f"""
<div class="reveal" style="visibility: hidden">
    <div class="slides paragraphs">
        <section>
            <section>
                <div class="par" id="{pars[0].get_id()}" t="{pars[0].get_hash()}" attrs="{{}}">
                    <h1 id="first">First</h1>
                    <p>First text</p>
                </div>
            </section>
        </section>
        <section>
            <section>
                <div class="par" id="{pars[1].get_id()}" t="{pars[1].get_hash()}" attrs="{{}}">
                    <h1 id="second">Second</h1>
                    <p>Second text</p>
                </div>
            </section>
        </section>
        <section>
            <section>
                <div class="par" id="{pars[2].get_id()}" t="{pars[2].get_hash()}" attrs="{{}}">
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
                <div class="par" id="{pars[3].get_id()}" t="{pars[3].get_hash()}" attrs="{{}}">
                    <p>New paragraph</p>
                </div>
            </section>
            <section>
                <div class="par" id="{pars[4].get_id()}" t="{pars[4].get_hash()}" attrs='{{"slide_break": "sub"}}'>
                    <p>New subslide</p>
                </div>
            </section>
            <section>
                <div class="par" id="{pars[6].get_id()}" t="{pars[6].get_hash()}" attrs="{{}}">
                    <p>Forced subslide</p>
                </div>
            </section>
        </section>
        <section>
            <section>
                <div class="par" id="{pars[8].get_id()}" t="{pars[8].get_hash()}" attrs="{{}}">
                    <p>New slide</p>
                </div>
            </section>
        </section>
    </div>
</div>
"""
        self.assert_same_html(slidesection[0], expected_html)
