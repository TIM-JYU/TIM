"""Server tests for graphviz plugin."""
from timApp.tests.server.timroutetest import TimRouteTest


class GraphvizTest(TimRouteTest):
    def test_graphviz_nonascii(self):
        """Non-ASCII characters work with graphviz."""
        self.login_test1()
        d = self.create_doc(initial_par="""
#- {plugin="graphviz"}
gvType: dot
size: Normal
gvData: |
 digraph test {
  ä->ö;
 }
        """)
        e = self.get(d.url, as_tree=True)
        self.assertEqual(len(e.cssselect('polygon')), 2)
