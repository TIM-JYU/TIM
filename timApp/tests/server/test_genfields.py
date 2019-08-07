from unittest import TestCase
from timApp.markdown.markdownconverter import genfields, gfrange

class TestGenfields(TestCase):
    def test_genfields(self):
        s1 = ["d1"]
        e1 = "{#d1 stem: 'd1'#}"

        r1 = genfields(s1)
        self.assertEqual(e1, r1, "Not same in normal case")

    def test_genfields2(self):
        s1 = ["d(1,3)"]
        e1 = "{#d1 stem: 'd1'#}{#d2 stem: 'd2'#}{#d3 stem: 'd3'#}"

        r1 = genfields(s1)
        self.assertEqual(e1, r1, "Not same in 3 field case")

    def test_genfields3(self):
        s1 = ["d(1,3)"]
        e1 = "{#d1 header: 'd1'#}{#d2 header: 'd2'#}{#d3 header: 'd3'#}"

        r1 = genfields(s1, "", "header")
        self.assertEqual(e1, r1, "Not same in 3 field header case")

    def test_genfields4(self):
        s1 = ["d(1,3)=demo"]
        e1 = "{#d1 stem: 'demo1'#}{#d2 stem: 'demo2'#}{#d3 stem: 'demo3'#}"

        r1 = genfields(s1)
        self.assertEqual(e1, r1, "Not same in 3 field demo case")

    def test_genfields5(self):
        s1 = ["d(1,2)=demo"]
        e1 = "{#d1 stem: 'demo1', autosave: true, cols: 3#}{#d2 stem: 'demo2', autosave: true, cols: 3#}"

        r1 = genfields(s1,"autosave: true, cols: 3")
        self.assertEqual(e1, r1, "Not same in 2 field attrs case")

    def test_genfields6(self):
        s1 = "d(1,2)=demo;s1"
        e1 = "{#d1 stem: 'demo1'#}{#d2 stem: 'demo2'#}{#s1 stem: 's1'#}"

        r1 = genfields(s1)
        self.assertEqual(e1, r1, "Not same in 2 field attrs case")

    def test_grange(self):
        s1 = "d"
        e1 = "{#d1 stem: 'd1'#}{#d2 stem: 'd2'#}"

        r1 = gfrange(s1,1,2)
        self.assertEqual(e1, r1, "Not same in normal case")

    def test_grange2(self):
        s1 = "d=demo"
        e1 = "{#d1 stem: 'demo1'#}{#d2 stem: 'demo2'#}"

        r1 = gfrange(s1,1,2)
        self.assertEqual(e1, r1, "Not same in demo case")

    def test_grange3(self):
        s1 = "d=demo;t"
        e1 = "{#d1 stem: 'demo1'#}{#d2 stem: 'demo2'#}{#t stem: 't'#}"

        r1 = gfrange(s1,1,2)
        self.assertEqual(e1, r1, "Not same in multiple case")

    def test_grange4(self):
        s1 = "d;t=ta;b=tb"
        e1 = "{#d1 stem: 'd1'#}{#d2 stem: 'd2'#}{#t stem: 'ta'#}{#b stem: 'tb'#}"

        r1 = gfrange(s1,1,2)
        self.assertEqual(e1, r1, "Not same in multiple case")

