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

    def test_genfields7(self):
        s1 = "d:cbfield(1,2)=demo;s1"
        e1 = "{#d1:cbfield stem: 'demo1'#}{#d2:cbfield stem: 'demo2'#}{#s1 stem: 's1'#}"

        r1 = genfields(s1)
        self.assertEqual(e1, r1, "Not same in 2 field attrs case")

    def test_genfields8(self):
        s1 = "d1:cbfield;d2:cbfield;s1"
        e1 = "{#d1:cbfield stem: 'd1'#}{#d2:cbfield stem: 'd2'#}{#s1 stem: 's1'#}"

        r1 = genfields(s1)
        self.assertEqual(e1, r1, "Not same in 2 field attrs case")

    def test_genfields9(self):
        s1 = "d1:cbfield=d1;d2:cbfield=d2;s1"
        e1 = "{#d1:cbfield stem: 'd1'#}{#d2:cbfield stem: 'd2'#}{#s1 stem: 's1'#}"

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

    def test_grange5(self):
        s1 = "d:cbfield;t:cbfield=ta;b=tb"
        e1 = "{#d1:cbfield stem: 'd1'#}{#d2:cbfield stem: 'd2'#}{#t:cbfield stem: 'ta'#}{#b stem: 'tb'#}"

        r1 = gfrange(s1,1,2)
        self.assertEqual(e1, r1, "Not same in field type case no alias")

    def test_grange6(self):
        s1 = "d:cbfield=;t:cbfield=ta;b=tb"
        e1 = "{#d1:cbfield stem: '1'#}{#d2:cbfield stem: '2'#}{#t:cbfield stem: 'ta'#}{#b stem: 'tb'#}"

        r1 = gfrange(s1,1,2)
        self.assertEqual(e1, r1, "Not same in field type case empty alias")

    def test_grange7(self):
        s1 = "d{0}a:cbfield=;t:cbfield=ta;b=tb"
        e1 = "{#d1a:cbfield stem: '1'#}{#d2a:cbfield stem: '2'#}{#t:cbfield stem: 'ta'#}{#b stem: 'tb'#}"

        r1 = gfrange(s1,1,2)
        self.assertEqual(e1, r1, "Not same in field type case own format empty alias")

    def test_grange8(self):
        s1 = "d{0}a:cbfield;t:cbfield=ta;b=tb"
        e1 = "{#d1a:cbfield stem: 'd1a'#}{#d2a:cbfield stem: 'd2a'#}{#t:cbfield stem: 'ta'#}{#b stem: 'tb'#}"

        r1 = gfrange(s1,1,2)
        self.assertEqual(e1, r1, "Not same in field type case own format no alias")

    def test_grange9(self):
        s1 = "d{0}a:cbfield=a{0}b;t:cbfield=ta;b=tb"
        e1 = "{#d1a:cbfield stem: 'a1b'#}{#d2a:cbfield stem: 'a2b'#}{#t:cbfield stem: 'ta'#}{#b stem: 'tb'#}"

        r1 = gfrange(s1,1,2)
        self.assertEqual(e1, r1, "Not same in field type case own format format in alias")

    def test_grange10(self):
        s1 = "d{0}a:cbfield=a;t:cbfield=ta;b=tb"
        e1 = "{#d1a:cbfield stem: 'a1'#}{#d2a:cbfield stem: 'a2'#}{#t:cbfield stem: 'ta'#}{#b stem: 'tb'#}"

        r1 = gfrange(s1,1,2)
        self.assertEqual(e1, r1, "Not same in field type case own format alias")

    def test_grange11(self):
        s1 = "d"
        e1 = "{#d5 stem: 'd5'#}{#d4 stem: 'd4'#}{#d3 stem: 'd3'#}"

        r1 = gfrange(s1,5,3)
        self.assertEqual(e1, r1, "Not same in 5,3")
