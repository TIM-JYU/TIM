from unittest import TestCase
from timApp.plugin.importdata.importData import conv_data_field_names
from timApp.plugin.importdata.importData import conv_data_csv
from timApp.plugin.importdata.importData import convert_data


class TestImportData(TestCase):
    def test_conv_data_field_names(self):
        data = ["aknakka;demoA;3", "vesal;demoB;4", "hopohessu;demoC;5"]
        fields = ["demoA = d1", "demoB=d2"]
        e1 = ["aknakka;d1;3", "vesal;d2;4"]

        r1 = conv_data_field_names(data, fields, ";").to_tim_format()
        self.assertEqual(e1, r1, "Not same in normal case")

    def test_conv_data_field_names2(self):
        data = ["aknakka;demoA;3", "vesal;demoB;4", "hopohessu;demoC;5"]
        fields = ["demoA = d1", "demoB=d2", "*"]
        e1 = ["aknakka;d1;3", "vesal;d2;4", "hopohessu;demoC;5"]

        r1 = conv_data_field_names(data, fields, ";").to_tim_format()
        self.assertEqual(e1, r1, "Not same in * case")

    def test_conv_data_field_names4(self):
        data = ["aknakka;demoA;3;demoB;2", "vesal;demoB;4;demoA;9", "hopohessu;demoC;5"]
        fields = ["demoA = d1", "demoB=d2", "*"]
        e1 = ["aknakka;d1;3;d2;2", "vesal;d2;4;d1;9", "hopohessu;demoC;5"]

        r1 = conv_data_field_names(data, fields, ";").to_tim_format()
        self.assertEqual(e1, r1, "Not same in * case")

    def test_conv_data_field_names5(self):
        data = ["aknakka;demoA;3;demoB;2", "vesal;demoB;4;demoA;9", "hopohessu;demoC;5"]
        fields = ["demoA = d1"]
        e1 = ["aknakka;d1;3", "vesal;d1;9"]

        r1 = conv_data_field_names(data, fields, ";").to_tim_format()
        self.assertEqual(e1, r1, "Not same in restricted case")

    def test_conv_data_field_names6(self):
        data = ["aknakka;demo1;3", "vesal;demo2;4", "hopohessu;demo3;5"]
        fields = ["demo1 = d1;demo2=d2"]
        e1 = ["aknakka;d1;3", "vesal;d2;4"]

        r1 = conv_data_field_names(data, fields, ";").to_tim_format()
        self.assertEqual(e1, r1, "Not same in widen case")

    def test_conv_data_field_names7(self):
        data = ["aknakka;demo1;3", "vesal;demo2;4", "hopohessu;demo3;5"]
        fields = ["demo1 = d1;demo2=d2;*"]
        e1 = ["aknakka;d1;3", "vesal;d2;4", "hopohessu;demo3;5"]

        r1 = conv_data_field_names(data, fields, ";").to_tim_format()
        self.assertEqual(e1, r1, "Not same in widen case")

    def test_conv_data_field_names8(self):
        data = ["aknakka;demo1;3", "vesal;demo2;4", "hopohessu;demo3;5"]
        fields = ["demo(1,4) = d"]
        e1 = ["aknakka;d1;3", "vesal;d2;4", "hopohessu;d3;5"]

        r1 = conv_data_field_names(data, fields, ";").to_tim_format()
        self.assertEqual(e1, r1, "Not same in widen case")

    def test_conv_data_csv(self):
        data = ["aknakka;1;3;4", "vesal;2;3;6"]
        fields = ["d1", "d2", "d3"]

        e1 = ["aknakka;d1;1;d2;3;d3;4", "vesal;d1;2;d2;3;d3;6"]

        r1 = conv_data_csv(data, fields, ";").to_tim_format()
        self.assertEqual(e1, r1, "Not same in normal case")

    def test_conv_data_csv2(self):
        data = ["aknakka;1;3;4;5;6", "vesal;2;3;6;5;6"]
        fields = ["d1", "d2", "d3"]

        e1 = ["aknakka;d1;1;d2;3;d3;4", "vesal;d1;2;d2;3;d3;6"]

        r1 = conv_data_csv(data, fields, ";").to_tim_format()
        self.assertEqual(e1, r1, "Not same in too many columns case")

    def test_conv_data_csv3(self):
        data = ["aknakka;1;3;4;5;6", "vesal;2;3"]
        fields = ["d1", "d2", "d3"]

        e1 = ["aknakka;d1;1;d2;3;d3;4", "vesal;d1;2;d2;3"]

        r1 = conv_data_csv(data, fields, ";").to_tim_format()
        self.assertEqual(e1, r1, "Not same in too few columns case")

    def test_conv_data_csv4(self):
        data = ["aknakka;1;3;4;5;6", "vesal;2;3"]
        fields = ["d1;d2;d3"]

        e1 = ["aknakka;d1;1;d2;3;d3;4", "vesal;d1;2;d2;3"]

        r1 = conv_data_csv(data, fields, ";").to_tim_format()
        self.assertEqual(e1, r1, "Not same in too few columns case")

    def test_conv_data_csv5(self):
        data = ["aknakka;1;3;4;5;6", "vesal;2;3"]
        fields = ["d(1,3)"]

        e1 = ["aknakka;d1;1;d2;3;d3;4", "vesal;d1;2;d2;3"]

        r1 = conv_data_csv(data, fields, ";").to_tim_format()
        self.assertEqual(e1, r1, "Not same in too few columns case")

    def test_convert_data(self):
        data = ["aknakka;d1;3", "vesal;d2;4"]
        fields = []
        e1 = ["aknakka;d1;3", "vesal;d2;4"]

        r1 = convert_data(data, fields, ";").to_tim_format()
        self.assertEqual(e1, r1, "Not same in normal case")

    def test_convert_data2(self):
        data = ["aknakka;1;3;4;5;6", "vesal;2;3"]
        fields = ["d1", "d2", "d3"]

        e1 = ["aknakka;d1;1;d2;3;d3;4", "vesal;d1;2;d2;3"]

        r1 = convert_data(data, fields, ";").to_tim_format()
        self.assertEqual(e1, r1, "Not same in CSV case")

    def test_convert_data3(self):
        data = ["aknakka,demoA,3", "vesal,demoB,4", "hopohessu,demoC,5"]
        fields = ["demoA = d1", "demoB=d2"]
        e1 = ["aknakka,d1,3", "vesal,d2,4"]

        r1 = conv_data_field_names(data, fields, ",").to_tim_format(',')
        self.assertEqual(e1, r1, "Not same in change name case")
