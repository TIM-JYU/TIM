from unittest import TestCase
from timApp.plugin.importdata.importData import conv_data_field_names
from timApp.plugin.importdata.importData import conv_data_csv

class TestImportData(TestCase):
    def test_conv_data_field_names(self):
        data = ["aknakka;demoA;3", "vesal;demoB;4", "hopohessu;demoC;5"]
        fields = ["demoA = d1", "demoB=d2"]
        e1 = ["aknakka;d1;3", "vesal;d2;4"]

        r1 = conv_data_field_names(data, fields, ";")
        self.assertEqual(e1, r1, "Not same in normal case")

    def test_conv_data_field_names2(self):
        data = ["aknakka;demoA;3", "vesal;demoB;4", "hopohessu;demoC;5"]
        fields = ["demoA = d1", "demoB=d2", "*"]
        e1 = ["aknakka;d1;3", "vesal;d2;4", "hopohessu;demoC;5"]

        r1 = conv_data_field_names(data, fields, ";")
        self.assertEqual(e1, r1, "Not same in normal case")
