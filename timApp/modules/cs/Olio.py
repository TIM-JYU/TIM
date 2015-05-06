__author__ = 'vesal'
import pprint
pp = pprint.PrettyPrinter(indent=4)


class QueryClass:
    def __init__(self):
        self.query = {}
        self.jso = None

o1 = QueryClass();
o2 = QueryClass();

o1.jso = "Koira"
o1.query["a"] = "kana"
pp.pprint(o1.query)

o2.jso = "Kettu"
o2.query["b"] = "mato"

pp.pprint(o1.query)
pp.pprint(o2.query)



