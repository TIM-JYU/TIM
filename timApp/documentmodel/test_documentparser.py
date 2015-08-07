import random
import unittest

from documentmodel.documentparser import DocumentParser, ValidationException
from documentmodel.documentwriter import DocumentWriter


class DocumentParserTest(unittest.TestCase):
    def test_parsing(self):
        doc_text = """
``` {plugin=csPlugin}
code

code
```

text1

text2

``` {plugin=mmcq}
code2
```

# Header 1

headerpar 1

headerpar 2

```
normal code
```

# Header 2

headerpar 3

headerpar 4

#- {.someClass}
text 3


text 4

``` {atom=true}
# Test1

# Test2

# Test3
```

```
code
```

test

# Test {a=b}
""".strip()
        dp = DocumentParser(doc_text)
        result = dp.get_blocks()
        expected = [{'md': '```\ncode\n\ncode\n```', 'attrs': {'plugin': 'csPlugin'}, 'type': 'code'},
                    {'md': 'text1\n\ntext2', 'type': 'autonormal'},
                    {'md': '```\ncode2\n```', 'attrs': {'plugin': 'mmcq'}, 'type': 'code'},
                    {'md': '# Header 1\n\nheaderpar 1\n\nheaderpar 2', 'type': 'header'},
                    {'md': '```\nnormal code\n```', 'type': 'code'},
                    {'md': '# Header 2\n\nheaderpar 3\n\nheaderpar 4', 'type': 'header'},
                    {'md': 'text 3\n\n\ntext 4', 'attrs':
                        {'classes': ['someClass']}, 'type': 'normal'},
                    {'md': '# Test1\n\n# Test2\n\n# Test3', 'type': 'atom'},
                    {'md': '```\ncode\n```', 'type': 'code'},
                    {'md': 'test', 'type': 'autonormal'},
                    {'type': 'header', 'attrs': {'a': 'b'}, 'md': '# Test'}]
        self.assertListEqual(expected, result)
        exported = DocumentWriter(result).get_text()
        self.assertListEqual(expected, DocumentParser(exported).get_blocks())
        random.seed(0)
        dp.add_missing_attributes()
        self.assertListEqual([{'id': 'SoMUq2gZwvpI', 't': 'LTB4MTg1ZDZmNmQ='},
                              {'id': 'WORjZumBVWdm', 't': 'LTB4YWE2YmM5Yw=='},
                              {'id': 'w8i8M6DPgWyR', 't': 'MHgxODVhZTVmMA=='},
                              {'id': 'JPCV9j6K4VSg', 't': 'MHgyNzUzYjQyNA=='},
                              {'id': 'Hluz6mrkDEWe', 't': 'LTB4MWU4MjE4'},
                              {'id': 'dZzusTxg3PW5', 't': 'LTB4NmIyOGFkNWQ='},
                              {'id': 'zW05KRpJQOG9', 't': 'MHg3NDExMDQ3NA=='},
                              {'id': 'E0DvQTlfKkJd', 't': 'LTB4MzFkNmM4N2Y='},
                              {'id': 'T4cWAefPZ9Po', 't': 'LTB4ODkxMjE0Nw=='},
                              {'id': 'ys55kUwXv6jY', 't': 'LTB4NDU5NDJkZWQ='},
                              {'id': 'ziJ7zlQXydZE', 't': 'LTB4YTU1MWNkMQ=='}],
                             [{'id': block['id'], 't': block['t']} for block in dp.get_blocks()])
        dp.validate_structure()
        self.assertEqual([], DocumentParser('').get_blocks())
        self.assertListEqual([{'md': '```', 'type': 'code'}],
                             DocumentParser('```').get_blocks())

        result = DocumentParser(doc_text).get_blocks(break_on_empty_line=True)
        self.assertListEqual([{'md': '```\ncode\n\ncode\n```', 'attrs': {'plugin': 'csPlugin'}, 'type': 'code'},
                              {'md': 'text1', 'type': 'autonormal'},
                              {'md': 'text2', 'type': 'autonormal'},
                              {'md': '```\ncode2\n```', 'attrs': {'plugin': 'mmcq'}, 'type': 'code'},
                              {'md': '# Header 1', 'type': 'header'},
                              {'md': 'headerpar 1', 'type': 'autonormal'},
                              {'md': 'headerpar 2', 'type': 'autonormal'},
                              {'md': '```\nnormal code\n```', 'type': 'code'},
                              {'md': '# Header 2', 'type': 'header'},
                              {'md': 'headerpar 3', 'type': 'autonormal'},
                              {'md': 'headerpar 4', 'type': 'autonormal'},
                              {'md': 'text 3', 'attrs':
                                  {'classes': ['someClass']}, 'type': 'normal'},
                              {'md': 'text 4', 'type': 'autonormal'},
                              {'md': '# Test1\n\n# Test2\n\n# Test3', 'type': 'atom'},
                              {'md': '```\ncode\n```', 'type': 'code'},
                              {'md': 'test', 'type': 'autonormal'},
                              {'md': '# Test', 'type': 'header', 'attrs': {'a': 'b'}}], result)
        result = DocumentParser(doc_text).get_blocks(break_on_code_block=False,
                                                     break_on_header=False,
                                                     break_on_normal=False)
        self.assertListEqual([{'md': '```\ncode\n\ncode\n```', 'attrs': {'plugin': 'csPlugin'}, 'type': 'code'},
                              {'md': 'text1\n\ntext2', 'type': 'autonormal'},
                              {'md': '```\ncode2\n```', 'attrs': {'plugin': 'mmcq'}, 'type': 'code'},
                              {
                                  'md': '# Header 1\n\nheaderpar 1\n\nheaderpar 2\n\n```\nnormal code\n```\n\n'
                                        '# Header 2\n\nheaderpar 3\n\nheaderpar 4',
                                  'type': 'header'},
                              {'md': 'text 3\n\n\ntext 4', 'attrs':
                                  {'classes': ['someClass']}, 'type': 'normal'},
                              {'md': '# Test1\n\n# Test2\n\n# Test3', 'type': 'atom'},
                              {'md': '```\ncode\n```\n\ntest', 'type': 'code'},
                              {'md': '# Test', 'attrs': {'a': 'b'}, 'type': 'header'}], result)

    def test_validation(self):
        failures = [
            """
#- {id=SoMUq2gZwvpI}

#- {id=SoMUq2gZwvpI}
        """,
            """
#- {area=test}
""",
            """
#- {area_end=test}
""",
            """
#- {area=test}
#- {area_end=test}
#- {area=test}
#- {area_end=test}
""",
            """
#- {area=test .some}
#- {area=test2 .some}
#- {area_end=test}
#- {area_end=test2}
""", """
#- {area=test area_end=test}
""", """
#- {#test}
#- {#test}
""", """
#- {id=someinvalid}
"""]

        oks = [
            """
#- {area=test}
#- {area_end=test}
""",
            """
#- {area=test .some}
#- {area=test2 .some}
#- {area=test4 .some}
#- {area_end=test4}
#- {area_end=test2}
#- {area=test3 .some}
#- {area_end=test3}
#- {area_end=test}
"""]
        for f in failures:
            with self.assertRaises(ValidationException):
                DocumentParser(f).validate_structure()
        for o in oks:
            DocumentParser(o).validate_structure()


if __name__ == '__main__':
    unittest.main()
