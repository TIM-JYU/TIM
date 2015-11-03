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

````
#- {rd=x rp=y}

``` {rp=x rd=y}
```

````

````
test
````
""".strip()
        dp = DocumentParser(doc_text)
        result = dp.get_blocks()
        expected = [{'md': '```\ncode\n\ncode\n```', 'attrs': {'plugin': 'csPlugin'}, 'type': 'code'},
                    {'md': 'text1\n\ntext2', 'type': 'autonormal', 'attrs': {}},
                    {'md': '```\ncode2\n```', 'attrs': {'plugin': 'mmcq'}, 'type': 'code'},
                    {'md': '# Header 1\n\nheaderpar 1\n\nheaderpar 2', 'type': 'header', 'attrs': {}},
                    {'md': '```\nnormal code\n```', 'type': 'code', 'attrs': {}},
                    {'md': '# Header 2\n\nheaderpar 3\n\nheaderpar 4', 'type': 'header', 'attrs': {}},
                    {'md': 'text 3\n\n\ntext 4', 'attrs':
                        {'classes': ['someClass']}, 'type': 'normal'},
                    {'md': '# Test1\n\n# Test2\n\n# Test3', 'type': 'atom', 'attrs': {}},
                    {'md': '```\ncode\n```', 'type': 'code', 'attrs': {}},
                    {'md': 'test', 'type': 'autonormal', 'attrs': {}},
                    {'type': 'header', 'attrs': {'a': 'b'}, 'md': '# Test'},
                    {'md': '````\n#- {rd=x rp=y}\n\n``` {rp=x rd=y}\n```\n\n````', 'type': 'code', 'attrs': {}},
                    {'md': '````\ntest\n````', 'type': 'code', 'attrs': {}}]
        self.assertListEqual(expected, result)
        exported = DocumentWriter(result).get_text()
        self.assertListEqual(expected, DocumentParser(exported).get_blocks())
        random.seed(0)
        dp.add_missing_attributes()
        self.assertListEqual([{'id': 'SoMUq2gZwvpI'},
                              {'id': 'WORjZumBVWdm'},
                              {'id': 'w8i8M6DPgWyR'},
                              {'id': 'JPCV9j6K4VSg'},
                              {'id': 'Hluz6mrkDEWe'},
                              {'id': 'dZzusTxg3PW5'},
                              {'id': 'zW05KRpJQOG9'},
                              {'id': 'E0DvQTlfKkJd'},
                              {'id': 'T4cWAefPZ9Po'},
                              {'id': 'ys55kUwXv6jY'},
                              {'id': 'ziJ7zlQXydZE'},
                              {'id': 'PCzBis5CPokx'},
                              {'id': 'AfibcQb2DGgM'}],
                             [{'id': block['id']} for block in dp.get_blocks()])
        self.assertListEqual([{'t': 'LTB4NGQwMTZhODI='},
                              {'t': 'MHg3ZjUxNmRhYw=='},
                              {'t': 'MHgyZTE2OTQzOA=='},
                              {'t': 'LTB4MjQzZTM5MmU='},
                              {'t': 'LTB4MWIyNGU1NzI='},
                              {'t': 'LTB4MzI5Y2Y0ZWM='},
                              {'t': 'MHgzNGFiZTAwYw=='},
                              {'t': 'LTB4NTZiNGY3ZGU='},
                              {'t': 'MHg3ZDY2ZjA3MQ=='},
                              {'t': 'MHgzMDYzZmNkYg=='},
                              {'t': 'MHg3NjQzNzAyYg=='},
                              {'t': 'LTB4MmJhMWVlZGI='},
                              {'t': 'MHgyNjJlNzU5OQ=='}],
                             [{'t': block['t']} for block in dp.get_blocks()])
        dp.validate_structure()
        self.assertEqual([], DocumentParser('').get_blocks())
        self.assertEqual('', DocumentWriter([]).get_text())
        self.assertEqual('#- {a="b"}\n', DocumentWriter([{'md': '', 'attrs': {'a': 'b'}}]).get_text())
        self.assertListEqual([{'md': '```', 'type': 'code', 'attrs': {}}],
                             DocumentParser('```').get_blocks())

        result = DocumentParser(doc_text).get_blocks(break_on_empty_line=True)
        self.assertListEqual([{'md': '```\ncode\n\ncode\n```', 'attrs': {'plugin': 'csPlugin'}, 'type': 'code'},
                              {'md': 'text1', 'type': 'autonormal', 'attrs': {}},
                              {'md': 'text2', 'type': 'autonormal', 'attrs': {}},
                              {'md': '```\ncode2\n```', 'attrs': {'plugin': 'mmcq'}, 'type': 'code'},
                              {'md': '# Header 1', 'type': 'header', 'attrs': {}},
                              {'md': 'headerpar 1', 'type': 'autonormal', 'attrs': {}},
                              {'md': 'headerpar 2', 'type': 'autonormal', 'attrs': {}},
                              {'md': '```\nnormal code\n```', 'type': 'code', 'attrs': {}},
                              {'md': '# Header 2', 'type': 'header', 'attrs': {}},
                              {'md': 'headerpar 3', 'type': 'autonormal', 'attrs': {}},
                              {'md': 'headerpar 4', 'type': 'autonormal', 'attrs': {}},
                              {'md': 'text 3', 'attrs':
                                  {'classes': ['someClass']}, 'type': 'normal'},
                              {'md': 'text 4', 'type': 'autonormal', 'attrs': {}},
                              {'md': '# Test1\n\n# Test2\n\n# Test3', 'type': 'atom', 'attrs': {}},
                              {'md': '```\ncode\n```', 'type': 'code', 'attrs': {}},
                              {'md': 'test', 'type': 'autonormal', 'attrs': {}},
                              {'md': '# Test', 'type': 'header', 'attrs': {'a': 'b'}},
                              {'attrs': {}, 'type': 'code',
                               'md': '````\n#- {rd=x rp=y}\n\n``` {rp=x rd=y}\n```\n\n````'},
                              {'attrs': {}, 'md': '````\ntest\n````', 'type': 'code'}], result)
        result = DocumentParser(doc_text).get_blocks(break_on_code_block=False,
                                                     break_on_header=False,
                                                     break_on_normal=False)
        expected = [{'md': '```\ncode\n\ncode\n```', 'attrs': {'plugin': 'csPlugin'}, 'type': 'code'},
                    {'md': 'text1\n\ntext2', 'type': 'autonormal', 'attrs': {}},
                    {'md': '```\ncode2\n```', 'attrs': {'plugin': 'mmcq'}, 'type': 'code'},
                    {
                        'md': '# Header 1\n\nheaderpar 1\n\nheaderpar 2\n\n```\nnormal code\n```\n\n'
                              '# Header 2\n\nheaderpar 3\n\nheaderpar 4',
                        'type': 'header', 'attrs': {}},
                    {'md': 'text 3\n\n\ntext 4', 'attrs':
                        {'classes': ['someClass']}, 'type': 'normal'},
                    {'md': '# Test1\n\n# Test2\n\n# Test3', 'type': 'atom', 'attrs': {}},
                    {'md': '```\ncode\n```\n\ntest', 'type': 'code', 'attrs': {}},
                    {'attrs': {'a': 'b'},
                     'md': '# Test\n'
                           '\n'
                           '````\n'
                           '#- {rd=x rp=y}'
                           '\n'
                           '\n'
                           '``` {rp=x rd=y}'
                           '\n'
                           '```\n'
                           '\n'
                           '````\n'
                           '\n'
                           '````\n'
                           'test\n'
                           '````',
                     'type': 'header'}]
        self.assertListEqual(expected, result)
        exported = DocumentWriter(result).get_text()

        # ignore 'type' because some of them are now 'atom'
        self.assertListEqual([{'md': p['md'], 'attrs': p['attrs']} for p in expected],
                             [{'md': p['md'], 'attrs': p['attrs']} for p in DocumentParser(exported).get_blocks()])
        self.assertListEqual([{'attrs': {}, 'type': 'code', 'md': '```\nasd'}], DocumentParser('```\nasd').get_blocks())
        self.assertListEqual([{'attrs': {}, 'type': 'code', 'md': '```'}], DocumentParser('```').get_blocks())

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
""",
# """
##- {#test}
##- {#test}
# """,
"""
#- {id=someinvalid}
""",
"""
```
``` {a=b}
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
""",
"""```
``{a=b}
"""]
        for f in failures:
            with self.assertRaises(ValidationException, msg=f):
                DocumentParser(f).validate_structure()
        for o in oks:
            DocumentParser(o).validate_structure()


if __name__ == '__main__':
    unittest.main()
