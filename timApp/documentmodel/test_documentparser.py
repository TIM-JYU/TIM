import random
import unittest

from documentmodel.documentparser import DocumentParser, SplitterException, ValidationException
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
""".strip()
        dp = DocumentParser(doc_text)
        result = dp.get_blocks()
        expected = [{'md': '```\ncode\n\ncode\n```', 'attrs': {'plugin': 'csPlugin'}, 'type': 'code'},
                    {'md': 'text1\n\ntext2\n', 'type': 'normal'},
                    {'md': '```\ncode2\n```', 'attrs': {'plugin': 'mmcq'}, 'type': 'code'},
                    {'md': '# Header 1\n\nheaderpar 1\n\nheaderpar 2\n', 'type': 'header'},
                    {'md': '# Header 2\n\nheaderpar 3\n\nheaderpar 4\n', 'type': 'header'},
                    {'md': 'text 3\n\n\ntext 4\n', 'attrs':
                        {'classes': ['someClass']}, 'type': 'normal'},
                    {'md': '# Test1\n\n# Test2\n\n# Test3', 'type': 'atom'}]
        self.assertListEqual(expected, result)
        exported = DocumentWriter(result).get_text()
        self.assertListEqual(expected, DocumentParser(exported).get_blocks())
        random.seed(0)
        dp.add_missing_attributes()
        self.assertListEqual([{'id': 'SoMUq2gZwvpI', 't': 'LTB4MTg1ZDZmNmQ='},
                              {'id': 'WORjZumBVWdm', 't': 'MHgzNTc5ZTBjNg=='},
                              {'id': 'w8i8M6DPgWyR', 't': 'MHgxODVhZTVmMA=='},
                              {'id': 'JPCV9j6K4VSg', 't': 'MHgyYzA4MGZkOQ=='},
                              {'id': 'Hluz6mrkDEWe', 't': 'MHg3OWI0NzJhYw=='},
                              {'id': 'dZzusTxg3PW5', 't': 'LTB4NWE5OGMxY2I='},
                              {'id': 'zW05KRpJQOG9', 't': 'LTB4MzFkNmM4N2Y='}],
                             [{'id': block['id'], 't': block['t']} for block in dp.get_blocks()])
        dp.validate_ids()
        self.assertEqual([], dp.set_text('').get_blocks())
        with self.assertRaises(SplitterException):
            dp.set_text('```').get_blocks()
        with self.assertRaises(ValidationException):
            dp.set_text('#- {id=SoMUq2gZwvpI}\n\n#- {id=SoMUq2gZwvpI}').validate_ids()
        result = dp.set_text(doc_text).get_blocks(break_on_empty_line=True)
        self.assertListEqual([{'md': '```\ncode\n\ncode\n```', 'attrs': {'plugin': 'csPlugin'}, 'type': 'code'},
                              {'md': 'text1', 'type': 'normal'},
                              {'md': 'text2', 'type': 'normal'},
                              {'md': '```\ncode2\n```', 'attrs': {'plugin': 'mmcq'}, 'type': 'code'},
                              {'md': '# Header 1\n', 'type': 'header'},
                              {'md': 'headerpar 1', 'type': 'normal'},
                              {'md': 'headerpar 2', 'type': 'normal'},
                              {'md': '# Header 2\n', 'type': 'header'},
                              {'md': 'headerpar 3', 'type': 'normal'},
                              {'md': 'headerpar 4', 'type': 'normal'},
                              {'md': 'text 3', 'attrs':
                                  {'classes': ['someClass']}, 'type': 'normal'},
                              {'md': 'text 4', 'type': 'normal'},
                              {'md': '# Test1\n\n# Test2\n\n# Test3', 'type': 'atom'}], result)


if __name__ == '__main__':
    unittest.main()
