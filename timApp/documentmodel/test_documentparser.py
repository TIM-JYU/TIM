import random
import unittest

from documentmodel.documentparser import DocumentParser, SplitterException


class DocumentParserTest(unittest.TestCase):
    def test_parsing(self):
        dp = DocumentParser(
            """
``` {plugin=csPlugin}
code1
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
"""
        )
        result = dp.parse_document()
        self.assertListEqual([{'md': '```\ncode1\n```', 'plugin': 'csPlugin'},
                              {'md': 'text1\n\ntext2\n'},
                              {'md': '```\ncode2\n```', 'plugin': 'mmcq'},
                              {'md': '# Header 1\n\nheaderpar 1\n\nheaderpar 2\n'},
                              {'md': '# Header 2\n\nheaderpar 3\n\nheaderpar 4\n'},
                              {'md': 'text 3\n\ntext 4\n',
                               'classes': ['someClass']},
                              {'md': '# Test1\n\n# Test2\n\n# Test3'}], result)
        random.seed(0)
        dp.add_missing_attributes()
        self.assertListEqual([{'id': 'SoMUq2gZwvpI', 't': 'MHgzOTdhMzhlYg=='},
                              {'id': 'WORjZumBVWdm', 't': 'MHgzNTc5ZTBjNg=='},
                              {'id': 'w8i8M6DPgWyR', 't': 'MHgxODVhZTVmMA=='},
                              {'id': 'JPCV9j6K4VSg', 't': 'MHgyYzA4MGZkOQ=='},
                              {'id': 'Hluz6mrkDEWe', 't': 'MHg3OWI0NzJhYw=='},
                              {'id': 'dZzusTxg3PW5', 't': 'LTB4MjAwYmUxMDI='},
                              {'id': 'zW05KRpJQOG9', 't': 'LTB4MzFkNmM4N2Y='}],
                             [{'id': block['id'], 't': block['t']} for block in dp.blocks])
        dp.validate_ids()
        self.assertEqual([], dp.set_text('').parse_document())
        with self.assertRaises(SplitterException):
            dp.set_text('```').parse_document()


if __name__ == '__main__':
    unittest.main()
