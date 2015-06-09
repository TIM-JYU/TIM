import unittest
from document.documentparser import DocumentParser


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
"""
        )
        result = dp.parse_document()
        self.assertListEqual([{'md': '```\ncode1\n```', 'plugin': 'csPlugin'},
                              {'md': 'text1\n\ntext2\n'},
                              {'md': '```\ncode2\n```', 'plugin': 'mmcq'},
                              {'md': '# Header 1\n\nheaderpar 1\n\nheaderpar 2\n'},
                              {'md': '# Header 2\n\nheaderpar 3\n\nheaderpar 4\n'},
                              {'md': 'text 3\n\ntext 4\n',
                               'classes': ['someClass']}], result)

        self.assertEqual([], dp.set_text('').parse_document())


if __name__ == '__main__':
    unittest.main()
