from timApp.admin.replace_in_documents import perform_replace, ReplaceArguments
from timApp.tests.server.timroutetest import TimRouteTest


class ReplaceTest(TimRouteTest):

    def test_replace(self):
        self.login_test1()
        d = self.create_doc(initial_par="""
#Test1

#Test2

#test3
#-
#asd4

#asd5
        """)
        args = ReplaceArguments(
            dryrun=False,
            format='',
            onlyfirst=False,
            regex=True,
            term=r'[a-zA-Z]',
            to=r'xy',
        )
        repls = [p.get_replacement() for p in perform_replace(d, args)]
        self.assertEqual(
            [('T', 'xy'),
             ('e', 'xy'),
             ('s', 'xy'),
             ('t', 'xy'),
             ('T', 'xy'),
             ('e', 'xy'),
             ('s', 'xy'),
             ('t', 'xy'),
             ('t', 'xy'),
             ('e', 'xy'),
             ('s', 'xy'),
             ('t', 'xy'),
             ('a', 'xy'),
             ('s', 'xy'),
             ('d', 'xy'),
             ('a', 'xy'),
             ('s', 'xy'),
             ('d', 'xy')], repls)
        self.assertEqual("""
``` {atom="true"}
#xyxyxyxy1

#xyxyxyxy2

#xyxyxyxy3
```

``` {atom="true"}
#xyxyxy4

#xyxyxy5
```
""".lstrip(), d.document.export_markdown(export_ids=False))

    def test_header_fix(self):
        self.login_test1()
        d = self.create_doc(initial_par="""
# Test1
#-
#Test2
#-
#test3
#-
##Test4

###Test5

#!/bin/bash

#include <iostream>
#-
```
#pragma omp parallel
```
        """)
        t = self.get(d.url, as_tree=True)
        self.assertEqual(1, len(t.cssselect('h1')))
        args = ReplaceArguments(
            dryrun=False,
            format='',
            onlyfirst=False,
            regex=True,
            term=r'(^|\n)(#+)([A-ZÖÄÅ][a-zöäå][^\n]+)',
            to=r'\1\2 \3',
        )
        repls = [p.get_replacement() for p in perform_replace(d, args)]
        self.assertEqual([('#Test2', '# Test2'), ('##Test4', '## Test4'), ('\n###Test5', '\n### Test5')], repls)
        self.assertEqual("""
# Test1

# Test2

#test3

``` {atom="true"}
## Test4

### Test5

#!/bin/bash

#include <iostream>
```

```
#pragma omp parallel
```
""".lstrip(), d.document.export_markdown(export_ids=False))
        t = self.get(d.url, as_tree=True)
        self.assertEqual(2, len(t.cssselect('h1')))

    def test_replace_no_regex(self):
        self.login_test1()
        d = self.create_doc(initial_par='[[[[[')
        args = ReplaceArguments(
            dryrun=False,
            format='',
            onlyfirst=False,
            regex=False,
            term=r'[[',
            to=r'x',
        )
        repls = [p.get_replacement() for p in perform_replace(d, args)]
        self.assertEqual([('[[', 'x'), ('[[', 'x')], repls)
        self.assertEqual('xx[\n', d.document.export_markdown(export_ids=False))

    def test_skip_invalid_yaml(self):
        self.login_test1()
        d = self.create_doc(settings={'macros': {'a': 'foo', 'b': 'bar'}}, initial_par="""
``` {plugin=csPlugin}
b: bar
asd
```

``` {plugin=csPlugin}
b: bar
a: %%a%%
```
""")
        args = ReplaceArguments(
            dryrun=False,
            format='',
            onlyfirst=False,
            regex=False,
            term=r'b: bar',
            to=r'x',
        )
        repls = [p for p in perform_replace(d, args)]
        self.assertEqual('YAML would be invalid after replacement, so not doing anything',
                         repls[0].error)
        self.assertEqual('YAML is invalid before replacement, so not doing anything',
                         repls[1].error)
        self.assertEqual('YAML would be invalid after replacement, so not doing anything',
                         repls[2].error)
        self.assertEqual('bar', d.document.get_settings().get_dict()['macros']['b'])
