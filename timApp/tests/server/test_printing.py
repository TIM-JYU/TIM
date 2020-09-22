"""Server tests for printing."""
import json
import urllib.parse

from timApp.document.docentry import DocEntry
from timApp.document.specialnames import TEMPLATE_FOLDER_NAME, PRINT_FOLDER_NAME
from timApp.tests.server.timroutetest import TimRouteTest
from timApp.util.flask.responsehelper import to_json_str
from timApp.util.utils import exclude_keys, EXAMPLE_DOCS_PATH


class PrintingTest(TimRouteTest):
    def test_print_invalid_request(self):
        self.login_test1()
        d = self.create_doc()
        self.json_post(f'/print/x', expect_status=404, expect_content='Document not found')
        self.json_post(f'/print/{d.path}', expect_status=400, expect_content='No filetype selected.')
        self.json_post(f'/print/{d.path}', {}, expect_status=400, expect_content='No filetype selected.',
                       )
        self.json_post(f'/print/{d.path}', {'fileType': 'x'}, expect_status=400,
                       expect_content='No template doc selected.')
        self.json_post(f'/print/{d.path}', {'fileType': 'x', 'templateDocId': 'x'}, expect_status=400,
                       expect_content='No value for printPluginsUserCode submitted.')
        self.json_post(f'/print/{d.path}', {'fileType': 'x', 'templateDocId': 99, 'printPluginsUserCode': False},
                       expect_status=400,
                       expect_content="The supplied parameter 'fileType' is invalid.")
        self.json_post(f'/print/{d.path}', {'fileType': 'latex', 'templateDocId': 'x', 'printPluginsUserCode': False},
                       expect_status=400,
                       expect_content='Invalid template doc id')
        self.json_post(f'/print/{d.path}', {'fileType': 'latex', 'templateDocId': 99, 'printPluginsUserCode': False},
                       expect_status=400,
                       expect_content='There is no template with id 99')

        self.login_test2()
        self.json_post(f'/print/{d.path}', expect_status=403)

    def test_print_latex_pdf(self):
        self.login_test1()
        content = 'Hello 1\n\n#-\nHello 2'
        d = self.create_doc(initial_par=content)
        folder = self.current_user.get_personal_folder().path
        t = self.create_empty_print_template()
        tj = json.loads(to_json_str(t))

        # Reduce test flakiness by removing timestamp.
        tj.pop('modified')
        tmpl = self.get(f'/print/templates/{d.path}').get('templates')
        tmpl[0].pop('modified')
        self.assertEqual([tj], tmpl)
        params_post = {'fileType': 'latex', 'templateDocId': t.id, 'printPluginsUserCode': False}
        params_url = {'file_type': 'latex', 'template_doc_id': t.id, 'plugins_user_code': False}
        exp_params = {'file_type': 'latex', 'template_doc_id': t.id}
        expected_url = f'http://localhost/print/{d.path}?{urllib.parse.urlencode(exp_params)}'
        self.json_post(f'/print/{d.path}', params_post,
                       expect_status=201,
                       expect_content={'success': True,
                                       'url': expected_url})
        result = self.get_no_warn(expected_url)
        self.assertEqual('Hello 1\n\nHello 2', result)
        t2 = self.create_doc(f'{folder}/{TEMPLATE_FOLDER_NAME}/{PRINT_FOLDER_NAME}/base',
                             from_file=f'{EXAMPLE_DOCS_PATH}/templates/print_base.md')
        tj2 = json.loads(to_json_str(t2))
        result = self.get(f'/print/templates/{d.path}').get('templates')
        self.assert_list_of_dicts_subset(result, map(lambda x: exclude_keys(x, 'modified'), [tj, tj2]))
        params_url = {'file_type': 'latex', 'template_doc_id': t2.id, 'plugins_user_code': False}
        expected_url = f'http://localhost/print/{d.path}?{urllib.parse.urlencode(params_url)}'
        result = self.get_no_warn(expected_url)

        with open('tests/server/expected/printing/hello_1_2.tex', encoding='utf-8') as f:
            self.assertEqual(result, f.read())

        params_url = {'file_type': 'pdf', 'template_doc_id': t2.id, 'plugins_user_code': False}
        expected_url = f'http://localhost/print/{d.path}?{urllib.parse.urlencode(params_url)}'
        result = self.get_no_warn(expected_url)

        # TODO: XeLaTeX doesn't support removing timestamps from PDF file, so we cannot do a binary compare.
        # Just check the file size for now.
        pdf_length = len(result)
        self.assertTrue(2813 <= pdf_length <= 2855, msg=f'Unexpected file length: {pdf_length}')
        self.login_test2()
        self.get(expected_url, expect_status=403)

    def test_print_latex_autonumber(self):
        self.login_test1()
        d = self.create_doc(initial_par="""
# first

```
# I'm not a header!
```

# second
#-
# third {.nonumber}
#-
# fourth

---------- --
not header x
-------------
        """, settings={'auto_number_headings': 1})
        t = self.create_empty_print_template()
        params_url = {'file_type': 'latex', 'template_doc_id': t.id, 'plugins_user_code': False, 'force': True}
        r = self.get(d.url, as_tree=True)
        self.assertTrue(r.cssselect('.parContent table'))
        r = self.get_no_warn(f'/print/{d.path}', query_string=params_url)
        self.assertEqual(r"""
\hypertarget{first}{%
\section{1. first}\label{first}}

\begin{verbatim}
# I'm not a header!
\end{verbatim}

\hypertarget{second}{%
\section{2. second}\label{second}}

\hypertarget{third}{%
\section*{third}\label{third}}
\addcontentsline{toc}{section}{third}

\hypertarget{fourth}{%
\section{3. fourth}\label{fourth}}

\begin{longtable}[]{@{}ll@{}}
\toprule
\endhead
not header & x\tabularnewline
\bottomrule
\end{longtable}
        """.strip(), r)
        d.document.add_setting('texmacros', {'texautonumber': 1})
        self.get(d.url, as_tree=True)
        r = self.get_no_warn(f'/print/{d.path}', query_string=params_url)
        no_numbers = r"""
\hypertarget{first}{%
\section{first}\label{first}}

\begin{verbatim}
# I'm not a header!
\end{verbatim}

\hypertarget{second}{%
\section{second}\label{second}}

\hypertarget{third}{%
\section*{third}\label{third}}
\addcontentsline{toc}{section}{third}

\hypertarget{fourth}{%
\section{fourth}\label{fourth}}

\begin{longtable}[]{@{}ll@{}}
\toprule
\endhead
not header & x\tabularnewline
\bottomrule
\end{longtable}
        """
        self.assertEqual(no_numbers.strip(), r)
        d.document.set_settings({})
        self.get(d.url, as_tree=True)
        r = self.get_no_warn(f'/print/{d.path}', query_string=params_url)
        self.assertEqual(no_numbers.strip(), r)

    def create_empty_print_template(self):
        p = f'{self.current_user.get_personal_folder().path}/{TEMPLATE_FOLDER_NAME}/{PRINT_FOLDER_NAME}/empty'
        t = DocEntry.find_by_path(p)
        if t:
            return t
        t = self.create_doc(p, initial_par="""
``` {.latex printing_template=""}
$body$
```
        """)
        return t
