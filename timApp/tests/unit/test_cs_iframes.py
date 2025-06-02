import tempfile
from unittest import TestCase
from unittest.mock import mock_open, patch
from timApp.modules.cs import iframes
from tim_common.fileParams import QueryClass

filepath = tempfile.TemporaryDirectory().name


def create_query(markup):
    """
    Create a QueryClass instance with the given markup.
    :param markup: markup to set in the query
    :return: QueryClass instance with the specified markup
    """
    query = QueryClass()
    query.jso = {"markup": markup}
    return query


def create_iframes_query(iframes_dict):
    """
    Create a QueryClass instance with iframes markup.
    :param iframes_dict: iframes to set in the query
    :return: QueryClass instance with the specified iframes
             and empty web dictionary
    """
    query = create_query({"iframes": iframes_dict})
    web = {}
    return query, web


def my_mock_open(filename, *_args, **_kwargs):
    """
    Mock open function to return different data based on the filename.
    :param filename: namo of file to mock
    :param _args: not used
    :param _kwargs: not used
    :return: mocked file object with predefined content
    """
    files = {
        filepath + "/h0.html": "htmldata0",
        filepath + "/h1.html": "htmldata1",
        filepath + "/h2.html": "htmldata2",
        filepath
        + "/iframes.yaml": """
defaults:
    sandbox: "allow-scripts, allow-same-origin"
    style: "background-color: red"
    width: 100
files:  
    - filename: h0.html
      remove: false
    - filename: h2.html
      sandbox: "allow-scripts"
      style: "background-color: green"
style: 'display: flex'
  
""",
    }
    m = mock_open(read_data=files.get(filename, "default"))
    return m()


class TestCSIframes(TestCase):
    def check(self, web, out, err, msg):
        """
        Check that the web content and output are as expected.
        :param web: pair of expected and actual content
        :param out: pair of expected and actual content
        :param err: pair of expected and actual content
        :param msg: message for assertion failure
        """
        self.assertEqual(out[0], out[1], f"out not same for {msg}")
        self.assertEqual(err[0], err[1], f"err not same for {msg}")
        self.assertEqual(web[0], web[1], f"web not same for {msg}")

    def test_stdout(self):
        """
        Test that if filename is stdout, then content is output
        and empty output is returned. Error is unchanged.
        """
        query, web = create_iframes_query([{"filename": "stdout"}])
        out, err = iframes.check_iframes(query, web, "htmldata", "olderror", filepath)
        expected_web = {
            "iframes": {
                "files": [
                    {"filename": "stdout", "content": "htmldata"},
                ],
            }
        }
        self.check((expected_web, web), ("", out), ("olderror", err), "2 html files")

    def test_error(self):
        """
        Test that if file is not found, then content is empty
        error includes the error message
        """
        query, web = create_iframes_query([{"filename": "h99.html"}])
        out, err = iframes.check_iframes(query, web, "oldout", "olderror", filepath)
        err = err.split("html")[0] + "html\n"
        expected_web = {"iframes": {"files": []}}
        self.check(
            (expected_web, web),
            ("oldout", out),
            ("olderror\nError reading iframe file: h99.html\n", err),
            "2 html files",
        )

    def test_2_html_files(self):
        """
        Test that two html files are read correctly
        and the first one is removed. out and err are unchanged.
        See file content in my_mock_open.
        """
        query, web = create_iframes_query(
            {
                "files": [
                    {"filename": "h0.html"},
                    {"filename": "h1.html", "remove": False},
                ]
            }
        )
        with patch("builtins.open", new=my_mock_open):
            with patch("os.remove") as mock_remove:
                out, err = iframes.check_iframes(
                    query, web, "oldout", "olderror", filepath
                )
            calls = [c.args[0] for c in mock_remove.call_args_list]
            self.assertIn(filepath + "/h0.html", calls)
            self.assertNotIn(filepath + "/h1.html", calls)
        expected_web = {
            "iframes": {
                "files": [
                    {"filename": "h0.html", "content": "htmldata0"},
                    {"filename": "h1.html", "content": "htmldata1"},
                ],
            }
        }
        self.check(
            (expected_web, web), ("oldout", out), ("olderror", err), "2 html files"
        )

    def test_read_iframes_yaml(self):
        """
        Test that iframes.yaml is read correctly and
        order of files is h0, h0, h2, h1.
        Sandbox is not changed for second h0 and h2.
        For iframes.yaml and file contents, see my_mock_open.
        """
        query, web = create_iframes_query(
            {
                "defaults": {
                    "height": 60,
                    "sandbox": "allow-scripts",
                    "style": "border: 1px solid black",
                },
                "style": "flex-wrap: wrap",
                "files": [
                    {"filename": "h0.html", "remove": False, "sandbox": "allow-modals"},
                    {"readIframes": "iframes.yaml", "sandbox": "allow-forms"},
                    {"filename": "h1.html"},
                ],
            }
        )
        with patch("builtins.open", new=my_mock_open):
            out, err = iframes.check_iframes(query, web, "oldout", "olderror", filepath)
        expected_web = {
            "iframes": {
                "files": [
                    {
                        "filename": "h0.html",
                        "content": "htmldata0",
                        "sandbox": "allow-modals",
                    },
                    {
                        "filename": "h0.html",
                        "content": "htmldata0",
                        "sandbox": "allow-forms",
                    },
                    {
                        "filename": "h2.html",
                        "content": "htmldata2",
                        "style": "background-color: green",
                        "sandbox": "allow-forms",
                    },
                    {
                        "filename": "h1.html",
                        "content": "htmldata1",
                    },
                ],
                "defaults": {
                    "sandbox": "allow-scripts",
                    "style": "background-color: red",
                    "height": 60,
                    "width": 100,
                },
                "style": "flex-wrap: wrap; display: flex",
            }
        }
        self.check(
            (expected_web, web),
            ("oldout", out),
            ("olderror", err),
            "read iframes.yaml",
        )
