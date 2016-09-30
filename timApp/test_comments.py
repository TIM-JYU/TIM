from lxml import html
from lxml.cssselect import CSSSelector
from lxml.html import HtmlElement
from typing import List

from documentmodel.docparagraph import DocParagraph
from timroutetest import TimRouteTest

comment_selector = CSSSelector('div.notes > div.note')


class CommentTest(TimRouteTest):
    def test_comments(self):
        self.login_test1()
        d = self.create_doc(initial_par='test')
        par = d.get_paragraphs()[0]
        comment1 = 'This is a comment.'
        comment2 = 'This is a comment 2.'
        comment3 = 'This is a comment 3.'
        expected_comments = [comment1, comment2, comment3]
        comment_private = 'This is a private comment.'

        self.post_comment(comment1, par)
        comments = self.post_comment(comment_private, par, public=False)
        self.assertEqual(2, len(comments))
        self.assertEqual(comment1, comments[0].find('p').text_content())
        self.assertEqual(comment_private, comments[1].find('p').text_content())
        timdb = self.get_db()
        timdb.users.grant_view_access(timdb.users.get_anon_group_id(), d.doc_id)

        self.login_anonymous()
        comments = self.post_comment(comment2, par)
        self.assertEqual(2, len(comments))
        comments = self.post_comment(comment3, par)
        self.assertEqual(3, len(comments))
        for e, a in zip(expected_comments, comments):
            self.assertEqual(e, a.find('p').text_content())

    def post_comment(self, comment_of_test1: str, par: DocParagraph, public: bool = True) -> List[HtmlElement]:
        resp = self.json_post('/postNote', {'text': comment_of_test1,
                                            'access': 'everyone' if public else 'justme',
                                            'docId': par.doc.doc_id,
                                            'par': par.get_id()}, as_json=True, expect_status=200)
        h = html.fromstring(resp['texts'])  # type: HtmlElement
        comments = comment_selector(h)
        return comments
