import unittest

from timApp.messaging.messagelist.messagelist_utils import message_body_to_md


class MailBodyConvertTest(unittest.TestCase):
    def test_mailbody_convert(self):
        self.assertEqual(
            message_body_to_md(
                """
Hello, world!
This is a newline!

This is a new paragraph.

- List item 1
- List item 2
- List item 3

* List item 1
* List item 2
* List item 3

> Foo
> Bar

Foo said:
> Foo
> Bar

```
Some code
This must behave normally with no extra spaces.
```"""
            ),
            """
Hello, world!  
This is a newline!

This is a new paragraph.

- List item 1

- List item 2

- List item 3

* List item 1

* List item 2

* List item 3

> Foo  
> Bar

Foo said:

> Foo  
> Bar

```
Some code
This must behave normally with no extra spaces.
```""",
        )

    def test_mailbody_convert_codeblocks(self):
        self.assertEqual(
            message_body_to_md(
                """
Special cases:

```
> No newline on top
```

> Quote with code:  
> ```
> Some code
> ```

``````
Below ticks should be handled verbatim
```
``````

Test:
> Foo"""
            ),
            """
Special cases:

```
> No newline on top
```

> Quote with code:  
> ```
> Some code
> ```

``````
Below ticks should be handled verbatim
```
``````

Test:

> Foo""",
        )

    def test_mailbody_convert_url(self):
        self.assertEqual(
            message_body_to_md(
                """
URL Test:

These should become clickable:
https://example.com
http://foo.com/blah_blah_(wikipedia)_(again)
https://www.example.com/foo/?bar=baz&inga=42&quux
http://foo.bar/?q=Test%20URL-encoded%20stuff

This should be converted to a plain link:
https://eur03.safelinks.protection.outlook.com/?url=https%3A%2F%2Ftim.education%2Fstatic%2Fimages%2Ffavicon.ico&amp;data=04%7C01%7C%7C1df387e6b946432d408c08d9670a93be%7Ce9662d58caa44bc1b138c8b1acab5a11%7C1%7C0%7C637654117695068317%7CUnknown%7CTWFpbGZsb3d8eyJWIjoiMC4wLjAwMDAiLCJQIjoiV2luMzIiLCJBTiI6Ik1haWwiLCJXVCI6Mn0%3D%7C1000&amp;sdata=PdzfSTk7Y0zveXhUz5bYPf4vUh6CZ2fF2Ccx91lIg6A%3D&amp;reserved=0

This is an inline link: [Example 1](https://example.com) and some text"""
            ),
            """
URL Test:

These should become clickable:  
<https://example.com>  
<http://foo.com/blah_blah_(wikipedia)_(again)>  
<https://www.example.com/foo/?bar=baz&inga=42&quux>  
<http://foo.bar/?q=Test%20URL-encoded%20stuff>

This should be converted to a plain link:  
<https://tim.education/static/images/favicon.ico>

This is an inline link: [Example 1](<https://example.com>) and some text""",
        )

    def test_mailbody_convert_quote(self):
        self.assertEqual(
            message_body_to_md(
                """
> Quote
> Quote 2
Non-quote

> Quote
>> Subquote
>> Subquote 2
> Quote 2

> Level 1
>> Level 2
>>> Level 3
> Level 1 back
Non-quote

> Quote
> ```
> Code
> ```
> Quote"""
            ),
            """
> Quote  
> Quote 2

Non-quote

> Quote
>
>> Subquote  
>> Subquote 2
>
> Quote 2

> Level 1
>
>> Level 2
>>
>>> Level 3
>
> Level 1 back

Non-quote

> Quote
> ```
> Code
> ```  
> Quote""",
        )

    def test_mailbody_convert_border(self):
        self.assertEqual(
            message_body_to_md(
                """
Some message

--- mail_boundary ---
<b>Some message</b>"""
            ),
            """
Some message
""",
        )

    def test_list_spacing(self):
        self.assertEqual(
            message_body_to_md(
                """
This is a list
- Point 1
- Point 2
"""
            ),
            """
This is a list

- Point 1

- Point 2""",
        )

    def test_hash_convert(self):
        self.assertEqual(
            message_body_to_md(
                """
# This is not a header
## This isn't one either
"""
            ),
            """
\\# This is not a header  
\\#\\# This isn't one either""",
        )

    def test_link_at_end_of_sentence(self):
        self.assertEqual(
            message_body_to_md(
                """This is a sentence that ends with a link: https://example.com/test."""
            ),
            """This is a sentence that ends with a link: <https://example.com/test>.""",
        )
