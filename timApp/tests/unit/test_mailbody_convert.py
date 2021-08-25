import unittest

from timApp.messaging.messagelist.messagelist_utils import message_body_to_md


class MailBodyConvertTest(unittest.TestCase):
    def test_mailbody_convert(self):
        self.assertEqual(message_body_to_md("""
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
```"""), """
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
```""")

    def test_mailbody_convert_special(self):
        self.assertEqual(message_body_to_md("""
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
> Foo"""), """
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

> Foo""")

    def test_mailbody_convert_url(self):
        self.assertEqual(message_body_to_md("""
URL Test:

These should become clickable:
https://example.com
http://foo.com/blah_blah_(wikipedia)_(again)
https://www.example.com/foo/?bar=baz&inga=42&quux
http://foo.bar/?q=Test%20URL-encoded%20stuff

This should be converted to a plain link:
https://eur03.safelinks.protection.outlook.com/?url=https%3A%2F%2Ftim.education%2Fstatic%2Fimages%2Ffavicon.ico&amp;data=04%7C01%7C%7C1df387e6b946432d408c08d9670a93be%7Ce9662d58caa44bc1b138c8b1acab5a11%7C1%7C0%7C637654117695068317%7CUnknown%7CTWFpbGZsb3d8eyJWIjoiMC4wLjAwMDAiLCJQIjoiV2luMzIiLCJBTiI6Ik1haWwiLCJXVCI6Mn0%3D%7C1000&amp;sdata=PdzfSTk7Y0zveXhUz5bYPf4vUh6CZ2fF2Ccx91lIg6A%3D&amp;reserved=0

This is an inline link: [Example 1](https://example.com) and some text"""), """
URL Test:

These should become clickable:  
<https://example.com>  
<http://foo.com/blah_blah_(wikipedia)_(again)>  
<https://www.example.com/foo/?bar=baz&inga=42&quux>  
<http://foo.bar/?q=Test%20URL-encoded%20stuff>

This should be converted to a plain link:  
<https://tim.education/static/images/favicon.ico>

This is an inline link: [Example 1](<https://example.com>) and some text""")
