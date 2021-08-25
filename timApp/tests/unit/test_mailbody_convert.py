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
