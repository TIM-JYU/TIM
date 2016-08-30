# consumer_class

A Python LTI Consumer class.
Install with `python setup.py install`.

## Example Usage

Initialize a Consumer object:

```
from consumer_class import Consumer

creds = {'consumer_key': '__consumer_key__', 'consumer_secret': '__consumer_secret__'}
cons = Consumer(credentials=creds, tp_url='http://timstack.it.jyu.fi:5900/')
```

Sign launch data and send initial POST request to Tool Provider:

```
res = cons.post()
```

The cookies from the HTTP response are now stored and can be used
in conjuction with future GET requests using `cons.get(url)` or `cons.get()` for `url=cons.tp_url`.

Output an HTML form for launching an `<iframe>` element:

```
my_attrs = {
    'target': 'myLaunchFrame',
    'name': 'myFrmLaunch',
    'id': 'myId_FrmLaunch'
}

html_form = cons.make(element='form', attrs=my_attrs)
```

Alternatively, call `cons.make(element='form')` if the default `<form>` attribute values are sufficient.
For making an `iframe`, set `element='iframe'` with the proper attributes instead.

## TODOs

* More accurate profiles!
