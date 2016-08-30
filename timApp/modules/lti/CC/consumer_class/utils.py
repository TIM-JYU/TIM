"""
utils.py
"""

class InvalidConsumerError(Exception):
    def __init__(self, value):
        self.value = value
    def __str__(self):
        return repr(self.value)

class ConsumerHTMLError(Exception):
    def __init__(self, value):
        self.value = value
    def __str__(self):
        return repr(self.value)

class ConsumerXMLError(Exception):
    def __init__(self, value):
        self.value = value
    def __str__(self):
        return repr(self.value)

def isValidXML(schema, **kwargs):
    """
    Validate XML against some preset schema.

    :param schema: URL to schema XML
    """
    import xmltodict
    import requests
    from lxml import etree
    # Can either provide a URL to some XML or the XML content itself
    schema_dict = {'url': schema}
    for item in kwargs.items():
        if 'url' in item:
            req = requests.get(item[1])
            testable_xml = req.text.encode('utf-8')
            req.connection.close()
        elif 'xml' in item:
            testable_xml = item[1].encode('utf-8')
        else:
            raise KeyError('Bad argument name "%s"' % (item[0]))
    req = requests.get(schema_dict['url'])
    schema_dict['xml'] = req.text.encode('utf-8')
    req.connection.close()
    schema_dict['root'] = etree.XML(schema_dict['xml'])
    schema_dict['final'] = etree.XMLSchema(schema_dict['root'])
    parser = etree.XMLParser(schema=schema_dict['final'])
    try:
        root = etree.fromstring(testable_xml, parser)
    except:
        return False
    return True
