from setuptools import setup

setup(
    name='consumer_class',
    version='0.3.1',
    packages=['consumer_class', 'test'],
    license='MIT',
    platforms=['any'],
    install_requires=['urllib3', 'lxml', 'requests', 'oauth2', 'bs4', 'xmltodict'],
    description='LTI Consumer class for interfacing with LTI Providers',
    keywords='lti',
    test_suite='test'
)
