.PHONY: init install test

init:
	pip3 install -r requirements.txt

install:
	python3 setup.py install

test:
	python3 -m unittest test.tests
	python3 -m unittest test.tests_outcomes

