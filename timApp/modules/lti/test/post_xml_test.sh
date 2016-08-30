#!/usr/bin/env bash
curl -X POST -d @grades.xml localhost/lti/grades/ --header "Content-Type:text/xml"
