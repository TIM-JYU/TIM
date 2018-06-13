from flask import Blueprint
from flask import Response
from flask import request

korppimock = Blueprint('korppimock',
                       __name__,
                       url_prefix='/korppi')

stored_appcookie = None


@korppimock.route('/authorize')
def korppi_authorize():
    appcookie = request.args.get('request')
    if appcookie:
        global stored_appcookie
        stored_appcookie = appcookie
        return Response('', mimetype='text/plain')
    auth = request.args.get('authorize')
    if auth == stored_appcookie:
        return Response('johmadoe\nDoe John Matt\njohn.m.doe@student.jyu.fi', mimetype='text/plain')
