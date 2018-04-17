from functools import wraps

from flask import request, redirect
from flask_openid import OpenID, SREG_KEYS, AX_MAPPING, REQUIRED_KEYS, OpenIDResponse, RegLookup, SessionWrapper
from openid.consumer.consumer import Consumer, SUCCESS, CANCEL, FAILURE, SETUP_NEEDED
from openid.extensions import ax
from openid.extensions.sreg import SRegRequest

KORPPI_ALIASES = {
    'http://axschema.org/namePerson': 'fullname',
    'http://axschema.org/namePerson/first': 'firstname',
    'http://axschema.org/namePerson/last': 'lastname',
    'http://axschema.org/contact/email': 'email'
}


class KorppiOpenID(OpenID):
    def attach_reg_info(self, auth_request, keys, optional_keys):
        """Fixes AX aliases so that Korppi is happy.
        """
        keys = set(keys or [])
        optional_keys = set(optional_keys or [])
        sreg_keys = list(SREG_KEYS & keys)
        sreg_optional_keys = list(SREG_KEYS & optional_keys)
        auth_request.addExtension(SRegRequest(required=sreg_keys,
                                              optional=sreg_optional_keys))
        ax_req = ax.FetchRequest()
        for key in (keys | optional_keys):
            for uri in AX_MAPPING.get(key, ()):
                ax_req.add(ax.AttrInfo(uri,
                                       required=key in REQUIRED_KEYS,
                                       alias=KORPPI_ALIASES.get(uri)))  # modified to set alias correctly
        auth_request.addExtension(ax_req)

    def loginhandler(self, f):
        """Reimplementation of loginhandler that calls after_login_func with a KorppiOpenIDResponse.
        """

        @wraps(f)
        def decorated(*args, **kwargs):
            if request.args.get('openid_complete') != 'yes':
                return f(*args, **kwargs)
            consumer = Consumer(SessionWrapper(self), self.store_factory())
            args = request.args.to_dict()
            args.update(request.form.to_dict())
            openid_response = consumer.complete(args, self.get_current_url())
            if openid_response.status == SUCCESS:
                return self.after_login_func(KorppiOpenIDResponse(
                    openid_response, self.extension_responses))
            elif openid_response.status == CANCEL:
                self.signal_error('The request was cancelled')
            elif openid_response.status == FAILURE:
                self.signal_error('OpenID authentication failure. Message: %s'
                                  % openid_response.message)
            elif openid_response.status == SETUP_NEEDED:
                self.signal_error('OpenID setup was needed')
            else:
                self.signal_error('OpenID authentication weird state: %s' %
                                  openid_response.status)
            return redirect(self.get_current_url())

        return decorated


class KorppiOpenIDResponse(OpenIDResponse):
    """OpenID response that also saves first and last name separately."""

    def __init__(self, resp, extensions):
        super().__init__(resp, extensions)
        lookup = RegLookup(resp, extensions)
        self.firstname = lookup.get_uri('http://axschema.org/namePerson/first')
        self.lastname = lookup.get_uri('http://axschema.org/namePerson/last')
