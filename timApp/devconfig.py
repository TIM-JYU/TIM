import os

DEBUG = True
PROFILE = False
DEBUG_SQL = False
MINIMUM_SCHEDULED_FUNCTION_INTERVAL = 5

if os.environ.get("RUN_MAILMAN_DEV", "0") == "1":
    MESSAGE_LISTS_ENABLED = True
    MAILMAN_URL = "http://mailman-core:8001/3.1"
    MAILMAN_USER = "restadmin"
    MAILMAN_PASS = "restpass"
    MAILMAN_EVENT_API_USER = "apiuser"
    MAILMAN_EVENT_API_KEY = "apikey"
    MAILMAN_UI_LINK_PREFIX = (
        f"{os.environ.get('TIM_HOST', 'http://localhost')}/mailman3/postorius/lists/"
    )
    os.environ["AUTHLIB_INSECURE_TRANSPORT"] = "true"
    OAUTH2_CLIENTS = [
        {
            "client_id": "mailman_tim",
            "client_secret": "test123",
            "client_name": "Mailman Test",
            "redirect_urls": ["http://localhost/mailman3/accounts/tim/login/callback/"],
            "allowed_scopes": ["profile"],
            "response_types": ["code", "token"],
            "grant_types": ["authorization_code"],
        }
    ]
