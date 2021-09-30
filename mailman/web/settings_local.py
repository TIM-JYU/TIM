EMAIL_BACKEND = "django.core.mail.backends.filebased.EmailBackend"
EMAIL_FILE_PATH = "/tmp/mailman-messages"
ACCOUNT_DEFAULT_HTTP_PROTOCOL = "http"
ACCOUNT_EMAIL_VERIFICATION = "none"


MAILMAN_WEB_SOCIAL_AUTH = [
    "allauth_tim",
]

SOCIALACCOUNT_PROVIDERS = {
    "tim": {
        "SCOPE": ["profile"],
        "APP": {
            "client_id": "mailman_tim",
            "secret": "test123",
        },
    },
}
