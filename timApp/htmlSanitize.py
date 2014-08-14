import bleach

tags = bleach.ALLOWED_TAGS + ['video', 'http', 'p']

# Sanitize html, bleach is whitelist based, see https://github.com/jsocol/bleach for more information
def sanitize_html(html_string):
    return bleach.linkify(html_string)
    
