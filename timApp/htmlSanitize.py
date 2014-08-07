import bleach

# Sanitize html, bleach is whitelist based, see https://github.com/jsocol/bleach for more information
def sanitize_html(html_string):
    return bleach.clean(html_string)
