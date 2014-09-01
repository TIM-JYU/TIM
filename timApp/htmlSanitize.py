# -*- coding: utf-8 -*-
import bleach


tags = bleach.ALLOWED_TAGS + ['video','p','code','div','span','br','pre','img','h1','h2','h3','h4','h5','h6','h7','table','tbody','thead','tfoot','td','tr','th','caption','colgroup','col','sub']

tim_attrs = {'*':['class', 'id','align'], 'video':['src','controls'], 'img':['src','width','height']}
bleach.ALLOWED_ATTRIBUTES.update(tim_attrs)



# Sanitize html, bleach is whitelist based, see https://github.com/jsocol/bleach for more information
def sanitize_html(html_string):
    return bleach.clean(html_string, tags)
