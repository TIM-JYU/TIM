"""

"""
import os
from typing import Union, List

import attr

from flask import jsonify, render_template_string
from marshmallow import Schema, fields, post_load
from marshmallow.utils import missing

from pluginserver_flask import GenericMarkupModel, GenericMarkupSchema, GenericHtmlSchema, GenericHtmlModel, \
     Missing, InfoSchema, create_blueprint
from timApp.markdown.dumboclient import call_dumbo
from timApp.tim_app import csrf


@attr.s(auto_attribs=True)
class TimMenuStateModel:
    """Model for the information that is stored in TIM database for each answer."""
    url: Union[str, Missing] = None
    separator: Union[str, Missing] = None
    openingSymbol: Union[str, Missing] = None


class TimMenuStateSchema(Schema):
    url = fields.Str(allow_none=True)
    separator = fields.Str(allow_none=True)
    openingSymbol = fields.Str(allow_none=True)

    @post_load
    def make_obj(self, data):
        res = TimMenuStateModel(**data)
        return res


class TimMenuItem:
    def __init__(self, text: str, level: int):
        self.text = text
        self.level = level

    def __str__(self):
        return f"{{level: {self.level}, text: '{self.text}'}}"

    def __repr__(self):
        return str(self)

@attr.s(auto_attribs=True)
class TimMenuItemModel:
    text: str
    level: int


@attr.s(auto_attribs=True)
class TimMenuMarkupModel(GenericMarkupModel):
    hoverOpen: Union[bool, Missing] = missing
    separator: Union[str, Missing] = missing
    openingSymbol: Union[str, Missing] = missing
    #menu: Union[List[str], Missing] = missing
    menu: Union[str, Missing] = missing


class TimMenuItemSchema(Schema):
    text = fields.Str(required=True)
    level = fields.Int(required=True)

    @post_load
    def make_obj(self, data):
        return TimMenuItemModel(**data)

class TimMenuMarkupSchema(GenericMarkupSchema):
    hoverOpen = fields.Bool(allow_none=True, default=True)
    separator = fields.Str(allow_none=True, default="&nbsp;")
    openingSymbol = fields.Str(allow_none=True, default="&#9662;")
    #menu = fields.List(fields.Str())
    menu = fields.Str()
    # menu = fields.List(fields.Nested(TimMenuItemSchema))
    @post_load
    def make_obj(self, data):
        return TimMenuMarkupModel(**data)


class TimMenuAttrs(Schema):
    """Common fields for HTML and answer routes."""
    markup = fields.Nested(TimMenuMarkupSchema)
    state = fields.Nested(TimMenuStateSchema, allow_none=True, required=True)


@attr.s(auto_attribs=True)
class TimMenuInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""
    data: str
    separator: str
    openingSymbol: str
    url: str

class TimMenuInputSchema(Schema):
    data = fields.Str(required=True)
    separator = fields.Str(required=False)
    openingSymbol = fields.Str(required=False)
    url = fields.Str(required=False)

    @post_load
    def make_obj(self, data):
        return TimMenuInputModel(**data)


def parse_menu_string(menu_str):
    menu_split = menu_str.split("\n")
    menuitem_list = []
    for item in menu_split:
        list_symbol_index = item.index("-")
        level = list_symbol_index//2
        text_markdown = item[list_symbol_index+1:]
        text_html = call_dumbo([text_markdown])[0]
        menuitem_list.append(TimMenuItem(text_html, level))
    return menuitem_list


@attr.s(auto_attribs=True)
class TimMenuHtmlModel(GenericHtmlModel[TimMenuInputModel, TimMenuMarkupModel, TimMenuStateModel]):
    def get_component_html_name(self) -> str:
        return 'timmenu-runner'

    def show_in_view_default(self) -> bool:
        return True

    def get_static_html(self) -> str:
        s = "TimMenu"
        return render_static_TimMenu(self, s)

    def get_browser_json(self):
        r = super().get_browser_json()
        # TODO: Error handling etc.
        print(parse_menu_string(r['markup']['menu']))
        r['markup']['menu'] = str(parse_menu_string(r['markup']['menu']))
        return r


class TimMenuHtmlSchema(TimMenuAttrs, GenericHtmlSchema):
    info = fields.Nested(InfoSchema, allow_none=True, required=True)

    @post_load
    def make_obj(self, data):
        # noinspection PyArgumentList
        return TimMenuHtmlModel(**data)


def render_static_TimMenu(m: TimMenuHtmlModel, s: str):
    return render_template_string(
        f"""
<div class="TimMenu">
 {s}
</div>
<br>
        """,
        **attr.asdict(m.markup),
    )


timMenu_plugin = create_blueprint(__name__, 'timMenu', TimMenuHtmlSchema(), csrf)

@timMenu_plugin.route('/reqs/')
@timMenu_plugin.route('/reqs')
def reqs():
    """Introducing templates for TimMenu plugin"""
    templates = ["""
``` {plugin="timMenu"}
separator: "|"
openingSymbol: " &#9661;"
menu:
 - Title 1
    - [Link1_text](link1_address)
    - [Link2_text](link2_address)
    - *linkless_item*
 - Title 2
    - [Link3_text](link3_address)
    - [Link4_text](link4_address)
 - '[Title 3](title3_link_address)'
 - '[Title 4](title4_link_address)'
```
""","""
``` {plugin="timMenu"}
- [link1_text](link1)
- Title 1
    - [link2_text](link2)
    - [link3_text](link3)
-  Title 2
    - [link4_text](link4)
    - [link5_text](link5)
```
    """]
    editor_tabs = [
            {
                'text': 'Insert',
                'items': [
                    {
                        'text': 'Others',
                        'items': [
                            {
                                'data': templates[0].strip(),
                                'text': 'TimMenu',
                                'expl': 'Add a dropdown menu bar',
                            },
                            {
                                'data': templates[1].strip(),
                                'text': 'TimMenu (simple)',
                                'expl': 'Add a minimal dropdown menu bar',
                            },
                        ],
                    },
                ],
            },
        ]
    if os.environ.get('SHOW_TEMPLATES', "True") == "False":
        editor_tabs = None
    return jsonify({
        "js": [],
        "multihtml": True,
        'editor_tabs': editor_tabs,
        #"default_automd": True,
    },
    )
