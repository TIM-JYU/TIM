"""
TimMenu plugin.
"""
import os
from typing import Union

import attr
import uuid

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
    def __init__(self, text: str, level: int, id: str = None, items = None, open = False):
        self.text = text
        self.level = level
        if not id:
            self.id = str(uuid.uuid4())
        else:
            self.id = id
        self.items = items
        self.open = open

    def __str__(self):
        return f"{{level: {self.level}, id: '{self.id}', text: '{self.text}', items: {self.items}}}"

    def __repr__(self):
        return str(self)

@attr.s(auto_attribs=True)
class TimMenuItemModel:
    text: str
    level: int


@attr.s(auto_attribs=True)
class TimMenuMarkupModel(GenericMarkupModel):
    hoverOpen: Union[bool, Missing] = missing
    topMenu: Union[bool, Missing] = missing
    separator: Union[str, Missing] = missing
    openingSymbol: Union[str, Missing] = missing
    backgroundColor: Union[str, Missing] = missing
    textColor: Union[str, Missing] = missing
    fontSize: Union[str, Missing] = missing
    menu: Union[str, Missing] = missing


class TimMenuItemSchema(Schema):
    text = fields.Str(required=True)
    level = fields.Int(required=True)

    @post_load
    def make_obj(self, data):
        return TimMenuItemModel(**data)

class TimMenuMarkupSchema(GenericMarkupSchema):
    hoverOpen = fields.Bool(allow_none=True, default=True)
    topMenu = fields.Bool(allow_none=True, default=False)
    separator = fields.Str(allow_none=True, default="&nbsp;")
    openingSymbol = fields.Str(allow_none=True, default="&#9662;")
    backgroundColor = fields.Str(allow_none=True)
    textColor = fields.Str(allow_none=True)
    fontSize = fields.Str(allow_none=True)
    menu = fields.Str()
    @post_load
    def make_obj(self, data):
        return TimMenuMarkupModel(**data)


class TimMenuAttrs(Schema):
    """Common fields for HTML and answer routes."""
    markup = fields.Nested(TimMenuMarkupSchema)
    state = fields.Nested(TimMenuStateSchema, allow_none=True, required=True)

class TimMenuError(Exception):
    """
    Generic timmenu-plugin error.
    """

@attr.s(auto_attribs=True)
class TimMenuInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""
    data: str
    separator: str
    openingSymbol: str
    backgroundColor: str
    textColor: str
    fontSize: str
    url: str


class TimMenuInputSchema(Schema):
    data = fields.Str(required=True)
    separator = fields.Str(required=False)
    openingSymbol = fields.Str(required=False)
    backgroundColor = fields.Str(required=False)
    fontSize = fields.Str(required=False)
    url = fields.Str(required=False)

    @post_load
    def make_obj(self, data):
        return TimMenuInputModel(**data)


def decide_menu_level(index: int, previous_level: int, max_level: int = 3) -> int:
    """
    Parse menu level from indentation with minimum step of two spaces. Following level
    is always at most one greater than the previous, since skipping a level would break
    the menu structure.
    :param index: Number of spaces before beginning of the list.
    :param previous_level: Previous menu's level.
    :param max_level: Level ceiling; further indentations go into the same menu level.
    :return: Menu level decided by indentation.
    """
    level = index // 2
    level = previous_level+1 if level > previous_level else level
    level = max_level if level > max_level else level
    return level


def parse_menu_string(menu_str):
    """
    Parses a markdown list formatted string into menu structure.
    :param menu_str: Menu as a single string.
    :return: Menu as a list of TimMenuItem objects.
    """
    menu_split = menu_str.split("\n")
    if not menu_split:
        # TODO: User can't see this yet!
        raise TimMenuError("'menu' is empty or invalid!")
    menuitem_list = []
    parents = [TimMenuItem(text="", level=-1, items=[])]
    previous_level = -1
    for item in menu_split:
        try:
            list_symbol_index = item.index("-")
        except ValueError:
            # Skip lines without list symbol "-".
            continue
        level = decide_menu_level(list_symbol_index, previous_level)
        previous_level = level
        text_markdown = item[list_symbol_index+1:]
        text_html = call_dumbo([text_markdown])[0].replace("<p>","<span>").replace("</p>","</span>")
        current = TimMenuItem(text=text_html, level=level, items=[])
        for parent in parents:
            if parent.level < level:
                parent.items.append(current)
                parents.insert(0, current)
                break
        # List has all menus that are parents to any others, but first one contains the whole menu tree.
        menuitem_list = parents[-1].items
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
        # TODO: Add schemas and models matching the structure to get rid of str(...).
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
menu: |!!
 - Title 1
    - [Item 1](item_1_address)
    - [Item 2](item_2_address)
    - [Item 3](item_3_address)
    - *Non-link item*
 - Title 2
    - [Item 4](item_4_address)
    - [Item 5](item_5_address)
 - [Title 3](title_3_address)
 - *Non-link title*
!!
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
