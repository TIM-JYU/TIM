"""
A button plugin to save other plugins on the same page
"""
from typing import Union, List

import attr
from flask import jsonify, render_template_string, Blueprint
from marshmallow import Schema, fields, post_load
from marshmallow.utils import missing
from webargs.flaskparser import use_args

from pluginserver_flask import GenericMarkupModel, GenericMarkupSchema, GenericHtmlSchema, GenericHtmlModel, \
    Missing, \
    InfoSchema, render_multihtml

multisave_route = Blueprint('ms', __name__, url_prefix="/ms")


# @attr.s(auto_attribs=True)
class MultisaveStateModel:
    """Model for the information that is stored in TIM database for each answer."""
    # userword: str


@attr.s(auto_attribs=True)
class MultisaveMarkupModel(GenericMarkupModel):
    jumplink: Union[str, Missing] = missing
    jumptarget: Union[str, Missing] = missing
    areas: Union[List[str], Missing] = missing
    tags: Union[List[str], Missing] = missing
    autoUpdateDuplicates: Union[bool, Missing] = True
    autoUpdateTables: Union[bool, Missing] = True
    emailMode: Union[bool, Missing] = False
    emailRecipients: Union[List[str], Missing] = missing
    emailPreMsg: Union[str, Missing] = missing
    emailSubject: Union[str, Missing] = missing
    destCourse: Union[str, Missing] = missing
    fields: Union[List[str], Missing] = missing


class MultisaveMarkupSchema(GenericMarkupSchema):
    jumplink = fields.String(allow_none=True)
    jumptarget = fields.String(allow_none=True)
    areas = fields.List(fields.Str())
    tags = fields.List(fields.Str())
    autoUpdateDuplicates = fields.Boolean(default=True)
    autoUpdateTables = fields.Boolean(default=True)
    emailMode = fields.Boolean(default=False)
    emailRecipients = fields.List(fields.Str())
    emailPreMsg = fields.String(allow_none=True)
    emailSubject = fields.String()
    destCourse = fields.String()
    fields = fields.List(fields.Str())  # Keep this last

    @post_load
    def make_obj(self, data, **_):
        return MultisaveMarkupModel(**data)


@attr.s(auto_attribs=True)
class MultisaveInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""


class MultisaveAttrs(Schema):
    """Common fields for HTML and answer routes."""
    markup = fields.Nested(MultisaveMarkupSchema)


@attr.s(auto_attribs=True)
class MultisaveHtmlModel(GenericHtmlModel[MultisaveInputModel, MultisaveMarkupModel, MultisaveStateModel]):

    def get_component_html_name(self) -> str:
        return 'multisave-runner'

    def get_static_html(self) -> str:
        return render_static_multisave(self)

    def get_browser_json(self):
        r = super().get_browser_json()
        return r


class MultisaveHtmlSchema(MultisaveAttrs, GenericHtmlSchema):
    info = fields.Nested(InfoSchema, allow_none=True, required=True)

    @post_load
    def make_obj(self, data, **_):
        # noinspection PyArgumentList
        return MultisaveHtmlModel(**data)


def render_static_multisave(m: MultisaveHtmlModel):
    return render_template_string("""
<div>
<button class="timButton">
{{ buttonText or button or "Save" }}
</button>
</div>""".strip(),
        **attr.asdict(m.markup),
    )


MULTISAVE_HTML_SCHEMA = MultisaveHtmlSchema()


@multisave_route.route('/multihtml/', methods=['post'])
@use_args(GenericHtmlSchema(many=True), locations=("json",))
def ms_multihtml(args):  # args: List[GenericHtmlSchema]):
    ret = render_multihtml(args)
    return ret


@multisave_route.route('/reqs/')
@multisave_route.route('/reqs')
def reqs():
    templates = ["""
``` {plugin="multisave"}
```""", """
``` {plugin="multisave"}
areas:
- 
```""", """
``` {plugin="multisave"}
fields:
- 
```""", """
``` {plugin="multisave"}
buttonText: "Send grade and credits to Sisu"
destCourse: jy-CUR-4668  # change th course Sisu-id
showInView: false
```"""]
    return jsonify({
        "js": ["/field/js/build/multisave.js"],
        "multihtml": True,
        "css": ["/field/css/field.css"],
        'editor_tabs': [
            {
                'text': 'Fields',
                'items': [
                    {
                        'text': 'Save/Import',
                        'items': [
                            {
                                'data': templates[0].strip(),
                                'text': 'Multisave for entire document',
                                'expl': 'Multisave for entire document',
                            },
                            {
                                'data': templates[1].strip(),
                                'text': 'Multisave for areas',
                                'expl': 'Multisave for areas',
                            },
                            {
                                'data': templates[2].strip(),
                                'text': 'Multisave for specific IDs',
                                'expl': 'Multisave for specific IDs',
                            },
                            {
                                'data': templates[3].strip(),
                                'text': 'Send to Sisu button',
                                'expl': 'Button for sending results to Sisu',
                            },
                        ],
                    },
                ],
            },
        ],
    },
    )
