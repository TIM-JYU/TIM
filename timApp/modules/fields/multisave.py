"""
A button plugin to save other plugins on the same page
"""
from typing import Union, List

from dataclasses import dataclass, asdict
from flask import jsonify, render_template_string, Blueprint, request
from marshmallow.utils import missing

from marshmallow_dataclass import class_schema
from pluginserver_flask import GenericMarkupModel, GenericHtmlModel, \
    Missing, \
    render_multihtml

multisave_route = Blueprint('ms', __name__, url_prefix="/ms")


@dataclass
class MultisaveStateModel:
    """Model for the information that is stored in TIM database for each answer."""


@dataclass
class MultisaveMarkupModel(GenericMarkupModel):
    areas: Union[List[str], Missing] = missing
    autoUpdateDuplicates: Union[bool, Missing] = True
    autoUpdateTables: Union[bool, Missing] = True
    destCourse: Union[str, Missing] = missing
    emailMode: Union[bool, Missing] = missing
    emailPreMsg: Union[str, Missing, None] = missing
    emailRecipients: Union[List[str], Missing] = missing
    emailSubject: Union[str, Missing] = missing
    fields: Union[List[str], Missing] = missing
    group: Union[str, Missing] = missing  # for destCourse
    jumplink: Union[str, Missing, None] = missing
    jumptarget: Union[str, Missing, None] = missing
    tags: Union[List[str], Missing] = missing


@dataclass
class MultisaveInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""


@dataclass
class MultisaveHtmlModel(GenericHtmlModel[MultisaveInputModel, MultisaveMarkupModel, MultisaveStateModel]):

    def get_component_html_name(self) -> str:
        return 'multisave-runner'

    def get_static_html(self) -> str:
        return render_static_multisave(self)


def render_static_multisave(m: MultisaveHtmlModel):
    return render_template_string("""
<div>
<button class="timButton">
{{ buttonText or button or "Save" }}
</button>
</div>""".strip(),
        **asdict(m.markup),
    )


MultisaveHtmlSchema = class_schema(MultisaveHtmlModel)


@multisave_route.route('/multihtml/', methods=['post'])
def ms_multihtml():
    ret = render_multihtml(request.get_json(), MultisaveHtmlSchema())
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
buttonText: "Lähetä arvioinnit Sisuun"
destCourse: jy-CUR-xxxx  # tähän kurssin Sisu-id
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
