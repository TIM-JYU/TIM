"""
A button plugin to save other plugins on the same page
"""
from dataclasses import dataclass, asdict
from typing import Union, List

from flask import render_template_string
from marshmallow.utils import missing

from tim_common.markupmodels import GenericMarkupModel
from tim_common.pluginserver_flask import GenericHtmlModel, \
    create_nontask_blueprint, PluginReqs
from tim_common.utils import Missing


@dataclass
class MultisaveStateModel:
    """Model for the information that is stored in TIM database for each answer."""


@dataclass
class MultisaveMarkupModel(GenericMarkupModel):
    areas: Union[List[str], Missing] = missing
    autoUpdateDuplicates: Union[bool, Missing] = True
    autoUpdateTables: Union[bool, Missing] = True
    allSavedText: Union[str, Missing] = missing
    fields: Union[List[str], Missing] = missing
    jumplink: Union[str, Missing, None] = missing
    jumptarget: Union[str, Missing, None] = missing
    listener: Union[bool, Missing] = False
    livefeed: Union[bool, Missing] = False
    savedText: Union[str, Missing] = missing
    tags: Union[List[str], Missing] = missing
    unsavedText: Union[str, Missing] = missing

    # Sisu export-related fields; TODO: Should be a separate plugin.
    destCourse: Union[str, Missing] = missing
    group: Union[str, List[str], Missing] = missing  # for destCourse
    includeUsers: Union[str, Missing] = missing  # TODO: Should be MembershipFilter, but cannot import
    testOnly: Union[bool, Missing] = missing


@dataclass
class MultisaveInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""


@dataclass
class MultisaveHtmlModel(GenericHtmlModel[MultisaveInputModel, MultisaveMarkupModel, MultisaveStateModel]):

    def get_component_html_name(self) -> str:
        return 'tim-multisave'

    def get_static_html(self) -> str:
        return render_static_multisave(self)


def render_static_multisave(m: MultisaveHtmlModel) -> str:
    return render_template_string("""
<div>
<button class="timButton">
{{ buttonText or button or "Save" }}
</button>
</div>""".strip(),
        **asdict(m.markup),
    )


def reqs() -> PluginReqs:
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
    return {
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
    }


multisave_route = create_nontask_blueprint(
    __name__,
    'ms',
    MultisaveHtmlModel,
    reqs,
)
