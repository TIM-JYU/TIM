"""
A button plugin to save other plugins on the same page
"""
from dataclasses import dataclass, asdict
from typing import Union

from flask import render_template_string
from marshmallow.utils import missing

from tim_common.markupmodels import GenericMarkupModel
from tim_common.pluginserver_flask import (
    GenericHtmlModel,
    create_nontask_blueprint,
    PluginReqs,
)
from tim_common.utils import Missing


@dataclass
class MultisaveStateModel:
    """Model for the information that is stored in TIM database for each answer."""


@dataclass
class MessageOverrides:
    expect: str
    replace: str


@dataclass
class MultisaveMarkupModel(GenericMarkupModel):
    aliases: dict[str, str] | Missing = missing
    areas: list[str] | Missing = missing
    autoUpdateDuplicates: bool | Missing = True
    autoUpdateTables: bool | Missing = True
    allSavedText: str | Missing = missing
    fields: list[str] | Missing = missing
    jumplink: str | Missing | None = missing
    jumptarget: str | Missing | None = missing
    listener: bool | Missing = False
    livefeed: bool | Missing = False
    savedText: str | Missing = missing
    tags: list[str] | Missing = missing
    unsavedText: str | Missing = missing
    timer: int | Missing = missing
    saveDelay: int | Missing = missing
    showMessages: bool | Missing = False
    messageOverrides: list[MessageOverrides] | Missing = missing

    # Sisu export-related fields; TODO: Should be a separate plugin.
    destCourse: str | Missing = missing
    group: str | list[str] | Missing = missing  # for destCourse
    includeUsers: (
        str | Missing
    ) = missing  # TODO: Should be MembershipFilter, but cannot import
    testOnly: bool | Missing = missing


@dataclass
class MultisaveInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""


@dataclass
class MultisaveHtmlModel(
    GenericHtmlModel[MultisaveInputModel, MultisaveMarkupModel, MultisaveStateModel]
):
    def get_component_html_name(self) -> str:
        return "tim-multisave"

    def get_static_html(self) -> str:
        return render_static_multisave(self)


def render_static_multisave(m: MultisaveHtmlModel) -> str:
    return render_template_string(
        """
<div>
<button class="timButton">
{{ buttonText or button or "Save" }}
</button>
</div>""".strip(),
        **asdict(m.markup),
    )


def reqs() -> PluginReqs:
    templates = [
        """
``` {plugin="multisave"}
```""",
        """
``` {plugin="multisave"}
livefeed: true
allSavedText: 'You have no unsaved answers'
unsavedText: 'You have {count} unsaved answers:'
```""",
        """
``` {plugin="multisave"}
fields:
- 
```""",
        """
``` {plugin="multisave"}
buttonText: "Lähetä arvioinnit Sisuun"
destCourse: jy-CUR-xxxx  # tähän kurssin Sisu-id
showInView: false
```""",
    ]
    return {
        "js": ["/field/js/build/multisave.js"],
        "multihtml": True,
        "css": ["/field/css/field.css"],
        "editor_tabs": [
            {
                "text": "Fields",
                "items": [
                    {
                        "text": "Save/Import",
                        "items": [
                            {
                                "data": templates[0].strip(),
                                "text": "Multisave for entire document",
                                "expl": "Multisave for entire document",
                            },
                            {
                                "data": templates[1].strip(),
                                "text": "Save button with warnings about unsaved tasks",
                                "expl": "Save button with warnings about unsaved tasks",
                            },
                            {
                                "data": templates[2].strip(),
                                "text": "Multisave for specific fields",
                                "expl": "Multisave for specific fields",
                            },
                            {
                                "data": templates[3].strip(),
                                "text": "Send to Sisu button",
                                "expl": "Button for sending results to Sisu",
                            },
                        ],
                    },
                ],
            },
        ],
    }


multisave_route = create_nontask_blueprint(
    __name__,
    "ms",
    MultisaveHtmlModel,
    reqs,
)
