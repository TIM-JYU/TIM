"""
A canvas component that can render different kinds of uploaded files
(images and PDFs) and allow teachers to velp them.
"""

import os
import re
from dataclasses import dataclass, asdict
from typing import Union, List

from flask import render_template_string
from marshmallow import validates, ValidationError, missing

from tim_common.markupmodels import GenericMarkupModel
from tim_common.pluginserver_flask import GenericHtmlModel, \
    GenericAnswerModel, register_plugin_app, launch_if_main, PluginAnswerResp, PluginAnswerWeb, PluginReqs, EditorTab
from tim_common.utils import Missing


class UploadedFile:
    path: str
    type: str


@dataclass
class ReviewCanvasStateModel:
    """Model for the information that is stored in TIM database for each answer."""
    # userword: str
    uploadedfile: UploadedFile


@dataclass
class ReviewCanvasMarkupModel(GenericMarkupModel):
    pass


@dataclass
class ReviewCanvasInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""
    # would it be possible to receive multiple files somehow?
    # Marshmallow appears to hate it if we make this into a list
    uploadedfile: UploadedFile


@dataclass
class ReviewCanvasHtmlModel(GenericHtmlModel[ReviewCanvasInputModel, ReviewCanvasMarkupModel, ReviewCanvasStateModel]):
    def get_component_html_name(self) -> str:
        return 'reviewcanvas-runner'

    def get_static_html(self) -> str:
        return render_static_reviewcanvas(self)


@dataclass
class ReviewCanvasAnswerModel(GenericAnswerModel[ReviewCanvasInputModel, ReviewCanvasMarkupModel, ReviewCanvasStateModel]):
    pass


def render_static_reviewcanvas(m: ReviewCanvasHtmlModel) -> str:
    return render_template_string(
        """
<div class="csRunDiv no-popup-menu">
{% if header %}<h4>{{ header }}</h4>{% endif %}
{% if stem %}<p class="stem">{{ stem }}</p>{% endif %}
<div><label>{{ inputstem or '' }}
<input type="file" class="form-control" placeholder="{{inputplaceholder or ''}}" size="{{cols or ''}}"></label>
</div>
<button class="timButton">
{{ buttonText or button or "Save" }}
</button>
{% if footer %}<p class="plgfooter">{{ footer }}</p>{% endif %}
</div>
        """.strip(),
        **asdict(m.markup),
    )


def answer(args: ReviewCanvasAnswerModel) -> PluginAnswerResp:
    web: PluginAnswerWeb = {}
    result: PluginAnswerResp = {'web': web}
    uploadedfile = args.input.uploadedfile
    save = {"uploadedfile": uploadedfile}
    result["save"] = save
    web['result'] = "saved"
    return result


def reqs() -> PluginReqs:
    templates = ["""
``` {#rc plugin="reviewcanvas"}
header: Header
stem: Stem
inputstem: "inputstem"
```"""]
    editor_tabs: List[EditorTab] = [
            {
                'text': 'Plugins',
                'items': [
                    {
                        'text': 'ReviewCanvas',
                        'items': [
                            {
                                'data': templates[0].strip(),
                                'text': 'Review Canvas',
                                'expl': 'Add a review canvas',
                            },
                        ],
                    },
                ],
            },
        ]

    result: PluginReqs = {"js": ["js/build/reviewcanvas.js"], "multihtml": True, 'editor_tabs': editor_tabs}

    return result


app = register_plugin_app(
    __name__,
    html_model=ReviewCanvasHtmlModel,
    answer_model=ReviewCanvasAnswerModel,
    answer_handler=answer,
    reqs_handler=reqs,
)


launch_if_main(__name__, app)
