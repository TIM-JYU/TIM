"""
TIM plugin: a radiobutton field
"""
import json
from typing import Union, List, Dict

from dataclasses import dataclass, asdict
from flask import jsonify, render_template_string, Blueprint, request
from marshmallow.utils import missing
from webargs.flaskparser import use_args

from marshmallow_dataclass import class_schema
from pluginserver_flask import GenericHtmlModel, \
    GenericAnswerModel, render_multihtml, render_multimd
from markupmodels import GenericMarkupModel
from utils import Missing

goaltable_route = Blueprint('goaltable', __name__, url_prefix="/goaltable")


@dataclass
class GoalTableStateModel:
    """Model for the information that is stored in TIM database for each answer."""
    c: Union[Dict[str, str], Missing] = missing
    styles: Union[Dict[str, str], Missing] = missing


@dataclass
class GoalTableMarkupModel(GenericMarkupModel):
    buttonText: Union[str, Missing, None] = missing
    editText: Union[str, Missing, None] = missing
    goalText: Union[str, Missing, None] = missing
    bloom: Union[bool, Missing, None] = missing
    borders: Union[bool, Missing, None] = missing
    goals: Union[List[str], Missing, None] = missing
    goalscale: Union[List[str], Missing, None] = missing
    mingoal: Union[int, Missing, None] = missing
    maxgoal: Union[int, Missing, None] = missing
    initgoal: Union[int, Missing, None] = missing


@dataclass
class GoalTableInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""
    # c: str
    c: Union[Dict[str, str], Missing] = missing
    nosave: Union[bool, Missing] = missing


@dataclass
class GoalTableHtmlModel(GenericHtmlModel[GoalTableInputModel, GoalTableMarkupModel, GoalTableStateModel]):
    def get_component_html_name(self) -> str:
        return 'goaltable-runner'

    # def show_in_view_default(self) -> bool:
    #    return False

    def get_static_html(self) -> str:
        return render_static_goaltable(self)

    def get_md(self):
        return render_md_goaltable(self)

    def get_review(self):
        if self.state and self.state.c:
            ret = json.dumps(self.state.c)
            ret = ret.replace(",", "\n")
            return f"<pre>{ret}</pre>"

        return "<pre>review</pre>"


@dataclass
class GoalTableAnswerModel(GenericAnswerModel[GoalTableInputModel, GoalTableMarkupModel, GoalTableStateModel]):
    pass


def render_static_goaltable(m: GoalTableHtmlModel):
    template = """
<div>
<h4>{{ header or '' }}</h4>
<p class="stem">{{ stem or '' }}</p>
"""
    table = f'<table><tr><th>{m.markup.goalText or "Osattava asia"}</th>'
    mingoal = max(m.markup.mingoal or 1, 1)
    maxgoal = m.markup.maxgoal or 6
    for i in range(mingoal, maxgoal+1):
        table += f"<th>{i}</th>"
    table += "</tr>"
    for s in m.markup.goals or []:
        parts = s.split(";", 3)
        goal = int(parts[1].strip() or "0")
        iid = s.find(";")
        ig = s.find(";", iid + 1)
        itemtext = s[ig + 1:].strip() or ""
        table += f"<tr><td>{itemtext}</td>"
        for i in range(mingoal, maxgoal+1):
            text = "  "
            if i == goal:
                text = " o "
            table += f"<td>{text}</td>"
        table += "</tr>"
    table += "</table>"
    template += table
    template += """<p class ="plgfooter" > {{''}} </p>
</div>"""
    return render_template_string(template.strip(), **asdict(m.markup),
    )


def render_md_goaltable(m: GoalTableHtmlModel):
    template = ""
    if m.markup.header:
        template += "#### " + m.markup.header + "\n\n"
    template += (m.markup.stem or "")  + "\n\n"

    table = "|" + (m.markup.goalText or "Osattava asia")
    mingoal = max(m.markup.mingoal or 1, 1)
    maxgoal = m.markup.maxgoal or 6
    for i in range(mingoal, maxgoal+1):
        table += " | " + str(i)
    table += " |\n|" + "-"*30
    for i in range(mingoal, maxgoal+1):
        table += "|---"
    table += " |\n"
    for s in m.markup.goals or []:
        parts = s.split(";", 3)
        goal = int(parts[1].strip() or "0")
        itemtext = parts[2].strip() or ""
        table += "| " + itemtext
        for i in range(mingoal, maxgoal+1):
            text = "  "
            if i == goal:
                text = " o "
            table += "|" + text
        table += " |\n"

    template += table
    result = template  # render_template_string(template.strip(), **attr.asdict(m.markup), )
    return result


GoalTableHtmlSchema = class_schema(GoalTableHtmlModel)
GoalTableAnswerSchema = class_schema(GoalTableAnswerModel)


@goaltable_route.route('/multihtml', methods=['post'])
def goaltable_multihtml():
    ret = render_multihtml(request.get_json(), GoalTableHtmlSchema())
    return ret


@goaltable_route.route('/multimd', methods=['post'])
def goaltable_multimd():
    ret = render_multimd(request.get_json(), GoalTableHtmlSchema)
    return ret


@goaltable_route.route('/answer', methods=['put'])
@use_args(GoalTableAnswerSchema(), locations=("json",))
def goaltable_answer(args: GoalTableAnswerModel):
    web = {}
    result = {'web': web}
    c = args.input.c

    nosave = args.input.nosave

    if not nosave:
        save = {"c": c}
        result["save"] = save
        web['result'] = "saved"

    return jsonify(result)


templates = [
"""``` {#PLUGINNAMEHERE plugin="goaltable"}
button: Tallenna
lazy: false
borders: true
header: Osaamistavoitteet
stem: Siirrä osaamisesi aina sitä vastaavalle kohdalle
#lang: en
mingoal: 0
maxgoal: 6
initgoal: 0
goals:
  - rak      ;3;Rakenteisen ohjelmoinnin perusajatus
  - alg      ;3;Algoritminen ajattelu                 
  - cshapr   ;3;C#-kielen perusteet                  
  - per      ;4;**Peräkkäisyys**
  - variables;6;Muuttujat                             
```""",
]

@goaltable_route.route('/reqs')
def goaltable_reqs():
    return jsonify({
        "js": ["/field/js/build/goaltable.js"],
        "css": ["/field/css/field.css"],
        "multihtml": True,
        "multimd": True,
        'editor_tabs': [
            {
                'text': 'Fields',
                'items': [
                    {
                        'text': 'Tables',
                        'items': [
                            {
                                'data': templates[0],
                                'text': 'GoalTable',
                                'expl': 'Taulukko esimerkiksi osaamistavoitteiden tekemiseksi',
                            },
                        ]
                    },
                ],
            },
        ],
    },
    )
