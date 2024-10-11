"""
TIM plugin: a radiobutton field
"""
import json
from dataclasses import dataclass, asdict
from typing import Union

from flask import render_template_string
from marshmallow.utils import missing

from tim_common.markupmodels import GenericMarkupModel
from tim_common.pluginserver_flask import (
    GenericHtmlModel,
    GenericAnswerModel,
    create_blueprint,
    value_or_default,
    PluginAnswerResp,
    PluginAnswerWeb,
    PluginReqs,
)
from tim_common.utils import Missing


@dataclass
class GoalTableStateModel:
    """Model for the information that is stored in TIM database for each answer."""

    c: dict[str, str] | Missing = missing
    styles: dict[str, str] | Missing = missing


@dataclass
class GoalTableMarkupModel(GenericMarkupModel):
    editText: str | Missing | None = missing
    goalText: str | Missing | None = missing
    bloom: bool | Missing | None = missing
    borders: bool | Missing | None = missing
    goals: list[str] | Missing | None = missing
    goalscale: list[str] | Missing | None = missing
    mingoal: int | Missing | None = missing
    maxgoal: int | Missing | None = missing
    initgoal: int | Missing | None = missing
    editMode: bool | Missing | None = missing

    def get_mingoal(self) -> int:
        return max(value_or_default(self.mingoal, 1), 1)

    def get_maxgoal(self) -> int:
        return value_or_default(self.maxgoal, 6)

    def get_goals(self) -> list[str]:
        goals = self.goals
        if not isinstance(goals, list):
            return []
        return goals


@dataclass
class GoalTableInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""

    # c: str
    c: dict[str, str] | Missing = missing
    nosave: bool | Missing = missing


@dataclass
class GoalTableHtmlModel(
    GenericHtmlModel[GoalTableInputModel, GoalTableMarkupModel, GoalTableStateModel]
):
    def get_component_html_name(self) -> str:
        return "goaltable-runner"

    # def show_in_view_default(self) -> bool:
    #    return False

    def get_static_html(self) -> str:
        return render_static_goaltable(self)

    def get_md(self) -> str:
        return render_md_goaltable(self)

    def get_review(self) -> str:
        if self.state and self.state.c:
            ret = json.dumps(self.state.c)
            ret = ret.replace(",", "\n")
            return f"<pre>{ret}</pre>"

        return "<pre>review</pre>"


@dataclass
class GoalTableAnswerModel(
    GenericAnswerModel[GoalTableInputModel, GoalTableMarkupModel, GoalTableStateModel]
):
    pass


def render_static_goaltable(m: GoalTableHtmlModel) -> str:
    template = """
<div>
<h4>{{ header or '' }}</h4>
<p class="stem">{{ stem or '' }}</p>
"""
    table = f'<table><tr><th>{m.markup.goalText or "Osattava asia"}</th>'
    mingoal = m.markup.get_mingoal()
    maxgoal = m.markup.get_maxgoal()
    for i in range(mingoal, maxgoal + 1):
        table += f"<th>{i}</th>"
    table += "</tr>"
    for s in m.markup.get_goals():
        parts = s.split(";", 3)
        goal = int(parts[1].strip() or "0")
        iid = s.find(";")
        ig = s.find(";", iid + 1)
        itemtext = s[ig + 1 :].strip() or ""
        table += f"<tr><td>{itemtext}</td>"
        for i in range(mingoal, maxgoal + 1):
            text = "  "
            if i == goal:
                text = " o "
            table += f"<td>{text}</td>"
        table += "</tr>"
    table += "</table>"
    template += table
    template += """<p class ="plgfooter" > {{''}} </p>
</div>"""
    return render_template_string(
        template.strip(),
        **asdict(m.markup),
    )


def render_md_goaltable(m: GoalTableHtmlModel) -> str:
    template = ""
    if isinstance(m.markup.header, str):
        template += "#### " + m.markup.header + "\n\n"
    template += (value_or_default(m.markup.stem, "")) + "\n\n"

    table = "|" + value_or_default(m.markup.goalText, "Osattava asia")
    mingoal = m.markup.get_mingoal()
    maxgoal = m.markup.get_maxgoal()
    for i in range(mingoal, maxgoal + 1):
        table += " | " + str(i)
    table += " |\n|" + "-" * 30
    for i in range(mingoal, maxgoal + 1):
        table += "|---"
    table += " |\n"
    for s in m.markup.get_goals():
        parts = s.split(";", 3)
        goal = int(parts[1].strip() or "0")
        itemtext = parts[2].strip() or ""
        table += "| " + itemtext
        for i in range(mingoal, maxgoal + 1):
            text = "  "
            if i == goal:
                text = " o "
            table += "|" + text
        table += " |\n"

    template += table
    result = (
        template  # render_template_string(template.strip(), **attr.asdict(m.markup), )
    )
    return result


def goaltable_answer(args: GoalTableAnswerModel) -> PluginAnswerResp:
    web: PluginAnswerWeb = {}
    result: PluginAnswerResp = {"web": web}
    c = args.input.c

    nosave = args.input.nosave

    if not nosave:
        save = {"c": c}
        result["save"] = save
        web["result"] = "saved"

    return result


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


def goaltable_reqs() -> PluginReqs:
    return {
        "js": ["/field/js/build/goaltable.js"],
        "css": ["/field/css/field.css"],
        "multihtml": True,
        "multimd": True,
        "editor_tabs": [
            {
                "text": "Fields",
                "items": [
                    {
                        "text": "Tables",
                        "items": [
                            {
                                "data": templates[0],
                                "text": "GoalTable",
                                "expl": "Taulukko esimerkiksi osaamistavoitteiden tekemiseksi",
                            },
                        ],
                    },
                ],
            },
        ],
    }


goaltable_route = create_blueprint(
    __name__,
    "goaltable",
    GoalTableHtmlModel,
    GoalTableAnswerModel,
    goaltable_answer,
    goaltable_reqs,
)
