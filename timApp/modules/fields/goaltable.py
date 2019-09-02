"""
TIM plugin: a radiobutton field
"""
import attr
from flask import jsonify, render_template_string, Blueprint
from marshmallow import fields, post_load
from marshmallow.utils import _Missing
from webargs.flaskparser import use_args

from pluginserver_flask import GenericMarkupModel, GenericMarkupSchema, GenericHtmlSchema, GenericHtmlModel, \
    GenericAnswerSchema, GenericAnswerModel, Missing, \
    InfoSchema, Schema, render_multihtml, render_multimd
import json
from typing import Union, List

from marshmallow.utils import missing

goaltable_route = Blueprint('goaltable', __name__, url_prefix="/goaltable")


@attr.s(auto_attribs=True)
class GoalTableStateModel:
    """Model for the information that is stored in TIM database for each answer."""
    c: Union[dict, _Missing] = None
    styles: Union[dict, _Missing] = None

class GoalTableStateSchema(Schema):
    # c = fields.Raw(required=True, allow_none=True)
    c = fields.Dict(keys=fields.Str(), values=fields.Str())
    styles = fields.Dict(keys=fields.Str(), values=fields.Str())

    @post_load
    def make_obj(self, data):
        res = GoalTableStateModel(**data)
        return res


@attr.s(auto_attribs=True)
class GoalTableMarkupModel(GenericMarkupModel):
    buttonText: Union[str, Missing] = missing
    editText: Union[str, Missing] = missing
    goalText: Union[str, Missing] = missing
    bloom: Union[bool, Missing] = missing
    borders: Union[bool, Missing] = missing
    goals: Union[List[str], Missing] = missing
    goalscale: Union[List[str], Missing] = missing
    mingoal: Union[int, Missing] = missing
    maxgoal: Union[int, Missing] = missing
    initgoal: Union[int, Missing] = missing


class GoalTableMarkupSchema(GenericMarkupSchema):
    buttonText = fields.Str(allow_none=True)
    editText = fields.Str(allow_none=True)
    goalText = fields.Str(allow_none=True)
    bloom = fields.Bool(allow_none=True)
    borders = fields.Bool(allow_none=True)
    goals = fields.List(fields.Str(allow_none=True))
    goalscale = fields.List(fields.Str(allow_none=True))
    mingoal = fields.Int(allow_none=True)
    maxgoal = fields.Int(allow_none=True)
    initgoal = fields.Int(allow_none=True)

    @post_load
    def make_obj(self, data):
        return GoalTableMarkupModel(**data)


@attr.s(auto_attribs=True)
class GoalTableInputModel:
    """Model for the information that is sent from browser (plugin AngularJS component)."""
    # c: str
    c: Union[dict, _Missing] = None
    nosave: bool = missing

class GoalTableInputSchema(Schema):
    # c = fields.Str(required=True)
    c = fields.Dict(keys=fields.Str(), values=fields.Str())
    nosave = fields.Bool()

    @post_load
    def make_obj(self, data):
        return GoalTableInputModel(**data)


class GoalTableAttrs(Schema):
    """Common fields for HTML and answer routes."""
    markup = fields.Nested(GoalTableMarkupSchema)
    state = fields.Nested(GoalTableStateSchema, allow_none=True, required=True)


@attr.s(auto_attribs=True)
class GoalTableHtmlModel(GenericHtmlModel[GoalTableInputModel, GoalTableMarkupModel, GoalTableStateModel]):
    def get_component_html_name(self) -> str:
        return 'goaltable-runner'

    # def show_in_view_default(self) -> bool:
    #    return False

    def get_static_html(self) -> str:
        return render_static_goaltable(self)

    def get_browser_json(self):
        r = super().get_browser_json()
        # r['state']['separator'] = ";"
        return r

    def get_md(self):
        return render_md_goaltable(self)

    def get_review(self):
        if self.state and self.state.c:
            ret = json.dumps(self.state.c)
            ret = ret.replace(",", "\n")
            return f"<pre>{ret}</pre>"

        return "<pre>review</pre>"



class GoalTableHtmlSchema(GoalTableAttrs, GenericHtmlSchema):
    info = fields.Nested(InfoSchema, allow_none=True, required=True)

    @post_load
    def make_obj(self, data):
        # noinspection PyArgumentList
        return GoalTableHtmlModel(**data)


@attr.s(auto_attribs=True)
class GoalTableAnswerModel(GenericAnswerModel[GoalTableInputModel, GoalTableMarkupModel, GoalTableStateModel]):
    pass


class GoalTableAnswerSchema(GoalTableAttrs, GenericAnswerSchema):
    input = fields.Nested(GoalTableInputSchema, required=False)

    @post_load
    def make_obj(self, data):
        # noinspection PyArgumentList
        return GoalTableAnswerModel(**data)


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
    return render_template_string(template.strip(), **attr.asdict(m.markup),
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


GOALTABLE_FIELD_HTML_SCHEMA = GoalTableHtmlSchema()

@goaltable_route.route('/multihtml/', methods=['post'])
@use_args(GenericHtmlSchema(many=True), locations=("json",))
def goaltable_multihtml(args):  # args: List[GenericHtmlSchema]):
    ret = render_multihtml(GOALTABLE_FIELD_HTML_SCHEMA, args)
    return ret


@goaltable_route.route('/multimd/', methods=['post'])
@use_args(GenericHtmlSchema(many=True), locations=("json",))
def goaltable_multimd(args):  # args: List[GenericHtmlSchema]):
    ret = render_multimd(GOALTABLE_FIELD_HTML_SCHEMA, args)
    return ret


@goaltable_route.route('/answer/', methods=['put'])
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

@goaltable_route.route('/reqs/')
@goaltable_route.route('/reqs')
def goaltable_reqs():
    """Introducing templates for cbfield plugin"""
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
