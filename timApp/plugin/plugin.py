import html
import re
from copy import deepcopy
from dataclasses import dataclass, field
from datetime import datetime, timezone
from enum import Enum
from typing import Tuple, Optional, Union, Iterable, Dict, Generator, Match, List, Any

import yaml
from jinja2 import Environment, BaseLoader
from jinja2.sandbox import SandboxedEnvironment
from marshmallow import missing, ValidationError

from timApp.answer.answer import Answer
from timApp.document.docentry import DocEntry
from timApp.document.docinfo import DocInfo
from timApp.document.docparagraph import DocParagraph
from timApp.document.document import Document
from timApp.document.editing.globalparid import GlobalParId
from timApp.document.macroinfo import MacroInfo
from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import ViewContext
from timApp.document.yamlblock import strip_code_block, YamlBlock, merge
from timApp.markdown.markdownconverter import expand_macros
from timApp.plugin.pluginOutputFormat import PluginOutputFormat
from timApp.plugin.pluginexception import PluginException
from timApp.plugin.plugintype import CONTENT_FIELD_NAME_MAP, PluginType
from timApp.plugin.taskid import TaskId, UnvalidatedTaskId, TaskIdAccess
from timApp.printing.printsettings import PrintFormat
from timApp.timdb.exceptions import TimDbException
from timApp.user.user import User
from timApp.util.rndutils import myhash, SeedClass
from timApp.util.utils import try_load_json, get_current_time, Range
from tim_common.markupmodels import PointsRule, KnownMarkupFields
from tim_common.marshmallow_dataclass import class_schema

date_format = '%Y-%m-%d %H:%M:%S'
AUTOMD = 'automd'

LAZYSTART = "<!--lazy "
LAZYEND = " lazy-->"
NOLAZY = "<!--nolazy-->"
NEVERLAZY = "NEVERLAZY"

JINJAENV = Environment(loader=BaseLoader)

PLG_RTEMPLATE = JINJAENV.from_string("""
<tim-plugin-loader type="{{abtype}}"
answer-id="{{aid or ''}}"
class="{{plgclass}}"
task-id="{{doc_task_id or ''}}">{{cont|safe}}</tim-plugin-loader>""".replace("\n", " "))


def render_template_string2(source, **context):
    """Renders a template from the given template source string
    with the given context. Template variables will be autoescaped.

    :param source: the source code of the template to be
                   rendered
    :param context: the variables that should be available in the
                    context of the template.
    """
    rtemplate = JINJAENV.from_string(source)
    return rtemplate.render(**context)


def render_template_string3(rtemplate, **context):
    """Renders a template from the given template source string
    with the given context. Template variables will be autoescaped.

    :param rtemplate: ready made render template
    :param context: the variables that should be available in the
                    context of the template.
    """
    res = rtemplate.render(**context)
    return res


# Maintains a mapping of plugin types to names of plugins' content field.
# Required if plugin wants to refer to a non-content field (such as points)
# because TIM does not know the structure of plugin state.

CONTENT_FIELD_TYPE_MAP = {
    'numericfield': float,
}

NEVERLAZY_PLUGINS = {
    'textfield',
    'multisave',
    'numericfield',
    'jsrunner',
    'tableForm',
    'timMenu',
    'cbcountfield',
    'cbfield',
    'rbfield',
    'dropdown',
    'userSelect',
}

NO_ANSWERBROWSER_PLUGINS = {
    'multisave',
    'jsrunner',
    'tableForm',
    'importData',
    'userSelect',
}

ALLOW_STYLES_PLUGINS = {
    'textfield',
    'numericfield',
}

WANT_FIELDS = {
    'csPlugin'
}


class PluginWrap(Enum):
    Nothing = 1
    NoLoader = 2
    Full = 3


@dataclass
class PluginRenderOptions:
    user_ctx: UserContext
    do_lazy: bool
    user_print: bool
    preview: bool
    target_format: PrintFormat
    output_format: PluginOutputFormat
    review: bool
    wraptype: PluginWrap
    viewmode: bool

    @property
    def is_html(self):
        return self.output_format == PluginOutputFormat.HTML


def get_value(values, key, default=None):
    """
    Gets the value either from key or -key
    :param values: dict where to find
    :param key: key to use
    :param default: value returned if key not found from either of key or -key
    :return: value for key, -key or default
    """
    if not values:
        return default
    if key in values:
        return values.get(key, default)
    if '-' + key in values:
        return values.get('-' + key, default)
    return default


def get_num_value(values, key, default=None):
    """
    Gets the value either from key or -key
    :param values: dict where to find
    :param key: key to use
    :param default: value returned if key not found from either of key or -key
    :return: value for key, -key or default
    """
    value = get_value(values, key, default)
    # noinspection PyBroadException
    try:
        value = float(value)
    except:
        value = default
    return value


KnownMarkupFieldsSchema = class_schema(KnownMarkupFields)()


class Plugin:
    deadline_key = 'deadline'
    starttime_key = 'starttime'
    points_rule_key = 'pointsRule'
    answer_limit_key = 'answerLimit'
    limit_defaults = {'mmcq': 1, 'mmcq2': 1, 'mcq': 1, 'mcq2': 1}

    def __init__(self, task_id: Optional[TaskId],
                 values: dict,
                 plugin_type: str,
                 par: DocParagraph):
        self.answer: Optional[Answer] = None
        self.answer_count = None
        self.options: Optional[PluginRenderOptions] = None
        self.task_id = task_id
        if task_id and (task_id.doc_id == par.doc.doc_id or not task_id.doc_id):
            # self.task_id = TaskId.parse(task_id, require_doc_id=False)
            # TODO check if par can be None here
            self.task_id.update_doc_id_from_block(par)
            self.task_id.maybe_set_hint(par.get_id())
            if par.get_attr('readonly') == 'view' and not self.task_id.access_specifier:
                self.task_id.access_specifier = TaskIdAccess.ReadOnly
        assert isinstance(values, dict)
        self.values = values
        try:
            self.known: KnownMarkupFields = KnownMarkupFieldsSchema.load(
                {k: v for k, v in values.items()},
                unknown='EXCLUDE',
            )
        except ValidationError as e:
            raise PluginException(f'Invalid markup: {e}') from e
        self.type = plugin_type
        self.ptype = PluginType.resolve(plugin_type)
        self.par = par
        self.output = None
        self.plugin_lazy = None

    # TODO don't set task_id in HTML or JSON at all if there isn't one.
    #  Currently at least csPlugin cannot handle taskID being None.
    @property
    def fake_task_id(self):
        return f'{self.par.doc.doc_id}..{self.par.get_id()}'

    @staticmethod
    def from_global_par(global_par_id: GlobalParId,
                        user_ctx: UserContext,
                        view_ctx: ViewContext, ) -> Tuple['Plugin', DocInfo]:
        doc = DocEntry.find_by_id(global_par_id.doc_id)
        if not doc:
            raise PluginException(f'Document not found: {global_par_id.doc_id}')
        # Check for par here to prevent potential TimDbException
        if not doc.document.has_paragraph(global_par_id.par_id):
            raise PluginException(doc.document.get_par_not_found_msg(global_par_id.par_id))
        par = doc.document.get_paragraph(global_par_id.par_id)
        return Plugin.from_paragraph(par, view_ctx, user_ctx), doc

    @staticmethod
    def from_task_id(
            task_id: str,
            user_ctx: UserContext,
            view_ctx: ViewContext,
            cached_doc: Optional[DocInfo] = None,
    ) -> Tuple['Plugin', DocInfo]:
        tid = TaskId.parse(task_id)
        if not cached_doc:
            d = DocEntry.find_by_id(tid.doc_id)
            if not d:
                raise PluginException(f'Document not found: {tid.doc_id}')
        else:
            d = cached_doc
            assert d.id == tid.doc_id
        doc = d.document
        doc.insert_preamble_pars()
        return find_plugin_from_document(doc, tid, user_ctx, view_ctx), d

    @staticmethod
    def from_paragraph(par: DocParagraph, view_ctx: ViewContext, user: Optional[UserContext] = None):
        doc = par.doc
        if not par.is_plugin():
            raise PluginException(f'The paragraph {par.get_id()} is not a plugin.')
        task_id_name = par.get_attr('taskId')
        plugin_name = par.get_attr('plugin')
        rnd_seed = get_simple_hash_from_par_and_user(par, user)  # TODO: RND_SEED get users rnd_seed for this plugin
        if user.answer_nr >= 0:
            rnd_seed = SeedClass(rnd_seed, user.answer_nr)#
        par.insert_rnds(rnd_seed)
        plugin_data = parse_plugin_values(
            par,
            global_attrs=doc.get_settings().global_plugin_attrs(),
            macroinfo=doc.get_settings().get_macroinfo(view_ctx, user),
        )
        p = Plugin(
            TaskId.parse(task_id_name, require_doc_id=False, allow_block_hint=False) if task_id_name else None,
            plugin_data,
            plugin_name,
            par=par,
        )
        return p

    def deadline(self, default=None):
        return self.known.deadline or default

    def starttime(self, default=None):
        return self.known.starttime or default

    def points_rule(self):
        return self.known.pointsRule or PointsRule(
            maxPoints=missing,
            allowUserMax=missing,
            allowUserMin=missing,
            multiplier=missing,
        )

    def max_points(self, default=None) -> Optional[str]:
        if self.known.pointsRule and self.known.pointsRule.maxPoints is not missing:
            return self.known.pointsRule.maxPoints
        return default

    def user_min_points(self, default=None):
        if self.known.pointsRule and self.known.pointsRule.allowUserMin is not missing:
            return self.known.pointsRule.allowUserMin
        return default

    def user_max_points(self, default=None):
        if self.known.pointsRule and self.known.pointsRule.allowUserMax is not missing:
            return self.known.pointsRule.allowUserMax
        return default

    def answer_limit(self) -> Optional[int]:
        if self.known.answerLimit is not missing:
            return self.known.answerLimit
        return self.limit_defaults.get(self.type)

    def show_points(self):
        return self.known.showPoints

    def points_multiplier(self, default=1):
        if self.known.pointsRule and self.known.pointsRule.multiplier is not missing:
            return self.known.pointsRule.multiplier
        return default

    def validate_points(self, points: Union[str, float]):
        try:
            points = float(points)
        except (ValueError, TypeError):
            raise PluginException('Invalid points format.')
        points_min = self.user_min_points()
        points_max = self.user_max_points()
        if points_min is None or points_max is None:
            raise PluginException('You cannot give yourself custom points in this task.')
        elif not (points_min <= points <= points_max):
            raise PluginException(f'Points must be in range [{points_min},{points_max}]')
        return points

    def to_paragraph(self, max_attr_width: Optional[float] = None) -> DocParagraph:
        yaml.Dumper.ignore_aliases = lambda *args: True
        text = '```\n' + yaml.dump(self.values, allow_unicode=True, default_flow_style=False,
                                   width=max_attr_width) + '\n```'
        attrs = {}
        if self.par:
            attrs = self.par.attrs
        if self.task_id:
            attrs['task_id'] = self.task_id.task_name
        attrs['plugin'] = self.type

        return DocParagraph.create(self.par.doc, par_id=self.par.get_id(), md=text, attrs=attrs)

    def set_value(self, key: str, value):
        self.values[key] = value
        return self

    def save(self, max_attr_width: Optional[float] = None) -> None:
        self.to_paragraph(max_attr_width).save()

    def get_info(self, users: Iterable[User], old_answers: int, look_answer: bool = False, valid: bool = True):
        user_ids = ';'.join([u.name for u in users])
        return {
            # number of earlier answers
            # TODO: this is None when browsing answers with answer browser; should determine the number of answers
            # posted before the current one
            'earlier_answers': old_answers,
            'max_answers': self.answer_limit(),
            'user_id': user_ids,
            # indicates whether we are just looking at an answer, not actually posting a new one
            'look_answer': look_answer,
            'valid': valid
        }

    def set_render_options(self, answer: Optional[Tuple[Answer, int]], options: PluginRenderOptions):
        if answer:
            self.answer, self.answer_count = answer
        self.options = options

    def render_json(self) -> Dict[str, Any]:
        options = self.options
        user = options.user_ctx.user
        if self.answer is not None:
            if self.task_id.is_points_ref:
                p = f'{self.answer.points:g}' if self.answer.points is not None else ''
                state = {self.ptype.get_content_field_name(): p}
            else:
                state = try_load_json(self.answer.content)
            # if isinstance(state, dict) and options.user is not None:
            if user.logged_in:
                info = self.get_info([user], old_answers=self.answer_count, valid=self.answer.valid)
            else:
                info = None
        else:
            state = None
            info = None
        access = {}
        if self.task_id and self.task_id.access_specifier == TaskIdAccess.ReadOnly and \
                not options.user_ctx.logged_user.has_teacher_access(self.par.doc.get_docinfo()):
            access = {'access': self.task_id.access_specifier.value}
        return {"markup": self.values,
                **access,
                "state": state,
                "taskID": self.task_id.doc_task if self.task_id else self.fake_task_id,
                "taskIDExt": self.task_id.extended_or_doc_task if self.task_id else self.fake_task_id,
                "doLazy": (options.do_lazy and self.type not in NEVERLAZY_PLUGINS) if isinstance(options.do_lazy,
                                                                                                 bool) else options.do_lazy,
                "userPrint": options.user_print,
                # added preview here so that whether or not the window is in preview can be
                # checked in python so that decisions on what data is sent can be made.
                "preview": options.preview,
                "viewmode": options.viewmode,
                "anonymous": not user.logged_in,
                "info": info,
                "user_id": user.name if user.logged_in else 'Anonymous',
                "targetFormat": options.target_format.value,
                "review": options.review,
                'current_user_id': options.user_ctx.logged_user.name,
                }

    def get_content_field_name(self):
        return CONTENT_FIELD_NAME_MAP.get(self.type, 'c')

    def allow_styles_field(self):
        return self.type in ALLOW_STYLES_PLUGINS

    def is_answer_valid(self, old_answers, tim_info):
        """Determines whether the currently posted answer should be considered valid.

        :param old_answers: The number of old answers for this task for the current user.
        :param tim_info: The tim_info structure returned by the plugin or empty object.
        :return: True if the answer should be considered valid, False otherwise.

        """
        answer_limit = self.answer_limit()
        if answer_limit is not None and (answer_limit <= old_answers):
            return False, 'You have exceeded the answering limit.'
        if self.starttime(default=datetime(1970, 1, 1, tzinfo=timezone.utc)) > get_current_time():
            return False, 'You cannot submit answers yet.'
        if self.deadline(default=datetime.max.replace(tzinfo=timezone.utc)) < get_current_time():
            return False, 'The deadline for submitting answers has passed.'
        if tim_info.get('notValid', None):
            return False, tim_info.get('validMsg', 'Answer is not valid')
        valid = tim_info.get('valid', True)
        valid_msg = tim_info.get('validMsg', "ok")
        return valid, valid_msg

    def is_cached(self) -> bool:
        cached = self.known.cache
        if cached is not missing:
            return bool(cached)  # Cast potential None to False
        return self.type == 'graphviz' and self.values.get('gvData') is not None  # Graphviz is cached by default

    def is_lazy(self) -> bool:
        if self.type in NEVERLAZY_PLUGINS:
            return False
        do_lazy = self.options.do_lazy
        plugin_lazy = self.plugin_lazy
        html = self.output
        if do_lazy == NEVERLAZY:
            return False
        markup_lazy = self.known.lazy
        if markup_lazy == False:
            return False  # user do not want lazy
        if self.is_cached():
            return False  # cache never lazy
        if not do_lazy and markup_lazy != True:
            return False
        if html is not None and html.find(NOLAZY) >= 0:
            return False  # not allowed to make lazy
        if markup_lazy == True:
            return True  # user wants lazy
        if plugin_lazy == False:
            return False
        return True

    def is_automd_enabled(self, default=False):
        if self.known.automd is not missing:
            return self.known.automd
        return default

    def set_output(self, output: str):
        self.output = output

    def get_answerbrowser_type(self):
        if self.is_cached():
            return None
        if self.par.is_question():
            return None
        # Some plugins don't have answers but they may still need to be loaded lazily.
        # We sometimes want answerbrowser for graphviz too, so we don't exclude it here.
        if self.type.startswith('show') or self.type in NO_ANSWERBROWSER_PLUGINS:
            return 'lazyonly' if self.is_lazy() else None
        return 'full'

    def get_container_class(self):
        return f'plugin{self.type}'

    def get_wrapper_tag(self):
        return 'div'

    def get_final_output(self):
        out = self.output
        if self.is_lazy() and out.find(LAZYSTART) < 0:
            header = self.known.header or self.known.headerText or ''
            stem = self.known.stem or 'Open plugin'
            # Plugins are possibly not visible in lazy form at all if both header and stem are empty,
            # so we add a placeholder in that case. This is the case for at least mcq and mmcq.
            if not header.strip() and not stem.strip():
                stem = "+ question"
            out = f'{LAZYSTART}{out}{LAZYEND}<span style="font-weight:bold">{header}</span><div><p>{stem}</p></div>'

        # Create min and max height for div
        style = ''
        mh = self.known.minHeight
        if mh:
            style = f'min-height:{html.escape(str(mh))};'
        mh = self.known.maxHeight
        if mh:
            style += f'max-height:{html.escape(str(mh))};overflow-y:auto;'
        if style:
            style = f'style="{style}"'

        html_task_id = self.task_id.extended_or_doc_task if self.task_id else self.fake_task_id
        doc_task_id = self.task_id.doc_task_with_field if self.task_id else None
        tag = self.get_wrapper_tag()
        if self.options.wraptype != PluginWrap.Nothing:
            abtype = self.get_answerbrowser_type()
            cont = f"""<{tag} id='{html_task_id}' data-plugin='/{self.type}' {style}>{out}</{tag}>""".strip()
            if abtype and self.options.wraptype == PluginWrap.Full and False:
                return f"""
<tim-plugin-loader type="{abtype}" answer-id="{self.answer.id if self.answer else None or ''}"
                   class="{self.get_container_class()}"
                   task-id="{doc_task_id or ''}">""".replace("\n", "") + \
                       cont + "</tim-plugin-loader>"  # 0.001 sec
            if abtype and self.options.wraptype == PluginWrap.Full:  # and False
                ret = render_template_string3(  # TODO: 0.05 sec
                    PLG_RTEMPLATE,
                    abtype=abtype,
                    plgclass=self.get_container_class(),
                    doc_task_id=doc_task_id,
                    cont=cont,
                    aid=self.answer.id if self.answer else None,
                )
                return ret  # .replace("\n", "") # TODO: for some reason this is important for tables
            if abtype and self.options.wraptype == PluginWrap.Full:
                return render_template_string2(
                    # TODO: 0.05 sec with rts3 2.3 sec with rts2, 8.5 sec with rts, 0.0001 sec with f
                    """
<tim-plugin-loader type="{{abtype}}"
                   answer-id="{{aid or ''}}"
                   class="{{plgclass}}"
                   task-id="{{doc_task_id or ''}}">{{cont|safe}}</tim-plugin-loader>""",
                    abtype=abtype,
                    answer_count=self.answer_count,
                    html_task_id=html_task_id,
                    out=out,
                    plgclass=self.get_container_class(),
                    style=style,
                    tag=tag,
                    type=self.type,
                    doc_task_id=doc_task_id,
                    cont=cont,
                    aid=self.answer.id if self.answer else None,
                ).replace("\n", "")
            else:
                return cont
        return out


def parse_plugin_values_macros(par: DocParagraph,
                               global_attrs: Dict[str, str],
                               macros: Dict[str, object],
                               env: SandboxedEnvironment) -> Dict:
    """
    Parses the markup values for a plugin paragraph, taking document attributes and macros into account.

    :param par: The plugin paragraph.
    :param global_attrs: Global (Document) attributes.
    :param macros: Dict of macros
    :param env: macro environment
    :return: The parsed markup values.
    """
    yaml_str = expand_macros_for_plugin(par, macros, env)
    return load_markup_from_yaml(yaml_str, global_attrs, par.get_attr('plugin'))


def expand_macros_for_plugin(par: DocParagraph, macros, env: SandboxedEnvironment):
    par_md = par.get_markdown()
    rnd_macros = par.get_rands()
    if rnd_macros:
        macros = {**macros, **rnd_macros}
    yaml_str = strip_code_block(par_md)
    if not par.get_nomacros():
        yaml_str = expand_macros(
            yaml_str,
            macros=macros,
            settings=par.doc.get_settings(),
            env=env,
        )
    return yaml_str


def load_markup_from_yaml(yaml_str: str, global_attrs: Dict[str, str], plugin_type: str):
    try:
        values = YamlBlock.from_markdown(yaml_str).values
    except Exception:
        raise PluginException("YAML is malformed: " + yaml_str)
    if global_attrs:
        if isinstance(global_attrs, str):
            raise PluginException('global_plugin_attrs should be a dict, not str')
        global_attrs = deepcopy(global_attrs)
        final_values = global_attrs.get('all')
        if not isinstance(final_values, dict):
            final_values = {}
        plugin_type_globals = global_attrs.get(plugin_type)
        if not isinstance(plugin_type_globals, dict):
            plugin_type_globals = {}
        merge(final_values, plugin_type_globals)
        merge(final_values, values)
        values = final_values
    return values


def parse_plugin_values(
        par: DocParagraph,
        global_attrs: Dict[str, str],
        macroinfo: MacroInfo,
) -> Dict:
    return parse_plugin_values_macros(par, global_attrs, macroinfo.get_macros(), macroinfo.jinja_env)


TASK_MATCH_PROG = re.compile(r'{#([\.\w:]*)([\s\S]*?)?#}')  # see https://regex101.com/r/XmnIZv/33


def find_inline_plugins_from_str(md) -> Generator[
    Tuple[UnvalidatedTaskId, Optional[str], Range, str], None, None]:
    # TODO make task id optional
    matches: Iterable[Match] = TASK_MATCH_PROG.finditer(md)
    for m in matches:
        task_str = m.group(1)
        task_id = UnvalidatedTaskId(task_str)
        p_yaml = m.group(2)
        p_range = (m.start(), m.end())
        yield task_id, p_yaml, p_range, md


def find_inline_plugins(block: DocParagraph, macroinfo: MacroInfo) -> Generator[
    Tuple[UnvalidatedTaskId, Optional[str], Range, str], None, None]:
    md = block.get_expanded_markdown(macroinfo=macroinfo)
    return find_inline_plugins_from_str(md)


def maybe_get_plugin_from_par(
        p: DocParagraph,
        task_id: TaskId,
        u: UserContext,
        view_ctx: ViewContext,
        match_exact_document: bool = False,
) -> Optional[Plugin]:
    t_attr = p.get_attr('taskId')
    if t_attr and p.get_attr('plugin'):
        try:
            p_tid = TaskId.parse(t_attr, allow_block_hint=False, require_doc_id=False)
        except PluginException:
            return None
        doc_id_match = not match_exact_document or match_exact_document and p.doc.doc_id == task_id.doc_id
        if ((p_tid.task_name == task_id.task_name and doc_id_match) or
                (task_id.doc_id and p_tid.doc_id and p_tid.doc_task == task_id.doc_task)):
            return Plugin.from_paragraph(p, view_ctx, user=u)
    def_plug = p.get_attr('defaultplugin')
    if def_plug:
        settings = p.doc.get_settings()
        for p_task_id, p_yaml, p_range, md in find_inline_plugins(
                block=p,
                macroinfo=settings.get_macroinfo(view_ctx, user_ctx=u),
        ):
            p_task_id = p_task_id.validate()
            if p_task_id.task_name != task_id.task_name:
                continue
            plugin_type = p_task_id.plugin_type or def_plug
            y = load_markup_from_yaml(finalize_inline_yaml(p_yaml), settings.global_plugin_attrs(), plugin_type)
            return InlinePlugin(
                task_id=p_task_id,
                values=y,
                plugin_type=plugin_type,
                p_range=p_range,
                par=p,
            )
    return None


class TaskNotFoundException(PluginException):
    """The exception that is thrown when a task cannot be found."""
    pass


@dataclass
class CachedPluginFinder:
    doc_map: Dict[int, DocInfo]
    curr_user: UserContext
    view_ctx: ViewContext
    cache: Dict[str, Optional[Plugin]] = field(default_factory=dict)

    def find(self, task_id: TaskId) -> Optional[Plugin]:
        cached = self.cache.get(task_id.doc_task, missing)
        if cached is not missing:
            return cached
        try:
            p = find_plugin_from_document(self.doc_map[task_id.doc_id].document, task_id, self.curr_user, self.view_ctx)
        except TaskNotFoundException:
            self.cache[task_id.doc_task] = None
            return None
        else:
            self.cache[task_id.doc_task] = p
            return p


def find_plugin_from_document(d: Document, task_id: TaskId, u: UserContext, view_ctx: ViewContext) -> Plugin:
    used_hint = False
    with d.__iter__() as it:
        for p in it:
            if task_id.block_id_hint and p.get_id() != task_id.block_id_hint:
                used_hint = True
                continue
            if p.is_reference():
                try:
                    ref_pars = p.get_referenced_pars()
                except TimDbException:  # Ignore invalid references
                    continue
                else:
                    for rp in ref_pars:
                        plug = maybe_get_plugin_from_par(rp, task_id, u, view_ctx, True)
                        if plug:
                            return plug
            plug = maybe_get_plugin_from_par(p, task_id, u, view_ctx)
            if plug:
                return plug

    err_msg = f'Task not found in the document: {task_id.task_name}'
    if used_hint:
        err_msg += ' (potentially because of wrong block id hint)'
    raise TaskNotFoundException(err_msg)


class InlinePlugin(Plugin):
    def __init__(
            self,
            task_id: Optional[TaskId],
            values: dict,
            plugin_type: str,
            p_range: Range,
            par: Optional[DocParagraph] = None,
    ):
        super().__init__(task_id, values, plugin_type, par)
        self.range = p_range

    def get_container_class(self):
        return f'{super().get_container_class()} inlineplugin'

    def get_wrapper_tag(self):
        return 'span'


def finalize_inline_yaml(p_yaml: Optional[str]):
    if not p_yaml:
        return ''
    if '\n' not in p_yaml:
        return f'{{{p_yaml}}}'
    return p_yaml


def find_task_ids(
        blocks: List[DocParagraph],
        view_ctx: ViewContext,
        user_ctx: UserContext,
        check_access=True,
) -> Tuple[List[TaskId], int, List[TaskId]]:
    """Finds all task plugins from the given list of paragraphs and returns their ids.
    :param user_ctx:
    """
    task_ids = []
    plugin_count = 0
    access_missing = []
    curr_user = user_ctx.logged_user

    def handle_taskid(t: TaskId):
        if not t.doc_id:
            t.update_doc_id_from_block(block)
        elif check_access:
            b = DocEntry.find_by_id(t.doc_id)
            if b and not curr_user.has_seeanswers_access(b):
                access_missing.append(t)
                return True

    for block in blocks:
        task_id = block.get_attr('taskId')
        plugin = block.get_attr('plugin')
        if plugin:
            plugin_count += 1
            if task_id:
                try:
                    tid = TaskId.parse(task_id, require_doc_id=False, allow_block_hint=False)
                except PluginException:
                    continue
                if handle_taskid(tid):
                    continue
                task_ids.append(tid)
        elif block.get_attr('defaultplugin'):
            for task_id, _, _, _ in find_inline_plugins(block, block.doc.get_settings().get_macroinfo(view_ctx)):
                try:
                    task_id = task_id.validate()
                except PluginException:
                    continue
                plugin_count += 1
                if handle_taskid(task_id):
                    continue
                task_ids.append(task_id)
    return task_ids, plugin_count, access_missing


def get_simple_hash_from_par_and_user(block: DocParagraph, uc: Optional[UserContext]) -> int:
    """
    Get simple int hash from TIM's document block and user.
    :param block: TIM's document block
    :param uc: The user context.
    :return: simple hash that can be used for example as a seed for random number generator
    """
    h = str(block.get_id()) + str(block.get_doc_id())
    if uc:
        h += uc.user.name
    rnd_seed = myhash(h) & 0xffffffff
    return rnd_seed
