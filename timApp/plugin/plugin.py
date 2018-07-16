from copy import deepcopy
from datetime import datetime, timezone
from typing import Tuple, Optional, Union, Iterable, Dict, NamedTuple

import yaml

from timApp.document.docparagraph import DocParagraph
from timApp.document.document import Document
from timApp.document.macroinfo import MacroInfo
from timApp.document.yamlblock import strip_code_block, YamlBlock, merge
from timApp.markdown.markdownconverter import expand_macros
from timApp.plugin.pluginexception import PluginException
from timApp.util.rndutils import get_simple_hash_from_par_and_user
from timApp.timdb.exceptions import TimDbException
from timApp.user.user import User
from timApp.util.utils import try_load_json

date_format = '%Y-%m-%d %H:%M:%S'


class PluginRenderOptions(NamedTuple):
    user: Optional[User]
    do_lazy: bool
    user_print: bool
    preview: bool
    target_format: str
    review: bool


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
        float(value)
    except:
        value = default
    return value


def handle_plugin_error(plugin_data, task_id_name):
    if 'error' in plugin_data:
        task_id_error = ' Task id: ' + task_id_name if task_id_name else ''
        if isinstance(plugin_data, str):
            raise PluginException(plugin_data + task_id_error)
        raise PluginException(plugin_data['error'] + task_id_error)


class Plugin:
    deadline_key = 'deadline'
    starttime_key = 'starttime'
    points_rule_key = 'pointsRule'
    answer_limit_key = 'answerLimit'
    limit_defaults = {'mmcq': 1}

    def __init__(self, task_id: Optional[str], values: dict, plugin_type: str, par: Optional[DocParagraph] = None):
        self.answer = None
        self.options: PluginRenderOptions = None
        self.task_id = task_id
        assert isinstance(values, dict)
        self.values = values
        self.type = plugin_type
        self.par = par
        self.points_rule_cache = None  # cache for points rule

    @property
    def full_task_id(self):
        return f'{self.par.doc.doc_id}.{self.task_id or ""}'

    @property
    def task_id_ext(self):
        return f'{self.full_task_id}.{self.par.get_id()}'

    @staticmethod
    def get_date(d):
        if isinstance(d, str):
            try:
                d = datetime.strptime(d, "%Y-%m-%d %H:%M:%S")
            except ValueError:
                raise PluginException(f'Invalid date format: {d}')
        if d is not None:
            if d.tzinfo is None:
                d = d.replace(tzinfo=timezone.utc)
        return d

    @staticmethod
    def from_task_id(task_id: str, user: Optional[User] = None):
        doc_id, task_id_name, par_id = Plugin.parse_task_id(task_id)
        doc = Document(doc_id)
        if par_id is not None:
            try:
                par = doc.get_paragraph(par_id)
            except TimDbException as e:
                raise PluginException(e)
        else:
            try:
                par = doc.get_paragraph_by_task(task_id_name)
            except TimDbException as e:
                raise PluginException(e)
        if par is None:
            raise PluginException('Task not found in the document: ' + task_id_name)
        return Plugin.from_paragraph(par, user)

    @staticmethod
    def from_paragraph_macros(par: DocParagraph, global_attrs: Dict[str, str],
                              macros: Dict[str, object],
                              macro_delimiter: str):
        if not par.is_plugin():
            raise PluginException(f'The paragraph {par.get_id()} is not a plugin.')
        task_id_name = par.get_attr('taskId')
        plugin_data = parse_plugin_values_macros(par, global_attrs, macros, macro_delimiter)
        handle_plugin_error(plugin_data, task_id_name)
        p = Plugin(task_id_name, plugin_data['markup'], par.get_attrs()['plugin'], par=par)
        return p

    @staticmethod
    def from_paragraph(par: DocParagraph, user: Optional[User] = None):
        doc = par.doc
        if not par.is_plugin():
            raise PluginException(f'The paragraph {par.get_id()} is not a plugin.')
        task_id_name = par.get_attr('taskId')
        rnd_seed = get_simple_hash_from_par_and_user(par, user)  # TODO: RND_SEED get users rnd_seed for this plugin
        par.insert_rnds(rnd_seed)
        plugin_data = parse_plugin_values(par,
                                          global_attrs=doc.get_settings().global_plugin_attrs(),
                                          macroinfo=doc.get_settings().get_macroinfo(user))
        handle_plugin_error(plugin_data, task_id_name)
        p = Plugin(task_id_name, plugin_data['markup'], par.get_attrs()['plugin'], par=par)
        return p

    @staticmethod
    def parse_task_id(task_id: str) -> Tuple[int, Optional[str], Optional[str]]:
        """Splits task id into document id and task name.

        :rtype: tuple[int, str]
        :type task_id: str
        :param task_id: task_id that is of the form "22.palindrome"
        :return: tuple of the form (22, "palindrome")

        """
        pieces = task_id.split('.')
        if not 2 <= len(pieces) <= 3:
            raise PluginException('The format of task_id is invalid. Expected 1 or 2 dot characters.')
        try:
            doc_id = int(pieces[0])
        except ValueError:
            raise PluginException(f'The format of task_id is invalid. Expected integral doc id but got {pieces[0]}.')
        task_id_name = pieces[1] if pieces[1] else None
        par_id = pieces[2] if len(pieces) == 3 else None
        return doc_id, task_id_name, par_id

    def deadline(self, default=None):
        return self.get_date(get_value(self.values, self.deadline_key, default))

    def starttime(self, default=None):
        return self.get_date(get_value(self.values, self.starttime_key, default))

    def points_rule(self):
        if self.points_rule_cache is None:
            self.points_rule_cache = get_value(self.values, self.points_rule_key, {})
            if not isinstance(self.points_rule_cache, dict):
                self.points_rule_cache = {}
        return self.points_rule_cache

    def max_points(self, default=None):
        return self.points_rule().get('maxPoints', default)

    def user_min_points(self, default=None):
        return self.points_rule().get('allowUserMin', default)

    def user_max_points(self, default=None):
        return self.points_rule().get('allowUserMax', default)

    def answer_limit(self):
        return get_value(self.values, self.answer_limit_key, self.limit_defaults.get(self.type))

    def points_multiplier(self, default=1):
        return self.points_rule().get('multiplier', default)

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

    def to_paragraph(self) -> DocParagraph:
        text = '```\n' + yaml.dump(self.values, default_flow_style=False) + '\n```'
        attrs = {}
        if self.par:
            attrs = self.par.attrs
        if self.task_id:
            attrs['task_id'] = self.task_id
        attrs['plugin'] = self.type

        return DocParagraph.create(self.par.doc, par_id=self.par.get_id(), md=text, attrs=attrs)

    def set_value(self, key: str, value):
        self.values[key] = value
        return self

    def save(self):
        self.to_paragraph().save()

    def get_info(self, users: Iterable[User], old_answers: int, look_answer: bool = False, valid: bool = True):
        user_ids = ';'.join([u.name for u in users])
        from timApp.auth.sessioninfo import get_current_user_name
        return {
            # number of earlier answers
            # TODO: this is None when browsing answers with answer browser; should determine the number of answers
            # posted before the current one
            'earlier_answers': old_answers,
            'max_answers': self.answer_limit(),
            'current_user_id': get_current_user_name(),
            'user_id': user_ids,
            # indicates whether we are just looking at an answer, not actually posting a new one
            'look_answer': look_answer,
            'valid': valid
        }

    def set_render_options(self, answer, options: PluginRenderOptions):
        self.answer = answer
        self.options = options

    def render_json(self):
        task_id = self.full_task_id
        options = self.options
        if self.answer is not None:
            state = try_load_json(self.answer['content'])
            if isinstance(state, dict) and options.user is not None:
                info = self.get_info([options.user], old_answers=self.answer.get('cnt'), valid=self.answer['valid'])
            else:
                info = None
        else:
            state = None
            info = None
        return {"markup": self.values,
                "state": state,
                "taskID": task_id,
                "taskIDExt": self.task_id_ext,
                "doLazy": options.do_lazy,
                "userPrint": options.user_print,
                # added preview here so that whether or not the window is in preview can be
                # checked in python so that decisions on what data is sent can be made.
                "preview": options.preview,
                "anonymous": options.user is not None,
                "info": info,
                "user_id": options.user.name if options.user is not None else 'Anonymous',
                "targetFormat": options.target_format,
                "review": options.review,
                }

    def is_answer_valid(self, old_answers, tim_info):
        """Determines whether the currently posted answer should be considered valid.

        :param old_answers: The number of old answers for this task for the current user.
        :param tim_info: The tim_info structure returned by the plugin or empty object.
        :return: True if the answer should be considered valid, False otherwise.

        """
        answer_limit = self.answer_limit()
        if answer_limit is not None and (answer_limit <= old_answers):
            return False, 'You have exceeded the answering limit.'
        if self.starttime(default=datetime(1970, 1, 1, tzinfo=timezone.utc)) > datetime.now(timezone.utc):
            return False, 'You cannot submit answers yet.'
        if self.deadline(default=datetime.max.replace(tzinfo=timezone.utc)) < datetime.now(timezone.utc):
            return False, 'The deadline for submitting answers has passed.'
        if tim_info.get('notValid', None):
            return False, 'Answer is not valid'
        return True, 'ok'


    def is_automd_enabled(self):
        if 'automd' in self.values and self.values['automd']:
            return True
        return False


def parse_plugin_values_macros(par: DocParagraph,
                               global_attrs: Dict[str, str],
                               macros: Dict[str, object],
                               macro_delimiter: str) -> Dict:
    """
    Parses the markup values for a plugin paragraph, taking document attributes and macros into account.

    :param par: The plugin paragraph.
    :param global_attrs: Global (Document) attributes.
    :param macros: Dict of macros
    :type macro_delimiter: delimiter for macros
    :return: The parsed markup values.
    """
    try:
        # We get the yaml str by removing the first and last lines of the paragraph markup
        par_md = par.get_markdown()
        rnd_macros = par.get_rands()
        if rnd_macros:
            macros = {**macros, **rnd_macros}
        yaml_str = strip_code_block(par_md)
        if not par.get_nomacros():
            yaml_str = expand_macros(yaml_str,
                                     macros=macros,
                                     macro_delimiter=macro_delimiter)
        try:
            values = YamlBlock.from_markdown(yaml_str).values
        except Exception:
            return {'error': "YAML is malformed: " + yaml_str}
        else:
            if global_attrs:
                if type(global_attrs) is str:
                    return {'error': 'global_plugin_attrs should be a dict, not str'}
                global_attrs = deepcopy(global_attrs)
                final_values = global_attrs.get('all', {})
                merge(final_values, global_attrs.get(par.get_attrs()['plugin'], {}))
                merge(final_values, values)
                values = final_values
            return {"markup": values}
    except Exception as e:
        return {'error': "Unknown error: " + str(e)}


def parse_plugin_values(par: DocParagraph,
                        global_attrs: Dict[str, str],
                        macroinfo: MacroInfo) -> Dict:
    return parse_plugin_values_macros(par, global_attrs, macroinfo.get_macros(), macroinfo.get_macro_delimiter())
