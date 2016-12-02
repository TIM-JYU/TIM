from copy import deepcopy

from typing import Tuple, Optional, Union

from datetime import datetime

import yaml

from documentmodel.docparagraph import DocParagraph
from documentmodel.document import Document
from markdownconverter import expand_macros
from timdb.models.user import User
from timdb.timdbexception import TimDbException
from utils import parse_yaml, merge

date_format = '%Y-%m-%d %H:%M:%S'


class Plugin:
    deadline_key = 'deadline'
    starttime_key = 'starttime'
    points_rule_key = 'pointsRule'
    answer_limit_key = 'answerLimit'
    limit_defaults = {'mmcq': 1}

    def __init__(self, task_id: Optional[str], values: dict, plugin_type: str, par: Optional[DocParagraph]=None):
        self.task_id = task_id
        self.values = values
        self.type = plugin_type
        self.par = par

    @staticmethod
    def get_date(d):
        if type(d) is str: d = datetime.strptime(d, "%Y-%m-%d %H:%M:%S")
        return d

    @staticmethod
    def from_task_id(task_id: str, user: Optional[User]=None):
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
    def from_paragraph(par: DocParagraph, user:Optional[User]=None):
        doc = par.doc
        if not par.is_plugin():
            raise TimDbException('The paragraph {} is not a plugin.'.format(par.get_id()))
        task_id_name = par.get_attr('taskId')
        plugin_data = parse_plugin_values(par,
                                          global_attrs=doc.get_settings().global_plugin_attrs(),
                                          macros=doc.get_settings().get_macros_with_user_specific(user),
                                          macro_delimiter=doc.get_settings().get_macro_delimiter())
        if 'error' in plugin_data:
            if type(plugin_data) is str:
                raise PluginException(plugin_data + ' Task id: ' + task_id_name)
            raise PluginException(plugin_data['error'] + ' Task id: ' + task_id_name)
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
        doc_id = int(pieces[0])
        task_id_name = pieces[1] if pieces[1] else None
        par_id = pieces[2] if len(pieces) == 3 else None
        return doc_id, task_id_name, par_id

    def deadline(self, default=None):
        return self.get_date(self.values.get(self.deadline_key, default))

    def starttime(self, default=None):
        return self.get_date(self.values.get(self.starttime_key, default))

    def points_rule(self, default=None):
        pr = self.values.get(self.points_rule_key, default)
        if pr: return pr
        return self.values.get("-"+self.points_rule_key, default)

    def max_points(self, default=None):
        return self.points_rule({}).get('maxPoints', default)

    def user_min_points(self, default=None):
        return self.points_rule({}).get('allowUserMin', default)

    def user_max_points(self, default=None):
        return self.points_rule({}).get('allowUserMax', default)

    def answer_limit(self):
        return self.values.get(self.answer_limit_key, self.limit_defaults.get(self.type))

    def points_multiplier(self, default=1):
        return self.points_rule({}).get('multiplier', default)

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
            raise PluginException('Points must be in range [{},{}]'.format(points_min, points_max))
        return points

    def to_paragraph(self) -> DocParagraph:
        text = '```\n' + yaml.dump(self.values) + '\n```'
        if self.task_id:
            return DocParagraph.create(self.par.doc, par_id=self.par.get_id(), md=text, attrs={'taskId': self.task_id, 'plugin': self.type})
        else:
            return DocParagraph.create(self.par.doc, par_id=self.par.get_id(), md=text, attrs={'plugin': self.type})

    def set_value(self, key: str, value):
        self.values[key] = value
        return self

    def save(self):
        self.to_paragraph().save()


class PluginException(Exception):
    """The exception that is thrown when an error occurs during a plugin call."""
    pass


def parse_plugin_values(par,
                        global_attrs=None,
                        macros=None,
                        macro_delimiter=None):
    """

    :type par: DocParagraph
    :return:
    :rtype: dict
    """
    try:
        # We get the yaml str by removing the first and last lines of the paragraph markup
        par_md = par.get_markdown()
        yaml_str = expand_macros(par_md[par_md.index('\n') + 1:par_md.rindex('\n')],
                                 macros=macros,
                                 macro_delimiter=macro_delimiter)
        #print("yaml str is: " + yaml_str)
        values = parse_yaml(yaml_str)
        if type(values) is str:
            return {'error': "YAML is malformed: " + values}
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
