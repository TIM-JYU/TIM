from documentmodel.document import Document
import utils

date_format = '%Y-%m-%d %H:%M:%S'


class Plugin:
    deadline_key = 'deadline'
    starttime_key = 'starttime'
    points_rule_key = 'pointsRule'
    answer_limit_key = 'answerLimit'
    limit_defaults = {'mmcq': 1}

    def __init__(self, task_id, values, plugin_type):
        self.task_id = task_id
        self.values = values
        self.type = plugin_type

    @staticmethod
    def from_task_id(task_id):
        doc_id, task_id_name = Plugin.parse_task_id(task_id)
        doc = Document(doc_id)
        par = doc.get_paragraph_by_task(task_id_name)
        if par is None:
            raise PluginException('Task not found in the document: ' + task_id_name)
        plugin_data = utils.parse_plugin_values(par,
                                                global_attrs=doc.get_settings().global_plugin_attrs(),
                                                macros=doc.get_settings().get_macros(),
                                                macro_delimiter=doc.get_settings().get_macro_delimiter())
        if 'error' in plugin_data:
            if type(plugin_data) is str:
                raise PluginException(plugin_data + ' Task id: ' + task_id_name)
            raise PluginException(plugin_data['error'] + ' Task id: ' + task_id_name)
        p = Plugin(task_id, plugin_data['markup'], par.get_attrs()['plugin'])
        return p

    @staticmethod
    def parse_task_id(task_id):
        """Splits task id into document id and task name.

        :rtype: tuple[int, str]
        :type task_id: str
        :param task_id: task_id that is of the form "22.palindrome"
        :return: tuple of the form (22, "palindrome")
        """
        pieces = task_id.split('.')
        if len(pieces) != 2:
            raise PluginException('The format of task_id is invalid. Expected exactly one dot character.')
        doc_id = int(pieces[0])
        task_id_name = pieces[1]
        return doc_id, task_id_name

    def deadline(self, default=None):
        return self.values.get(self.deadline_key, default)

    def starttime(self, default=None):
        return self.values.get(self.starttime_key, default)

    def points_rule(self, default=None):
        return self.values.get(self.points_rule_key, default)

    def max_points(self, default=None):
        return self.points_rule({}).get('maxPoints', default)

    def user_min_points(self, default=None):
        return self.points_rule({}).get('allowUserMin', default)

    def user_max_points(self, default=None):
        return self.points_rule({}).get('allowUserMax', default)

    def answer_limit(self):
        return self.values.get(self.answer_limit_key, self.limit_defaults.get(self.type))


class PluginException(Exception):
    """The exception that is thrown when an error occurs during a plugin call."""
    pass
