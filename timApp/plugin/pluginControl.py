# -*- coding: utf-8 -*-
"""Functions for dealing with plugin paragraphs."""
import json
from collections import OrderedDict
from typing import List, Tuple, Optional, Dict
from xml.sax.saxutils import quoteattr

import yaml
import yaml.parser

from timApp.auth.accesshelper import has_edit_access
from timApp.document.docparagraph import DocParagraph
from timApp.document.document import dereference_pars, Document
from timApp.document.yamlblock import YamlBlock
from timApp.markdown.dumboclient import call_dumbo
from timApp.markdown.markdownconverter import expand_macros
from timApp.plugin.containerLink import plugin_reqs, get_plugin
from timApp.plugin.containerLink import render_plugin_multi, render_plugin, get_plugins
from timApp.plugin.plugin import Plugin, PluginRenderOptions
from timApp.plugin.pluginOutputFormat import PluginOutputFormat
from timApp.plugin.pluginexception import PluginException
from timApp.printing.printsettings import PrintFormat
from timApp.user.user import User
from timApp.util.rndutils import get_simple_hash_from_par_and_user
from timApp.util.timtiming import taketime
from timApp.util.utils import get_error_html, get_error_tex


def get_error_plugin(plugin_name, message, response=None,
                     plugin_output_format: PluginOutputFormat = PluginOutputFormat.HTML):
    """

    :param response:
    :type message: str
    :type plugin_name: str
    """
    if plugin_output_format == PluginOutputFormat.MD:
        return get_error_tex(f'Plugin {plugin_name} error:', message, response)

    return get_error_html(f'Plugin {plugin_name} error: {message}', response)


def find_task_ids(blocks: List[DocParagraph]) -> Tuple[List[str], int]:
    """Finds all task plugins from the given list of paragraphs and returns their ids."""
    task_ids = []
    plugin_count = 0
    for block in blocks:
        task_id = block.get_attr('taskId')
        plugin = block.get_attr('plugin')
        if plugin:
            plugin_count += 1
        # Need "and plugin" to ignore e.g. manual heading ids
        if task_id and plugin:
            task_ids.append(f"{block.doc.doc_id}.{task_id}")
    return task_ids, plugin_count


def pluginify(doc: Document,
              pars: List[DocParagraph],
              user: Optional[User],
              timdb,
              custom_answer=None,
              sanitize=True,
              do_lazy=False,
              edit_window=False,
              load_states=True,
              review=False,
              wrap_in_div=True,
              output_format: PluginOutputFormat = PluginOutputFormat.HTML,
              user_print: bool = False,
              target_format: PrintFormat=PrintFormat.LATEX,
              dereference=True) -> Tuple[List[DocParagraph], List[str], List[str], List[str]]:
    """"Pluginifies" or sanitizes the specified DocParagraphs by calling the corresponding plugin route for each plugin
    paragraph.

    :param doc Document / DocumentVersion object.
    :param pars: A list of DocParagraphs to be processed.
    :param user: The current user object.
    :param timdb: A reference to the database.
    :param custom_answer: Optional answer that will used as the state for the plugin instead of answer database.
    If this parameter is specified, the expression len(blocks) MUST be 1.
    :param sanitize: Whether the blocks should be sanitized before processing.
    :param do_lazy Whether to use lazy versions of the plugins.
    :param edit_window Whether the method is called from the edit window or not.
    :param output_format: Desired output format (html/md) for plugins
    :param user_print: Whether the plugins should output the original values or user's input (when exporting markdown).
    :param target_format: for MD-print what exact format to use
    :param dereference: should pars be checked id dereference is needed
    :return: Processed HTML blocks along with JavaScript, CSS stylesheet and AngularJS module dependencies.
    """

    taketime("answ", "start")
    if dereference:
        pars = dereference_pars(pars, context_doc=doc)
    if not edit_window and has_edit_access(doc.get_docinfo()):
        for p in pars:
            if p.is_translation_out_of_date():
                p.add_class('tr-outofdate')
    if sanitize:
        for par in pars:
            par.sanitize_html()

    # init these for performance as they stay the same for all pars
    md_out = (output_format == PluginOutputFormat.MD)
    html_out = False if md_out else (output_format == PluginOutputFormat.HTML)

    html_pars = [par.get_final_dict(use_md=md_out) for par in pars]

    # taketime("answ", "sansitize")

    if custom_answer is not None:
        if len(pars) != 1:
            raise PluginException('len(blocks) must be 1 if custom state is specified')
    plugins: Dict[str, Dict[int, Plugin]] = {}
    task_ids = []

    answer_map = {}
    # enum_pars = enumerate(pars)
    plugin_opts = PluginRenderOptions(do_lazy=do_lazy,
                                      user_print=user_print,
                                      preview=edit_window,
                                      target_format=target_format,
                                      user=user,
                                      review=review,
                                      wrap_in_div=wrap_in_div
                                      )

    if load_states and custom_answer is None and user is not None:
        for idx, block in enumerate(pars):  # find taskid's
            attr_taskid = block.get_attr('taskId')
            plugin_name = block.get_attr('plugin')
            if plugin_name and attr_taskid:
                task_id = f"{block.get_doc_id()}.{attr_taskid or ''}"
                if not task_id.endswith('.'):
                    task_ids.append(task_id)
        answers = timdb.answers.get_newest_answers(user.id, task_ids)
        # TODO: RND_SEED get all users rand_seeds for this doc's tasks. New table?
        # Close database here because we won't need it for a while
        timdb.close()
        for answer in answers:
            answer_map[answer['task_id']] = answer

    for idx, block in enumerate(pars):
        attr_taskid = block.get_attr('taskId')
        plugin_name = block.get_attr('plugin')
        is_gamified = block.get_attr('gamification')
        is_gamified = not not is_gamified
        settings = block.doc.get_settings()
        macroinfo = settings.get_macroinfo(user=user)
        macros = macroinfo.get_macros()
        macro_delimiter = macroinfo.get_macro_delimiter()

        if is_gamified:
            # md = block.get_expanded_markdown()  # not enough macros
            md = block.get_markdown()
            try:
                # md = Plugin.from_paragraph_macros(md, global_attrs, macros, macro_delimiter)
                md = expand_macros(md,
                                   macros=macros,
                                   settings=settings,
                                   macro_delimiter=macro_delimiter)

                gamified_data = YamlBlock.from_markdown(md).values
                runner = 'gamification-map'
                html_pars[idx][output_format.value] = f'<{runner} data={quoteattr(json.dumps(gamified_data))}></{runner}>'
            except yaml.YAMLError as e:
                html_pars[idx][output_format.value] = '<div class="error"><p>Gamification error:</p><pre>' + \
                                                      str(e) + \
                                                      '</pre><p>From block:</p><pre>' + \
                                                      md + \
                                                      '</pre></div>'

        if plugin_name:
            task_id = f"{block.get_doc_id()}.{attr_taskid or ''}"
            new_seed = False
            rnd_seed = None
            answer = {}

            if load_states:
                if custom_answer is not None:
                    answer = custom_answer
                else:
                    answer = answer_map.get(task_id, None)
                if answer is not None:
                    rnd_seed = answer.get('rndseed', None)

            if rnd_seed is None:
                rnd_seed = get_simple_hash_from_par_and_user(block, user) # TODO: RND_SEED: get users seed for this plugin
                new_seed = True

            try:
                if block.insert_rnds(rnd_seed) and new_seed:  # do not change order!  inserts must be done
                    # TODO: RND_SEED save rnd_seed to user data
                    pass

                # plugin = Plugin.from_paragraph(block, user)
                joint_macros = macros
                rands = block.get_rands()
                if rands:
                    joint_macros = {**macros, **rands}
                plugin = Plugin.from_paragraph_macros(block,
                                                      settings.global_plugin_attrs(),
                                                      joint_macros,
                                                      macro_delimiter)
                if plugin_name == 'qst':
                    plugin.values['isTask'] = not block.is_question()
            except Exception as e:
                html_pars[idx][output_format.value] = get_error_plugin(plugin_name, str(e),
                                                                       plugin_output_format=output_format)
                continue
            if plugin_name not in plugins:
                plugins[plugin_name] = OrderedDict()

            plugin.set_render_options(answer if load_states and answer is not None else None, plugin_opts)
            plugins[plugin_name][idx] = plugin
        else:
            if block.nocache and not is_gamified:  # get_nocache():
                # if block.get_nocache():
                texts = [block.get_expanded_markdown(macroinfo)]
                htmls = call_dumbo(texts, options=block.get_dumbo_options(base_opts=settings.get_dumbo_options()))
                html_pars[idx][output_format.value] = htmls[0]  # to collect all together before dumbo

                # taketime("answ", "markup", len(plugins))

    js_paths = []
    css_paths = []
    modules = []

    # taketime("answ", "done", len(answers))

    for plugin_name, plugin_block_map in plugins.items():
        taketime("plg", plugin_name)
        try:
            plugin_lazy = get_plugin(plugin_name).get("lazy", True)
            resp = plugin_reqs(plugin_name)
        except PluginException as e:
            for idx in plugin_block_map.keys():
                html_pars[idx][output_format.value] = get_error_plugin(plugin_name, str(e),
                                                                       plugin_output_format=output_format)
            continue
        # taketime("plg e", plugin_name)
        try:
            reqs = json.loads(resp)
            if plugin_name == 'mmcq' or plugin_name == 'mcq':
                reqs['multihtml'] = True
                reqs['multimd'] = True
        except ValueError as e:
            for idx in plugin_block_map.keys():
                html_pars[idx][output_format.value] = get_error_plugin(
                    plugin_name, f'Failed to parse JSON from plugin reqs route: {e}', resp,
                    plugin_output_format=output_format)
            continue
        plugin_js_files, plugin_css_files, plugin_modules = plugin_deps(reqs)
        for src in plugin_js_files:
            if src.startswith("http") or src.startswith("/"):  # absolute URL
                js_paths.append(src)
            elif src.endswith('.js'):  # relative JS URL
                js_paths.append(f"/{plugin_name}/{src}")
            else:  # module name
                js_paths.append(src)
        for src in plugin_css_files:
            if src.startswith("http") or src.startswith("/"):
                css_paths.append(src)
            else:
                css_paths.append(f"/{plugin_name}/{src}")
        for mod in plugin_modules:
            modules.append(mod)

        # Remove duplicates, preserving order
        js_paths = list(OrderedDict.fromkeys(js_paths))
        css_paths = list(OrderedDict.fromkeys(css_paths))
        modules = list(OrderedDict.fromkeys(modules))

        default_auto_md = reqs.get('default_automd', False)

        if (html_out and reqs.get('multihtml')) or (md_out and reqs.get('multimd')):
            try:
                # taketime("plg m", plugin_name)
                response = render_plugin_multi(
                    doc,
                    plugin_name,
                    [val for _, val in plugin_block_map.items()],
                    plugin_output_format=output_format,
                    default_auto_md=default_auto_md)
                # taketime("plg e", plugin_name)
            except PluginException as e:
                for idx in plugin_block_map.keys():
                    html_pars[idx][output_format.value] = get_error_plugin(plugin_name, str(e),
                                                                           plugin_output_format=output_format)
                continue
            try:
                plugin_htmls = json.loads(response)
            except ValueError as e:
                for idx in plugin_block_map.keys():
                    html_pars[idx][output_format.value] = \
                        get_error_plugin(plugin_name,
                                         f'Failed to parse plugin response from multihtml route: {e}',
                                         response, plugin_output_format=output_format)
                continue

            for idx, plugin, html in zip(plugin_block_map.keys(), plugin_block_map.values(), plugin_htmls):
                plugin.plugin_lazy = plugin_lazy
                plugin.set_output(html)
                html_pars[idx]['answerbrowser_type'] = plugin.get_answerbrowser_type()
                html_pars[idx][output_format.value] = plugin.get_final_output()
        else:
            for idx, plugin in plugin_block_map.items():
                if md_out:
                    err_msg_md = "Plugin does not support printing yet. " \
                                 "Please refer to TIM help pages if you want to learn how you can manually " \
                                 "define what to print here."
                    html_pars[idx][output_format.value] = get_error_plugin(plugin_name,
                                                                           err_msg_md,
                                                                           plugin_output_format=output_format)
                else:
                    try:
                        html = render_plugin(doc=doc,
                                             plugin=plugin,
                                             output_format=output_format)
                    except PluginException as e:
                        html_pars[idx][output_format.value] = get_error_plugin(plugin_name, str(e),
                                                                               plugin_output_format=output_format)
                        continue

                    plugin.set_output(html)
                    html_pars[idx]['answerbrowser_type'] = plugin.get_answerbrowser_type()
                    html_pars[idx][output_format.value] = plugin.get_final_output()

    # taketime("phtml done")

    return pars, js_paths, css_paths, modules


def get_markup_value(markup, key, default):
    return markup.get(key, default)


def get_all_reqs():
    allreqs = {}
    for plugin, vals in get_plugins().items():
        if vals.get('skip_reqs', False):
            continue
        try:
            resp = plugin_reqs(plugin)
        except PluginException:
            continue
        try:
            reqs = json.loads(resp)
            allreqs[plugin] = reqs
        except ValueError:
            continue
    return allreqs


def plugin_deps(p: Dict) -> Tuple[List[str], List[str], List[str]]:
    """

    :param p: is json of plugin requirements of the form:
              {"js": ["js.js"], "css":["css.css"], "angularModule":["module"]}
    """
    js_files = []
    modules = []
    css_files = []
    if "css" in p:
        for cssF in p['css']:
            css_files.append(cssF)
    if "js" in p:
        for jsF in p['js']:
            js_files.append(jsF)
    if "angularModule" in p:
        for ng in p['angularModule']:
            modules.append(ng)
    return js_files, css_files, modules
