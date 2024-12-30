from dataclasses import field
from typing import Any, Union

from flask_babel import gettext

from timApp.answer.answer import AnswerData
from timApp.answer.answers import (
    ExistingAnswersInfo,
    get_existing_answers_info,
    get_existing_answers_info_multiple_tasks,
    get_existing_answers_info_batch,
)
from timApp.answer.routes import get_postanswer_plugin_etc

# from timApp.answer.routes import InputAnswer, get_postanswer_plugin_etc
from timApp.auth.accesshelper import (
    get_doc_or_abort,
    is_in_answer_review,
    AccessDenied,
    get_origin_from_request,
    get_plugin_from_request,
    TaskAccessVerification,
    verify_task_access,
)
from timApp.auth.accesstype import AccessType
from timApp.auth.get_user_rights_for_item import get_user_rights_for_item
from timApp.auth.sessioninfo import get_current_user_object
from timApp.document.docsettings import DISABLE_ANSWER_REVIEW_MODE
from timApp.document.usercontext import UserContext
from timApp.document.viewcontext import ViewContext, ViewRoute
from timApp.plugin.plugin import Plugin
from timApp.plugin.pluginexception import PluginException
from timApp.plugin.taskid import TaskId, TaskIdAccess
from timApp.user.user import has_no_higher_right, User
from timApp.util.flask.requesthelper import get_urlmacros_from_request


# def post_answer_impl(
#     task_id_ext: str,
#     answerdata: InputAnswer,
#     answer_browser_data: dict,
#     answer_options: dict,
#     curr_user: User,
#     urlmacros: UrlMacros,
#     other_session_users: list[User],
#     origin: OriginInfo | None,
#     error: str | None = None,
# ) -> AnswerRouteResult:

InputAnswer = Union[AnswerData, list[Any], int, float, str]


def mass_answer(
    inputs: dict[str, InputAnswer],
    abData: dict[str, Any] = field(default_factory=dict),
    options: dict[str, Any] = field(default_factory=dict),
):
    # docs = {} #
    tids: dict[str, TaskId] = {}
    doc_id: None | int = None
    for save, inp in inputs.items():
        tid = TaskId.parse(save)
        tids[save] = tid
        if doc_id is None:
            doc_id = tid.doc_id
        elif tid.doc_id != doc_id:
            raise PluginException("Mass answer to multiple documents is unimplemented")
    d = get_doc_or_abort(doc_id)
    d.document.insert_preamble_pars()
    curr_user = get_current_user_object()
    disable_answer = d.document.get_settings().disable_answer()
    if disable_answer == DISABLE_ANSWER_REVIEW_MODE:
        if is_in_answer_review(d, curr_user):
            raise AccessDenied(
                gettext("You cannot submit new answers to the document.")
            )
    else:
        rights = get_user_rights_for_item(d, curr_user)
        if has_no_higher_right(disable_answer, rights):
            raise AccessDenied("Answering is disabled for this document.")

    # TODO: Teacher mode?

    urlmacros = get_urlmacros_from_request()
    origin = get_origin_from_request()
    view_ctx = ViewContext(ViewRoute.View, False, urlmacros=urlmacros, origin=origin)
    context_user = UserContext(curr_user, curr_user)
    task_datas: dict[
        str,
        tuple[
            TaskAccessVerification, ExistingAnswersInfo, list[User], bool, bool, bool
        ],
    ] = {}
    users = [curr_user]
    # ex_ans_infos = get_existing_answers_info_multiple_tasks(
    #     users, list(tids.values()), True
    # )
    # TODO: get these single query
    # ex_ans_infos = {
    #     t.doc_task: get_existing_answers_info(users, t, True) for t in tids.values()
    # }
    plugs: dict[str, Plugin] = {}
    # plugdata: vr,    answerinfo,    users,    allow_save,    ask_new,    force_answer,
    plugdatas = {
        t.doc_task: get_postanswer_plugin_etc(
            d, t, abData, curr_user, None, urlmacros, users, [], origin, False
        )
        for t in tids.values()
    }
    validities = {
        t: data[0].plugin.is_answer_valid(data[1].count, {})
        for t, data in plugdatas.items()
    }
    # for save, inp in inputs.items():
    #     tid = tids[save]
    #     doc, found_plugin = get_plugin_from_request(
    #         d.document, tid, context_user, view_ctx
    #     )
    #     plugs[save] = found_plugin
    #     ainf = ex_ans_infos[save]
    #     vr = verify_task_access(
    #         doc.docinfo,
    #         tid,
    #         AccessType.view,
    #         TaskIdAccess.ReadWrite,
    #         context_user,
    #         view_ctx,
    #     )
    # get_existing_answers_info
    # answerinfo = get_existing_answers_info(users, tid, True) # query all plugin answers here
    pass


# def get_postanswer_plugin_etc(
#     d: DocInfo,
#     tid: TaskId,
#     answer_browser_data: dict,
#     curr_user: User,
#     ctx_user: User | None,
#     urlmacros: UrlMacros,
#     users: list[User] | None,
#     other_session_users: list[User],
#     origin: OriginInfo | None,
#     force_answer: bool,
# ) -> tuple[TaskAccessVerification, ExistingAnswersInfo, list[User], bool, bool, bool]:
#     allow_save = True
#     ask_new = False
#
#     context_user = UserContext(ctx_user or curr_user, curr_user)
#     view_ctx = ViewContext(ViewRoute.View, False, urlmacros=urlmacros, origin=origin)
#     doc, found_plugin = get_plugin_from_request(d.document, tid, context_user, view_ctx)
#     # newtask = found_plugin.value.get("newtask", False)
#     newtask = found_plugin.is_new_task()
#     assert found_plugin.task_id is not None
#     if (
#         found_plugin.known.useCurrentUser or found_plugin.task_id.is_global
#     ):  # For plugins that is saved only for current user
#         users = [curr_user]
#     if users is None:
#         users = [curr_user] + other_session_users
#     if newtask:  # found_plugin.par.get_attr("seed") == "answernr":
#         force_answer = True  # variable tasks are always saved even with same answer
#
#     answerinfo = get_existing_answers_info(users, tid, True)
#     answernr = -1
#     answernr_to_user = None
#
#     if newtask:  # only if task is with new random after every answer
#         # Next three lines was there originally for stack, but let's see if we manage without them
#         # if isinstance(answerdata, dict):
#         # answernr = answerdata.get("answernr", -1)
#         # ask_new = answerdata.get("askNew", False)
#         if answernr < 0:
#             answernr = answer_browser_data.get("answernr", -1)
#         answernr_to_user = answernr
#         if answernr < 0:
#             answernr_to_user = answerinfo.count
#             answernr = answerinfo.count
#         if not ask_new:
#             ask_new = answernr == answerinfo.count
#             allow_save = ask_new
#
#     try:
#         vr = verify_task_access(
#             d,
#             tid,
#             AccessType.view,
#             TaskIdAccess.ReadWrite,
#             context_user=context_user,
#             view_ctx=view_ctx,
#             allow_grace_period=True,
#             answernr=answernr_to_user,
#         )
#     except (PluginException, TimDbException) as e:
#         raise PluginException(str(e))
#     return vr, answerinfo, users, allow_save, ask_new, force_answer


# def post_answer_impl(
#     task_id_ext: str,
#     answerdata: InputAnswer,
#     answer_browser_data: dict,
#     answer_options: dict,
#     curr_user: User,
#     urlmacros: UrlMacros,
#     other_session_users: list[User],
#     origin: OriginInfo | None,
#     error: str | None = None,
# ) -> AnswerRouteResult:
#     receive_time = get_current_time()
#     tid = TaskId.parse(task_id_ext)
#     if tid.doc_id is None:
#         raise PluginException(f"Task ID is missing document ID: {task_id_ext}")
#     d = get_doc_or_abort(tid.doc_id)
#     d.document.insert_preamble_pars()
#
#     # It is rare but possible that the current user has been deleted (for example as the result of merging 2 accounts).
#     # We assume it's the case here, so we clear the session and ask to log in again.
#     if curr_user.is_deleted:
#         clear_session()
#         raise AccessDenied("Please refresh the page and log in again.")
#
#     disable_answer = d.document.get_settings().disable_answer()
#     if disable_answer == DISABLE_ANSWER_REVIEW_MODE:
#         if is_in_answer_review(d, curr_user):
#             raise AccessDenied(
#                 gettext("You cannot submit new answers to the document.")
#             )
#     else:
#         rights = get_user_rights_for_item(d, curr_user)
#         if has_no_higher_right(disable_answer, rights):
#             raise AccessDenied("Answering is disabled for this document.")
#
#     force_answer = answer_options.get(
#         "forceSave", False
#     )  # Only used in feedback plugin.
#     is_teacher_mode = answer_browser_data.get("teacher", False)
#     save_teacher = answer_browser_data.get("saveTeacher", False)
#     should_save_answer = answer_browser_data.get("saveAnswer", True) and tid.task_name
#
#     if save_teacher:
#         verify_teacher_access(d, user=curr_user)
#     users = None
#
#     ctx_user = None
#
#     if is_teacher_mode:
#         answer_id = answer_browser_data.get("answer_id", None)
#         user_id = answer_browser_data.get("userId", None)
#
#         if answer_id is not None:
#             answer = db.session.get(Answer, answer_id)
#             if not answer:
#                 raise PluginException(f"Answer not found: {answer_id}")
#             expected_task_id = answer.task_id
#             if expected_task_id != tid.doc_task:
#                 raise PluginException("Task ids did not match")
#
#             # Later on, we may call users.append, but we don't want to modify the users of the existing
#             # answer. Therefore, we make a copy of the user list so that SQLAlchemy no longer associates
#             # the user list with the answer.
#             users = list(answer.users_all)
#             if not users:
#                 raise PluginException("No users found for the specified answer")
#             # For now global fields use current user in browser
#             # We set answerer user to be current user later so we ignore user mismatch in global case
#             if user_id not in (u.id for u in users) and not tid.is_global:
#                 raise PluginException("userId is not associated with answer_id")
#         elif (
#             user_id and user_id != curr_user.id and False
#         ):  # TODO: Vesa's hack to no need for belong teachers group
#             teacher_group = UserGroup.get_teachers_group()
#             if curr_user not in teacher_group.users:
#                 raise PluginException(
#                     "Permission denied: you are not in teachers group."
#                 )
#         if user_id:
#             ctx_user = db.session.get(User, user_id)
#             if not ctx_user:
#                 raise PluginException(f"User {user_id} not found")
#             users = [ctx_user]  # TODO: Vesa's hack to save answer to student
#
#     view_ctx = ViewContext(ViewRoute.View, False, urlmacros=urlmacros, origin=origin)
#     (
#         vr,
#         answerinfo,
#         users,
#         allow_save,
#         ask_new,
#         force_answer,
#     ) = get_postanswer_plugin_etc(
#         d,
#         tid,
#         answer_browser_data,
#         curr_user,
#         ctx_user,
#         urlmacros,
#         users,
#         other_session_users,
#         origin,
#         force_answer,
#     )
#     plugin = vr.plugin
#
#     if tid.is_points_ref:
#         if not isinstance(answerdata, dict):
#             raise PluginException("Invalid answer data format")
#         return AnswerRouteResult(
#             result=handle_points_ref(answerdata, curr_user, d, plugin.ptype, tid),
#             plugin=plugin,
#             extra={},
#         )
#
#     get_task = (
#         isinstance(answerdata, dict)
#         and answerdata.get("getTask", False)
#         and plugin.ptype.can_give_task()
#     )
#     if not (should_save_answer or get_task) or is_teacher_mode:
#         verify_seeanswers_access(d, user=curr_user)
#
#     uploads = []
#
#     if not curr_user.logged_in and not plugin.known.anonymous:
#         raise RouteException("You must be logged in to answer this task.")
#
#     if isinstance(answerdata, dict):
#         file = answerdata.get("uploadedFile", "")
#         trimmed_file = file.replace("/uploads/", "")
#         type = answerdata.get("type", "")
#         if trimmed_file and type == "upload":
#             uploads = check_answerupload_file_accesses([trimmed_file], curr_user)
#         files: list[dict] = answerdata.get("uploadedFiles", None)
#         if files is not None:
#             trimmed_files = [f["path"].replace("/uploads/", "") for f in files]
#             uploads = check_answerupload_file_accesses(trimmed_files, curr_user)
#
#     # Load old answers
#
#     valid, _ = plugin.is_answer_valid(answerinfo.count, {})
#     info = plugin.get_info(
#         users,
#         answerinfo.count,
#         look_answer=is_teacher_mode and not save_teacher,
#         valid=valid,
#     )
#     if ask_new:
#         info["askNew"] = True
#
#     # Get the newest answer (state). Only for logged in users.
#     state = (
#         try_load_json(answerinfo.latest_answer.content)
#         if curr_user.logged_in and answerinfo.latest_answer
#         else None
#     )
#     # TODO: get state from AB selected answer if new_task == true
#     # TODO: Why state is needed for new answers?
#     # TODO: Stack gets default for the field there???
#     answer_id = answer_browser_data.get("answer_id", None)
#     if answer_id is not None and curr_user.logged_in:
#         answer = db.session.get(Answer, answer_id)
#         if answer:
#             state = try_load_json(answer.content)
#
#     preprocessor = answer_call_preprocessors.get(plugin.type)
#     if preprocessor:
#         preprocessor(answerdata, curr_user, d, plugin, view_ctx)
#
#     # uncomment this to follow what answers are used in browser tests
#     # print(json.dumps(answerdata))
#
#     answer_call_data = {
#         "markup": plugin.values,
#         "state": state,
#         "input": answerdata,
#         "taskID": tid.doc_task,
#         "info": info,
#     }
#
#     result = {}
#     result_errors: list[str] = [error] if error else []
#     web = ""
#
#     def set_postoutput(result: dict, output: Any | None, outputname: str) -> None:
#         if not outputname or (not output and not preoutput):
#             return
#         parts = outputname.split(".")
#         r = result
#         lastkey = parts[-1]
#         for p in parts[:-1]:
#             if not p in r:
#                 r[p] = {}
#             r = r[p]
#         r[lastkey] = r.get(lastkey, "") + str(output)
#
#     def add_value(result: dict, key: str, data: dict) -> None:
#         value = data.get(key, None)
#         if value is None:
#             return
#         if value.startswith("md:"):
#             value = call_dumbo([value[3:]])[0]
#         result[key] = result.get(key, "") + value
#
#     def postprogram_result(data: dict, output: Any | None, outputname: str) -> None:
#         result["web"] = data.get("web", web)
#         err = data.get("error", None)
#         if err:
#             if err.startswith("md:"):
#                 err = call_dumbo([err[3:]])[0]
#             result_errors.append(err)
#         add_value(result, "feedback", data)
#         add_value(result, "topfeedback", data)
#         if output.startswith("md:"):
#             output = call_dumbo([output[3:]])[0]
#         set_postoutput(result, output, outputname)
#
#     preoutput = ""
#     preprogram = plugin.values.get("preprogram", None)
#     if preprogram and plugin.type != "jsrunner":
#         try:
#             params = JsRunnerParams(
#                 code=preprogram,
#                 data=answer_call_data,
#                 error_text=PRE_POST_ERROR,
#                 caller="preprogram:",
#             )
#             answer_call_data, preoutput = jsrunner_run(params)
#         except JsRunnerError as e:
#             return AnswerRouteResult(
#                 result={"web": {"error": "Error in JavaScript: " + e.args[0]}},
#                 plugin=plugin,
#                 extra={},
#             )
#
#     if preoutput:
#         postprogram_result(
#             answer_call_data, preoutput, plugin.values.get("preoutput", "feedback")
#         )
#
#     jsonresp = call_plugin_answer_and_parse(answer_call_data, plugin.type)
#
#     web = jsonresp.get("web")
#     if web is None:
#         raise PluginException(f"Got malformed response from plugin: {jsonresp}")
#     result["web"] = web
#     extra = {}
#     try:
#         extra["points"] = jsonresp.get("save", {}).get("points", {})
#         extra["type"] = answer_call_data.get("markup", {}).get("type", "")
#     except:
#         pass
#
#     if "savedata" in jsonresp:
#         siw = answer_call_data.get("markup", {}).get("showInView", False)
#         overwrite_opts = AllowedOverwriteOptions.from_markup(
#             answer_call_data.get("markup", {})
#         )
#         add_group = None
#         if plugin.type == "importData":
#             add_group = plugin.values.get("addUsersToGroup")
#         pr_data = plugin.values.get("peerReviewField", None)
#         saveresult = save_fields(
#             jsonresp,
#             curr_user,
#             d,
#             allow_non_teacher=siw,
#             add_users_to_group=add_group,
#             pr_data=pr_data,
#             overwrite_opts=overwrite_opts,
#             view_ctx=view_ctx,
#             saver_plugin=plugin,
#         )
#
#         # TODO: Could report the result to other plugins too.
#         if plugin.type == "importData":
#             web["fieldresult"] = saveresult
#
#     def add_reply(obj: dict, key: str, run_markdown: bool = False) -> None:
#         if key not in plugin.values:
#             return
#         text_to_add = plugin.values[key]
#         if run_markdown:
#             dumbo_result = call_dumbo([text_to_add])
#             text_to_add = dumbo_result[0]
#         obj[key] = text_to_add
#
#     noupdate = False  # if true do not send new id
#
#     resultmd = result["web"].get("md", None)
#     if resultmd:
#         result["web"]["md"] = call_dumbo([resultmd])[0]
#     resultmd = result["web"].get("outdata", {}).get("md", None)
#     if resultmd:  # mostly for jsrunner
#         result["web"]["outdata"]["md"] = call_dumbo([resultmd])[0]
#
#     if not get_task:
#         add_reply(result["web"], "-replyImage")
#         add_reply(result["web"], "-replyMD", True)
#         add_reply(result["web"], "-replyHTML")
#     if "save" in jsonresp and not get_task:
#         # TODO: RND_SEED: save used rnd_seed for this answer if answer is saved, found from par.get_rnd_seed()
#         save_object = jsonresp["save"]
#         tags = []
#         tim_info = jsonresp.get("tim_info", {})
#         if tim_info.get("noupdate", False):
#             noupdate = True
#         points = tim_info.get("points", None)
#         multiplier = plugin.points_multiplier()
#         if multiplier and points is not None:
#             points *= plugin.points_multiplier()
#         elif not multiplier:
#             points = None
#         # Save the new state
#         try:
#             tags = save_object["tags"]
#         except (TypeError, KeyError):
#             pass
#
#         def get_name_and_val(name1: str, name2: str = "") -> tuple[str, Any]:
#             """
#             Try with name1, -name1 amnd name2
#             return working name and value or "", None
#             """
#             name = name1
#             val = plugin.values.get(name, None)
#             if val:
#                 return name, val
#
#             name = "-" + name1
#             val = plugin.values.get(name, None)
#             if val:
#                 return name, val
#
#             if name2:
#                 name = name2
#                 val = plugin.values.get(name, None)  # old name
#             if val:
#                 return name, val
#
#             name = ""
#             return name, val
#
#         postprogram_name, postprogram = get_name_and_val("postprogram", "postProgram")
#
#         postlibraries_name, postlibraries = get_name_and_val("postlibraries")
#
#         postoutput = plugin.values.get("postoutput", "feedback")
#
#         if postprogram and postlibraries:
#             libs = ""
#             postlibraries_edit = plugin.values.get("postlibrariesEdit", {})
#             libnr = 0
#             for lib in postlibraries:
#                 try:
#                     content = get_from_url(lib)
#                     if content.startswith('{"error"'):
#                         web["error"] += lib + "\n" + content
#                         postprogram = ""
#                         break
#                     libedit = postlibraries_edit.get(libnr, None)
#                     if libedit:
#                         libdel = libedit.get("deleteAfter", None)
#                         if libdel:
#                             delpos = content.find(libdel)
#                             if delpos >= 0:
#                                 content = content[0:delpos]
#                     libs += content
#                 except Exception as ex:
#                     web["error"] += lib + "\n" + str(ex)
#                     postprogram = ""
#                 libnr += 1
#             if postprogram:
#                 postprogram = libs + "\n//=== END LIBRARIES ===\n" + postprogram
#
#         if (not is_teacher_mode and should_save_answer) or ("savedata" in jsonresp):
#             is_valid, explanation = plugin.is_answer_valid(answerinfo.count, tim_info)
#             if vr.is_invalid:
#                 is_valid = False
#                 explanation = (
#                     vr.invalidate_reason
#                     + " Your answer was saved but marked as invalid."
#                 )
#             elif vr.is_expired:
#                 fixed_time = (
#                     receive_time
#                     - d.document.get_settings().answer_submit_time_tolerance()
#                 )
#                 if fixed_time > (vr.access.accessible_to or maxdate):
#                     is_valid = False
#                     explanation = "Your view access to this document has expired, so this answer was saved but marked as invalid."
#             points_given_by = None
#             if answer_browser_data.get("giveCustomPoints"):
#                 try:
#                     points = plugin.validate_points(answer_browser_data.get("points"))
#                 except PluginException as e:
#                     result_errors.append(str(e))
#                 else:
#                     points_given_by = get_current_user_group()
#
#             if postprogram:
#                 data = {
#                     "users": [u.to_json() for u in users],
#                     "answer_call_data": answer_call_data,
#                     "points": points,
#                     "save_object": save_object,
#                     "tags": tags,
#                     "is_valid": is_valid,
#                     "invalid_explanation": explanation,
#                     "force_answer": force_answer,
#                     "allow_save": allow_save,
#                     "error": "",
#                     "web": web,
#                 }
#                 _, postprogram_fields = get_name_and_val(
#                     "postprogram_fields", "postprogramFields"
#                 )
#                 if postprogram_fields and isinstance(postprogram_fields, list):
#                     # TODO: Add support for multiple users in the same session
#                     field_data, field_aliases, _, _ = get_fields_and_users(
#                         postprogram_fields,
#                         RequestedGroups(groups=[curr_user.get_personal_group()]),
#                         d,
#                         curr_user,
#                         view_ctx,
#                         access_option=GetFieldsAccess.from_bool(True),
#                     )
#                     # We only obtain current user's fields
#                     user_fields = field_data[0]["fields"]
#                     data["fields"] = {"values": user_fields, "names": field_aliases}
#                 try:
#                     params = JsRunnerParams(
#                         code=postprogram,
#                         data=data,
#                         error_text=PRE_POST_ERROR,
#                         caller="postprogram:",
#                     )
#                     data, output = jsrunner_run(params)
#                     points = data.get("points", points)
#                     save_object = data.get("save_object", save_object)
#                     is_valid = data.get("is_valid", is_valid)
#                     explanation = data.get("invalid_explanation", explanation)
#                     force_answer = data.get("force_answer", force_answer)
#                     allow_save = data.get("allow_save", allow_save)
#                     refresh = data.get("refresh", False)
#                     if refresh:
#                         result["refresh"] = True
#                     postprogram_result(data, output, postoutput)
#                 except JsRunnerError as e:
#                     return AnswerRouteResult(
#                         result={"web": {"error": "Error in JavaScript: " + e.args[0]}},
#                         plugin=plugin,
#                         extra={},
#                     )
#
#             if (points or save_object is not None or tags) and allow_save:
#                 points_changed = (
#                     answerinfo.latest_answer
#                     and answerinfo.latest_answer.points != points
#                 )
#                 a = save_answer(
#                     users,
#                     tid,
#                     save_object,
#                     points,
#                     tags,
#                     is_valid,
#                     points_given_by,
#                     force_answer,
#                     plugintype=plugin.ptype,
#                     max_content_len=current_app.config["MAX_ANSWER_CONTENT_SIZE"],
#                     origin=origin,
#                     overwrite_existing=plugin.known.saveSingleAnswer,
#                 )
#                 result["savedNew"] = a.id if a else None
#                 if a:
#                     notify_doc_watchers(
#                         d,
#                         "",
#                         NotificationType.AnswerAdded,
#                         plugin.par,
#                         answer_number=answerinfo.count + 1,
#                         task_id=task_id_ext,
#                         curr_user=curr_user,
#                     )
#                     send_answer_backup_if_enabled(a)
#
#                 # This allows to refresh points in the UI
#                 # Setting true directly simplifies handling tests
#                 if not a and points_changed:
#                     result["refreshPoints"] = True
#             else:
#                 result["savedNew"] = None
#             if noupdate:
#                 result["savedNew"] = None
#
#             # Validity info can be different from error (e.g. answer can be valid but error is given by postprogram)
#             result["valid"] = is_valid
#             if not is_valid and explanation:
#                 result_errors.append(explanation)
#         elif save_teacher:
#             # Getting points from teacher ignores points automatically computed by the task
#             # For now we accept task points since most of the time that's what a teacher might want
#             # TODO: Accept teacher's points or task points depending on context (e.g. button)
#             # points = answer_browser_data.get("points", points)
#             points = points_to_float(points)
#             points_changed = (
#                 answerinfo.latest_answer and answerinfo.latest_answer.points != points
#             )
#             a = save_answer(
#                 users,
#                 tid,
#                 save_object,
#                 points,
#                 tags,
#                 valid=True,
#                 points_given_by=get_current_user_group(),
#                 saver=curr_user,
#                 plugintype=plugin.ptype,
#                 max_content_len=current_app.config["MAX_ANSWER_CONTENT_SIZE"],
#                 origin=origin,
#                 overwrite_existing=plugin.known.saveSingleAnswer,
#             )
#             # TODO: Could call backup here too, but first we'd need to add support for saver in export/import.
#             result["savedNew"] = a.id if a else None
#
#             if not a and points_changed:
#                 result["refreshPoints"] = True
#         else:
#             result["savedNew"] = None
#             if postprogram:
#                 data = {
#                     "users": [u.to_json() for u in users],
#                     "answer_call_data": answer_call_data,
#                     "points": points,
#                     "save_object": save_object,
#                     "tags": tags,
#                     "is_valid": True,
#                     "invalid_explanation": "ok",
#                     "force_answer": force_answer,
#                     "allow_save": allow_save,
#                     "error": "",
#                     "web": web,
#                 }
#                 try:
#                     params = JsRunnerParams(
#                         code=postprogram,
#                         data=data,
#                         error_text=PRE_POST_ERROR,
#                         caller="postprogram:",
#                     )
#                     data, output = jsrunner_run(params)
#                     points = data.get("points", points)
#                     output += "\nPoints: " + str(points)
#                     postprogram_result(data, output, postoutput)
#                 except JsRunnerError as e:
#                     return AnswerRouteResult(
#                         result={"web": {"error": "Error in JavaScript: " + e.args[0]}},
#                         plugin=plugin,
#                         extra={},
#                     )
#         if result["savedNew"] is not None and uploads:
#             # Associate this answer with the upload entries
#             for upload in uploads:
#                 upload.answer_id = result["savedNew"]
#
#     db.session.commit()
#
#     for u in users:
#         if (
#             origin and origin.doc_id != d.id
#         ):  # Origin might be different from the actual document
#             clear_doc_cache(origin.doc_id, u)
#         clear_doc_cache(d, u)
#
#     try:
#         if postprogram_name:
#             result["web"]["markup"].pop(
#                 postprogram_name
#             )  # TODO: stdy why someone puts markup here
#     except:
#         pass
#     if result_errors:
#         result["errors"] = result_errors
#
#     return AnswerRouteResult(result=result, plugin=plugin, extra=extra)
