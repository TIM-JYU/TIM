import json
import time
from dataclasses import dataclass, field
from datetime import timedelta
from random import randrange

import dateutil.parser
from flask import Blueprint, render_template, g
from flask import Response
from flask import current_app
from flask import request
from flask import session
from sqlalchemy import func, select, delete
from sqlalchemy.exc import OperationalError
from sqlalchemy.orm import joinedload
from sqlalchemy.orm.exc import StaleDataError

from timApp.auth.accesshelper import (
    verify_ownership,
    get_doc_or_abort,
    has_ownership,
    verify_edit_access,
)
from timApp.auth.login import log_in_as_anonymous
from timApp.auth.sessioninfo import (
    get_current_user_id,
    logged_in,
    get_current_user_object,
)
from timApp.document.docentry import DocEntry
from timApp.document.randutils import hashfunc
from timApp.lecture.askedjson import get_asked_json_by_hash, AskedJson
from timApp.lecture.askedquestion import (
    AskedQuestion,
    get_asked_question,
    user_activity_lock,
)
from timApp.lecture.lecture import Lecture
from timApp.lecture.lectureanswer import LectureAnswer, get_totals
from timApp.lecture.lectureutils import (
    is_lecturer_of,
    verify_is_lecturer,
    get_current_lecture_info,
)
from timApp.lecture.message import Message
from timApp.lecture.question import Question
from timApp.lecture.question_utils import (
    calculate_points_from_json_answer,
    create_points_table,
    qst_handle_randomization,
)
from timApp.lecture.questionactivity import QuestionActivityKind, QuestionActivity
from timApp.lecture.runningquestion import Runningquestion
from timApp.lecture.showpoints import Showpoints
from timApp.lecture.useractivity import Useractivity
from timApp.plugin.qst.qst import get_question_data_from_document
from timApp.timdb.sqa import db, tim_main_execute
from timApp.user.user import User
from timApp.util.error_handlers import suppress_wuff
from timApp.util.flask.requesthelper import (
    get_option,
    verify_json_params,
    use_model,
    RouteException,
    NotExist,
)
from timApp.util.flask.responsehelper import json_response, ok_response, empty_response
from timApp.util.logger import log_info
from timApp.util.utils import get_current_time

lecture_routes = Blueprint("lecture", __name__, url_prefix="")


@dataclass
class AskedIdModel:
    asked_id: int


@lecture_routes.get("/getLectureInfo")
def get_lecture_info():
    """Route to get info from lectures.

    Gives answers, and messages and other necessary info.

    """
    lecture = get_lecture_from_request(check_access=False)
    messages = lecture.messages.order_by(Message.timestamp.asc()).all()
    is_lecturer = is_lecturer_of(lecture)
    u = get_current_user_object()
    lecture_questions: list[AskedQuestion] = (
        lecture.asked_questions.options(
            joinedload(AskedQuestion.answers_all).raiseload(
                LectureAnswer.asked_question
            )
        )
        .options(
            joinedload(AskedQuestion.answers_all)
            .joinedload(LectureAnswer.user)
            .raiseload(User.groups)
        )
        .all()
    )

    if is_lecturer or u.is_admin:
        answers: list[LectureAnswer] = [
            a for q in lecture_questions for a in q.answers_all
        ]
        answerers = list({a.user for a in answers})
    else:
        answers = [
            a for q in lecture_questions for a in q.answers_all if a.user_id == u.id
        ]
        answerers = [get_current_user_object()]

    return json_response(
        {
            "answerers": answerers,
            "answers": [
                a.to_json(include_question=False, include_user=False) for a in answers
            ],
            "isLecturer": is_lecturer,
            "messages": messages,
            "questions": lecture_questions,
        },
        date_conversion=True,
    )


@lecture_routes.get("/getLectureAnswerTotals/<int:lecture_id>")
def get_lecture_answer_totals(lecture_id):
    lec = Lecture.find_by_id(lecture_id)
    is_lecturer = is_lecturer_of(lec)
    u = get_current_user_object()
    results = get_totals(lec, None if is_lecturer or u.is_admin else u)
    sum_field_name = get_option(request, "sum_field_name", "sum")
    count_field_name = get_option(request, "count_field_name", "count")

    def generate_text():
        for u, p, c in results:
            yield f"{u.name};{sum_field_name};{p}\n"
        yield "\n"
        for u, p, c in results:
            yield f"{u.name};{count_field_name};{c}\n"

    return Response(generate_text(), mimetype="text/plain")


@lecture_routes.get("/getAllMessages")
def get_all_messages():
    """Route to get all the messages from some lecture."""
    lecture = get_lecture_from_request(check_access=False)
    messages = lecture.messages.order_by(Message.timestamp.asc()).all()
    return json_response(messages, date_conversion=True)


@dataclass
class GetUpdatesModel:
    client_last_id: int = field(metadata={"data_key": "c"})
    current_points_id: int | None = field(metadata={"data_key": "p"}, default=None)
    current_question_id: int | None = field(metadata={"data_key": "i"}, default=None)
    doc_id: int | None = field(metadata={"data_key": "d"}, default=None)
    use_questions: bool = field(metadata={"data_key": "q"}, default=False)
    use_wall: bool = field(metadata={"data_key": "m"}, default=False)


@lecture_routes.get("/getUpdates")
@use_model(GetUpdatesModel)
def get_updates(m: GetUpdatesModel):
    # taketime("before update")
    ret = do_get_updates(m)
    db.session.commit()
    # taketime("after update")
    return json_response(ret, date_conversion=True)


@lecture_routes.before_request
def lecture_before_request():
    tim_main_execute("SET LOCAL lock_timeout = '1s'")


EXTRA_FIELD_NAME = "extra"


@suppress_wuff(
    OperationalError,
    "https://github.com/TIM-JYU/TIM/issues/1975",
    r"LockNotAvailable.*canceling statement due to lock timeout",
)
def do_get_updates(m: GetUpdatesModel):
    """Gets updates from some lecture.

    Checks updates in 1 second frequently and answers if there is updates.

    """
    client_last_id = m.client_last_id
    current_question_id = m.current_question_id
    current_points_id = m.current_points_id
    use_wall = m.use_wall
    use_questions = m.use_questions
    session["use_questions"] = use_questions

    step = 0
    lecture = get_current_lecture()

    doc_id = m.doc_id
    if not lecture:
        return get_running_lectures(doc_id)
    lecture_id = lecture.lecture_id
    if not lecture.is_running:
        empty_lecture(lecture)
        db.session.commit()
        return get_running_lectures(doc_id)

    list_of_new_messages = []

    lecturers = []
    students = []
    u = get_current_user_object()
    user_name = u.name

    update_activity(lecture, u)

    options = lecture.options_parsed
    teacher_poll = options.get("teacher_poll", "")
    teacher_poll = teacher_poll.split(";")

    poll_interval_ms = 4000
    long_poll = False
    poll_interval_t_ms = 4000
    long_poll_t = False

    # noinspection PyBroadException
    try:
        poll_interval_ms = int(float(options.get("poll_interval", 4)) * 1000)
        long_poll = bool(options.get("long_poll", False))
    except:
        pass

    # noinspection PyBroadException
    try:
        poll_interval_t_ms = int(float(options.get("poll_interval_t", 1)) * 1000)
        long_poll_t = bool(options.get("long_poll_t", False))
    except:
        pass

    poll_interval_ms += randrange(-100, 500)

    if teacher_poll:
        # noinspection PyBroadException
        try:
            if teacher_poll.index(user_name) >= 0:
                poll_interval_ms = poll_interval_t_ms
                long_poll = long_poll_t
        except:
            pass

    is_lecturer = is_lecturer_of(lecture)
    if is_lecturer:
        poll_interval_ms = poll_interval_t_ms
        long_poll = long_poll_t
    if long_poll:
        poll_interval_ms = 1000

    lecture_ending = 100
    base_resp = None

    basic_info = {
        "ms": poll_interval_ms,
    }
    while step <= 10:
        lecture = get_current_lecture()
        if not lecture:
            return get_running_lectures(doc_id)
        lecture_ending = check_if_lecture_is_ending(lecture)
        if is_lecturer:
            lecturers, students = get_lecture_users(lecture)
        # Gets new messages if the wall is in use.
        if use_wall:
            list_of_new_messages = (
                lecture.messages.filter(Message.msg_id > client_last_id)
                .order_by(Message.msg_id.asc())
                .all()
            )

        # Check if current question is still running and user hasn't already answered on it on another tab.
        if current_question_id:
            q = get_asked_question(current_question_id)
            if q and q.running_question:
                # Always report question end time in case it has been stopped or extended.
                basic_info["question_end_time"] = q.running_question.end_time

        base_resp = {
            **basic_info,
            "msgs": list_of_new_messages,
            "lectureEnding": lecture_ending,
            "lectureId": lecture_id,
            "lecturers": lecturers,
            "students": students,
        }

        if current_points_id:
            q = get_asked_question(current_points_id)
            if q and q.has_activity(QuestionActivityKind.Pointsclosed, u):
                return {
                    **base_resp,
                    EXTRA_FIELD_NAME: {
                        "points_closed": True,
                    },
                }

        # Gets new questions if the questions are in use.
        if use_questions:
            new_question = get_new_question(
                lecture, current_question_id, current_points_id
            )
            if new_question:
                return {**base_resp, EXTRA_FIELD_NAME: new_question}

        if list_of_new_messages:
            return base_resp

        if not long_poll or current_app.config["TESTING"]:
            # Don't loop when testing.
            break

        # Database updates may have happened during sleep, so we have to expire all objects so that they will be
        # reloaded. Additionally, we don't want to keep the connection open during sleep, so we call commit()
        # instead of expire_all().
        db.session.commit()

        # For long poll wait 1 sec before new check.
        time.sleep(1)
        step += 1

    if lecture_ending != 100 or lecturers or students:
        return base_resp

    # No new updates, except possibly new question end time.
    return basic_info


@lecture_routes.get("/getQuestionManually")
def get_question_manually():
    """Route to use to get question manually (instead of getting question in /getUpdates)."""
    lecture = get_current_lecture_or_abort()
    new_question = get_new_question(lecture, None, None, True)
    return json_response(new_question, date_conversion=True)


def hide_points_and_try_shuffle_question(question: AskedQuestion, user_id: int):
    # Hides points from question json and shuffles rows if required
    q_copy = question.to_json(hide_points=True)
    q_json = {"markup": q_copy["json"]["json"], "user_id": user_id}
    qst_handle_randomization(q_json)
    q_copy["json"]["json"] = q_json["markup"]
    return q_copy


def q_log(s: str):
    log_info(s)


def get_new_question(
    lecture: Lecture, current_question_id=None, current_points_id=None, force=False
):
    """
    :param current_points_id: TODO: what is this?
    :param current_question_id: The id of the current question.
    :param lecture: lecture to get running questions from
    :param force: Return question, even if it already has been shown to user
    :return: None if no questions are running
             dict with data of new question if there is a question running and user hasn't answered to that question.
             {'type': 'already_answered'} if there is a question running and user has answered to that.
    """
    current_user = get_current_user_id()
    u = get_current_user_object()
    rqs: list[Runningquestion] = lecture.running_questions
    with user_activity_lock(u):
        if rqs and rqs[0].asked_question.is_running:
            question: AskedQuestion = rqs[0].asked_question
            asked_id = question.asked_id
            already_shown = question.has_activity(QuestionActivityKind.Usershown, u)
            already_answered = question.has_activity(
                QuestionActivityKind.Useranswered, u
            )
            s = f"q: {u.name}, r, as={already_shown is not None}, aa={already_answered is not None}, f={force}, aid={asked_id}"
            if already_answered:
                if force:
                    q_log(f"{s}, ret=already_answered")
                    return {"type": "already_answered"}
                else:
                    q_log(f"{s}, ret=None")
                    return None
            if (not already_shown or force) or (asked_id != current_question_id):
                q = get_asked_question(asked_id)
                answer = q.answers.filter_by(user_id=current_user).first()
                question.add_activity(QuestionActivityKind.Usershown, u)
                if answer:
                    q_log(f"{s}, ret=answer")
                    return {"type": "answer", "data": answer}
                else:
                    q_log(f"{s}, ret=question")
                    return {
                        "type": "question",
                        "data": q
                        if lecture.lecturer == current_user
                        else hide_points_and_try_shuffle_question(q, current_user),
                    }
            q_log(f"{s}, ret=None")
        else:
            question_to_show_points = get_shown_points(lecture)
            s = ""
            if question_to_show_points:
                asked_id = question_to_show_points.asked_id
                already_shown = question_to_show_points.has_activity(
                    QuestionActivityKind.Pointsshown, u
                )
                already_closed = question_to_show_points.has_activity(
                    QuestionActivityKind.Pointsclosed, u
                )
                s = f"q: {u.name}, nr, as={already_shown is not None}, ac={already_closed is not None}, f={force}, aid={asked_id}"
                if already_closed:
                    if force:
                        db.session.delete(already_closed)
                    else:
                        q_log(f"{s}, ret=None")
                        return None
                if not (already_shown or force) or (asked_id != current_points_id):
                    question = get_asked_question(asked_id)
                    question.add_activity(QuestionActivityKind.Pointsshown, u)
                    answer = question.answers.filter_by(user_id=current_user).first()
                    if answer:
                        q_log(f"{s}, ret=result")
                        return {"type": "result", "data": answer}
            q_log(s or f"q: {u.name}, nr, f={force}, ret=None")
            return None


def get_shown_points(lecture) -> AskedQuestion | None:
    return lecture.asked_questions.join(Showpoints).first()


def check_if_lecture_is_ending(lecture: Lecture):
    """Checks if the lecture is about to end. 1 -> ends in 1 min. 5 -> ends in 5 min. 100 -> goes on atleast for 5 mins.

    :param lecture: The lecture object.
    :return:

    """
    lecture_ending = 100
    if is_lecturer_of(lecture):
        time_now = get_current_time()
        ending_time = lecture.end_time
        time_left = ending_time - time_now
        if time_left.total_seconds() <= 60:
            return 1
        elif time_left.total_seconds() <= 60 * 5:
            return 5
    return lecture_ending


@dataclass
class SendMessageModel:
    message: str


@lecture_routes.post("/sendMessage")
@use_model(SendMessageModel)
def send_message(m: SendMessageModel):
    """Route to add message to database."""
    lecture = get_current_lecture_or_abort()
    msg = Message(message=m.message, user_id=get_current_user_id())
    lecture.messages.append(msg)
    db.session.commit()
    return json_response(msg, date_conversion=True)


def get_lecture_session_data():
    for k in ("use_questions",):
        if session.get(k) is None:
            session[k] = True
    return {
        "useQuestions": session["use_questions"],
    }


def lecture_dict(lecture: Lecture):
    info = get_current_lecture_info()
    lecturers, students = get_lecture_users(lecture) if info.is_lecturer else ([], [])
    return {
        "lecture": lecture,
        "isInLecture": info.in_lecture,
        "isLecturer": info.is_lecturer,
        "lecturers": lecturers,
        "students": students,
        **get_lecture_session_data(),
    }


@lecture_routes.get("/checkLecture")
def check_lecture():
    """Route to check if the current user is in some lecture in specific document."""
    lectures = get_current_user_object().lectures
    lecture = lectures[0] if lectures else None

    if lecture:
        if lecture.is_running:
            return json_response(lecture_dict(lecture), date_conversion=True)
        else:
            leave_lecture(lecture)
            empty_lecture(lecture)
            db.session.commit()
    doc_id = request.args.get("doc_id")
    if doc_id is not None:
        return json_response(get_running_lectures(int(doc_id)), date_conversion=True)
    else:
        return empty_response()


def switch_to_lecture(l: Lecture):
    u = get_current_user_object()
    u.lectures = [l]


@lecture_routes.post("/startFutureLecture")
def start_future_lecture():
    lecture = get_lecture_from_request(check_access=True)
    time_now = get_current_time()
    lecture.start_time = time_now
    switch_to_lecture(lecture)
    db.session.commit()
    return json_response(lecture_dict(lecture), date_conversion=True)


@lecture_routes.get("/getAllLecturesFromDocument")
def get_all_lectures():
    if not request.args.get("doc_id"):
        raise RouteException("missing argument(s)")

    doc_id = int(request.args.get("doc_id"))

    lectures = Lecture.get_all_in_document(doc_id)
    time_now = get_current_time()
    current_lectures = []
    past_lectures = []
    future_lectures = []
    for lecture in lectures:
        if lecture.start_time <= time_now < lecture.end_time:
            current_lectures.append(lecture)
        elif lecture.end_time <= time_now:
            past_lectures.append(lecture)
        else:
            future_lectures.append(lecture)

    return json_response(
        {
            "currentLectures": current_lectures,
            "futureLectures": future_lectures,
            "pastLectures": past_lectures,
        },
        date_conversion=True,
    )


@lecture_routes.get("/showLectureInfo/<int:lecture_id>")
def show_lecture_info(lecture_id):
    lecture = Lecture.find_by_id(lecture_id)
    if not lecture:
        raise RouteException("Lecture not found")

    doc = DocEntry.find_by_id(lecture.doc_id)
    return render_template(
        "lectureInfo.jinja2", item=doc, lecture=lecture, translations=doc.translations
    )


@lecture_routes.get("/showLectureInfoGivenName")
def show_lecture_info_given_name():
    lecture = get_lecture_from_request(check_access=False)
    return json_response(
        lecture.to_json(
            show_password=is_lecturer_of(lecture) or get_current_user_object().is_admin
        ),
        date_conversion=True,
    )


@lecture_routes.get("/getLectureByCode")
def lecture_needs_password():
    lecture = get_lecture_from_request(check_access=False)
    return json_response(lecture, date_conversion=True)


def get_lecture_users(lecture: Lecture):
    lecturers = []
    students = []

    activity = (
        db.session.execute(select(Useractivity).filter_by(lecture=lecture))
        .scalars()
        .all()
    )
    cur_time = get_current_time()

    for ac in activity:
        user_id = ac.user_id
        active = ac.active
        person = {
            "user": ac.user,
            "activeSecondsAgo": int((cur_time - active).total_seconds()),
        }
        if lecture.lecturer == user_id:
            lecturers.append(person)
        else:
            students.append(person)

    return lecturers, students


def get_running_lectures(doc_id=None):
    """Gets all running and future lectures.

    :param doc_id: The document id for which to get lectures.

    """
    time_now = get_current_time()
    list_of_lectures = []
    is_lecturer = False
    if doc_id:
        list_of_lectures = Lecture.get_all_in_document(doc_id, time_now)
        d = get_doc_or_abort(doc_id)
        is_lecturer = bool(has_ownership(d))
    current_lectures = []
    future_lectures = []
    for lecture in list_of_lectures:
        if lecture.start_time <= time_now < lecture.end_time:
            current_lectures.append(lecture)
        else:
            future_lectures.append(lecture)
    return {
        "isLecturer": is_lecturer,
        "lectures": current_lectures,
        "futureLectures": future_lectures,
    }


@lecture_routes.post("/createLecture")
def create_lecture():
    doc_id, start_time, end_time, lecture_code = verify_json_params(
        "doc_id", "start_time", "end_time", "lecture_code"
    )
    start_time = dateutil.parser.parse(start_time)
    end_time = dateutil.parser.parse(end_time)
    lecture_id, password, options = verify_json_params(
        "lecture_id", "password", "options", require=False
    )
    d = get_doc_or_abort(doc_id)
    verify_ownership(d)

    if not options:
        options = {}

    if not password:
        password = ""
    current_user = get_current_user_id()
    lec = Lecture.find_by_code(lecture_code, doc_id)
    if lec and not lecture_id:
        raise RouteException(
            "Can't create two or more lectures with the same name to the same document."
        )

    options = json.dumps(options)
    if lecture_id is None:
        lecture = Lecture(doc_id=doc_id, lecturer=current_user)
        db.session.add(lecture)
    else:
        lecture = Lecture.find_by_id(lecture_id)
        if not lecture:
            raise NotExist()
    lecture.start_time = start_time
    lecture.end_time = end_time
    lecture.password = password
    lecture.lecture_code = lecture_code
    lecture.options = options

    current_time = get_current_time()

    if start_time <= current_time <= end_time and not get_current_lecture():
        switch_to_lecture(lecture)
    db.session.commit()
    return json_response(lecture, date_conversion=True)


@suppress_wuff(
    StaleDataError,
    "https://github.com/TIM-JYU/TIM/issues/1976",
    r"DELETE statement on table 'lectureusers' expected to delete",
)
def empty_lecture(lec: Lecture):
    lec.users = []
    clean_dictionaries_by_lecture(lec)


@lecture_routes.post("/endLecture")
def end_lecture():
    lecture = get_lecture_from_request()
    now = get_current_time()
    lecture.end_time = now
    empty_lecture(lecture)
    db.session.commit()
    return json_response(get_running_lectures(lecture.doc_id), date_conversion=True)


def clean_dictionaries_by_lecture(lecture: Lecture):
    """Cleans data from lecture that isn't running anymore.

    :param lecture: The lecture.

    """
    for q in lecture.running_questions:
        db.session.delete(q)
    stop_showing_points(lecture)
    for a in lecture.useractivity:
        db.session.delete(a)
    db.session.execute(
        delete(QuestionActivity)
        .where(
            (
                QuestionActivity.asked_id.in_(
                    select(AskedQuestion.asked_id).filter_by(
                        lecture_id=lecture.lecture_id
                    )
                )
            )
            & QuestionActivity.kind.in_(
                [
                    QuestionActivityKind.Usershown,
                    QuestionActivityKind.Pointsshown,
                    QuestionActivityKind.Pointsclosed,
                    QuestionActivityKind.Useranswered,
                ]
            )
        )
        .execution_options(synchronize_session="fetch")
    )


def delete_question_temp_data(question: AskedQuestion, lecture: Lecture):
    delete_activity(
        question,
        [
            QuestionActivityKind.Usershown,
            QuestionActivityKind.Useranswered,
            QuestionActivityKind.Pointsclosed,
            QuestionActivityKind.Pointsshown,
        ],
    )
    db.session.execute(
        delete(Runningquestion).where(Runningquestion.lecture_id == lecture.lecture_id)
    )
    stop_showing_points(lecture)


@lecture_routes.post("/extendLecture")
def extend_lecture():
    new_end_time = request.args.get("new_end_time")
    if not new_end_time:
        raise RouteException("missing argument(s)")
    lecture = get_lecture_from_request()
    lecture.end_time = new_end_time
    db.session.commit()
    return ok_response()


@dataclass
class DeleteLectureModel:
    lecture_id: int


@lecture_routes.post("/deleteLecture")
@use_model(DeleteLectureModel)
def delete_lecture(m: DeleteLectureModel):
    lecture = get_lecture_from_request(lecture_id=m.lecture_id)
    with db.session.no_autoflush:
        empty_lecture(lecture)
        for t in (Message, LectureAnswer, AskedQuestion):
            db.session.execute(delete(t).where(t.lecture_id == lecture.lecture_id))
        db.session.delete(lecture)
    db.session.commit()

    return json_response(get_running_lectures(lecture.doc_id), date_conversion=True)


def get_lecture_from_request(
    check_access=True, lecture_id: int | None = None
) -> Lecture:
    lecture_id = get_option(request, "lecture_id", lecture_id, cast=int)
    if not lecture_id:
        lecture_code = get_option(request, "lecture_code", None)
        doc_id = get_option(request, "doc_id", None)
        lecture = Lecture.find_by_code(lecture_code, doc_id)
    else:
        lecture = Lecture.find_by_id(lecture_id)
    if not lecture:
        raise NotExist("Lecture not found")
    if check_access:
        verify_is_lecturer(lecture)
    return lecture


@lecture_routes.post("/joinLecture")
def join_lecture():
    """Route to join lecture.

    Checks that the given password is correct.

    """
    lecture = get_lecture_from_request(check_access=False)
    password_quess = request.args.get("password_quess")

    # TODO Allow lecturer always join, even if the lecture is full
    lecture_full = lecture.is_full

    correct_password = True
    if lecture.password and lecture.password != password_quess:
        correct_password = False

    u = get_current_user_object()
    if lecture.is_running and not lecture_full and correct_password:
        if not logged_in():
            u = log_in_as_anonymous(session)
            g.user = u
        switch_to_lecture(lecture)

        update_activity(lecture, u)

        db.session.commit()

    return json_response(
        {
            "correctPassword": correct_password,
            **lecture_dict(lecture),
        },
        date_conversion=True,
    )


def update_activity(lecture: Lecture, u: User):
    ua = Useractivity(user_id=u.id, lecture_id=lecture.lecture_id, active=func.now())
    db.session.merge(ua)


@lecture_routes.post("/leaveLecture")
def leave_lecture_route():
    lecture = get_lecture_from_request(check_access=False)
    leave_lecture(lecture)
    db.session.commit()
    return ok_response()


def leave_lecture(lecture: Lecture):
    u = get_current_user_object()
    if u in lecture.users:
        lecture.users.remove(u)


@lecture_routes.post("/extendQuestion")
def extend_question():
    asked_id = int(request.args.get("asked_id"))
    extend = int(request.args.get("extend"))
    q = get_asked_question(asked_id)
    if not q:
        raise NotExist()
    rq: Runningquestion = q.running_question
    if not q.is_running:
        raise RouteException("Question is not running")
    rq.end_time += timedelta(seconds=extend)
    db.session.commit()
    return ok_response()


def get_current_lecture() -> Lecture | None:
    u = get_current_user_object()
    lectures: list[Lecture] = u.lectures
    if not lectures:
        return None
    if len(lectures) > 1:
        raise Exception(
            f"User {u.name} has joined to multiple lectures which should not be possible."
        )
    return lectures[0]


def get_current_lecture_or_abort() -> Lecture:
    lec = get_current_lecture()
    if not lec:
        raise RouteException("Not joined to any lecture")
    return lec


@lecture_routes.post("/askQuestion")
def ask_question():
    if not (
        request.args.get("question_id")
        or request.args.get("asked_id")
        or request.args.get("par_id")
    ):
        raise RouteException("Bad request")
    question_id = None
    asked_id = None
    par_id = None
    if request.args.get("question_id"):
        question_id = int(request.args.get("question_id"))
    elif request.args.get("asked_id"):
        asked_id = int(request.args.get("asked_id"))
    else:
        par_id = request.args.get("par_id")

    lecture = get_current_lecture_or_abort()
    verify_is_lecturer(lecture)

    if question_id or par_id:
        doc_id = get_option(request, "doc_id", None, cast=int)
        if not doc_id:
            raise RouteException("doc_id missing")
        if question_id:
            question = db.session.get(Question, question_id)  # Old version???
            question_json_str = question.questionjson
            markup = json.loads(question_json_str)
        else:
            d = get_doc_or_abort(doc_id)
            markup = get_question_data_from_document(d, par_id)
            question_json_str = json.dumps(markup.markup)
        question_hash = hashfunc(question_json_str)
        asked_json = get_asked_json_by_hash(question_hash)
        if not asked_json:
            asked_json = AskedJson(json=question_json_str, hash=question_hash)
        asked_time = get_current_time()

        # Set points and expl as None because they're already contained in the JSON.
        # Only if /updatePoints is called, they are set.
        question = AskedQuestion(
            lecture=lecture,
            doc_id=doc_id,
            asked_time=asked_time,
            points=None,
            expl=None,
            asked_json=asked_json,
            par_id=par_id,
        )
        db.session.add(question)
    elif asked_id:
        question = get_asked_question(asked_id)
        if not question:
            raise NotExist("Asked question not found.")
        question.asked_time = get_current_time()
        lecture = question.lecture
    else:
        raise RouteException("Missing parameters")

    delete_question_temp_data(question, lecture)
    rq = Runningquestion(
        lecture=lecture,
        asked_question=question,
        ask_time=question.asked_time,
        end_time=question.end_time,
    )
    db.session.add(rq)
    db.session.commit()
    return json_response(question, date_conversion=True)


class ShowAnswerPointsModel(AskedIdModel):
    current_question_id: int | None = None
    current_points_id: int | None = None


@lecture_routes.post("/showAnswerPoints")
@use_model(ShowAnswerPointsModel)
def show_points(m: ShowAnswerPointsModel):
    lecture = get_current_lecture_or_abort()
    verify_is_lecturer(lecture)
    asked_id = m.asked_id
    q = get_asked_question(asked_id)
    if not q:
        raise NotExist()

    stop_showing_points(lecture)
    sp = Showpoints(asked_question=q)
    db.session.add(sp)

    current_question_id = m.current_question_id
    current_points_id = m.current_points_id
    new_question = get_new_question(lecture, current_question_id, current_points_id)
    db.session.commit()
    if new_question is not None:
        return json_response(new_question, date_conversion=True)
    return empty_response()


def stop_showing_points(lecture: Lecture):
    db.session.execute(
        delete(Showpoints)
        .where(
            Showpoints.asked_id.in_(
                select(AskedQuestion.asked_id).filter_by(lecture_id=lecture.lecture_id)
            )
        )
        .execution_options(synchronize_session="fetch")
    )


@lecture_routes.post("/updatePoints/")
def update_question_points():
    """Route to get add question to database."""
    if "asked_id" not in request.args or "points" not in request.args:
        raise RouteException("missing argument(s)")
    asked_id = int(request.args.get("asked_id"))
    points = request.args.get("points")
    expl = request.args.get("expl")
    asked_question = get_asked_question(asked_id)
    verify_is_lecturer(asked_question.lecture)
    asked_question.points = points
    asked_question.expl = expl
    points_table = create_points_table(points)
    question_answers: list[LectureAnswer] = asked_question.answers.all()
    default_points = asked_question.get_default_points()
    for answer in question_answers:
        answer.points = calculate_points_from_json_answer(
            answer.get_parsed_answer(), points_table, default_points
        )
    db.session.commit()
    return ok_response()


def delete_activity(question: AskedQuestion, kinds):
    db.session.execute(
        delete(QuestionActivity)
        .where(
            (QuestionActivity.asked_id == question.asked_id)
            & QuestionActivity.kind.in_(kinds)
        )
        .execution_options(synchronize_session="fetch")
    )


@lecture_routes.get("/getQuestionByParId")
def get_question_by_par_id():
    if not request.args.get("par_id") or not request.args.get("doc_id"):
        raise RouteException("missing argument(s)")
    doc_id = int(request.args.get("doc_id"))
    par_id = request.args.get("par_id")
    edit = request.args.get("edit", False)
    d = get_doc_or_abort(doc_id)
    verify_edit_access(d)
    question = get_question_data_from_document(d, par_id, edit)
    return json_response(question)


@lecture_routes.get("/getAskedQuestionById")
def get_asked_question_by_id():
    if not request.args.get("asked_id"):
        raise RouteException("missing argument(s)")
    asked_id = int(request.args.get("asked_id"))
    question = get_asked_question(asked_id)
    verify_is_lecturer(question.lecture)
    return json_response(question, date_conversion=True)


@lecture_routes.get("/getQuestionAnswer")
def get_question_answer_by_id():
    answer_id = get_option(request, "id", default=None, cast=int)
    if answer_id:
        raise RouteException("missing argument(s)")
    ans = LectureAnswer.get_by_id(answer_id)
    if not ans:
        raise NotExist("Answer not found")
    verify_is_lecturer(ans.asked_question.lecture)
    return json_response(ans, date_conversion=True)


@lecture_routes.post("/stopQuestion")
@use_model(AskedIdModel)
def stop_question(m: AskedIdModel):
    """Route to stop question from running."""
    asked_id = m.asked_id
    aq = get_asked_question(asked_id)
    if not aq:
        raise NotExist("Asked question not found")
    lecture = get_current_lecture_or_abort()
    verify_is_lecturer(lecture)
    if not aq.is_running:
        return json_response({"status": "Question not running anymore"})
    aq.running_question.end_time = get_current_time()
    delete_activity(
        aq, [QuestionActivityKind.Usershown, QuestionActivityKind.Useranswered]
    )
    db.session.commit()
    return ok_response()


@lecture_routes.get("/getLectureAnswers")
def get_lecture_answers():
    """Changing this to long poll requires removing threads."""
    asked_id = get_option(request, "asked_id", None, cast=int)

    if not asked_id:
        raise RouteException("Bad request")

    question = get_asked_question(asked_id)
    verify_is_lecturer(question.lecture)
    if not question:
        raise NotExist("Asked question not found")
    after = get_option(
        request, "after", default=question.asked_time, cast=dateutil.parser.parse
    )

    lecture_answers = (
        question.answers.filter(LectureAnswer.answered_on > after)
        .order_by(LectureAnswer.answered_on.asc())
        .all()
    )
    return json_response(
        [
            a.to_json(include_question=False, include_user=False)
            for a in lecture_answers
        ],
        date_conversion=True,
    )


@dataclass
class AnswerToQuestionModel(AskedIdModel):
    input: list[list[str]]


@lecture_routes.put("/answerToQuestion")
@use_model(AnswerToQuestionModel)
def answer_to_question(m: AnswerToQuestionModel):
    asked_id = m.asked_id
    answer = m.input
    lecture = get_current_lecture_or_abort()
    lecture_id = lecture.lecture_id
    u = get_current_user_object()
    asked_question = get_asked_question(asked_id)

    lecture_answer: LectureAnswer = asked_question.answers.filter_by(
        user_id=u.id
    ).first()
    with user_activity_lock(u):
        already_answered = asked_question.has_activity(
            QuestionActivityKind.Useranswered, u
        )
        if not asked_question.is_running:
            if (
                not asked_question.running_question
                or get_current_time()
                > asked_question.running_question.end_time + timedelta(seconds=5)
            ):
                return json_response(
                    {
                        "questionLate": "The question has already finished. Your answer was not saved."
                    }
                )
        if already_answered:
            return json_response(
                {
                    "alreadyAnswered": "You have already answered to question. Your first answer is saved."
                }
            )

        asked_question.add_activity(QuestionActivityKind.Useranswered, u)

    if (not lecture_answer) or (lecture_answer and answer != lecture_answer.answer):
        whole_answer = answer
        time_now = get_current_time()
        if not is_lecturer_of(lecture):
            whole_answer, question_points = asked_question.build_answer_and_points(
                whole_answer, u
            )
        else:
            question_points = asked_question.get_effective_points()
        points_table = create_points_table(question_points)
        default_points = asked_question.get_default_points()
        points = calculate_points_from_json_answer(answer, points_table, default_points)
        answer = json.dumps(whole_answer)
        if lecture_answer and u.id != 0:
            lecture_answer.answered_on = time_now
            lecture_answer.answer = answer
            lecture_answer.points = points
        else:
            ans = LectureAnswer(
                user_id=u.id,
                question_id=asked_id,
                lecture_id=lecture_id,
                answer=answer,
                answered_on=time_now,
                points=points,
            )
            db.session.add(ans)
        db.session.commit()

    return ok_response()


@lecture_routes.put("/closePoints")
def close_points():
    asked_id = get_option(request, "asked_id", None, cast=int)
    if not asked_id:
        raise RouteException("Missing asked_id")

    lecture = get_current_lecture_or_abort()

    q = get_asked_question(asked_id)
    if not q:
        raise NotExist()

    points = get_shown_points(lecture)
    if points:
        q.add_activity(QuestionActivityKind.Pointsclosed, get_current_user_object())
        db.session.commit()

    return ok_response()
