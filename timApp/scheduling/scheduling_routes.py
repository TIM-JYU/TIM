from dataclasses import field, dataclass
from datetime import datetime
from typing import Dict, Any, Optional, List, Generator

from flask import current_app, Response
from isodate import Duration
from sqlalchemy.orm import joinedload

from timApp.auth.accesshelper import get_doc_or_abort, verify_manage_access, AccessDenied, verify_logged_in, \
    verify_admin
from timApp.auth.accesstype import AccessType
from timApp.auth.auth_models import BlockAccess
from timApp.auth.sessioninfo import get_current_user_object, user_context_with_logged_in
from timApp.celery_sqlalchemy_scheduler import IntervalSchedule, PeriodicTask
from timApp.document.docentry import DocEntry
from timApp.document.viewcontext import default_view_ctx
from timApp.document.yamlblock import parse_yaml
from timApp.item.block import Block, BlockType
from timApp.plugin.plugin import Plugin
from timApp.timdb.sqa import db
from timApp.user.user import get_owned_objects_query
from timApp.user.usergroup import UserGroup
from timApp.util.flask.requesthelper import RouteException, NotExist
from timApp.util.flask.responsehelper import ok_response, to_json_str, json_response
from timApp.util.flask.typedblueprint import TypedBlueprint

scheduling = TypedBlueprint(
    'scheduling',
    __name__,
    url_prefix='/scheduling/',
)


@dataclass
class Interval:
    every: float
    period: str


@dataclass
class ScheduledFunctionItem:
    block_id: int
    name: str
    expires: datetime
    last_run_at: Optional[datetime]
    total_run_count: int
    interval: Interval
    owners: List[UserGroup]
    doc_path: str


@scheduling.route('functions')
def get_scheduled_functions(all_users: bool = False) -> Response:
    verify_logged_in()
    u = get_current_user_object()

    q = (
        Block.query
            .filter_by(type_id=BlockType.ScheduledFunction.value)
            .join(BlockAccess)
            .filter(BlockAccess.type == AccessType.owner.value)
            .join(PeriodicTask)
            .with_entities(PeriodicTask)
            .options(
            joinedload(PeriodicTask.block)
                .joinedload(Block.accesses)
                .joinedload(BlockAccess.usergroup)
        )
    )

    if all_users:
        verify_admin()

    if not all_users:
        q = q.filter(BlockAccess.block_id.in_(get_owned_objects_query(u).subquery()))

    scheduled_fns: List[PeriodicTask] = q.all()

    docentries = DocEntry.query.filter(DocEntry.id.in_([t.task_id.doc_id for t in scheduled_fns])).all()
    d_map = {d.id: d for d in docentries}

    def gen() -> Generator[ScheduledFunctionItem, None, None]:
        for t in scheduled_fns:
            yield ScheduledFunctionItem(
                block_id=t.block_id,
                name=t.task_id.task_name,
                expires=t.expires,
                last_run_at=t.last_run_at,
                total_run_count=t.total_run_count,
                interval=Interval(every=t.interval.every, period=t.interval.period),
                owners=t.block.owners,
                doc_path=d_map[t.task_id.doc_id].path
            )

    return json_response(list(gen()), date_conversion=True)


@scheduling.route('functions', methods=['post'])
def add_scheduled_function(
        doc_id: int,
        plugin_name: str,
        interval: Duration,
        expires: datetime,
        args: Dict[str, Any] = field(default_factory=dict),
) -> Response:
    d = get_doc_or_abort(doc_id)

    verify_manage_access(d)

    u = get_current_user_object()
    if not u.belongs_to_any_of(UserGroup.get_teachers_group(), UserGroup.get_function_scheduler_group()):
        raise AccessDenied()
    p, _ = Plugin.from_task_id(
        f'{doc_id}.{plugin_name}',
        user_ctx=user_context_with_logged_in(None),
        view_ctx=default_view_ctx,
    )
    secs = int(interval.total_seconds())
    min_interval = current_app.config['MINIMUM_SCHEDULED_FUNCTION_INTERVAL']
    if secs < min_interval:
        raise RouteException(f'Minimum interval is {min_interval} seconds.')
    schedule = IntervalSchedule.query.filter_by(every=secs, period=IntervalSchedule.SECONDS).first()
    if not schedule:
        schedule = IntervalSchedule(every=secs, period=IntervalSchedule.SECONDS)
        db.session.add(schedule)

    assert p.task_id is not None
    task_id_str = p.task_id.doc_task
    existing = PeriodicTask.query.filter_by(name=task_id_str).first()
    if existing:
        raise RouteException('A scheduled function for this plugin already exists.')
    b = Block(type_id=BlockType.ScheduledFunction.value)
    periodic_task = PeriodicTask(
        block=b,
        interval=schedule,
        name=task_id_str,
        task='timApp.tim_celery.run_user_function',
        args=to_json_str([u.id, task_id_str, args]),
        expires=expires
    )
    b.set_owner(u.get_personal_group())
    db.session.add(periodic_task)
    db.session.commit()
    return json_response(periodic_task)


@scheduling.route('functions/<int:function_id>', methods=['delete'])
def delete_scheduled_plugin_run(
        function_id: int,
) -> Response:
    pto: PeriodicTask = (
        Block.query
            .filter_by(id=function_id, type_id=BlockType.ScheduledFunction.value)
            .join(PeriodicTask)
            .with_entities(PeriodicTask)
    ).first()
    if not pto:
        raise NotExist('scheduled function not found')
    u = get_current_user_object()
    if not u.has_manage_access(pto.block) and not u.is_admin:
        raise AccessDenied()
    db.session.delete(pto)
    db.session.delete(pto.block)
    db.session.commit()
    return ok_response()


@scheduling.route('parseYaml')
def parse_yaml_r(yaml: str) -> Response:
    try:
        parsed, _ = parse_yaml(yaml)
    except:
        raise RouteException('YAML is invalid.')
    return json_response(parsed)
