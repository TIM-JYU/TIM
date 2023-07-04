from typing import Callable

from sqlalchemy import select

from timApp.admin.search_in_documents import (
    SearchArgumentsBasic,
    search,
    create_basic_search_argparser,
)
from timApp.admin.util import enum_docs, process_items, BasicArguments
from timApp.document.docinfo import DocInfo
from timApp.item.block import BlockType, Block
from timApp.item.blockassociation import BlockAssociation
from timApp.timdb.sqa import db
from timApp.upload.uploadedfile import UploadedFile
from timApp.user.usergroup import UserGroup

upload_regexes = [
    r"\[[^\[\]]*\]\(/(?P<type>files|images)/(?P<id>\d+)/(?P<name>[^()]+)\)",  # normal images and links
    r'"/(?P<type>files|images)/(?P<id>\d+)/(?P<name>[^"]+)"',  # relative path in quotes
    r'(?::|src=) *"?/(?P<type>files|images)/(?P<id>\d+)/(?P<name>[^"\n]+)"?',  # in plugin markdown, e.g. "file: /images/123456/a.jpg"
]


def associate_old_uploads() -> None:
    """Associates old uploads with documents and removes access for those uploads from anonymous users.
    This means only document viewers will be able to view the uploaded files, as it is with new uploads.
    """
    anon = UserGroup.get_anonymous_group()

    def del_anon(u: UploadedFile) -> None:
        if not u.filesystem_path.exists():
            print(f"Upload does not exist in filesystem: {u.relative_filesystem_path}")
            return
        for acc in u.block.accesses.values():
            if acc.usergroup == anon:
                print(f"Deleting anon access from upload {u.relative_filesystem_path}")
                db.session.delete(acc)

    for d in enum_docs():
        for r in upload_regexes:
            associate_document(
                d,
                SearchArgumentsBasic(format="", onlyfirst=False, regex=True, term=r),
                del_anon,
            )
    orphans = db.session.execute(
        select(Block)
        .filter(
            Block.type_id.in_([BlockType.File.value, BlockType.Image.value])
            & Block.id.notin_(select(BlockAssociation.child))
        )
    ).scalars().all()
    print(f"Deleting anon accesses from {len(orphans)} orphan uploads")
    for o in orphans:
        del_anon(UploadedFile(o))
    db.session.commit()


def associate_document(
    d: DocInfo,
    search_opt: SearchArgumentsBasic,
    del_anon: Callable[[UploadedFile], None] | None = None,
) -> int:
    found = 0
    for r in search(d, search_opt, use_exported=False):
        found = r.num_pars_found
        kind, up_id, filename = (
            r.match.group("type"),
            int(r.match.group("id")),
            r.match.group("name"),
        )
        up = UploadedFile.find_by_id(up_id)
        if not up:
            print(f"Upload not found: {up_id}/{filename}")
            continue
        if del_anon:
            del_anon(up)
        if d.block not in up.parents:
            print(f"{d.url}: adding child {up.relative_filesystem_path}")
            up.parents.append(d.block)
        else:
            print(f"{d.url}: already has child {up.relative_filesystem_path}")
    return found


def search_and_print(d: DocInfo, _args: BasicArguments) -> int:
    found = 0
    for r in upload_regexes:
        found += associate_document(
            d, SearchArgumentsBasic(format="", onlyfirst=False, regex=True, term=r)
        )
    return found


def main() -> None:
    parser = create_basic_search_argparser(
        "Scans documents for uploaded files and associates them with the containing documents",
        is_readonly=False,
        require_term=False,
    )
    process_items(search_and_print, parser)


if __name__ == "__main__":
    main()
