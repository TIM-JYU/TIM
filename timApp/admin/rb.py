from typing import List

from sqlalchemy import select

from timApp.admin.associate_old_uploads import upload_regexes
from timApp.admin.search_in_documents import (
    create_basic_search_argparser,
    SearchArgumentsBasic,
    search,
)
from timApp.admin.util import process_items, DryrunnableArguments
from timApp.document.docinfo import DocInfo
from timApp.item.blockassociation import BlockAssociation
from timApp.timdb.sqa import run_sql, db
from timApp.upload.uploadedfile import UploadedFile


def remove_blockassociations(d: DocInfo, _) -> int:
    found = 0
    associations: List[int] = []
    for c in d.block.children:
        associations.append(c.id)
    if not associations:
        return found
    files: List[UploadedFile] = []
    dead_files: List[int] = []
    for r in upload_regexes:
        search_opt = SearchArgumentsBasic(
            format="", onlyfirst=False, regex=True, term=r
        )
        for r in search(d, search_opt, use_exported=False):
            kind, up_id, filename = (
                r.match_pattern.group("type"),
                int(r.match_pattern.group("id")),
                r.match_pattern.group("name"),
            )
            associations.remove(up_id)
    if not associations:
        return found
    for a in associations:
        up = UploadedFile.find_by_id(a)
        if up:
            ba = (
                run_sql(
                    select(BlockAssociation).filter_by(parent=d.id, child=a).limit(1)
                )
                .scalars()
                .first()
            )
            if not ba:
                db.session.rollback()
                raise Exception(
                    f"Error finding blockassociation to file {up.filename} ({up.id}) in document {d.title} ({d.id})"
                )
            db.session.delete(ba)
            found += 1
            files.append(up)
        else:
            dead_files.append(a)
    print("Document:", d.path, d.id)
    print(
        "Removing unused association from following files:",
        [f"{f.filename} ({f.id})" for f in files],
    )
    if dead_files:
        print("Unknown associations (not altered):", dead_files)
    return found


def main() -> None:
    parser = create_basic_search_argparser(
        "Remove unused blockassociations",
        is_readonly=False,
        require_term=False,
    )
    process_items(remove_blockassociations, parser)


if __name__ == "__main__":
    main()
