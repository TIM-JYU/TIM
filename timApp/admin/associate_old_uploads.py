from timApp.admin.search_in_documents import SearchArgumentsBasic, search
from timApp.admin.util import enum_docs
from timApp.item.block import BlockType, Block
from timApp.item.blockassociation import BlockAssociation
from timApp.timdb.sqa import db
from timApp.upload.uploadedfile import UploadedFile
from timApp.user.usergroup import UserGroup


def associate_old_uploads():
    """Associates old uploads with documents and removes access for those uploads from anonymous users.
    This means only document viewers will be able to view the uploaded files, as it is with new uploads.
    """
    s = SearchArgumentsBasic(
        format='',
        onlyfirst=False,
        regex=True,
        term=r'\[[^\[\]]+\]\(/(files|images)/(\d+)/([^()]+)\)'
    )
    anon = UserGroup.get_anonymous_group()

    def del_anon(u: UploadedFile):
        if not u.filesystem_path.exists():
            print(f'Upload does not exist in filesystem: {u.relative_filesystem_path}')
            return
        for acc in u.block.accesses:
            if acc.usergroup == anon:
                print(f'Deleting anon access from upload {u.relative_filesystem_path}')
                db.session.delete(acc)

    for d in enum_docs():
        for r in search(d, s, use_exported=False):
            kind, up_id, filename = r.match.group(1), int(r.match.group(2)), r.match.group(3)
            up = UploadedFile.find_by_id_and_type(up_id, BlockType.File if kind == 'files' else BlockType.Image)
            if not up:
                print(f'Upload not found: {up_id}')
                continue
            del_anon(up)
            if d.block not in up.parents:
                print(f'{d.url}: adding child {up.relative_filesystem_path}')
                up.parents.append(d.block)
            else:
                print(f'{d.url}: already has child {up.relative_filesystem_path}')
    orphans = Block.query.filter(Block.type_id.in_([BlockType.File.value, BlockType.Image.value]) & Block.id.notin_(
        BlockAssociation.query.with_entities(BlockAssociation.child))).all()
    print(f'Deleting anon accesses from {len(orphans)} orphan uploads')
    for o in orphans:
        del_anon(UploadedFile(o))
    db.session.commit()
