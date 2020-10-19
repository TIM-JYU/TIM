import {IBookmark} from "tim/bookmark/bookmark.service";
import {angularDialog} from "tim/ui/angulardialog/dialog.service";

export async function showBookmarkDialog(bookmark: IBookmark) {
    const {BookmarkDialogComponent} = await import(
        "./bookmark-dialog.component"
    );
    return (await angularDialog.open(BookmarkDialogComponent, bookmark)).result;
}
