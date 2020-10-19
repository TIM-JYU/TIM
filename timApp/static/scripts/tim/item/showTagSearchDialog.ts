import {angularDialog} from "tim/ui/angulardialog/dialog.service";

export async function showTagSearchDialog() {
    const {SearchTagsDialogComponent} = await import(
        "./search-tags-dialog.component"
    );
    return await (
        await angularDialog.open(SearchTagsDialogComponent, undefined)
    ).result;
}
