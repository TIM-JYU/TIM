import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import type {
    IRenameParams,
    RenameResult,
} from "tim/document/editing/plugin-rename-dialog.component";
import type {IManageResponse} from "tim/document/editing/edittypes";

export async function showRenameDialog(p: IRenameParams) {
    const {PluginRenameDialogComponent} = await import(
        "./plugin-rename-dialog.component"
    );
    return (await angularDialog.open(PluginRenameDialogComponent, p)).result;
}

export function isManageResponse(r: RenameResult): r is IManageResponse {
    return (r as IManageResponse).fulltext != null;
}
