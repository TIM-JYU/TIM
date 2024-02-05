import {angularDialog} from "tim/ui/angulardialog/dialog.service";

export interface ICreateItemDialogParams {
    location?: string;
    source?: string;
    itemType?: string;
}

export async function showCreateItem(p?: ICreateItemDialogParams) {
    const {CreateItemDialogComponent} = await import(
        "./create-item-dialog.component"
    );
    return (await angularDialog.open(CreateItemDialogComponent, p)).result;
}
