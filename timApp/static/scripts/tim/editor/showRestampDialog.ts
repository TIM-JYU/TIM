import {IStampingData} from "tim/editor/pareditor";
import {angularDialog} from "tim/ui/angulardialog/dialog.service";

export async function showRestampDialog(data: IStampingData) {
    const {RestampDialogComponent} = await import("./restamp-dialog.component");
    return (await angularDialog.open(RestampDialogComponent, data)).result;
}
