import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import {IAllAnswersParams} from "tim/answer/all-answers-dialog.component";

export async function showAllAnswersDialog(p: IAllAnswersParams) {
    const {AllAnswersDialogComponent} = await import(
        "./all-answers-dialog.component"
    );
    return (await angularDialog.open(AllAnswersDialogComponent, p)).result;
}
