import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import {to2} from "tim/util/utils";

export async function showImportAnswersDialog() {
    const {ImportAnswersDialogComponent} = await import(
        "./import-answers-dialog.component"
    );
    return to2(
        (await angularDialog.open(ImportAnswersDialogComponent, undefined))
            .result
    );
}
