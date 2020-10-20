import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import {
    IQuestionDialogParams,
    IQuestionDialogResult,
} from "tim/document/question/question-edit-dialog.component";

export async function showQuestionEditDialog(
    params: IQuestionDialogParams
): Promise<IQuestionDialogResult> {
    const {QuestionEditDialogComponent} = await import(
        "./question-edit-dialog.component"
    );
    return (await angularDialog.open(QuestionEditDialogComponent, params))
        .result;
}
