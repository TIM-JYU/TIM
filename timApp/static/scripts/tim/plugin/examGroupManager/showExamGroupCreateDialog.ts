import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import type {ExamGroup} from "tim/plugin/examGroupManager/exam-group-manager.component";
import type {ExamGroupDialogParams} from "tim/plugin/examGroupManager/exam-group-create-dialog.component";

export async function showExamGroupCreateDialog(
    params?: ExamGroupDialogParams
): Promise<ExamGroup> {
    const {ExamGroupCreateDialogComponent} = await import(
        "./exam-group-create-dialog.component"
    );
    return (await angularDialog.open(ExamGroupCreateDialogComponent, params))
        .result;
}
