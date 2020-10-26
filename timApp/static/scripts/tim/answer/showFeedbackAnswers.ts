import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import {IFeedbackAnswersParams} from "tim/answer/feedback-answers-dialog.component";

export async function showFeedbackAnswers(p: IFeedbackAnswersParams) {
    const {FeedbackAnswersDialogComponent} = await import(
        "./feedback-answers-dialog.component"
    );
    return (await angularDialog.open(FeedbackAnswersDialogComponent, p)).result;
}
