import {IItem} from "tim/item/IItem";
import {ILecture, ILectureMessage} from "tim/lecture/lecturetypes";
import {angularDialog} from "tim/ui/angulardialog/dialog.service";
import {IStatisticsParams} from "tim/lecture/question-statistics-dialog.component";
import {IAnswerQuestionParams} from "tim/lecture/answer-to-question-dialog.component";
import type {LectureWallDialogComponent as LectureWallDialogComponentType} from "tim/lecture/lecture-wall-dialog.component";
import {QuestionPreviewParams} from "tim/lecture/question-preview-dialog.component";

export async function showLectureDialog(item: IItem | ILecture) {
    const {LectureDialogComponent} = await import("./lecture-dialog.component");
    return (await angularDialog.open(LectureDialogComponent, item)).result;
}

export async function showStatisticsDialog(p: IStatisticsParams) {
    const {QuestionStatisticsDialogComponent} = await import(
        "./question-statistics-dialog.component"
    );
    return await (
        await angularDialog.open(QuestionStatisticsDialogComponent, p)
    ).result;
}

export async function showQuestionAnswerDialog(p: IAnswerQuestionParams) {
    const {AnswerToQuestionDialogComponent} = await import(
        "./answer-to-question-dialog.component"
    );
    return await (await angularDialog.open(AnswerToQuestionDialogComponent, p))
        .result;
}

export async function showLectureWall(messages: ILectureMessage[]) {
    const {LectureWallDialogComponent} = await import(
        "./lecture-wall-dialog.component"
    );
    return await angularDialog.open<
        {messages: ILectureMessage[]},
        void,
        LectureWallDialogComponentType
    >(LectureWallDialogComponent, {messages});
}

export async function showLectureEnding(lecture: ILecture) {
    const {LectureEndingDialogComponent} = await import(
        "./lecture-ending-dialog.component"
    );
    return await (
        await angularDialog.open(LectureEndingDialogComponent, lecture)
    ).result;
}

export async function showQuestionAskDialog(p: QuestionPreviewParams) {
    const {QuestionPreviewDialogComponent} = await import(
        "./question-preview-dialog.component"
    );
    return await (await angularDialog.open(QuestionPreviewDialogComponent, p))
        .result;
}
