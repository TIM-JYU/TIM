import type {IAskedQuestion, QuestionOrAnswer} from "tim/lecture/lecturetypes";
import {isAskedQuestion} from "tim/lecture/lecturetypes";
import {getStorage} from "tim/util/utils";
import type {AnswerToQuestionDialogComponent} from "tim/lecture/answer-to-question-dialog.component";

export function getAskedQuestionFromQA(qa: QuestionOrAnswer): IAskedQuestion {
    if (isAskedQuestion(qa)) {
        return qa;
    } else {
        return qa.asked_question;
    }
}

export let currentQuestion: AnswerToQuestionDialogComponent | undefined;

export function setCurrentQuestion(
    q: AnswerToQuestionDialogComponent | undefined
) {
    currentQuestion = q;
}

export const QUESTION_STORAGE = "lectureQuestion";

export function isOpenInAnotherTab(qa: QuestionOrAnswer) {
    return getStorage(QUESTION_STORAGE) === getAskedQuestionFromQA(qa).asked_id;
}
