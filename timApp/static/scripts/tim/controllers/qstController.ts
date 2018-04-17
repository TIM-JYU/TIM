/**
 * Created by vesal on 28.12.2016.
 */
import {IController, IRootElementService} from "angular";
import * as timHelper from "tim/timHelper";
import {timApp as qstApp} from "../app";
import {showMessageDialog} from "../dialog";
import {IPreviewParams, makePreview} from "../directives/dynamicAnswerSheet";
import {AnswerTable, IQuestionMarkup} from "../lecturetypes";
import {$http} from "../ngimport";
import {LectureController} from "./lectureController";
import {showQuestionAskDialog} from "./questionAskController";
import {getParId} from "./view/parhelpers";

export interface IQstAttributes {
    markup: IQuestionMarkup;
    doLazy: boolean;
    anonymous: boolean;
    info: {};
    preview: boolean;
    show_result: boolean;
    state: AnswerTable | null;
    targetFormat: string;
    taskID: string;
    taskIDExt: string;
    userPrint: boolean;
}

class QstController implements IController {
    private static $inject = ["$element"];
    private error: string;
    private isRunning: boolean;
    private result: string;
    private taskId?: string;
    private errors: string[];
    private lctrl?: LectureController;
    private isLecturer: boolean;
    private preclass: string;
    private plugin?: string;
    private json: string;
    private cursor: string;
    private element: IRootElementService;
    private attrs: IQstAttributes;
    private preview: IPreviewParams;
    private button: string;
    private resetText: string;
    private stem: string;
    private newAnswer: AnswerTable;

    constructor($element: IRootElementService) {
        this.element = $element;
        this.updateAnswer = this.updateAnswer.bind(this);
    }

    public $onInit() {
        this.isLecturer = (this.lctrl && this.lctrl.isLecturer) || false;
        this.attrs = JSON.parse(this.json);
        // console.log(this.attrs);
        this.preview = makePreview(this.attrs.markup, {answerTable: this.attrs.state || [], enabled: !this.attrs.markup.invalid});
        this.errors = [];
        this.result = "";
        this.preclass = "qst";
        this.button = this.attrs.markup.button || "Save";
        this.resetText = this.attrs.markup.resetText || "Reset";
        this.stem = this.attrs.markup.stem || "";
        this.newAnswer = this.preview.answerTable;
    }

    public $postLink() {
        this.cursor = "\u0383"; // "\u0347"; // "\u02FD";
        this.plugin = this.element.parent().attr("data-plugin");
        this.taskId = this.element.parent().attr("id");
    }

    private getHeader() {
        return timHelper.toHeading(this.attrs.markup.header || "", "h4");
    }

    private getFooter() {
        return timHelper.toHeading(this.attrs.markup.footer || "", 'p class="plgfooter"');
    }

    private isTask() {
        return this.attrs.markup.isTask;
    }

    private updateAnswer(at: AnswerTable) {
        this.newAnswer = at;
        console.log("answer updated:", JSON.stringify(at));
    }

    private getQuestionTitle() {
        return this.attrs.markup.questionTitle;
    }

    private getQuestionTitleShort() {
        return this.attrs.markup.questionTitle;
    }

    private questionClicked() {
        const $par = this.element.parents(".par");
        const parId = getParId($par);
        if (!parId) {
            showMessageDialog("Not a valid paragraph.");
            return;
        }
        this.showQuestionNew(parId);
    }

    private async showQuestionNew(parId: string) {
        if (this.lctrl == null || this.lctrl.viewctrl == null) {
            await showMessageDialog("Cannot show question because not in lecture mode or in a document.");
            return;
        }
        const result = await showQuestionAskDialog(
            {
                docId: this.lctrl.viewctrl.docId,
                parId: parId,
                showAsk: this.lctrl.lectureSettings.inLecture,
            });
    }

    private initCode() {
        this.error = "";
        this.result = "";
    }

    private saveText() {
        this.doSaveText(false);
    }

    private checkQstMode() {
        return this.lctrl && this.lctrl.viewctrl && this.lctrl.viewctrl.item.rights.teacher;
    }

    private isInvalid() {
        return this.attrs.markup.invalid;
    }

    private async doSaveText(nosave: boolean) {
        this.error = "... saving ...";
        this.isRunning = true;

        this.result = "";

        const answers = this.newAnswer;

        const params = {
            input: {
                answers,
                nosave: false,
            },
        };

        if (nosave) {
            params.input.nosave = true;
        }
        let url = "/qst/answer";
        if (this.plugin && this.taskId) {
            url = this.plugin;
            const i = url.lastIndexOf("/");
            if (i > 0) {
                url = url.substring(i);
            }
            url += "/" + this.taskId + "/answer/";  // Häck piti vähän muuttaa, jotta kone häviää.
        } else {
            this.error = "plugin or taskId missing";
            return;
        }
        try {
            var response = await $http<{
                web: {
                    result: string,
                    show_result: boolean,
                    state: AnswerTable,
                    markup: IQuestionMarkup,
                },
            }>({
                    method: "PUT",
                    url,
                    data: params,
                    headers: {"Content-Type": "application/json"},
                    timeout: 20000,
                },
            );
        } catch (e) {
            this.isRunning = false;
            this.errors.push(e.status);
            this.error = "Ikuinen silmukka tai jokin muu vika?";
            return;
        }
        const data = response.data;
        this.isRunning = false;
        this.error = "";
        this.result = data.web.result;
        if (data.web.markup && data.web.show_result) {
            this.preview = makePreview(data.web.markup, {answerTable: data.web.state, enabled: true});
        }
    }
}

qstApp.component("qstRunner", {
    bindings: {
        json: "@",
    },
    controller: QstController,
    require: {
        lctrl: "?^timLecture",
    },
    template: `
<div class="csRunDiv qst no-popup-menu" ng-if="$ctrl.isTask()">
<p ng-bind-html="$ctrl.getHeader()"></p>
    <p ng-if="$ctrl.stem" class="stem" ng-bind-html="$ctrl.stem"></p>
    <dynamic-answer-sheet
            questiondata="$ctrl.preview"
            on-answer-change="$ctrl.updateAnswer"></dynamic-answer-sheet>
    <button class="timButton" ng-bind-html="$ctrl.button" ng-if="$ctrl.button" ng-disabled="$ctrl.isRunning || $ctrl.isInvalid()"
            ng-click="$ctrl.saveText()"></button>
    &nbsp;&nbsp;
    <a class="questionAddedNew" ng-show="$ctrl.checkQstMode() && !$ctrl.isInvalid()" ng-click="$ctrl.questionClicked()">
        <span class="glyphicon glyphicon-question-sign" title="Ask question"></span>
    </a>
    <span ng-show="$ctrl.result">{{$ctrl.result}}</span>
    <p class="plgfooter" ng-bind-html="$ctrl.getHeader()"></p>
</div>
<div ng-if="!$ctrl.isTask()">
    <a class="questionAddedNew" ng-click="$ctrl.questionClicked()">
        <span class="glyphicon glyphicon-question-sign" title="{{$ctrl.getQuestionTitle()}}"></span>
    </a>
    <p class="questionNumber" ng-bind="$ctrl.getQuestionTitleShort()"></p>
</div>
`,
});
