/**
 * Created by vesal on 28.12.2016.
 */
import {IController, IRootElementService} from "angular";
import * as timHelper from "tim/timHelper";
import {timApp} from "../app";
import {AnswerTable, IPreviewParams, makePreview} from "../directives/dynamicAnswerSheet";
import {IAskedJsonJson, IAskedJsonJsonJson} from "../lecturetypes";
import {$http} from "../ngimport";
import {LectureController} from "./lectureController";

const TESTWITHOUTPLUGINS = false; // if one wants to test without qst plugins

function directiveTemplate() {
    if (TESTWITHOUTPLUGINS) {
        return "";
    }
    return `
<div class="csRunDiv qst no-popup-menu"><p></p>
    <p ng-if="$ctrl.stem" class="stem" ng-bind-html="$ctrl.stem"></p>
    <dynamic-answer-sheet
            questiondata="$ctrl.preview"
            answertable="$ctrl.answerdata"
            on-answer-change="$ctrl.updateAnswer"></dynamic-answer-sheet>
    <button class="timButton" ng-bind-html="$ctrl.button" ng-if="$ctrl.button" ng-disabled="$ctrl.isRunning"
            ng-click="$ctrl.saveText();"></button>
    &nbsp;&nbsp;
    <a class="questionAddedNew" ng-show="$ctrl.checkQstMode()">
        <span class="glyphicon glyphicon-question-sign" title="Ask question"></span>
    </a>
    <span ng-show="$ctrl.result">{{$ctrl.result}}</span>
    <p class="plgfooter"></p>
</div>
`;
}

function directiveTemplateQuestion() {
    if (TESTWITHOUTPLUGINS) {
        return "";
    }
    return `
<div class="csRunDiv qst no-popup-menu">
    <h4 class="questionTitle" ng-if="$ctrl.questionTitle"
        ng-bind-html="$ctrl.questionTitle"></h4>
    <dynamic-answer-sheet
            questiondata="$ctrl.preview"></dynamic-answer-sheet>
    <button class="timButton" ng-bind-html="$ctrl.button" ng-if="$ctrl.button" ng-disabled="$ctrl.isRunning"
            ng-click="$ctrl.saveText();"></button>&nbsp;&nbsp;
</div>
`;
}

export interface IQstAttributes {
    markup: {
        json: IAskedJsonJsonJson,
        isQuestion: boolean,
        user_id: string,
        header: string,
        footer: string,
        button: string,
        resetText: string,
        stem: string,
    };
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
    private taskId: string;
    private errors: string[];
    private lctrl: LectureController;
    private isLecturer: boolean;
    private preclass: string;
    private plugin: string;
    private json: string;
    private cursor: string;
    private element: IRootElementService;
    attrs: IQstAttributes;
    private preview: IPreviewParams;
    private button: string;
    private resetText: string;
    private stem: string;
    private answerdata: AnswerTable;
    private newAnswer: AnswerTable;

    constructor($element: IRootElementService) {
        this.element = $element;
        this.updateAnswer = this.updateAnswer.bind(this);
    }

    $onInit() {
        if (TESTWITHOUTPLUGINS) {
            return;
        }
        this.isLecturer = this.lctrl && this.lctrl.isLecturer;
        this.attrs = JSON.parse(this.json);
        // console.log(this.attrs);
        this.preview = makePreview({json: this.attrs.markup.json, expl: {}}, this.attrs.state || []);
        this.answerdata = this.preview.answerTable;
        this.errors = [];
        this.result = "";
        this.preclass = "qst";
        this.button = this.attrs.markup.button || "Save";
        this.resetText = this.attrs.markup.resetText || "Reset";
        this.stem = this.attrs.markup.stem;
        this.newAnswer = this.answerdata;
    }

    $postLink() {
        this.cursor = "\u0383"; // "\u0347"; // "\u02FD";
        this.plugin = this.element.parent().attr("data-plugin");
        this.taskId = this.element.parent().attr("id");
        return;

        // Otsikot.  Oletetaan että 1. elementti korvaatan header-otsikolla ja viimeinen footerilla
        if (!this.attrs.markup.isQuestion) {
            this.element[0].children[0].outerHTML = timHelper.toHeading(this.attrs.markup.header, "h4");
        }
        const n = this.element[0].children.length;
        if (!this.attrs.markup.isQuestion) {
            if (n > 1) {
                this.element[0].children[n - 1].outerHTML = timHelper.toHeading(this.attrs.markup.footer, 'p class="plgfooter"');
            }
        }
    }

    updateAnswer(at: AnswerTable) {
        this.newAnswer = at;
        console.log("answer updated:", JSON.stringify(at));
    }

    initCode() {
        this.error = "";
        this.result = "";
    }

    saveText() {
        this.doSaveText(false);
    }

    checkQstMode(nosave: boolean) {
        const w: any = window;
        return (w.in_lecture || w.lectureMode) && w.item.rights.teacher; //  $scope.$parent.$parent.wallName; // TODO: better check if in lecture page
    }

    doSaveText(nosave: boolean) {
        this.error = "... saving ...";
        this.isRunning = true;

        this.result = "";

        const answers = this.newAnswer;

        const params = {
            input: {
                answers,
                markup: {taskId: this.taskId, user_id: this.attrs.markup.user_id},
                nosave: false,
            },
        };

        if (nosave) {
            params.input.nosave = true;
        }
        let url = "/qst/answer";
        if (this.plugin) {
            url = this.plugin;
            const i = url.lastIndexOf("/");
            if (i > 0) {
                url = url.substring(i);
            }
            url += "/" + this.taskId + "/answer/";  // Häck piti vähän muuttaa, jotta kone häviää.
        }

        $http<{
            web: {
                error: string,
                result: string,
                show_result: boolean,
                state: AnswerTable,
                markup: IAskedJsonJson,
            },
        }>({
                method: "PUT",
                url,
                data: params,
                headers: {"Content-Type": "application/json"},
                timeout: 20000,
            },
        ).then((response) => {
            const data = response.data;
            this.isRunning = false;
            this.error = data.web.error;
            this.result = data.web.result;
            if (data.web.markup && data.web.show_result) {
                this.preview = makePreview({
                    expl: data.web.markup.expl,
                    json: data.web.markup.json,
                    points: data.web.markup.points,
                }, data.web.state);
            }
        }, (response) => {
            this.isRunning = false;
            this.errors.push(response.status);
            this.error = "Ikuinen silmukka tai jokin muu vika?";
        });
    }
}

timApp.component("qstRunner", {
    bindings: {
        json: "@",
    },
    controller: QstController,
    require: {
        lctrl: "?^timLecture",
    },
    template: directiveTemplate,
});

timApp.component("questionRunner", {
    bindings: {
        json: "@",
    },
    controller: QstController,
    require: {
        lctrl: "?^timLecture",
    },
    template: directiveTemplateQuestion,
});
