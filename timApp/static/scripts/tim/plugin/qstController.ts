/**
 * Created by vesal on 28.12.2016.
 */
import angular, {IController} from "angular";
import deepEqual from "deep-equal";
import {getParId} from "../document/parhelpers";
import {IPreviewParams, makePreview} from "../document/question/dynamicAnswerSheet";
import {ChangeType, ITimComponent, ViewCtrl} from "../document/viewctrl";
import {LectureController} from "../lecture/lectureController";
import {AnswerTable, IQuestionMarkup} from "../lecture/lecturetypes";
import {showQuestionAskDialog} from "../lecture/questionAskController";
import {showMessageDialog} from "../ui/dialog";
import {$http} from "../util/ngimport";
import {Binding, defaultErrorMessage, defaultTimeout, to} from "../util/utils";
import {IGenericPluginTopLevelFields} from "./attributes";
import {PluginBaseCommon, pluginBindings, PluginMeta} from "./util";

// Represents fields that are not actually stored in plugin markup but that are added by TIM alongside markup
// in view route so that extra information can be passed to qst component. TODO: they should not be inside markup.
interface IQstExtraInfo {
    invalid?: boolean;
    isTask: boolean;
    savedText?: string;
}

interface IQstAttributes extends IGenericPluginTopLevelFields<IQuestionMarkup & IQstExtraInfo> {
    state: AnswerTable | null;
    show_result: boolean;
    savedText: string;
}

class QstController extends PluginBaseCommon implements IController, ITimComponent {
    static $inject = ["$element"];
    private error?: string;
    private log?: string;
    private isRunning: boolean = false;
    private result?: string;
    private taskId?: string;
    private errors: string[];
    private vctrl?: ViewCtrl;
    private lctrl!: LectureController;
    private isLecturer: boolean = false;
    private preclass: string;
    private plugin?: string;
    private json!: Binding<string, "@">;
    private cursor: string;
    attrsall!: IQstAttributes; // $onInit
    private preview!: IPreviewParams; // $onInit
    private button: string = "";
    private resetText: string = "";
    private stem: string = "";
    private savedAnswer?: AnswerTable;
    private newAnswer?: AnswerTable;
    private changes = false;
    protected pluginMeta: PluginMeta;

    constructor(public element: JQLite) {
        super();
        this.pluginMeta = new PluginMeta(element);
        this.updateAnswer = this.updateAnswer.bind(this);
        this.errors = [];
        this.preclass = "qst";
        this.cursor = "\u0383"; // "\u0347"; // "\u02FD";
    }

    getContent() {
        return JSON.stringify(this.newAnswer);
    }

    get disableUnchanged() {
        return this.attrsall.markup.disableUnchanged;
    }

    get undoButton() {
        return this.attrsall.markup.undo?.button;
    }

    get undoTitle() {
        return this.attrsall.markup.undo?.title;
    }

    get undoConfirmation() {
        return this.attrsall.markup.undo?.confirmation;
    }

    isUnSaved(userChange?: boolean | undefined): boolean {
        return this.changes;
    }

    async save(): Promise<{ saved: boolean; message: string | undefined; }> {
        if (this.isUnSaved()) {
            return this.doSaveText(false);
        } else {
            return {saved: false, message: undefined};
        }
    }

    public $onInit() {
        if (this.vctrl) {
            this.vctrl.addTimComponent(this);
        }
        this.lctrl = this.vctrl?.lectureCtrl ?? LectureController.createAndInit(this.vctrl);
        this.isLecturer = this.lctrl?.isLecturer || false;
        this.attrsall = JSON.parse(this.json) as IQstAttributes;
        this.preview = makePreview(this.attrsall.markup, {
            answerTable: this.attrsall.state ?? [],
            showCorrectChoices: this.attrsall.show_result,
            showExplanations: this.attrsall.show_result,
            enabled: !this.attrsall.markup.invalid,
        });
        this.result = "";
        this.button = this.attrsall.markup.button ?? this.attrsall.markup.buttonText ?? "Save";
        this.resetText = this.attrsall.markup.resetText ?? "Reset";
        this.stem = this.attrsall.markup.stem ?? "";
    }

    public $postLink() {
        this.plugin = this.element.parent().attr("data-plugin");
        this.taskId = this.element.parent().attr("id");
    }

    private getHeader() {
        return this.attrsall.markup.header;
    }

    private getFooter() {
        return this.attrsall.markup.footer;
    }

    private isTask() {
        return this.attrsall.markup.isTask;
    }

    private checkChanges() {
        const oldVal = this.changes;
        this.changes = !deepEqual(this.savedAnswer, this.newAnswer);
        if (oldVal != this.changes) {
            this.updateListeners(this.changes ? ChangeType.Modified : ChangeType.Saved);
        }
    }

    updateListeners(state: ChangeType) {
        if (!this.vctrl) {
            return;
        }
        const taskId = this.pluginMeta.getTaskId();
        if (!taskId) {
            return;
        }
        this.vctrl.informChangeListeners(taskId, state, (this.attrsall.markup.tag ? this.attrsall.markup.tag : undefined));
    }

    private updateAnswer(at: AnswerTable) {
        // updateAnswer is called always at least once from dynamicAnswerSheet (see the $onChanges in that file).
        // Upon first call, we record the currently saved answer.
        if (this.newAnswer === undefined) {
            this.savedAnswer = at;
        }
        this.newAnswer = at;
        this.checkChanges();
        if (this.changes) {
            this.result = undefined;
        }
    }

    private getQuestionTitle() {
        return this.attrsall.markup.questionTitle;
    }

    private getQuestionTitleShort() {
        return this.attrsall.markup.questionTitle;
    }

    private questionClicked() {
        const par = this.element.parents(".par");
        const parId = getParId(par);
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
        const result = await to(showQuestionAskDialog(
            {
                docId: this.lctrl.viewctrl.docId,
                parId: parId,
                showAsk: this.lctrl.lectureSettings.inLecture,
            }));
        if (result.ok) {
            this.lctrl.lastQuestion = result.result;
        }
    }

    private initCode() {
        this.error = "";
        this.result = "";
    }

    private saveText() {
        this.doSaveText(false);
    }

    private checkQstMode() {
        return this.lctrl?.viewctrl?.item.rights.teacher;
    }

    private isInvalid() {
        return this.attrsall.markup.invalid;
    }

    private async doSaveText(nosave: boolean) {
        this.log = undefined;
        this.error = undefined;
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
        const url = this.pluginMeta.getAnswerUrl();

        const r = await to($http<{
            web: {
                result: string,
                show_result: boolean,
                state: AnswerTable,
                markup: IQuestionMarkup,
                error?: string;
            },
        }>({
                method: "PUT",
                url,
                data: params,
                headers: {"Content-Type": "application/json"},
                timeout: defaultTimeout,
            },
        ));
        if (!r.ok) {
            this.isRunning = false;
            this.errors.push(r.result.data?.error);
            console.log(r);
            this.error = r.result.data?.error ?? this.attrsall.markup.connectionErrorMessage ?? defaultErrorMessage;
            return {saved: false, message: r.result.data?.error ?? this.attrsall.markup.connectionErrorMessage};
        }
        const data = r.result.data;
        this.isRunning = false;
        let result = data.web.result;
        if (result == "Saved" && this.attrsall.markup.savedText) { result = this.attrsall.markup.savedText; }
        this.result = result;
        this.log = data.web.error;
        if (data.web.markup && data.web.show_result) {
            this.preview = makePreview(data.web.markup, {answerTable: data.web.state, enabled: true});
            this.preview.showExplanations = true;
            this.preview.showCorrectChoices = true;
        }
        this.savedAnswer = this.newAnswer;
        this.checkChanges();
        return {saved: true, message: undefined};
    }

    tryResetChanges(): void {
        if (this.undoConfirmation && !window.confirm(this.undoConfirmation)) {
            return;
        }
        this.resetChanges();
    }

    resetChanges(): void {
        this.newAnswer = this.savedAnswer;
        this.preview = makePreview(this.attrsall.markup, {
            answerTable: this.savedAnswer,
            showCorrectChoices: this.attrsall.show_result,
            showExplanations: this.attrsall.show_result,
            enabled: !this.attrsall.markup.invalid,
        });
        this.checkChanges();
    }

    protected getElement() {
        return this.element;
    }
}

const qstApp = angular.module("qstApp", ["ngSanitize"]);
export const moduleDefs = [qstApp];

qstApp.component("qstRunner", {
    bindings: pluginBindings,
    controller: QstController,
    require: {
        vctrl: "?^timView",
    },
    template: `
<div class="csRunDiv qst no-popup-menu" ng-if="$ctrl.isTask()">
    <h4 ng-if="::$ctrl.getHeader()" ng-bind-html="::$ctrl.getHeader()"></h4>
    <p ng-if="::$ctrl.stem" class="stem" ng-bind-html="::$ctrl.stem"></p>
    <dynamic-answer-sheet
            questiondata="$ctrl.preview"
            on-answer-change="$ctrl.updateAnswer"></dynamic-answer-sheet>
    <button class="timButton" ng-bind-html="$ctrl.button" ng-if="$ctrl.button" ng-disabled="$ctrl.isRunning || $ctrl.isInvalid() || ($ctrl.disableUnchanged && !$ctrl.isUnSaved())"
            ng-click="$ctrl.saveText()"></button>
    <a href="" ng-if="$ctrl.undoButton && $ctrl.isUnSaved()" title="{{::$ctrl.undoTitle}}" ng-click="$ctrl.tryResetChanges();">
        &nbsp;{{::$ctrl.undoButton}}
    </a>

    &nbsp;&nbsp;
    <a class="questionAddedNew" ng-show="$ctrl.checkQstMode() && !$ctrl.isInvalid()" ng-click="$ctrl.questionClicked()">
        <span class="glyphicon glyphicon-question-sign" title="Ask question"></span>
    </a>
    <span ng-show="$ctrl.result">{{$ctrl.result}}</span>
    <p class="plgfooter" ng-bind-html="::$ctrl.getFooter()"></p>
    <div ng-if="$ctrl.error" class="error" style="font-size: 12px" ng-bind-html="$ctrl.error"></div>
</div>
<div ng-if="!$ctrl.isTask()">
    <a class="questionAddedNew" ng-click="$ctrl.questionClicked()">
        <span class="glyphicon glyphicon-question-sign" title="{{$ctrl.getQuestionTitle()}}"></span>
    </a>
    <p class="questionNumber" ng-bind="$ctrl.getQuestionTitleShort()"></p>
</div>
<div ng-if="$ctrl.log" class="qstLog"  ng-bind-html="$ctrl.log"></div>
`,
});
