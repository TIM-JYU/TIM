/**
 * Created by vesal on 28.12.2016.
 */
import {IController} from "angular";
import angular from "angular";
import {getParId} from "../document/parhelpers";
import {IPreviewParams, makePreview} from "../document/question/dynamicAnswerSheet";
import {ITimComponent, ViewCtrl} from "../document/viewctrl";
import {LectureController} from "../lecture/lectureController";
import {AnswerTable, IQuestionMarkup} from "../lecture/lecturetypes";
import {showQuestionAskDialog} from "../lecture/questionAskController";
import {showMessageDialog} from "../ui/dialog";
import {$http} from "../util/ngimport";
import {Binding, to} from "../util/utils";
import {IGenericPluginTopLevelFields, IPluginAttributes} from "./attributes";
import {pluginBindings, PluginMeta} from "./util";
import {TaskId} from "./taskid";

// Represents fields that are not actually stored in plugin markup but that are added by TIM alongside markup
// in view route so that extra information can be passed to qst component. TODO: they should not be inside markup.
interface IQstExtraInfo {
    invalid?: boolean;
    isTask: boolean;
}

type IQstAttributes = IPluginAttributes<IQuestionMarkup & IQstExtraInfo, AnswerTable>;

class QstController implements IController, ITimComponent {
    static $inject = ["$element"];
    private error?: string;
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
    private attrs!: IQstAttributes; // $onInit
    private preview!: IPreviewParams; // $onInit
    private button: string = "";
    private resetText: string = "";
    private stem: string = "";
    private newAnswer: AnswerTable = [];
    private pluginMeta: PluginMeta;
    private saveResponse: { saved: boolean, message: (string | undefined) } = {saved: false, message: undefined};
    // Duplicate attrs for ITimComp compatibility
    // TODO: Extend PluginBase
    public attrsall!: IGenericPluginTopLevelFields<IQuestionMarkup>;

    constructor(private element: JQLite) {
        this.pluginMeta = new PluginMeta(element);
        this.updateAnswer = this.updateAnswer.bind(this);
        this.errors = [];
        this.preclass = "qst";
        this.cursor = "\u0383"; // "\u0347"; // "\u02FD";
    }

    /**
     * Returns the name given to the plugin.
     */
    getName(): string | undefined {
        const taskId = this.pluginMeta.getTaskId();
        if (taskId) {
            return taskId.name;
        }
    }

    getContent() {
        return JSON.stringify(this.newAnswer);
    }

    getAreas(): string[] {
        const returnList: string[] = [];
        const parents = this.element.parents(".area");
        if (parents[0]) {
            const areaList = parents[0].classList;
            areaList.forEach(
                (value) => {
                    const m = value.match(/^area_(\S+)$/);
                    if (m) {
                        returnList.push(m[1]);
                    }
                },
            );
        }
        return returnList;
    }

    getTaskId(): TaskId | undefined {
        return this.pluginMeta.getTaskId();
    }

    belongsToArea(area: string): boolean {
        return this.getAreas().includes(area);
    }

    isForm() {
        return false;
    }

    isUnSaved(userChange?: boolean | undefined): boolean {
        return false;
    }

    async save(): Promise<{ saved: boolean; message: string | undefined; }> {
        if (this.isUnSaved()) {
            return this.doSaveText(false);
        } else {
            this.saveResponse.saved = false;
            this.saveResponse.message = undefined;
            return this.saveResponse;
        }
    }

    public getPar() {
        return this.element.parents(".par");
    }

    resetField(): string | undefined {
        return undefined;
    }

    supportsSetAnswer(): boolean {
        return false;
    }

    setAnswer(content: { [index: string]: unknown }): { ok: boolean, message: (string | undefined) } {
        return {ok: false, message: "Plugin doesn't support setAnswer"};
    }

    public $onInit() {
        if (this.vctrl) {
            this.vctrl.addTimComponent(this);
        }
        this.lctrl = this.vctrl && this.vctrl.lectureCtrl || LectureController.createAndInit(this.vctrl);
        this.isLecturer = (this.lctrl && this.lctrl.isLecturer) || false;
        this.attrs = JSON.parse(this.json) as IQstAttributes;
        this.attrsall = JSON.parse(this.json) as IGenericPluginTopLevelFields<IQuestionMarkup>;
        // console.log(this.attrs);
        this.preview = makePreview(this.attrs.markup, {
            answerTable: this.attrs.state ?? [],
            showCorrectChoices: this.attrs.show_result,
            showExplanations: this.attrs.show_result,
            enabled: !this.attrs.markup.invalid
        });
        this.result = "";
        this.button = this.attrs.markup.button ?? "Save";
        this.resetText = this.attrs.markup.resetText ?? "Reset";
        this.stem = this.attrs.markup.stem ?? "";
        this.newAnswer = this.preview.answerTable;
    }

    public $postLink() {
        this.plugin = this.element.parent().attr("data-plugin");
        this.taskId = this.element.parent().attr("id");
    }

    private getHeader() {
        return this.attrs.markup.header;
    }

    private getFooter() {
        return this.attrs.markup.footer;
    }

    private isTask() {
        return this.attrs.markup.isTask;
    }

    private updateAnswer(at: AnswerTable) {
        this.newAnswer = at;
    }

    private getQuestionTitle() {
        return this.attrs.markup.questionTitle;
    }

    private getQuestionTitleShort() {
        return this.attrs.markup.questionTitle;
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
        const url = this.pluginMeta.getAnswerUrl();

        const r = await to($http<{
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
        ));
        if (!r.ok) {
            this.isRunning = false;
            this.errors.push(r.result.data.error);
            this.error = "Ikuinen silmukka tai jokin muu vika?";
            this.saveResponse.message = r.result.data.error;
            return this.saveResponse;
        }
        const data = r.result.data;
        this.isRunning = false;
        this.error = "";
        this.result = data.web.result;
        if (data.web.markup && data.web.show_result) {
            this.preview = makePreview(data.web.markup, {answerTable: data.web.state, enabled: true});
            this.preview.showExplanations = true;
            this.preview.showCorrectChoices = true;
        }
        if (this.result === "saved") {
            this.saveResponse.saved = true;
        }
        return this.saveResponse;
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
    <button class="timButton" ng-bind-html="$ctrl.button" ng-if="$ctrl.button" ng-disabled="$ctrl.isRunning || $ctrl.isInvalid()"
            ng-click="$ctrl.saveText()"></button>
    &nbsp;&nbsp;
    <a class="questionAddedNew" ng-show="$ctrl.checkQstMode() && !$ctrl.isInvalid()" ng-click="$ctrl.questionClicked()">
        <span class="glyphicon glyphicon-question-sign" title="Ask question"></span>
    </a>
    <span ng-show="$ctrl.result">{{$ctrl.result}}</span>
    <p class="plgfooter" ng-bind-html="::$ctrl.getFooter()"></p>
</div>
<div ng-if="!$ctrl.isTask()">
    <a class="questionAddedNew" ng-click="$ctrl.questionClicked()">
        <span class="glyphicon glyphicon-question-sign" title="{{$ctrl.getQuestionTitle()}}"></span>
    </a>
    <p class="questionNumber" ng-bind="$ctrl.getQuestionTitleShort()"></p>
</div>
`,
});
