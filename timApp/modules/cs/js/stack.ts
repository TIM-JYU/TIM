import angular from "angular";
import * as t from "io-ts";
import {ParCompiler} from "tim/editor/parCompiler";
import {GenericPluginMarkup, Info, withDefault} from "tim/plugin/attributes";
import {PluginBase, pluginBindings} from "tim/plugin/util";
import {$http, $sce} from "tim/util/ngimport";
import {defaultTimeout, to, windowAsAny} from "tim/util/utils";

const stackApp = angular.module("stackApp", ["ngSanitize"]);
export const moduleDefs = [stackApp];
const STACK_VARIABLE_PREFIX = "stackapi_";

// this.attrs
const StackMarkup = t.intersection([
    t.partial({
        beforeOpen: t.string,
        buttonBottom: t.boolean,
        by: t.string,
        correctresponse: t.boolean,
        generalfeedback: t.boolean,
        open: t.boolean,
        timWay: t.boolean,
    }),
    GenericPluginMarkup,
    t.type({
        autopeek: withDefault(t.boolean, true),
        lang: withDefault(t.string, "fi"),
    }),
]);
const StackAll = t.intersection([
    t.partial({
        by: t.string,
        timWay: t.boolean,
        usercode: t.string,
    }),
    t.type({
        info: Info,
        markup: StackMarkup,
        preview: t.boolean,
    }),
]);

type StackResult =
    | string
    | {
          answernotes: string;
          api_time: number;
          error: false;
          formatcorrectresponse: string;
          generalfeedback: string;
          questiontext: string;
          request_time: number;
          score: number;
          summariseresponse: unknown;
      }
    | {
          error: true;
          message: string;
      };

interface IStackData {
    answer: Record<string, string>;
    prefix: string;
    seed?: number;
    verifyvar: string;
}

class StackController extends PluginBase<
    t.TypeOf<typeof StackMarkup>,
    t.TypeOf<typeof StackAll>,
    typeof StackAll
> {
    get english() {
        return this.attrs.lang === "en";
    }

    buttonText() {
        const txt = super.buttonText();
        if (txt) {
            return txt;
        }
        return this.english ? "Send" : "Lähetä";
    }

    private span: string = "";
    private error: string = "";
    private userCode: string = "";
    private stackoutput: string = "";
    private stackinputfeedback: string = "";
    private stackpeek: boolean = false;
    private stackfeedback: string = "";
    private stackformatcorrectresponse: string = "";
    private stackscore: string = "";
    private stacksummariseresponse: string = "";
    private stackanswernotes: string = "";
    private stacktime: string = "";
    private isRunning: boolean = false;
    private inputrows: number = 1;
    private timWay: boolean = false; // if answer is given to TIM TextArea-field
    private isOpen: boolean = false;
    private lastInputFieldId: string = "";
    private lastInputFieldValue: string = "";
    private lastInputFieldElement: HTMLInputElement | undefined;
    private button: string = "";

    private timer: NodeJS.Timer | undefined;

    private taskUrl: string = "";

    $onInit() {
        super.$onInit();
        this.button = this.buttonText();
        const aa = this.attrsall;
        this.userCode = aa.usercode ?? this.attrs.by ?? "";
        this.timWay = aa.timWay ?? this.attrs.timWay ?? false;

        this.element.on("keydown", (event) => {
            if (event.ctrlKey || event.metaKey) {
                switch (String.fromCharCode(event.which).toLowerCase()) {
                    case "s":
                        event.preventDefault();
                        this.runSend(false);
                        break;
                }
            }
        });

        if (this.attrs.open) {
            this.runGetTask();
        }
    }

    processNodes(
        res: Record<string, string>,
        nodes:
            | HTMLCollectionOf<HTMLInputElement>
            | HTMLCollectionOf<HTMLTextAreaElement>
            | HTMLCollectionOf<HTMLSelectElement>,
        id: string
    ): Record<string, string> {
        for (const element of nodes) {
            if (
                element.name.startsWith(STACK_VARIABLE_PREFIX) &&
                !element.name.includes("_val") &&
                element.name.includes(id)
            ) {
                if (
                    element instanceof HTMLInputElement &&
                    (element.type === "checkbox" || element.type === "radio")
                ) {
                    if (element.checked) {
                        res[element.name] = element.value;
                    }
                } else {
                    res[element.name] = element.value;
                }
            }
        }
        return res;
    }

    outputAsHtml() {
        const s = $sce.trustAsHtml(this.stackoutput);
        return s;
    }

    stackinputfeedbackAsHtml() {
        const s = $sce.trustAsHtml(this.stackinputfeedback);
        return s;
    }

    collectAnswer(id: string) {
        const parent = this.element[0];
        const inputs = parent.getElementsByTagName("input");
        const textareas = parent.getElementsByTagName("textarea");
        const selects = parent.getElementsByTagName("select");
        let res: Record<string, string> = {};
        if (!this.timWay) {
            res = this.processNodes(res, inputs, id);
            res = this.processNodes(res, textareas, id);
            res = this.processNodes(res, selects, id);
            if (Object.keys(res).length && this.userCode) {
                this.userCode = JSON.stringify(res);
            } else {
                try {
                    res = JSON.parse(this.userCode) as Record<string, string>;
                } catch {
                    // this.timWay = true;
                }
            } // note: cannot be else because timWay may change during try
        }
        if (this.timWay) {
            res[STACK_VARIABLE_PREFIX + "ans1"] = this.userCode;
        }
        return res;
    }

    collectData() {
        return {
            answer: this.collectAnswer(""),
            prefix: STACK_VARIABLE_PREFIX,
        };
    }

    replace(s: string): string {
        // s = s.replace('https://stack-api-server/plots/', '/stackserver/plots/');
        return s;
    }

    async handleServerResult(r: StackResult, getTask: boolean) {
        if (typeof r === "string") {
            this.error = r.toString();
            return;
        }
        if (r.error) {
            this.error = r.message;
            return;
        }

        const qt = this.replace(r.questiontext);
        const i = qt.indexOf('<div class="stackinputfeedback"');
        const helper = await import("../stack/ServerSyncValues");
        windowAsAny().ServerSyncValues = helper.ServerSyncValues;
        windowAsAny().findParentElementFromScript =
            helper.findParentElementFromScript;
        if (this.attrs.buttonBottom || i < 0) {
            this.stackoutput = qt;
            this.stackinputfeedback = "";
        } else {
            this.stackoutput = qt.substr(0, i) + "\n";
            this.stackinputfeedback = qt.substr(i);
        }

        if (!getTask) {
            this.stackfeedback = this.replace(r.generalfeedback);
            this.stackformatcorrectresponse = this.replace(
                r.formatcorrectresponse
            );
            this.stacksummariseresponse = this.replace(
                JSON.stringify(r.summariseresponse)
            );
            this.stackanswernotes = this.replace(JSON.stringify(r.answernotes));
        }
        this.stackscore = r.score.toString();
        this.stacktime =
            "Request Time: " +
            r.request_time.toFixed(2) +
            " Api Time: " +
            r.api_time.toFixed(2);

        ParCompiler.processAllMathDelayed(this.element, 1);
        const html = this.element.find(".stackOutput");
        const inputs = html.find("input");
        const inputse = html.find("textarea");
        $(inputs).keyup((e) => this.inputHandler(e));
        $(inputse).keyup((e) => this.inputHandler(e));
        if (getTask) {
            // remove input validation texts
            const divinput = this.element.find(".stackinputfeedback");
            divinput.remove();
        }
        // await ParCompiler.processAllMath(this.element);
    }

    inputHandler(e: JQuery.TriggeredEvent) {
        const target = e.currentTarget as HTMLInputElement;
        this.lastInputFieldElement = target;
        const id: string = target.id;
        if (
            this.lastInputFieldId === id &&
            this.lastInputFieldValue === target.value
        ) {
            return;
        }
        this.lastInputFieldId = id;
        this.lastInputFieldValue = target.value;
        this.scope.$evalAsync(() => {
            this.autoPeekInput(id);
        });
        // await this.autoPeekInput(id);
    }

    async handleServerPeekResult(r: StackResult) {
        if (typeof r === "string") {
            this.error = r.toString();
            return;
        }
        if (r.error) {
            this.error = r.message;
            return;
        }
        const peekDiv = this.element.find(".peekdiv");
        const peekDivC = peekDiv.children();
        // editorDiv.empty();
        const pdiv = $(
            '<div><div class="math">' + r.questiontext + "</div></div>"
        );
        await ParCompiler.processAllMath(pdiv);
        peekDivC.replaceWith(pdiv); // TODO: still flashes
    }

    autoPeekInput(id: string) {
        this.stopTimer();
        this.timer = setTimeout(() => this.timedAutoPeek(id), 500);
    }

    async timedAutoPeek(id: string) {
        this.stopTimer();
        if (!this.attrs.autopeek) {
            return;
        }
        await this.doPeek(id);
    }

    async doPeek(id: string) {
        id = id.substr(STACK_VARIABLE_PREFIX.length);
        // answ[STACK_VARIABLE_PREFIX + id] = target.value;
        const isub = id.indexOf("_sub_");
        if (isub > 0) {
            id = id.substr(0, isub); // f.ex in matrix case stackapi_ans1_sub_0_1
        }
        const answ = this.collectAnswer(id);
        const data = {
            answer: answ,
            prefix: STACK_VARIABLE_PREFIX,
            verifyvar: id,
        };

        await this.runValidationPeek(data);
    }

    async runPeek() {
        // called from template
        // let data = this.collectData();
        // await this.runValidationPeek(data, 'ans1');
        if (this.lastInputFieldId) {
            await this.doPeek(this.lastInputFieldId);
        }
    }

    async runValidationPeek(data: IStackData) {
        if (this.pluginMeta.isPreview()) {
            this.error = "Cannot run plugin while previewing.";
            return;
        }
        this.isRunning = true;
        if (!this.stackpeek) {
            // remove extra fields from sceen
            let divinput = this.element.find(".stackinputfeedback");
            divinput.remove();
            divinput = this.element.find(".stackprtfeedback");
            divinput.remove();
            divinput = this.element.find(".stackpartmark");
            divinput.remove();
        }
        this.stackpeek = true;
        const url = this.getTaskUrl();
        data.seed = 1;
        const params = {
            input: {
                nosave: true,
                stackData: data,
                type: "stack",
                usercode: "",
            },
        };
        this.error = "";
        const r = await to(
            $http<{
                web: {
                    stackResult: StackResult;
                };
            }>({
                data: params,
                method: "PUT",
                timeout: defaultTimeout,
                url: url,
            })
        );
        this.isRunning = false;
        if (!r.ok) {
            this.error = r.result.data.error;
            return;
        }
        await this.handleServerPeekResult(r.result.data.web.stackResult);
    }

    getTaskUrl(): string {
        if (this.taskUrl) {
            return this.taskUrl;
        }
        const url = this.pluginMeta.getAnswerUrl();
        this.taskUrl = url;
        return url;
    }

    async runGetTask() {
        this.isOpen = true;
        await this.runSend(true);
    }

    async runSend(getTask = false) {
        if (this.pluginMeta.isPreview()) {
            this.error = "Cannot run plugin while previewing.";
            return;
        }
        this.stackpeek = false;
        this.error = "";
        this.isRunning = true;
        const url = this.getTaskUrl();
        const stackData = this.collectData();
        const params = {
            input: {
                getTask: getTask,
                stackData: stackData,
                type: "stack",
                usercode: this.timWay
                    ? this.userCode
                    : JSON.stringify(stackData.answer),
            },
        };

        const r = await to(
            $http<{
                web: {stackResult: StackResult; error?: string};
            }>({method: "PUT", url: url, data: params, timeout: defaultTimeout})
        );
        this.isRunning = false;

        if (!r.ok) {
            this.error = r.result.data.error;
            return;
        }
        if (!r.result.data.web) {
            this.error = "No web reply from csPlugin!";
            return;
        }
        if (r.result.data.web.error) {
            this.error = r.result.data.web.error;
            return;
        }
        const stackResult = r.result.data.web.stackResult;
        await this.handleServerResult(stackResult, getTask);
        if (this.lastInputFieldId) {
            this.lastInputFieldElement = this.element.find(
                "#" + this.lastInputFieldId
            )[0] as HTMLInputElement;
            if (this.lastInputFieldElement) {
                this.lastInputFieldElement.focus();
                this.lastInputFieldElement.selectionStart = 0;
                this.lastInputFieldElement.selectionEnd = 1000;
            }
        }
    }

    getDefaultMarkup() {
        return {};
    }

    getAttributeType() {
        return StackAll;
    }

    private stopTimer(): boolean {
        if (!this.timer) {
            return false;
        }
        clearTimeout(this.timer);
        this.timer = undefined;
        return true;
    }
}

const common = {
    bindings: pluginBindings,
    controller: StackController,
};

/*

Feedback (examples from 2x2 matrix multiplication)

  https://stack2.maths.ed.ac.uk/demo2018/mod/quiz/attempt.php?attempt=1502&cmid=147&page=4&scrollpos=281#q9

Correct:

<div class="stackprtfeedback stackprtfeedback-prt1" id="yui_3">
 <div class="correct" id="yui_3">
 <span style="font-size: 1.5em; color:green;" id="yui_3_17">
   <i class="fa fa-check" id="yui_3_17_2_"></i></span>
   Correct answer, well done.
 </div>
 <div class="gradingdetails">
   Marks for this submission: 1.00/1.00. Accounting for previous tries, this gives <strong>0.90/1.00</strong>.
 </div>
</div>

.fa-check:before {
    content: "\f00c";
}

Wrong:

<div class="outcome clearfix" id="yui_3">
  <h4 class="accesshide">Feedback</h4>
  <div class="feedback">
  <p></p>
  <div class="stackprtfeedback stackprtfeedback-prt1">
    <div class="incorrect">
      <span style="font-size: 1.5em; color:red;"><i class="fa fa-times"></i></span>
      Incorrect answer.
    </div><span class="filter_mathjaxloader_equation">The entries underlined in red below are those that are incorrect.
    <span class="filter_mathjaxloader_equation">...

    <div class="gradingdetails">
      Marks for this submission: 0.00/1.00. Accounting for previous tries, this gives <strong>0.90/1.00</strong>.
      This submission attracted a penalty of 0.10. Total penalties so far: 0.20.
    </div>
   </div>
   <p></p>
  </div>
</div>

.fa-remove:before, .fa-close:before, .fa-times:before {
    content: "\f00d";
}

// style="min-height: 20em; max-height: 20em; overflow: auto"

*/

stackApp.component("stackRunner", {
    ...common,
    template: `
<div ng-cloak class="csRunDiv math que stack no-popup-menu" >
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p ng-if="::$ctrl.stem" class="stem" ng-bind-html="::$ctrl.stem"></p>
    <p ng-if="!$ctrl.isOpen" class="stem" ng-bind-html="::$ctrl.attrs.beforeOpen"></p>

    <div class="no-popup-menu stackOutput" ng-if="::$ctrl.timWay" >
        <div class="csRunCode"><textarea class="csRunArea csInputArea"
                                 name="stackapi_ans1" id="stackapi_ans1"
                                 rows={{$ctrl.inputrows}}
                                 ng-model="$ctrl.userCode"
                                 ng-trim="false"
                                 ng-change="$ctrl.autoPeek()"
                                 placeholder="{{$ctrl.inputplaceholder}}"></textarea></div>
    </div>
    <div ng-cloak id="output" ng-if="::!$ctrl.timWay" class="stackOutput" ng-bind-html="$ctrl.outputAsHtml()">
    <!--<div ng-cloak id="output" ng-if="::!$ctrl.timWay" class="stackOutput" ng-bind-html="$ctrl.output">-->
    </div>
    <!-- <div class="peekdiv" id="peek" ng-bind-html="$ctrl.stackpeek"></div> -->
    <p class="csRunMenu">
        <button ng-if="!$ctrl.isOpen"
                class="timButton btn-sm"
                ng-click="$ctrl.runGetTask()"
                ng-bind-html="'Show task'"></button>
        <button ng-if="$ctrl.isOpen"
                ng-disabled="$ctrl.isRunning"
                title="(Ctrl-S)"
                class="timButton btn-sm"
                ng-click="$ctrl.runSend()"
                ng-bind-html="::$ctrl.button"></button>
        <button ng-if="::!$ctrl.attrs.autopeek"
                class="timButton btn-sm"
                ng-disabled="$ctrl.isRunning"
                ng-click="$ctrl.runPeek()"
                ng-bind-html="'Peek'"></button>
    </p>
    <div ng-cloak ng-if="$ctrl.stackpeek" class="peekdiv" id="peek" style="min-height: 10em;"><div></div></div>
    <div ng-cloak id="stackinputfeedback"
         class="stackinputfeedback1"
         ng-bind-html="$ctrl.stackinputfeedbackAsHtml()"></div>
    <span class="csRunError"
          ng-if="$ctrl.error"
          ng-style="$ctrl.tinyErrorStyle" ng-bind-html="$ctrl.error"></span>

    <div ng-if="$ctrl.stackfeedback">
        <div ng-if="$ctrl.attrs.generalfeedback">
            <h5>General feedback:</h5>
            <div id="generalfeedback" ng-bind-html="$ctrl.stackfeedback"></div>
        </div>
        <div ng-if="::$ctrl.attrs.correctresponse">
            <h5>Format correct response:</h5>
            <div id="formatcorrectresponse" ng-bind-html="$ctrl.stackformatcorrectresponse"></div>
            <div style="font-size: 0.7em;">
                <p>Score: <span id="score" ng-bind-html="$ctrl.stackscore"></span></p>
                <p>Summarise response: <span id="summariseresponse"
                                             ng-bind-html="$ctrl.stacksummariseresponse"></span></p>
                <p>Answer notes: <span id="answernotes" ng-bind-html="$ctrl.stackanswernotes"></span></p>
                <p>Time: <span id="time" ng-bind-html="$ctrl.stacktime"></span></p>
            </div>
        </div>
    </div>

    <p class="plgfooter" ng-if="::$ctrl.footer" ng-bind-html="::$ctrl.footer"></p>
</div>
`,
});
