import angular from "angular";
import * as t from "io-ts";
import {GenericPluginMarkup, PluginBase, withDefault} from "tim/plugin/util";
import {to} from "tim/util/utils";
import {$http, $sce} from "tim/util/ngimport";
import {ParCompiler} from "tim/editor/parCompiler";

const stackApp = angular.module("stackApp", ["ngSanitize"]);
const STACK_VARIABLE_PREFIX = 'stackapi_';


function ifIsS(value: number | undefined, name: string) {
    if (!value) {
        return "";
    }
    return name + '="' + value + '" ';
}


const StackMarkup = t.intersection([
    t.partial({ // this.attrs
        by: t.string,
        timWay:t.boolean,
        correctresponse: t.boolean,
        generalfeedback: t.boolean,
        open: t.boolean,
        autopeek: withDefault(t.boolean, true),
        beforeOpen: t.string,
        buttonBottom: t.boolean,
        lang: withDefault(t.string, "fi"),
    }),
    GenericPluginMarkup,
    t.type({
        // autoplay: withDefault(t.boolean, true),
        // file: t.string, eikö kuulu, mulle kuuluu, ei oo mute, joo moi
        // open: withDefault(t.boolean, false),
    }),
]);
const StackAll = t.intersection([
    t.partial({
        by: t.string,
    }),
    t.type({
        markup: StackMarkup,
    }),
]);


class StackController extends PluginBase<t.TypeOf<typeof StackMarkup>,
    t.TypeOf<typeof StackAll>,
    typeof StackAll> {
    private static $inject = ["$scope", "$element", "$sce"];
    private span: string = "";
    private error: string = "";
    private userCode: string = "";
    private stackoutput: string = "";
    private stackinputfeedback: string ="";
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
    private lastInputFieldId: string = '';
    private lastInputFieldValue: string = '';
    private lastInputFieldElement: HTMLInputElement | null = null;
    private button: string = '';


    $onInit() {
        super.$onInit();
        this.button = this.buttonText;
        // this.width = this.attrs.width;
        // this.height = this.attrs.height;
        let aa:any = this.attrsall;
        this.userCode = aa.usercode || this.attrs.by || "";
        this.timWay = aa.timWay || this.attrs.timWay || false;

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

        if ( this.attrs.open )
            this.runGetTask();
    }


    processNodes(res:any, nodes:any, id: string) {
      for (var i = 0; i < nodes.length; i++) {
        let element = nodes[i];
        if (element.name.indexOf(STACK_VARIABLE_PREFIX) === 0 &&
            element.name.indexOf('_val') === -1 &&
            element.name.indexOf(id) >= 0
        ) {
          if (element.type === 'checkbox' || element.type === 'radio') {
            if (element.checked) {
              res[element.name] = element.value
            }
          } else {
            res[element.name] = element.value
          }
        }
      }
      return res;
    }

    get english() {
        return this.attrs.lang === "en";
    }

    get buttonText() {
        const txt = this.attrs.button || this.attrs.buttonText;
        if (txt) {
            return txt;
        }
        return this.english ? "Send" : "Lähetä";
    }

    outputAsHtml() {
        let s = $sce.trustAsHtml(this.stackoutput);
        return s;
    }

    stackinputfeedbackAsHtml() {
        let s = $sce.trustAsHtml(this.stackinputfeedback);
        return s;
    }

    collectAnswer(id:string) {
        let parent = this.element[0];
        let inputs = parent.getElementsByTagName('input');
        let textareas = parent.getElementsByTagName('textarea');
        let selects = parent.getElementsByTagName('select');
        let res: any = {};
        if ( !this.timWay ) {
            res = this.processNodes(res, inputs, id);
            res = this.processNodes(res, textareas, id);
            res = this.processNodes(res, selects, id);
            if (Object.keys(res).length && this.userCode ) {
                this.userCode = JSON.stringify(res);
            } else {
                  try {
                      res = JSON.parse(this.userCode);
                  } catch {
                      // this.timWay = true;
                  }
            } // note: can not be else, because timWay may change during try
        }
        if ( this.timWay ) res[STACK_VARIABLE_PREFIX +'ans1'] = this.userCode;
        return res;
    }


    collectData() {
        return {
          prefix: STACK_VARIABLE_PREFIX,
          answer: this.collectAnswer(''),
      };
    }

    replace(s:string): string {
        // s = s.replace('https://stack-api-server/plots/', '/stackserver/plots/');
        return s;
    }

    async handleServerResult(r:any, getTask: boolean) {
        try {
            if (typeof r === 'string' || r instanceof String) {
                this.error = r.toString();
                return;
            }
            let json: any = r;
            if ( r.error ) {
                this.error = r.message;
                return;
            }

            let qt =  this.replace(json.questiontext);
            let i = qt.indexOf('<div class="stackinputfeedback"');
            if ( this.attrs.buttonBottom || i<0 ) {
                this.stackoutput = qt;
                this.stackinputfeedback = "";
            } else {
                this.stackoutput = qt.substr(0,i)+"\n";
                this.stackinputfeedback = qt.substr(i);
            }

            if ( !getTask ) {
                this.stackfeedback = this.replace(json.generalfeedback);
                this.stackformatcorrectresponse = this.replace(json.formatcorrectresponse);
                this.stacksummariseresponse = this.replace(JSON.stringify(json.summariseresponse));
                this.stackanswernotes = this.replace(JSON.stringify(json.answernotes));
            }
            this.stackscore = json.score.toString();
            this.stacktime = 'Request Time: '
                + (json.request_time).toFixed(2)
                + ' Api Time: ' + (json.api_time).toFixed(2);

            await ParCompiler.processAllMath(this.element);
            let html = this.element.find('.stackOutput');
            let inputs = html.find('input');
            let inputse = html.find('textarea');
            $(inputs).keyup(e=>this.inputHandler(e));
            $(inputse).keyup(e=>this.inputHandler(e));
            if ( getTask )  { // remove input validation texts
                let divinput = this.element.find('.stackinputfeedback');
                divinput.remove();
            }

        } finally {
            this.isRunning = false;
        }
    }

    async inputHandler(e:any) {
        let target: HTMLInputElement = e.currentTarget as HTMLInputElement;
        this.lastInputFieldElement = target;
        let id: string = target.id;
        if ( this.lastInputFieldId === id && this.lastInputFieldValue === target.value ) return;
        this.lastInputFieldId = id;
        this.lastInputFieldValue = target.value;
        this.scope.$evalAsync(() => { this.autoPeekInput(id); });
        // await this.autoPeekInput(id);
    }



    async handleServerPeekResult(r:any) {
        try {
            if (typeof r === 'string' || r instanceof String) {
                this.error = r.toString();
                return;
            }
            let json: any = r;

            let peekDiv = this.element.find(".peekdiv");
            let peekDivC = peekDiv.children();
            // editorDiv.empty();
            let pdiv = $('<div><div class="math">'+json.questiontext +'</div></div>');
            await ParCompiler.processAllMath(pdiv);
            peekDivC.replaceWith(pdiv); // TODO: vielä välähtää

        } finally {
            this.isRunning = false;
        }
    }

    private timer:any;
    private stopTimer(): boolean {
        if (!this.timer) return false;
        clearTimeout(this.timer);
        this.timer = null;
        return true;
    }

    async autoPeekInput(id:string) {
        this.stopTimer();
        this.timer = setTimeout( () => this.timedAutoPeek(id) ,500);
    }

    async timedAutoPeek(id:string) {
        this.stopTimer();
        if (!this.attrs.autopeek) return;
        await this.doPeek(id);
    }

    async doPeek(id:string)
    {
        id = id.substr(STACK_VARIABLE_PREFIX.length);
        // answ[STACK_VARIABLE_PREFIX + id] = target.value;
        let isub = id.indexOf('_sub_');
        if ( isub > 0 ) id = id.substr(0, isub); // f.ex in matrix case stackapi_ans1_sub_0_1
        let answ = this.collectAnswer(id);
        let data: any = {
            prefix: STACK_VARIABLE_PREFIX,
            verifyvar: id,
            answer: answ,
        };

        await this.runValidationPeek(data);
    }

    async runPeek() { //kutsutaan templatesta
        // let data = this.collectData();
        // await this.runValidationPeek(data, 'ans1');
        if ( this.lastInputFieldId )
            await this.doPeek(this.lastInputFieldId);
    }


    async runValidationPeek(data:any) {
        this.isRunning = true;
        if (!this.stackpeek) { // remove extra fields from sceen
            let divinput = this.element.find('.stackinputfeedback');
            divinput.remove();
            divinput = this.element.find('.stackprtfeedback');
            divinput.remove();
            divinput = this.element.find('.stackpartmark');
            divinput.remove();
        }
        this.stackpeek = true;
        let url = this.getTaskUrl();
        data.seed = 1;
        let params = {
            input: {
                usercode: '',
                stackData: data,
                nosave: true,
                type: 'stack'
            },
        };
        this.error = '';
        const r:any = await to($http<any>({method: "PUT", url: url, data: params, timeout: 20000}, ));
        if ( !r.result ) return;
        if ( !r.result.data ) return;
        if ( !r.result.data.web ) return;
        if ( r.result.data.web.error ) {
            this.error = r.result.data.web.error;
            return;
        }
        if ( !r.result.data.web.stackResult ) return;
        await this.handleServerPeekResult(r.result.data.web.stackResult);
    }

    private taskUrl: string = '';

    getTaskUrl(): string {
        if ( this.taskUrl ) return this.taskUrl;
        let url = "/cs/answer";
        const plugin = this.getPlugin();
        if (plugin) {
            url = plugin;
            const i = url.lastIndexOf("/");
            if (i > 0) {
                url = url.substring(i);
            }
            url += "/" + this.getTaskId() + "/answer/";
        }
        this.taskUrl = url;
        return url;
    }


    async runGetTask() {
        this.isOpen = true;
        await this.runSend(true);
    }

    async runSend(getTask: boolean) {
        this.stackpeek = false;
        getTask = getTask == true;
        this.error = "";
        this.isRunning = true;
        let url = this.getTaskUrl();
        let stackData = this.collectData();
        let params = {
            input: {
                usercode: this.timWay ? this.userCode : JSON.stringify(stackData.answer),
                stackData: stackData,
                getTask: getTask,
                type: 'stack'
            },
        };

        const r:any = await to($http<
            any
        >({method: "PUT", url: url, data: params, timeout: 20000},
        ));

        if ( !r.result.data ) {
            this.error = 'Timeout';
            return;
        }
        let error = r.result.data.error;
        if ( !error ) {
            if ( r.result.data.web )
                error = r.result.data.web.error;
        }
        if ( error ) {
            this.error = error;
            this.isRunning = false;
            return;
        }
        if ( !r.result.data.web ) {
            this.error = 'No web reply from csPlugin!';
            this.isRunning = false;
            return;
        }
        let stackResult = r.result.data.web.stackResult;
        await this.handleServerResult(stackResult, getTask);
        if ( this.lastInputFieldId ) {
            this.lastInputFieldElement = this.element.find('#'+this.lastInputFieldId)[0] as any;
            if ( this.lastInputFieldElement ) {
                this.lastInputFieldElement.focus();
                this.lastInputFieldElement.selectionStart = 0;  this.lastInputFieldElement.selectionEnd = 1000;
            }
        }
    }


    getDefaultMarkup() {
        return {};
    }

    protected getAttributeType() {
        return StackAll;
    }
}

const common = {
    bindings: {
        json: "@",
    },
    controller: StackController,
};

/*


Palutteita (esimerkit 2x2 matriisien kertolaskusta

  https://stack2.maths.ed.ac.uk/demo2018/mod/quiz/attempt.php?attempt=1502&cmid=147&page=4&scrollpos=281#q9

Oikein:

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

Väärin:

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
        <button ng-if="!$ctrl.isOpen"  ng-click="$ctrl.runGetTask()"  ng-bind-html="'Show task'"></button>
        <button ng-if="$ctrl.isOpen" ng-disabled="$ctrl.isRunning" title="(Ctrl-S)" ng-click="$ctrl.runSend()"
                ng-bind-html="::$ctrl.button"></button>
        <button ng-if="::!$ctrl.attrs.autopeek" ng-disabled="$ctrl.isRunning"  ng-click="$ctrl.runPeek()"
                ng-bind-html="'Peek'"></button>
    </p>
    <div ng-cloak ng-if="$ctrl.stackpeek" class="peekdiv" id="peek" style="min-height: 10em;"><div></div></div>
    <div ng-cloak id="stackinputfeedback" class="stackinputfeedback1" ng-bind-html="$ctrl.stackinputfeedbackAsHtml()"></div>
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
                <p>Summarise response: <span id="summariseresponse" ng-bind-html="$ctrl.stacksummariseresponse"></span></p>
                <p>Answer notes: <span id="answernotes" ng-bind-html="$ctrl.stackanswernotes"></span></p>
                <p>Time: <span id="time" ng-bind-html="$ctrl.stacktime"></span></p>
            </div>
        </div>
    </div>                    

    <p class="plgfooter" ng-if="::$ctrl.footer" ng-bind-html="::$ctrl.footer"></p>
</div>
`,
});

