import angular from "angular";
import * as t from "io-ts";
import {GenericPluginMarkup, PluginBase, withDefault} from "tim/plugin/util";
import {to} from "tim/util/utils";
import {$http, $timeout, $sce} from "tim/util/ngimport";
import {ParCompiler} from "tim/editor/parCompiler";
import {string} from "../../../static/scripts/jspm_packages/npm/io-ts@1.4.1/lib";

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
    private stackfeedback: string = "";
    private stackformatcorrectresponse: string = "";
    private stackscore: string = "";
    private stacksummariseresponse: string = "";
    private stackanswernotes: string = "";
    private stacktime: string = "";
    private isRunning: boolean = false;
    private inputrows: number = 1;
    private timWay: boolean = false; // if answer is given to TIM TextArea-field


    $onInit() {
        super.$onInit();
        // this.width = this.attrs.width;
        // this.height = this.attrs.height;
        let aa:any = this.attrsall;
        this.userCode = aa.usercode || this.attrs.by || "";
        this.timWay = aa.timWay || this.attrs.timWay || false;

        this.element.bind("keydown", (event) => {
            if (event.ctrlKey || event.metaKey) {
                switch (String.fromCharCode(event.which).toLowerCase()) {
                    case "s":
                        event.preventDefault();
                        this.runSend();
                        break;
                }
            }
        });

    }


      processNodes(res:any, nodes:any) {
        for (var i = 0; i < nodes.length; i++) {
          var element = nodes[i];
          if (element.name.indexOf(STACK_VARIABLE_PREFIX) === 0 && element.name.indexOf('_val') === -1) {
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


      outputAsHtml() {
         return $sce.trustAsHtml(this.stackoutput);
      }

      collectAnswer() {
          let parent = this.element[0];
          let inputs = parent.getElementsByTagName('input');
          let textareas = parent.getElementsByTagName('textarea');
          let selects = parent.getElementsByTagName('select');
          let res: any = {};
          if ( !this.timWay ) {
              res = this.processNodes(res, inputs);
              res = this.processNodes(res, textareas);
              res = this.processNodes(res, selects);
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
        let res: any = {
          prefix: STACK_VARIABLE_PREFIX,
          answer: this.collectAnswer(),
        }
        return res;
      }


    async handleServerResult(r:any) {
        try {
            if (typeof r === 'string' || r instanceof String) {
                this.error = r.toString();
                return;
            }
            let json: any = r;
            this.stackoutput = json.questiontext;
            this.stackfeedback = json.generalfeedback;
            this.stackformatcorrectresponse = json.formatcorrectresponse;
            this.stackscore = json.score.toString();
            this.stacksummariseresponse = JSON.stringify(json.summariseresponse);
            this.stackanswernotes = JSON.stringify(json.answernotes);
            this.stacktime = 'Request Time: '
                + (json.request_time).toFixed(2)
                + ' Api Time: ' + (json.api_time).toFixed(2);

            await ParCompiler.processAllMathDelayed(this.element);
        } finally {
            this.isRunning = false;
        }
    }


    async runPeek() { // this is just for test purposes
        this.isRunning = true;
        let url = "http://tim3/stackserver/api/endpoint.php";
        // this.stackoutput = "";
        let data = this.collectData();
        data.question = "";
        data.seed = 1;
        data.question =`
name: test
question_html: "<p>[[validation:ans1]]</p>"
inputs:
  ans1:
    type: algebraic
    model_answer: ta+c
    box_size: 20
    syntax_attribute: value
    forbid_words: int
    require_lowest_terms: true
    check_answer_type: true
    show_validations: with_varlist
`;
        const r = await to($http.post<{texts: string | Array<{html: string}>}>(
            url, data
        ));
        await this.handleServerResult(r.result.data);
    }


    async runSend() {
        this.error = "";
        this.isRunning = true;
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
        let stackData = this.collectData();
        let params = {
            input: {
                usercode: this.timWay ? this.userCode : JSON.stringify(stackData.answer),
                stackData: stackData,
                nosave: false,
                type: 'stack'
            },
        };

        const r:any = await to($http<
            any
        >({method: "PUT", url: url, data: params, timeout: 20000},
        ));

        let error = r.result.data.web.error;
        if ( error ) {
            this.error = error;
            this.isRunning = false;
            return;
        }
        let stackResult = r.result.data.web.stackResult;
        await this.handleServerResult(stackResult);
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

stackApp.component("stackRunner", {
    ...common,
    template: `
<div class="csRunDiv math">
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p ng-if="::$ctrl.stem" class="stem" ng-bind-html="::$ctrl.stem"></p>
    <div class="no-popup-menu">
                <div class="csRunCode"><textarea class="csRunArea csInputArea" ng-if="::$ctrl.timWay"
                                         rows={{$ctrl.inputrows}}
                                         ng-model="$ctrl.userCode"
                                         ng-trim="false"
                                         placeholder="{{$ctrl.inputplaceholder}}"></textarea></div>
    </div>
                    
    <div id="output" ng-bind-html="$ctrl.outputAsHtml()"></div>
    <p class="csRunMenu">
        <button ng-if="true" ng-disabled="$ctrl.isRunning" title="(Ctrl-S)" ng-click="$ctrl.runSend()"
                ng-bind-html="'Send'"></button>
        <button ng-if="true" ng-disabled="$ctrl.isRunning"  ng-click="$ctrl.runPeek()"
                ng-bind-html="'Peek'"></button>
    </p>

    <span class="csRunError"
          ng-if="$ctrl.error"
          ng-style="$ctrl.tinyErrorStyle" ng-bind-html="$ctrl.error"></span>

    <div ng-if="$ctrl.stackfeedback">
        <div ng-if="::$ctrl.attrs.generalfeedback">
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

