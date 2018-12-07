import angular from "angular";
import * as t from "io-ts";
import {GenericPluginMarkup, PluginBase, withDefault} from "tim/plugin/util";
import {to} from "tim/util/utils";
import {$http, $timeout, $sce} from "tim/util/ngimport";
import {ParCompiler} from "tim/editor/parCompiler";

const stackApp = angular.module("stackApp", ["ngSanitize"]);


function ifIsS(value: number | undefined, name: string) {
    if (!value) {
        return "";
    }
    return name + '="' + value + '" ';
}


const StackMarkup = t.intersection([
    t.partial({
        by: t.string,
        userinput: t.string,  // ATTR: käyttäjän syöte
        question: t.string,
        defaults: t.string,
        readOnly: t.boolean,
        feedback: t.boolean,
        hasScore: t.boolean,
        lang: t.string,
        seed: t.number

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
        userinput: t.string,
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
    private userCode: string = "";
    private stackoutput: string = "";
    private stackfeedback: string = "";
    private stackformatcorrectresponse: string = "";
    private stackscore: string = "";
    private stacksummariseresponse: string = "";
    private stackanswernotes: string = "";
    private stacktime: string = "";


    $onInit() {
        super.$onInit();
        // this.width = this.attrs.width;
        // this.height = this.attrs.height;
        this.userCode = this.attrsall.by || this.attrs.by || "";

    }


      processNodes(res:any, nodes:any) {
        for (var i = 0; i < nodes.length; i++) {
          var element = nodes[i];
          if (element.name.indexOf('stackapi_') === 0 && element.name.indexOf('_val') === -1) {
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
        let inputs = parent.getElementsByTagName('input')
        let textareas = parent.getElementsByTagName('textarea')
        let selects = parent.getElementsByTagName('select')
        let res = {};
        res = this.processNodes(res, inputs);
        res = this.processNodes(res, textareas);
        res = this.processNodes(res, selects);
        return res;
      }


      collectData() {
        let res: any = {
          question: this.attrs.question,
          readOnly: this.attrs.readOnly || false, //document.getElementById('readOnly').checked,
          feedback: this.attrs.feedback || true, //document.getElementById('feedback').checked,
          score: this.attrs.hasScore || true, //document.getElementById('hasScore').checked,
          lang: this.attrs.lang || '', //document.getElementById('lang').value,
          prefix: 'stackapi_',
          answer: this.collectAnswer(),
          seed: this.attrs.seed || 12313122
        }
        if (this.attrs.defaults) {
          res['defaults'] = this.attrs.defaults
        }
        return res;
      }


    async runCode() {
        let url = "http://tim3/stackserver/api/endpoint.php";
        this.stackoutput = "";

        const r = await to($http.post<{texts: string | Array<{html: string}>}>(
            url, this.collectData()
        ));

        let json:any = r.result.data;
        this.stackoutput = json.questiontext;
        this.stackfeedback = json.generalfeedback;
        this.stackformatcorrectresponse = json.formatcorrectresponse;
        this.stackscore = json.score.toString();
        this.stacksummariseresponse = JSON.stringify(json.summariseresponse);
        this.stackanswernotes = JSON.stringify(json.answernotes);
        this.stacktime = '<b>Request Time:</b> ' + json.request_time +
                  ' <b>Api Time:</b> ' + json.api_time;

        // MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
        await ParCompiler.processAllMathDelayed(this.element);
    }


    showStack() {
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
                <div class="csRunCode"><textarea class="csRunArea csInputArea"
                                         rows={{$ctrl.inputrows}}
                                         ng-model="$ctrl.userCode"
                                         ng-trim="false"
                                         placeholder="{{$ctrl.inputplaceholder}}"></textarea></div>
    </div>
        <p class="csRunMenu">
            <button ng-if="true" ng-disabled="$ctrl.isRunning" title="(Ctrl-S)" ng-click="$ctrl.runCode()"
                    ng-bind-html="'Send'"></button>
                    </p>
                    
    <div id="output" ng-bind-html="$ctrl.outputAsHtml()"></div>
    <h5>General feedback:</h5>
    <div id="generalfeedback" ng-bind-html="$ctrl.stackfeedback"></div>
    <h5>Format correct response:</h5>
    <div id="formatcorrectresponse" ng-bind-html="$ctrl.stackformatcorrectresponse"></div>
    <p>Score: <span id="score" ng-bind-html="$ctrl.stackscore"></span></p>
    <p>Summarise response:</p>
    <div id="summariseresponse" ng-bind-html="$ctrl.stacksummariseresponse"></div>
    <p>Answer notes:</p>
    <div id="answernotes" ng-bind-html="$ctrl.stackanswernotes"></div>
    <p>Time:</p>
    <div id="time" ng-bind-html="$ctrl.stacktime"></div>
                    
    <p class="plgfooter" ng-if="::$ctrl.footer" ng-bind-html="::$ctrl.footer"></p>
</div>
`,
});

