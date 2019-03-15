/**
 * Defines the client-side implementation of a feedback-plugin.
 */
import angular, {INgModelOptions} from "angular";
import * as t from "io-ts";
import {ITimComponent, ViewCtrl} from "tim/document/viewctrl";
import {GenericPluginMarkup, nullable, PluginBase, withDefault} from "tim/plugin/util";
import {$http} from "tim/util/ngimport";
import {to} from "tim/util/utils";

const feedbackApp = angular.module("feedbackApp", ["ngSanitize"]);
export const moduleDefs = [feedbackApp];

const MatchElement = t.type({
    index: t.Integer,
    answer: t.string,
});

interface MatchElementT extends t.TypeOf<typeof MatchElement> {

}

// type MatchElementT = t.TypeOf<typeof MatchElement>;

const MatchelementArray = t.array(MatchElement);

const Choice = t.type({
    match: t.union ([t.array(t.string),MatchelementArray]),
    levels:t.array(t.string)
});


const QuestionItem = t.type({
    pluginNames: t.array(t.string),
    dropdownWords: t.array(t.array(t.string)),
    dragWords: t.array(t.string),
    correctAnswer: t.array(t.string),
    correctAnswerFeedback: t.string,
    choices: t.array(Choice),
});



const FeedbackMarkup = t.intersection([
    t.partial({
        inputstem: t.string,
        followid: t.string,
        field: t.string,
        feedbackLevel: t.number,
        toNextTaskRule: t.string,
        questionItems: t.array(QuestionItem),
        instructionID: t.string,
        sampleItemID: t.string,
        nextTask: t.string,
        pluginID: t.string,

    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
        autoupdate: withDefault(t.number, 500),
        cols: withDefault(t.number, 20),
        //questionItems: t.array(QuestionItem),
    }),
]);
const FeedbackAll = t.intersection([
    t.partial({
        // userword: t.string,
    }),
    t.type({markup: FeedbackMarkup}),
]);

class FeedbackController extends PluginBase<t.TypeOf<typeof FeedbackMarkup>, t.TypeOf<typeof FeedbackAll>, typeof FeedbackAll> implements ITimComponent {
    private result?: string;
    private error?: string;
    private vctrl!: ViewCtrl;
    private userAnswer = "";
    private feedback = "";
    private index = 0;
    private questionPluginID?: string;  // tarkasta
    private feedbackLevel = 5;
    private currentFeedbackLevel = 0;
    private listOfValidQuestionItems?: string[];
    private feedbackLevelRise = false;

    setCurrentFeedbackLevel(number: number){
        this.currentFeedbackLevel === number;
    }

    /*
    findQuestionplugin(pluginNames:string[]){
        if(this.attrs.questionItems) {
            for (let i = 0; i < this.attrs.questionItems.length; i++) {
                if (this.attrs.questionItems[i].pluginNames.length !== pluginNames.length) continue;
                for (let j = 0; j < pluginNames.length; j++) {
                    if (this.attrs.questionItems[i].pluginNames[j] !== pluginNames[j]) break;
                    if (j === pluginNames.length - 1) return i

                }
            }
            return -1;
        }
    }
*/

    openBlock() {
        return;
    }

    hideBlock(){
        return;
    }

    printFeedback(){
        return;
    }

    getDefaultMarkup() {
        return {};
    }

    buttonText() {
        return "OK";
    }

    wordButtonText() {
        return "Get words";
    }

    $onInit() {
        super.$onInit();
        this.addToCtrl();
        this.feedbackLevel = this.attrs.feedbackLevel || 5;
    }

    /**
     * Adds this plugin to ViewCtrl so other plugins can get information about the plugin though it.
     */
    addToCtrl() {
        this.vctrl.addTimComponent(this);
    }

    get autoupdate(): number {
        return this.attrs.autoupdate;
    }

    async doSave(nosave: boolean) {
        this.error = "... saving ...";

        this.result = undefined;
        const params = {
            input: {
                nosave: false,
            },
        };

        if (nosave) {
            params.input.nosave = true;
        }
        const url = this.pluginMeta.getAnswerUrl();
        const r = await to($http.put<{web: {result: string, error?: string}}>(url, params));
        if (r.ok) {
            const data = r.result.data;
            this.error = data.web.error;
            this.result = data.web.result;
        } else {
            this.error = "Infinite loop or some other error?";
        }
    }

    /**
     * Returns the content inside this plugin
     * @returns {string} The content inside this plugin
     */
    getContent(): string {
      return "";
    }

    /**
     * Does nothing at the moment
     */
    save(): string {
        this.doSave(false);
        return "";
    }

    /**
     * Handles the checking of user's answer's correctness.
     */
    handleAnswer() {
        if (this.attrs.questionItems) {
            const selections = this.getAnswerFromPlugin();
            const isCorrect = this.checkCorrectAnswer(this.index, selections);
            if (isCorrect) {
                this.feedback = this.attrs.questionItems[this.index].correctAnswerFeedback;
                this.feedback = this.feedback.replace("[answer]", this.userAnswer);
            }
            else {
                const matchIndex = this.compareChoices(this.index, selections);
                console.log(this.attrs.questionItems[this.index].choices[matchIndex]);
                console.log(matchIndex);
                this.feedback = this.attrs.questionItems[this.index].choices[matchIndex].levels[this.currentFeedbackLevel!-1];
                this.feedback = this.feedback.replace("[answer]", this.userAnswer);
            }
        }
    }

    checkCorrectAnswer(index: number, answer: string[]): boolean {
        if (this.attrs.questionItems![index].correctAnswer.length === answer.length) {
            const correct = this.attrs.questionItems![index].correctAnswer;
            const comparison: string[] = answer.filter(selection => correct.indexOf(selection) < 0);
            if (comparison.length === 0) {
                return true;
            }
            else {
                if(this.currentFeedbackLevel < this.feedbackLevel) {
                    this.currentFeedbackLevel++;
                }
            }
        }
        return false;
    }

    compareChoices(index: number, answer: string[]): number {
        const choices = this.attrs.questionItems![index].choices;
        if (choices.length === 0) {
            return -1;
        }

        for (let i = 0; i < choices.length; i++) {
            const match = choices[i].match;
            if (!MatchelementArray.is(match)) {
                if (this.checkMatchStringArray(match, answer)) {
                    return i;
                }
            }
            // Oliotaulukon tapauksessa
            else {
                this.checkMatchObjectArray(answer, match)
            }
        }
        return -1;
    }

    checkMatchStringArray(match: string[], answer: string[]): boolean {
        if (match.length === answer.length) {
            const comparison: string[] = answer.filter(selection => match.indexOf(selection) < 0);
            if (comparison.length === 0) {
                return true;
            }
        }
        return false;
    }

    checkMatchObjectArray(answer: string[], match: MatchElementT[]): number{
        return -1;
    }



    /**
     * Gets the user's answer from the visible question item this feedback-plugin is assigned to.
     * TODO: Refactor, add a way to get the answer from the visible question item
     * TODO: Maybe add getting answers in an area and with regexp?
     *
     * @returns(string[]) Returns the user's selections to the question item
     */
    getAnswerFromPlugin(): string[] {
        if (this.attrs.questionItems) {
            const plugins = this.attrs.questionItems[0].pluginNames;
            const selections: string[] = new Array<string>(plugins.length);
            const timComponent = this.vctrl.getTimComponentByName(plugins[0]);

            if (timComponent) {
                const par = timComponent.getPar();
                const content = par.children(".parContent");
                const nodes = content[0].childNodes;
                // console.log(nodes);
                let answer = "";
                let i = 0;

                for (let n in nodes) {
                    let node = nodes[n];
                    if (node.nodeName === "#text") {
                        let text = node.textContent || "";
                        text.trim();
                        answer = answer + text;
                    }
                    if (node.nodeName === "TIM-PLUGIN-LOADER") {
                        // Makeshift way to go through all the plugins in a paragraph
                        let plugin = this.vctrl.getTimComponentByName(plugins[i]);
                        if (plugin) {
                            let content = plugin.getContent().trim();
                            selections[i] = content;
                            answer = answer + content;
                            i++;
                        }
                    }
                }
                this.userAnswer = answer;
            }
            return selections;
        }
        return [];
    }

    /**
     * Sets the list of words used in a dropdown plugin (drag&drop target tba). Goes through every question
     * item and every plugin inside the question item. It is assumed for now that the list of words and list
     * of plugins are in the same order and that there are the same amount of them both.
     *
     * TODO: drag&drop
     */
    setPluginWords() {
        if (this.attrs.questionItems) {
            const items = this.attrs.questionItems;
            for (let item of items) {
                let i = 0;
                for(let plugin of item.pluginNames) {
                    const timComponent = this.vctrl.getTimComponentByName(plugin);
                    if (timComponent) {
                        timComponent.setPluginWords!(item.dropdownWords[i]);
                        i++;
                    }
                }
            }
        }
    }

    getGroups():string[] {
        return [""];
    }
    getName(): (string | undefined) {
        if (this.attrs.followid) { return this.attrs.followid; }
        const taskId = this.pluginMeta.getTaskId();
        if (taskId) { return taskId.split(".")[1]; }
    }

    belongsToGroup(group: string): boolean {
        return false;
    }

    protected getAttributeType() {
        return FeedbackAll;
    }
}

feedbackApp.component("feedbackRunner", {
    bindings: {
        json: "@",
    },
    controller: FeedbackController,
    require: {
        vctrl: "^timView",
    },
    template: `
<div class="csRunDiv no-popup-menu">
    <tim-markup-error ng-if="::$ctrl.markupError" data="::$ctrl.markupError"></tim-markup-error>
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p ng-if="::$ctrl.stem">{{::$ctrl.stem}}</p>
    <div class="form-inline"><label>{{::$ctrl.inputstem}} <span>
        {{$ctrl.feedback}}</span></label>
    </div>
    <button class="timButton"
            ng-if="::$ctrl.buttonText()"
            ng-click="$ctrl.handleAnswer()">
        {{::$ctrl.buttonText()}}
    </button>
    <button class="timButton"
            ng-if="::$ctrl.wordButtonText()"
            ng-click="$ctrl.setPluginWords()">
        {{::$ctrl.wordButtonText()}}
    </button>
    <div ng-if="$ctrl.error" ng-bind-html="$ctrl.error"></div>
    <pre ng-if="$ctrl.result">{{$ctrl.result}}</pre>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div>
`,
});

/*

<div class="csRunDiv no-popup-menu">
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <p ng-if="::$ctrl.stem">{{::$ctrl.stem}}</p>
    <div class="form-inline"><label>{{::$ctrl.inputstem}} <span>
        {{$ctrl.userword}}</span></label>
    </div>
    <button class="timButton"
            ng-if="::$ctrl.buttonText()"
            ng-click="$ctrl.getAnswer()">
        {{::$ctrl.buttonText()}}
    </button>
    <div ng-if="$ctrl.error" ng-bind-html="$ctrl.error"></div>
    <pre ng-if="$ctrl.result">{{$ctrl.result}}</pre>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div>
 */