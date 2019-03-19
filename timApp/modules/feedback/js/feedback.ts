/**
 * Defines the client-side implementation of a feedback-plugin.
 */
import angular from "angular";
import * as t from "io-ts";
import {ITimComponent, ViewCtrl} from "tim/document/viewctrl";
import {GenericPluginMarkup, nullable, PluginBase, withDefault} from "tim/plugin/util";
import {$http} from "tim/util/ngimport";
import {to} from "tim/util/utils";

const feedbackApp = angular.module("feedbackApp", ["ngSanitize"]);
export const moduleDefs = [feedbackApp];

const answerPlaceHolder = "[answer]";

const MatchElement = t.type({
    answer: t.string,
    index: t.Integer,
});

interface MatchElementT extends t.TypeOf<typeof MatchElement> {

}

// type MatchElementT = t.TypeOf<typeof MatchElement>;

const MatchElementArray = t.array(MatchElement);

let StringArray = t.array(t.string);
const Choice = t.type({
    levels: StringArray,
    match: t.union([StringArray, MatchElementArray]),
});

const QuestionItem = t.type({
    pluginNames: StringArray,
    words: t.array(StringArray),
    correctAnswer: StringArray,
    correctAnswerFeedback: t.string,
    choices: t.array(Choice),
});

const FeedbackMarkup = t.intersection([
    t.partial({
        feedbackLevel: t.number,
        toNextTaskRule: t.string,
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
        questionItems: t.array(QuestionItem),
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
    private feedbackLevelRise = false;
    private pluginMode = 0;
    private showMode = "Instruction paragraph"; // for demo

    setCurrentFeedbackLevel(number: number) {
        this.currentFeedbackLevel === number;
    }

    openBlock() {
        return;
    }

    hideBlock() {
        return;
    }

    printFeedback() {
        return;
    }

    getDefaultMarkup() {
        return {questionItems: []};
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
        this.setPluginWords();
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
        const r = await to($http.put<{ web: { result: string, error?: string } }>(url, params));
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
        if (this.pluginMode === 0) {
            this.pluginMode = 1;
            this.showMode = "Question item paragraph";
            return;
        }

        if (this.pluginMode === 1) {
            if (this.index === this.attrs.questionItems.length) {
                this.feedback = "No more question items, thanks for playing";
                this.pluginMode = 4;
                return;
            }

            const selections = this.getAnswerFromPlugins();
            const isCorrect = this.checkCorrectAnswer(this.index, selections);
            if (isCorrect) {
                this.feedback = this.attrs.questionItems[this.index].correctAnswerFeedback;
                this.feedback = this.feedback.replace(answerPlaceHolder, this.userAnswer);
            } else {
                const matchIndex = this.compareChoices(this.index, selections);
                if (matchIndex === -1) {
                    this.feedback = "You have no choices defined, got no matches or using match objects";
                } else {
                    const feedbackLevels = this.attrs.questionItems[this.index].choices[matchIndex].levels;
                    if (this.currentFeedbackLevel > (feedbackLevels.length)) {
                        this.feedback = feedbackLevels[feedbackLevels.length - 1]
                            .replace(answerPlaceHolder, this.userAnswer);
                    } else if (this.currentFeedbackLevel < this.feedbackLevel) {
                        this.feedback = feedbackLevels[this.currentFeedbackLevel - 1]
                            .replace(answerPlaceHolder, this.userAnswer);
                    }
                }
            }
            this.index++;
            this.pluginMode = 2;
            this.showMode = "Feedback paragraph";
            return;
        }

        if (this.pluginMode === 2) {
            this.pluginMode = 1;
            this.feedback = "";
            this.showMode = "Question item paragraph";
        }
    }

    checkCorrectAnswer(index: number, answer: string[]): boolean {
        const correct = this.attrs.questionItems[index].correctAnswer;
        if (correct.length === answer.length) {
            for (let i = 0; i < correct.length; i++) {
                if (correct[i] !== answer[i]) {
                    if (this.currentFeedbackLevel < this.feedbackLevel) {
                        this.currentFeedbackLevel++;
                    }
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    compareChoices(index: number, answer: string[]): number {
        const choices = this.attrs.questionItems[index].choices;
        if (choices.length === 0) {
            return -1;
        }

        for (let i = 0; i < choices.length; i++) {
            const match = choices[i].match;
            if (StringArray.is(match)) {
                if (this.checkMatchStringArray(match, answer)) {
                    return i;
                }
            }
            // Oliotaulukon tapauksessa
            else {
                this.checkMatchObjectArray(match, answer);
                return -1;
            }
        }
        return -1;
    }

    checkMatchStringArray(match: string[], answer: string[]): boolean {
        if (match.length === 0) {
            return true;
        }
        if (match.length === answer.length) {
            for (let i = 0; i < match.length; i++) {
                if (match[i] !== answer[i]) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    checkMatchObjectArray(match: MatchElementT[], answer: string[]): number {
        return -1;
    }

    compareAnswers(match: string[], answer: string[]): boolean {
        return false;
    }

    /**
     * Gets the user's answer from the visible question item this feedback-plugin is assigned to.
     * TODO: Refactor, add a way to get the answer from the visible question item
     * TODO: Maybe add getting answers in an area and with regexp?
     *
     * @returns(string[]) Returns the user's selections to the question item
     */
    getAnswerFromPlugins(): string[] {
        const plugins = this.attrs.questionItems[this.index].pluginNames;
        const selections: string[] = [];
        const timComponent = this.vctrl.getTimComponentByName(plugins[0]);

        if (timComponent) {
            const par = timComponent.getPar();
            const content = par.children(".parContent");
            const nodes = content[0].childNodes;
            // console.log(nodes);
            let answer = "";

            for (let n = 0; n < nodes.length; ++n) {
                const node = nodes[n];
                if (node.nodeName === "#text") {
                    let text = node.textContent || "";
                    text.trim();
                    answer = answer + text;
                }
                if (node.nodeName === "TIM-PLUGIN-LOADER") {
                    // Makeshift way to go through all the plugins in a paragraph
                    // TODO: Check for later to skip drag sources in paragraph node.getAttribute("class") === "plugindragsource"
                    if (node instanceof Element) {
                        let name = node.getAttribute("task-id")!.split(".")[1];
                        if (name) {
                            let plugin = this.vctrl.getTimComponentByName(name);
                            if (plugin) {
                                let content = plugin.getContent().trim();
                                selections.push(content);
                                answer = answer + content;
                            }
                        }
                    }
                }
            }
            this.userAnswer = answer;
        }
        return selections;
    }

    /**
     * Sets the list of words used in a dropdown plugin (drag&drop target tba). Goes through every question
     * item and every plugin inside the question item. It is assumed for now that the list of words and list
     * of plugins are in the same order and that there are the same amount of them both.
     *
     * TODO: drag&drop
     */
    setPluginWords() {
        const items = this.attrs.questionItems;
        for (const item of items) {
            let i = 0;
            for (const plugin of item.pluginNames) {
                const timComponent = this.vctrl.getTimComponentByName(plugin);
                if (timComponent && timComponent.setPluginWords) {
                    timComponent.setPluginWords(item.words[i]);
                    i++;
                } else {
                    this.error = "No plugin with such a name or missing setPluginWords-method";
                }
            }
        }
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
    <div>{{$ctrl.showMode}}</div>
    <div class="form-inline"><label><span>
        {{$ctrl.feedback}}</span></label>
    </div>
    <button class="timButton"
            ng-if="::$ctrl.buttonText()"
            ng-click="$ctrl.handleAnswer()">
        {{::$ctrl.buttonText()}}
    </button>
    <div ng-if="$ctrl.error" ng-bind-html="$ctrl.error"></div>
    <pre ng-if="$ctrl.result">{{$ctrl.result}}</pre>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div>
`,
});
/*
<button class="timButton"
            ng-if="::$ctrl.wordButtonText()"
            ng-click="$ctrl.setPluginWords()">
        {{::$ctrl.wordButtonText()}}
    </button>
 */