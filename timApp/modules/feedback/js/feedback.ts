/**
 * Defines the client-side implementation of a feedback-plugin.
 */
import angular from "angular";
import * as t from "io-ts";
import {ITimComponent, ViewCtrl} from "tim/document/viewctrl";
import {GenericPluginMarkup, nullable, PluginBase, withDefault, Info} from "tim/plugin/util";
import {$http} from "tim/util/ngimport";
import {to} from "tim/util/utils";

const feedbackApp = angular.module("feedbackApp", ["ngSanitize"]);
export const moduleDefs = [feedbackApp];

const answerPlaceHolder = "@@answer@@";
// const answerPlaceHolder = "@@answer[^@]*@@";

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
    correct: withDefault(t.boolean, false),
    levels: StringArray,
    match: t.union([StringArray, MatchElementArray]),
});

interface QuestionItemT extends t.TypeOf<typeof QuestionItem> {

}

const QuestionItem = t.type({
    pluginNames: StringArray,
    words: t.array(StringArray),
    choices: t.array(Choice),
});

const FeedbackMarkup = t.intersection([
    t.partial({
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
    t.type({
        info: Info,
        markup: FeedbackMarkup,
        preview: t.boolean,
    }),
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
    private taskEnd = false;
    private answer: string[] = [];

    openBlock() {
        return;
    }

    hideBlock() {
        return;
    }

    getCorrectChoice(item: QuestionItemT): number {
        for (let i = 0; i < item.choices.length; i++) {
            if (item.choices[i].correct) {
                return i;
            }
        }
        return -1;
    }

    printFeedback(feedback: string) {
        this.feedback = feedback.replace(answerPlaceHolder, this.userAnswer);
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

    async doSave(nosave: boolean, correct: boolean, sentence: string) {
        this.error = "... saving ...";

        this.result = undefined;
        const params = {
            input: {
                nosave: false,
                feedback: this.feedback,
                correct: true,
                sentence: sentence,
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
            if (data.web.error) {
                return data.web.error;
            }
            // this.result = data.web.result;
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
     * Handles saving of the feedback plugin. To get the correct answer to the database, replace user selections
     * with the ones that are correct.
     */
    async save() {
        // TODO: finish this, can you replace just one instance?
        const values: { [id: string]: string } = {};
        const item = this.attrs.questionItems[this.index];
        const plugins = item.pluginNames;
        const index = this.getCorrectChoice(item);
        const words = item.choices[index].match;

        for (let i = 0; i < plugins.length; i++) {
            if (StringArray.is(words)) {
                values[plugins[i]] = words[i];
            }
        }
        const sentence = this.getSentence(this.answer, values);
        return this.doSave(false, true, sentence);
    }

    /**
     * Handles the checking of user's answer's correctness.
     */
    async handleAnswer() {
        if (this.pluginMode === 0) {
            this.pluginMode = 1;
            // For demonstration
            this.showMode = "Question item paragraph";
            // TODO: instruction time saving here?
            const instructionQuestion = this.vctrl.getTimComponentByName(this.attrs.instructionID || "");
            if (this.attrs.instructionID && instructionQuestion) {
                // TODO: need to use this in something?
                const success = this.savePlugin(instructionQuestion);
            }
            return;
        }

        if (this.pluginMode === 1) {
            if (this.index > this.attrs.questionItems.length) {
                this.feedback = "No more question items, thanks for playing";
                this.pluginMode = 4;
                return;
            }

            const selections = this.getAnswerFromPlugins();
            const matchIndex = this.compareChoices(this.index, selections);

            if (matchIndex === -1) {
                this.error = "You have no choices defined, got no matches or using match objects";
            } else {
                const choice = this.attrs.questionItems[this.index].choices[matchIndex];
                const feedbackLevels = choice.levels;
                if (choice.correct) {
                    // TODO: Do you want to make more than one correct feedback?
                    this.printFeedback(feedbackLevels[feedbackLevels.length - 1]);
                    // this.taskEnd = true;
                } else {
                    if (this.currentFeedbackLevel < feedbackLevels.length) {
                        this.printFeedback(feedbackLevels[this.currentFeedbackLevel++]);
                    } else {
                        this.printFeedback(feedbackLevels[feedbackLevels.length - 1]);
                        // this.taskEnd = true;
                    }
                }
            }
            // TODO: plugin saving here?
            const plugins = this.attrs.questionItems[this.index];
            for (const p of plugins.pluginNames) {
                const plugin = this.vctrl.getTimComponentByName(p);
                if (plugin) {
                    const success = await this.savePlugin(plugin);
                    if (!success) {
                        this.error = "You need to select something";
                        return;
                    }
                    this.error = "";
                }
            }

            // this.index++;
            this.pluginMode = 2;
            // For demonstration
            this.showMode = "Feedback paragraph";
            return;
        }

        if (this.pluginMode === 2) {
            //TODO: feedback saving here?
            const success = this.save();
            if (!success) {
                this.error = success;
                return;
            }

            this.pluginMode = 1;
            this.feedback = "";
            // For demonstration
            this.showMode = "Question item paragraph";
        }
    }

    /**
     * Saves the selected plugin's content to the database
     *
     * @param plugin The plugin to be saved
     * @returns{boolean} Whether the saving was succesful
     */
    async savePlugin(plugin: ITimComponent) {
        const success = await plugin.save();
        if (success) {
            return false;
        }
        return true;
    }

    /**
     * Returns the index of the choice matching the selections the user made to the plugins of the question item.
     *
     * @param index The index of the question item
     * @param answer Array of the answers the user has selected
     * @returns(number) The index of the choice that matches user selections
     */
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
            // If the match is a MatchObjectArray instead of a string array
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

    /**
     * Gets the user's answer from the visible question item this feedback-plugin is assigned to.
     * TODO: Refactor, add a way to get the answer from the visible question item
     * TODO: Maybe add getting answers in an area and with regexp?
     *
     * @returns(string[]) The user's selections to the question item
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
            let answer: string[] = [];
            const values: { [id: string]: string } = {};
            for (let n = 0; n < nodes.length; ++n) {
                const node = nodes[n];
                if (node.nodeName === "#text") {
                    let text = node.textContent || "";
                    answer.push(text.trim());
                }
                if (node.nodeName === "TIM-PLUGIN-LOADER") {
                    // Makeshift way to go through all the plugins in a paragraph
                    // TODO: Check for later to skip drag sources in paragraph node.getAttribute("class") === "plugindragsource"
                    if (node instanceof Element) {
                        let name = node.getAttribute("task-id")!.split(".")[1];
                        if (name) {
                            let plugin = this.vctrl.getTimComponentByName(name);
                            if (plugin) {
                                // values[name] = timComponent.getContent().trim();
                                let content = plugin.getContent().trim();
                                values[name] = content;
                                selections.push(content);
                                answer.push(name);
                            }
                        }
                    }
                }
            }
            this.answer = answer;
            this.userAnswer = this.getSentence(answer, values);
        }
        return selections;
    }

    getSentence(sentence: string[], choices: { [id: string]: string }): string {
        const temp = [...sentence];
        for (let c in choices) {
            const choice = choices[c];
            const index = temp.indexOf(c);
            temp[index] = choice;
        }
        return temp.toString().replace(/,/g, " ");
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
    <div class="form-inline"><span>
        {{$ctrl.feedback}}</span>
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