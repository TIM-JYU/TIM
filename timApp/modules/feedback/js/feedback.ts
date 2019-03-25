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
const answerPlaceHolderRegExp = RegExp(/@@answer\[[0-9]+\]@@/g);
const wordPlaceHolderRegExp = RegExp(/@@word\[[0-9]+\]@@/g);

enum Mode {
    Instruction = 0,
    QuestionItem = 1,
    Feedback = 2,
    EndTask = 3,
}

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
    private userAnswer: string[] = [];
    private feedback = "";
    private index = 0;
    private currentFeedbackLevel = 0;
    private feedbackLevelRise = false;
    private pluginMode = Mode.Instruction;
    private showMode = "Instruction paragraph"; // for demo
    private answerArray: string[] = [];
    private userSelections: string[] = [];
    private correctAnswer = false;

    /**
     * Show the block for the current question item
     * TODO: finish this
     */
    showBlock() {
        const name = this.attrs.questionItems[this.index].pluginNames[0];
        const plugin = this.vctrl.getTimComponentByName(name);
        if (plugin) {
            const test = plugin.getPar().children(".parContent");
            if (test instanceof Element) {
            }
        }
        return;
    }

    hideBlock() {
        return;
    }

    /**
     * Gets the match that is correct for the question item
     *
     * @param item question item that the choices are being checked from
     * @returns{number} the index of the correct match
     */
    getCorrectChoice(item: QuestionItemT): number {
        for (let i = 0; i < item.choices.length; i++) {
            if (item.choices[i].correct) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Prints the feedback the user should see for their answer.
     *
     *
     * @param feedback what feedback should be presented to the user
     */
    printFeedback(feedback: string) {
        const answer = this.userAnswer.toString().replace(/,/g, " ");
        this.feedback = feedback.replace(answerPlaceHolder, answer);

        const reanswer = feedback.match(answerPlaceHolderRegExp);
        if (reanswer) {
            this.replacePlaceHolder(reanswer, this.userAnswer);
        }

        const reword = feedback.match(wordPlaceHolderRegExp);
        if (reword) {
            this.replacePlaceHolder(reword, this.userSelections);
        }
    }

    /**
     * Replace the placeholders found in feedback with words from the given array.
     *
     * @param re array of regexp-matches
     * @param wordarray words that should be replace the placeholders
     */
    replacePlaceHolder(re: RegExpMatchArray, wordarray: string[]) {
        for (const placeholder of re) {
            const index = placeholder.match(/[0-9]/)!.toString();
            const replacement = wordarray[parseInt(index)];
            this.feedback = this.feedback.replace(placeholder, replacement);
        }
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
                correct: correct,
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
        const sentence = this.getSentence(this.answerArray, values).toString().replace(/,/g, " ");
        return this.doSave(false, this.correctAnswer, sentence);
    }

    /**
     * Handles the checking of user's answer's correctness.
     */
    async handleAnswer() {
        if (this.pluginMode === Mode.Instruction) {
            this.pluginMode = Mode.QuestionItem;
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

        if (this.pluginMode === Mode.QuestionItem) {
            if (this.index > this.attrs.questionItems.length) {
                this.printFeedback("No more question items, thanks for playing");
                this.pluginMode = Mode.EndTask;
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
                    // TODO: How many times one has to answer correctly to end the task
                    this.correctAnswer = true;
                    this.pluginMode = Mode.Feedback;
                } else {
                    if (this.currentFeedbackLevel < feedbackLevels.length) {
                        this.printFeedback(feedbackLevels[this.currentFeedbackLevel++]);
                        this.correctAnswer = false;
                        this.pluginMode = Mode.Feedback;
                    } else {
                        // this.printFeedback(feedbackLevels[feedbackLevels.length - 1]);
                        this.pluginMode = Mode.EndTask;
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
            // this.pluginMode = Mode.Feedback;
            // For demonstration
            this.showMode = "Feedback paragraph";
            return;
        }

        if (this.pluginMode === Mode.Feedback) {
            const success = this.save();
            if (!success) {
                this.error = success;
                return;
            }
            if (this.correctAnswer) {
                this.pluginMode = Mode.EndTask; // end the task for now
                return;
            }
            this.pluginMode = Mode.QuestionItem;
            this.printFeedback("");
            // For demonstration
            this.showMode = "Question item paragraph";
            return;
        }

        if (this.pluginMode === Mode.EndTask) {
            this.feedback = "The task has ended";
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

    /**
     * Checks if the answers selected in the question item plugins match any of the match arrays defined
     * in the choices for the current question item.
     *
     * @param match array defined in the markup for question item's choices
     * @param answer array of selections the user has made as the answers for the question item
     * @returns{boolean} whether a match was found or not
     */
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
        this.userSelections = [];
        const timComponent = this.vctrl.getTimComponentByName(plugins[0]);

        if (timComponent) {
            const par = timComponent.getPar();
            const content = par.children(".parContent");
            const nodes = content[0].childNodes;
            const answer: string[] = [];
            const values: { [id: string]: string } = {};
            for (let n = 0; n < nodes.length; ++n) {
                const node = nodes[n];
                if (node.nodeName === "#text") {
                    let text = node.textContent || "";
                    const words = text.trim().split(" ");
                    for (const word of words) {
                        answer.push(word);
                    }
                }

                if (node.nodeName === "TIM-PLUGIN-LOADER") {
                    // TODO: Check for later to skip drag sources in paragraph node.getAttribute("class") === "plugindragsource"
                    if (node instanceof Element) {
                        let name = node.getAttribute("task-id")!.split(".")[1];
                        if (name) {
                            let plugin = this.vctrl.getTimComponentByName(name);
                            if (plugin) {
                                let content = plugin.getContent().trim();
                                values[name] = content;
                                this.userSelections.push(content);
                                answer.push(name);
                            }
                        }
                    }
                }
            }
            this.answerArray = answer;
            this.userAnswer = this.getSentence(answer, values);
        }
        return this.userSelections;
    }

    /**
     * Goes through the sentence it is given and replaces words with the ones in the dictionary it is given
     *
     * @param sentence the sentence where words are being replaced
     * @param choices dictionary that tells which word you want to replace and with what
     * @returns{string[]} Array with the words replaced
     */
    getSentence(sentence: string[], choices: { [id: string]: string }): string[] {
        const temp = [...sentence];
        for (let c in choices) {
            const choice = choices[c];
            const index = temp.indexOf(c);
            if (index >= 0) {
                temp[index] = choice;
            }
        }
        return temp;
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