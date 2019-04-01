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

const answerPlaceHolder = "|answer|";
const answerPlaceHolderRegExp = /\|answer\[[0-9]+\]\|/g;
const wordPlaceHolderRegExp = /\|word\[[0-9]+\]\|/g;

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
    choices: t.array(Choice),
    pluginNames: StringArray,
    words: t.array(StringArray),
});

const FeedbackMarkup = t.intersection([
    t.partial({
        correctStreak: t.number,
        instructionID: t.string,
        nextTask: t.string,
        pluginID: t.string,
        sampleItemID: t.string,
    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
        autoupdate: withDefault(t.number, 500),
        cols: withDefault(t.number, 20),
        questionItems: t.array(QuestionItem),
        teacherHide: withDefault(t.boolean, false),
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
    private index: number = -2;
    private currentFeedbackLevel = 0;
    private feedbackLevelRise = false;
    private pluginMode = Mode.Instruction;
    private showMode = "Instruction paragraph"; // for demo
    private answerArray: string[] = [];
    private userSelections: string[] = [];
    private correctAnswer = false;
    private questionCount?: number;
    private correctArray?: boolean[];
    private streak = 0;
    private teacherRight: boolean = false;
    private feedbackMax = 0;
    private selectionMap: Map<string, string> = new Map();

    $onInit() {
        super.$onInit();
        this.addToCtrl();
        this.setPluginWords();
        this.questionCount = this.attrs.questionItems.length;
        this.correctArray = new Array(this.questionCount).fill(false);
        const questionIndex = this.getRandomQuestion(this.correctArray);
        if (questionIndex !== undefined) {
            this.index = questionIndex;
        }
        this.teacherRight = this.vctrl.item.rights.teacher;
        if (!this.teacherRight || this.attrs.teacherHide) {
            this.hideQuestionItems();
        }
        this.feedbackMax = this.checkFeedbackLevels();
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

    getDefaultMarkup() {
        return {questionItems: []};
    }

    buttonText() {
        return "OK";
    }

    /**
     * Checks that the question items have the same amount of feedbacklevels apart from the correct choice
     *
     * @returns{number} The maximum level of feedback
     */
    checkFeedbackLevels(): number {
        const items = this.attrs.questionItems;
        const levels: number[] = [];
        for (const item of items) {
            for (const choice of item.choices) {
                if (!choice.correct) {
                    levels.push(choice.levels.length);
                }
            }
        }

        if (!levels.every(x => x === levels[0])) {
            this.error = "Different number of feedback levels";
        }
        return levels[0];
    }

    /**
     * Hide all the question items from the user
     */
    hideQuestionItems() {
        const items = this.attrs.questionItems;
        for (let i = 0; i < items.length; i++) {
            this.setBlockVisibility(i, false);
        }
    }

    /**
     * Changes the visibility of the block of text that has the question items of the given index.
     *
     * @param index The question items to be shown or hidden
     * @param show Whether to show or hide the question item
     */
    setBlockVisibility(index: number, show: boolean) {
        const name = this.attrs.questionItems[index].pluginNames[0];
        const plugin = this.vctrl.getTimComponentByName(name);
        if (plugin) {
            const node = plugin.getPar().children(".parContent")[0];
            this.changeVisibility(node, show);
        }
    }

    /**
     * Make a block visible to the user
     *
     * @param index The question item block to be shown
     */
    showBlock(index: number) {
        this.setBlockVisibility(index, true);
    }

    /**
     * Hide a block from the user
     *
     * @param index The question item block to be hidden
     */
    hideBlock(index: number) {
        this.setBlockVisibility(index, false);
    }

    /**
     * Hide a component and its paragraph
     *
     * @param component the component and paragraph around it you want to hide
     */
    hideComponent(component: ITimComponent) {
        const node = component.getPar().children(".parContent")[0];
        this.changeVisibility(node, false);
    }

    /**
     * Hides a HTML element from the user
     *
     * @param node Node to be hidden
     * @param visible Whether the node should be visible or hidden
     */
    changeVisibility(node: Element, visible: boolean) {
        if (visible) {
            node.classList.remove("hidden");
        }
        if (!visible) {
            node.classList.add("hidden");
        }
    }

    /**
     * Gets the match that is correct for the question item
     *
     * @param item question item that the choices are being checked from
     * @returns{number} the index of the correct match
     */
    getCorrectChoice(item: QuestionItemT): number | undefined {
        for (let i = 0; i < item.choices.length; i++) {
            if (item.choices[i].correct) {
                return i;
            }
        }
        return;
    }

    /**
     * Handles saving of the feedback plugin. To get the correct answer to the database, replaces user selections
     * with the ones that are correct.
     */
    async save() {
        // TODO: finish this, can you replace just one instance?
        const values = new Map<string, string>();
        const item = this.attrs.questionItems[this.index];
        const plugins = item.pluginNames;
        const index = this.getCorrectChoice(item);
        if (index !== undefined) {
            const words = item.choices[index].match;
            for (let i = 0; i < plugins.length; i++) {
                if (StringArray.is(words)) {
                    values.set(plugins[i], words[i]);
                }
            }
        }
        // TODO: make better names
        const sentence = this.getSentence(this.answerArray, values).toString().replace(/,/g, " ");
        const answer = this.getSentence(this.answerArray, this.selectionMap).toString().replace(/,/g, " ");
        const failure = await this.doSave(false, this.correctAnswer, sentence, answer);
        return failure;
    }

    async doSave(nosave: boolean, correct: boolean, sentence: string, answer: string) {
        //this.error = "... saving ...";

        this.result = undefined;
        const params = {
            input: {
                nosave: false,
                feedback: this.feedback,
                correct: correct,
                sentence: sentence,
                answer: answer,
            },
            options: {
                forceSave: true,
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
     * Saves all the plugins in the question item to the database
     *
     * @param plugins plugins to be saved
     */
    async savePlugins(plugins: QuestionItemT) {
        for (const p of plugins.pluginNames) {
            const plugin = this.vctrl.getTimComponentByName(p);
            if (plugin) {
                const success = await this.savePlugin(plugin);
                if (!success) {
                    this.error = "Error while saving plugins";
                    return false;
                }
                this.error = "";
            }
        }
        return true;
    }

    /**
     * Saves the selected plugin's content to the database
     *
     * @param plugin The plugin to be saved
     * @returns{boolean} Whether the saving was succesful
     */
    async savePlugin(plugin: ITimComponent) {
        if (plugin.setForceAnswerSave) {
            plugin.setForceAnswerSave(true);
            const failure = await plugin.save();
            if (failure) {
                return false;
            }
            return true;
        }

    }

    /**
     * Handles the checking of user's answer's correctness.
     *
     * TODO: fix the task end logic in this....
     */
    async handleAnswer() {
        if (this.pluginMode === Mode.EndTask) {
            if(this.attrs.nextTask) {
                this.printFeedback(this.attrs.nextTask!);
            }
            return;
        }

        if (this.pluginMode === Mode.Instruction) {
            const instructionQuestion = this.vctrl.getTimComponentByName(this.attrs.instructionID || "");
            if (this.attrs.instructionID && instructionQuestion) {
                // TODO: need to use this in something?
                const success = await this.savePlugin(instructionQuestion);
                if (!success) {
                    this.printFeedback("Please select a choice");
                    return;
                }
                if (!this.teacherRight || this.attrs.teacherHide) {
                    this.hideComponent(instructionQuestion);
                }
            }
            this.printFeedback("");
            this.pluginMode = Mode.QuestionItem;
            // For demonstration
            this.showMode = "Question item paragraph, question " + (this.index + 1);
            this.showBlock(this.index);
            return;
        }

        if (this.pluginMode === Mode.QuestionItem) {
            //TODO: Is this actually ever triggered
            if (this.index > this.attrs.questionItems.length) {
                this.printFeedback("No more question items, thanks for playing");
                this.pluginMode = Mode.EndTask;
                return;
            }

            // Gets all the plugins from the visible question item and compares to choices-array to check which matches
            const selections = this.getAnswerFromPlugins();
            const matchIndex = this.compareChoices(this.index, selections);

            if (matchIndex === -1) {
                this.error = "You have no choices defined, got no matches or using match objects";
            } else {
                // Get the choice in matchIndex and the feedbacks assigned to it
                if (matchIndex !== undefined) {
                    const choice = this.attrs.questionItems[this.index].choices[matchIndex];
                    const feedbackLevels = choice.levels;
                    if (choice.correct) {
                        // TODO: Do you want to make more than one correct feedback?
                        this.printFeedback(feedbackLevels[feedbackLevels.length - 1]);
                        this.correctAnswer = true;
                        this.correctArray![this.index] = true;
                        this.streak++;
                        this.pluginMode = Mode.Feedback;
                    } else {
                        // TODO: put this somewhere else maybe, also problems if the choice numbers differ in question items
                        this.feedbackMax = feedbackLevels.length;
                        this.printFeedback(feedbackLevels[this.currentFeedbackLevel++]);
                        this.correctAnswer = false;
                        this.streak = 0;
                        this.pluginMode = Mode.Feedback;
                    }
                }
            }

            const plugins = this.attrs.questionItems[this.index];

            const success = await this.savePlugins(plugins);
            if (!success) {
                this.error = "Error saving plugins";
                return;
            }

            if (!this.teacherRight || this.attrs.teacherHide) {
                this.hideBlock(this.index);
            }
            // For demonstration
            this.showMode = "Feedback paragraph";
            return;
        }

        if (this.pluginMode === Mode.Feedback) {
            const failure = await this.save();
            if (failure) {
                this.error = "Error saving plugin";
                return;
            }

            this.printFeedback("");

            const questionIndex = this.getRandomQuestion(this.correctArray || []);
            if (questionIndex !== undefined) {
                this.index = questionIndex;
            }

            if (this.index === -1 || this.streak === this.attrs.correctStreak || this.currentFeedbackLevel === this.feedbackMax) {
                this.pluginMode = Mode.EndTask;
                return;
            }

            this.setPluginWords();

            // For demonstration
            this.showMode = "Question item paragraph, question " + (this.index + 1);
            this.pluginMode = Mode.QuestionItem;
            this.showBlock(this.index);
            return;
        }
    }

    /**
     * Returns a random array index from the question items that are not correctly answered to, excluding the current
     * question item if it is not the only possibility
     *
     * @param arr Array of the correctness state of question items
     * @returns{number} Index of the question to be shown
     */
    getRandomQuestion(arr: boolean[]): number | undefined {
        const falses: number[] = [];
        for (let i = 0; i < arr.length; i++) {
            if (!arr[i]) {
                falses.push(i);
            }
        }

        if (falses.length === 0) {
            return;
        }

        if (falses.length === 1) {
            return falses[0];
        }

        if (this.index !== -2) {
            for (let i = 0; i < falses.length; i++) {
                if (falses[i] === this.index) {
                    falses.splice(i, 1);
                }
            }
        }

        const r = Math.random() * falses.length;
        const index = Math.floor(r);
        return falses[index];
    }

    /**
     * Returns the index of the choice matching the selections the user made to the plugins of the question item.
     *
     * @param index The index of the question item
     * @param answer Array of the answers the user has selected
     * @returns(number) The index of the choice that matches user selections
     */
    compareChoices(index: number, answer: string[]): number | undefined {
        const choices = this.attrs.questionItems[index].choices;
        if (choices.length === 0) {
            return;
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
                return;
            }
        }
        return;
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

    /**
     * TODO
     * @param match
     * @param answer
     */
    checkMatchObjectArray(match: MatchElementT[], answer: string[]): number | undefined {
        return;
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

    /**
     * Gets the user's answer from the visible question item this feedback-plugin is assigned to.
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
            const treeWalker = document.createTreeWalker(content[0], NodeFilter.SHOW_ALL,
                {
                    acceptNode: (node) => {
                        if (node.nodeName === "#text") {
                            return NodeFilter.FILTER_ACCEPT;
                        }
                        if (node.nodeName === "TIM-PLUGIN-LOADER") {
                            return NodeFilter.FILTER_ACCEPT;
                        }
                        if (node.nodeName === "P") {
                            return NodeFilter.FILTER_ACCEPT;
                        }
                        return NodeFilter.FILTER_REJECT;
                    },
                });

            const answer: string[] = [];
            const values = new Map<string, string>();
            while (treeWalker.nextNode()) {
                const node = treeWalker.currentNode;
                if (node.nodeName === "#text" && node.textContent !== null) {
                    const words = node.textContent.trim().split(" ");
                    for (const word of words) {
                        if (word !== "") {
                            answer.push(word);
                        }
                    }
                }

                if (node.nodeName === "TIM-PLUGIN-LOADER") {
                    // TODO: Check for later to skip drag sources in paragraph node.getAttribute("class") === "plugindragsource"
                    if (node instanceof Element) {
                        let name = node.getAttribute("task-id")!.split(".")[1];
                        if (name) {
                            let plugin = this.vctrl.getTimComponentByName(name);
                            if (plugin) {
                                if (plugin.getContentArray) {
                                    let content = plugin.getContentArray();
                                    let contentString = "";
                                    for (const c of content) {
                                        contentString = contentString + " " + c;
                                    }
                                    contentString = contentString.trim();
                                    values.set(name, contentString);
                                    this.userSelections.push(contentString);
                                    answer.push(name);
                                } else {
                                    let content = plugin.getContent().trim();
                                    values.set(name, content);
                                    this.userSelections.push(content);
                                    answer.push(name);
                                }
                            }
                        }
                    }
                }
            }
            this.selectionMap = values;
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
    getSentence(sentence: string[], choices: Map<string, string>): string[] {
        const temp = [...sentence];
        for (const [k, v] of choices) {
            const index = temp.indexOf(k);
            if (index >= 0) {
                temp[index] = v;
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
     * TODO: randomize words
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

    /**
     * Returns the content inside this plugin
     * @returns {string} The content inside this plugin
     */
    getContent(): string {
        return "";
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
    <div class="form-inline">
    <span ng-bind-html="$ctrl.feedback"></span>
    </div>
    <button class="timButton"
            ng-if="::$ctrl.buttonText()"
            ng-click="$ctrl.handleAnswer()">
        {{::$ctrl.buttonText()}}
    </button>
    <div class="wrong" ng-if="$ctrl.error" ng-bind-html="$ctrl.error"></div>
    <pre ng-if="$ctrl.result">{{$ctrl.result}}</pre>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</div>
`,
});