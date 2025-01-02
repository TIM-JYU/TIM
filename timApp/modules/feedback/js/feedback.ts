/**
 * Defines the client-side implementation of a feedback-plugin.
 *
 */
import * as t from "io-ts";
import type {
    ApplicationRef,
    DoBootstrap,
    OnDestroy,
    OnInit,
} from "@angular/core";
import {Component, NgModule} from "@angular/core";
import type {ITimComponent} from "tim/document/viewctrl";
import {
    GenericPluginMarkup,
    Info,
    nullable,
    withDefault,
} from "tim/plugin/attributes";
import {documentglobals} from "tim/util/globals";
import {injectStyle} from "tim/util/utils";
import type {EditMode} from "tim/document/popup-menu-dialog.component";
import {HttpClientModule} from "@angular/common/http";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {PurifyModule} from "tim/util/purify.module";
import {registerPlugin} from "tim/plugin/pluginRegistry";
import {CommonModule} from "@angular/common";

const answerPlaceHolder = "|answer|";
const correctPlaceHolder = "|correct|";
const answerPlaceHolderRegExp = /\|answer\[[0-9]+\]\|/g;
const answerWordsPlaceHolderRegExp = /\|answer\[[0-9]+-[0-9]+\]\|/g;
const matchPlaceHolderRegExp = /\|match\[[0-9]+\]\|/g;
const matchWordPlaceHolderRegExp = /\|match\[[0-9]+:[0-9]+\]\|/g;
const matchWordsPlaceHolderRegExp = /\|match\[[0-9]+:[0-9]+-[0-9]+\]\|/g;
const partPlaceHolderRegExp = /\|part\[[0-9]+\]\|/g;
const answerRegExpArray = [
    answerPlaceHolderRegExp,
    answerWordsPlaceHolderRegExp,
];
const matchRegExpArray = [
    matchPlaceHolderRegExp,
    matchWordPlaceHolderRegExp,
    matchWordsPlaceHolderRegExp,
];
const keywordPlaceHolder = /\|kw:.*\|/;

// TODO: A placeholder for the level the learner currently is to be shown back to them.

enum Mode {
    // TODO: Make it so that the instructions and practice item are not presented at the same time.
    Instruction = 0,
    QuestionItem = 1,
    Feedback = 2,
    EndTask = 3,
}

const MatchElement = t.type({
    answer: t.string,
    index: t.Integer,
});

interface IMatchElementT extends t.TypeOf<typeof MatchElement> {}

const MatchElementArray = t.array(MatchElement);
const StringArray = t.array(t.string);

const Choice = t.type({
    correct: withDefault(t.boolean, false),
    levels: StringArray,
    match: t.union([StringArray, MatchElementArray]),
});

interface IQuestionItemT extends t.TypeOf<typeof QuestionItem> {}

// TODO: Change words to optional so it works with plugins that set their own words. Check that getSentence() doesn't break at the same time.
const QuestionItem = t.intersection([
    t.partial({
        area: t.string,
        dragSource: t.string,
    }),
    t.type({
        choices: t.array(Choice),
        pluginNames: StringArray,
        words: t.array(StringArray),
    }),
]);

const FeedbackMarkup = t.intersection([
    t.partial({
        nextTask: t.string,
        practiceID: t.string,
    }),
    GenericPluginMarkup,
    t.type({
        // All withDefaults should come here, NOT in t.partial.
        autoupdate: withDefault(t.number, 500),
        cols: withDefault(t.number, 20),
        correctsInRow: withDefault(t.number, 1),
        questionItems: t.array(QuestionItem),
        showAnswers: withDefault(t.boolean, false),
        shuffle: withDefault(t.boolean, false),
    }),
]);
const FeedbackAll = t.intersection([
    t.partial({
        state: nullable(
            t.type({
                correct: t.boolean,
                correct_answer: t.string,
                feedback: t.string,
                user_answer: t.string,
            })
        ),
    }),
    t.type({
        info: Info,
        markup: FeedbackMarkup,
        preview: t.boolean,
    }),
]);

// noinspection TypeScriptUnresolvedVariable
@Component({
    selector: "tim-feedback-runner",
    template: `
<div class="feedbackContent">
    <tim-markup-error *ngIf="markupError" [data]="markupError"></tim-markup-error>
    <div class="error" *ngIf="error" [innerHtml]="error | purify"></div>
    <h4 *ngIf="header" [innerHtml]="header | purify"></h4>
    <p *ngIf="stem">{{stem}}</p>
    <div class="form-inline">
    <span [innerHtml]="feedback | purify"></span>
    </div>
    <button class="timButton feedbackButton"
            *ngIf="buttonText()"
            (click)="handleAnswer()"
            [disabled]="markupError || saving">
        {{buttonText()}}
    </button>
    <div class="feedbackAnswer" *ngIf="showAnswers && attrsall.state">
        <p>User answer: <span [innerHtml]="attrsall.state.user_answer | purify"></span></p>
        <p>Feedback: <span [innerHtml]="attrsall.state.feedback | purify"></span></p>
        <p>Correct answer: <span [innerHtml]="attrsall.state.correct_answer | purify"></span></p>
    </div>
    <p *ngIf="footer" [innerHtml]="footer | purify"></p>
</div>
`,
})
export class FeedbackPluginComponent
    extends AngularPluginBase<
        t.TypeOf<typeof FeedbackMarkup>,
        t.TypeOf<typeof FeedbackAll>,
        typeof FeedbackAll
    >
    implements ITimComponent, OnInit, OnDestroy
{
    error?: string;

    private userAnswer: string[] = [];
    feedback = "";
    private questionItemIndex!: number;
    private currentFeedbackLevel = 0;
    private pluginMode = Mode.Instruction;
    private answerArray: string[] = [];
    private userSelections: string[] = [];
    private correctAnswer = false;
    private correctAnswerString = "";
    private isAnsweredArray!: boolean[];
    private streak = 0;
    private teacherRight: boolean = false;
    feedbackMax = 0;
    private selectionMap: Map<string, string> = new Map();
    private correctMap: Map<string, string> = new Map();
    private editMode?: EditMode | null;
    private edited = false;
    private btnText = "Begin";
    private instrHidden = false;
    private itemHidden = true;
    saving = false;
    showAnswers?: boolean;
    private forceSave = false;
    private partArray: string[] = [];

    ngOnInit() {
        super.ngOnInit();

        this.addToCtrl();
        this.vctrl.listen("editModeChange", this.editModeChanged);
        this.setPluginWords();
        this.isAnsweredArray = new Array(this.markup.questionItems.length).fill(
            false
        );
        if (this.markup.shuffle) {
            const questionIndex = this.getRandomQuestion(this.isAnsweredArray);
            if (questionIndex !== undefined) {
                this.questionItemIndex = questionIndex;
            }
        } else {
            this.questionItemIndex = 0;
        }
        this.teacherRight = this.vctrl.item.rights.teacher;
        // TODO: Add an attribute that prevents the hiding of elements as an alternative to the edit mode prevention, maybe only in init?
        if (!this.attrsall.preview) {
            this.hideQuestionItems();
        }
        this.checkCorrectAnswers();
        this.feedbackMax = this.checkFeedbackLevels();
        this.checkPlugins();
        this.checkCorrectAnswersCount();
        this.checkInstructions();
        this.checkDefaultMatch();
        this.editMode = documentglobals().editMode;
        if (this.editMode != null) {
            this.edited = true;
        }
        if (this.vctrl.teacherMode || this.markup.showAnswers) {
            this.showAnswers = true;
        }
        if (!this.showAnswers) {
            injectStyle("/static/stylesheets/hideanswerbrowser.css");
        }
        if (
            !this.vctrl.item.rights.editable ||
            !this.vctrl.item.rights.teacher
        ) {
            injectStyle("/static/stylesheets/viewhide.css");
            this.vctrl.actionsDisabled = true;
        }
        this.showDocument();
    }

    ngOnDestroy(): void {
        this.vctrl.removeListener("editModeChange", this.editModeChanged);
        if (!this.attrsall.preview) {
            this.vctrl.removeTimComponent(this);
        }
    }

    /**
     * Adds this plugin to ViewCtrl so other plugins can get information about the plugin though it.
     */
    addToCtrl() {
        if (!this.attrsall.preview) {
            this.vctrl.addTimComponent(this);
        }
    }

    get autoupdate(): number {
        return this.markup.autoupdate;
    }

    getDefaultMarkup() {
        return {questionItems: []};
    }

    buttonText() {
        return this.btnText;
    }

    setButtonText(text: string) {
        this.btnText = text;
    }

    /**
     * Makes the paragraph area visible. Used in conjuncture with CSS in the document settings that hides the paragraph
     * area. Removes the flickering of elements in the page as they get initialized.
     */
    showDocument() {
        const doc = document.querySelectorAll(".paragraphs");
        if (doc.length > 0) {
            doc[0].setAttribute("style", "display:block");
        }
    }

    /**
     * Check that the question items have plugins defined in them.
     */
    checkPlugins() {
        const items = this.markup.questionItems;
        for (let i = 0; i < items.length; i++) {
            if (items[i].pluginNames.length === 0 && !this.error) {
                this.error = `Question item in index ${i} does not have any plugins defined`;
            }
        }
    }

    /**
     * Checks that the question items have the same amount of feedbacklevels apart from the correct choice.
     *
     * @returns{number} The maximum level of feedback.
     */
    checkFeedbackLevels(): number {
        const items = this.markup.questionItems;
        const levels: number[] = [];
        for (const item of items) {
            for (const choice of item.choices) {
                if (!choice.correct) {
                    levels.push(choice.levels.length);
                }
            }
        }

        if (!levels.every((x) => x === levels[0]) && !this.error) {
            this.error = "Different number of feedback levels";
        }
        return levels[0];
    }

    /**
     * Checks that the tasks question items correct choices have a match for every plugin.
     */
    checkCorrectAnswersCount() {
        const items = this.markup.questionItems;
        for (const item of items) {
            for (const choice of item.choices) {
                if (choice.correct) {
                    if (
                        item.pluginNames.length !== choice.match.length &&
                        !this.error
                    ) {
                        this.error = `${item.pluginNames.toString()}'s correct answer is missing a match for some of its plugins`;
                    }
                }
            }
        }
    }

    /**
     * Check that the task has a practice item or a TIM block with instructions.
     */
    checkInstructions() {
        const id = this.markup.practiceID;
        const instruction = document.querySelectorAll(".par.instruction");

        if (id) {
            const inst = this.vctrl.getTimComponentByName(id);
            if (!inst && !this.error) {
                this.error =
                    "Feedback plugin has instruction plugin defined but it cannot be found from the document.";
            }
        }
        if (!id && instruction.length < 1 && !this.error) {
            this.error =
                "Missing an instruction block or it has no .instruction-class defined.";
        }
    }

    /**
     * Check that all the question items have correct answers defined.
     */
    checkCorrectAnswers() {
        const items = this.markup.questionItems;
        for (const item of items) {
            const missing = item.choices.every((x) => !x.correct);
            if (missing && !this.error) {
                this.error = `A question item (${item.pluginNames.toString()}) is missing the correct answer.`;
            }
        }
    }

    /**
     * Check that all the question items have a default match defined.
     *
     * TODO: Should all the YAML checks be in one function?
     */
    checkDefaultMatch() {
        const items = this.markup.questionItems;
        for (const item of items) {
            const defaultMatch = item.choices.filter(
                (x) => x.match.length === 0
            );
            if (defaultMatch.length === 0 && !this.error) {
                this.error = `A question item (${item.pluginNames.toString()}) is missing default feedback.`;
            }
        }
    }

    /**
     * Hide all the question items from the user.
     */
    hideQuestionItems() {
        const items = this.markup.questionItems;
        for (let i = 0; i < items.length; i++) {
            const area = this.markup.questionItems[i].area;
            if (area) {
                this.hideArea(area);
            } else {
                this.hideBlock(i);
            }
        }
    }

    /**
     * Changes the visibility of the block of text that has the question items of the given index.
     *
     * @param index The question items to be shown or hidden.
     * @param show Whether to show or hide the question item.
     */
    setBlockVisibility(index: number, show: boolean) {
        if (index < this.markup.questionItems.length) {
            const name = this.markup.questionItems[index].pluginNames[0];
            const plugin = this.vctrl.getTimComponentByName(name);
            if (plugin) {
                const node = plugin.getPar()?.getContent();
                this.changeVisibility(node, show);
            }
        }
    }

    /**
     * Make a block visible to the user.
     *
     * @param index The question item block to be shown.
     */
    showBlock(index: number) {
        this.setBlockVisibility(index, true);
    }

    /**
     * Hide a block from the user.
     *
     * @param index The question item block to be hidden.
     */
    hideBlock(index: number) {
        this.setBlockVisibility(index, false);
    }

    /**
     * Hide a component and its paragraph.
     *
     * @param component The component and paragraph around it to be hidden.
     */
    hideComponent(component: ITimComponent) {
        const node = component.getPar()?.getContent();
        this.changeVisibility(node, false);
    }

    /**
     * Hide a paragraph from the page.
     *
     * @param paragraph The paragraph to hide.
     */
    hideParagraph(paragraph?: Element) {
        this.changeVisibility(paragraph, false);
    }

    /**
     * Show a paragraph in the page.
     *
     * @param paragraph The paragraph to show.
     */
    showParagraph(paragraph?: Element) {
        this.changeVisibility(paragraph, true);
    }

    /**
     * Hide a TIM area from the page.
     *
     * TODO: Hide all areas with the same name instead of the first?
     */
    hideArea(area: string) {
        const areaElement = document.querySelectorAll(`.${area}`);
        if (areaElement.length > 0) {
            this.hideParagraph(areaElement[0]);
        }
    }

    /**
     * Show a TIM area in the page.
     *
     * * TODO: Show all areas with the same name instead of the first?
     */
    showArea(area: string) {
        const areaElement = document.querySelectorAll(`.${area}`);
        if (areaElement.length > 0) {
            this.showParagraph(areaElement[0]);
        }
    }

    /**
     * Hides a HTML element from the user.
     *
     * @param node Node to be hidden.
     * @param visible Whether the node should be visible or hidden.
     */
    changeVisibility(node: Element | undefined, visible: boolean) {
        if (!node) {
            return;
        }
        if (visible) {
            node.classList.remove("hidden");
        } else {
            node.classList.add("hidden");
        }
    }

    /**
     * Gets the match that is correct for the question item.
     *
     * @param item Question item that the choices are being checked from.
     * @returns{number} The index of the correct match.
     */
    getCorrectChoice(item: IQuestionItemT): number | undefined {
        for (let i = 0; i < item.choices.length; i++) {
            if (item.choices[i].correct) {
                return i;
            }
        }
        return;
    }

    /**
     * Handles saving of the feedback plugin. To get the correct answer to the database, replaces user selections
     * with the selections that are correct.
     */
    async save() {
        const userAnswer = this.getSentence(
            this.answerArray,
            this.selectionMap
        ).join(" ");
        return await this.doSave(
            false,
            this.correctAnswer,
            this.correctAnswerString,
            userAnswer
        );
    }

    async doSave(
        nosave: boolean,
        correct: boolean,
        correctAnswer: string,
        userAnswer: string
    ) {
        this.saving = true;
        const params = {
            input: {
                correct: correct,
                correct_answer: correctAnswer,
                feedback: this.feedback,
                nosave: false,
                user_answer: userAnswer,
            },
            options: {
                forceSave: this.forceSave,
            },
        };

        if (nosave) {
            params.input.nosave = true;
        }

        const r = await this.postAnswer<{
            web: {result: string; error?: string};
        }>(params);
        this.saving = false;
        if (r.ok) {
            const data = r.result;
            this.error = data.web.error;
        } else {
            this.error = r.result.error.error;
        }
        return {saved: r.ok, message: this.error};
    }

    /**
     * Force the plugin to save its information.
     *
     * @param force Whether to force the plugin to always save itself when the answer route is called.
     */
    setForceAnswerSave(force: boolean) {
        this.forceSave = force;
    }

    /**
     * Saves all the plugins in the question item to the database.
     *
     * @param plugins Plugins to be saved.
     */
    async savePlugins(plugins: IQuestionItemT) {
        for (const p of plugins.pluginNames) {
            const plugin = this.vctrl.getTimComponentByName(p);
            if (plugin) {
                const success = await this.savePlugin(plugin);
                if (!success) {
                    this.error = "Error while saving plugins";
                    return false;
                }
                this.error = undefined;
            }
        }
        return true;
    }

    /**
     * Saves the selected plugins content to the database.
     *
     * @param plugin The plugin to be saved.
     * @returns{boolean} Whether the save was successful.
     */
    async savePlugin(plugin: ITimComponent) {
        this.saving = true;
        if (plugin.setForceAnswerSave) {
            plugin.setForceAnswerSave(true);
        }
        const failure = await plugin.save();
        this.saving = false;
        return failure.message == null;
    }

    /**
     * Handles the checking of users answers correctness and cycles through the different modes in the plugin.
     *
     */
    async handleAnswer() {
        const answer = document.querySelectorAll(".feedbackAnswer");
        if (answer.length > 0) {
            answer[0].setAttribute("style", "display:none");
        }
        if (this.pluginMode === Mode.EndTask) {
            const button = document.querySelectorAll(".feedbackButton");
            if (button.length > 0) {
                this.hideParagraph(button[0]);
            }
            if (this.markup.nextTask) {
                const next = this.markup.nextTask;
                this.printFeedback(next);
            }
            return;
        }

        if (this.pluginMode === Mode.Instruction) {
            const instructionQuestion = this.vctrl.getTimComponentByName(
                this.markup.practiceID ?? ""
            );
            if (this.markup.practiceID && instructionQuestion) {
                if (instructionQuestion.getContent() === undefined) {
                    this.printFeedback("Please select a choice");
                    return;
                }
                const success = await this.savePlugin(instructionQuestion);
                if (!success) {
                    this.error = "Error saving a plugin";
                    return;
                }
                if (!this.teacherRight || this.editMode == null) {
                    this.hideComponent(instructionQuestion);
                }
            } else {
                const instruction =
                    document.querySelectorAll(".par.instruction");
                if (
                    instruction &&
                    (!this.teacherRight || this.editMode == null)
                ) {
                    this.hideParagraph(instruction[0]);
                }
                const result = await this.doSave(false, false, "", "");
                if (!result.saved) {
                    this.error = "Error saving a plugin";
                    return;
                }
            }
            this.instrHidden = true;
            this.setButtonText("OK");
            this.printFeedback("");
            this.pluginMode = Mode.QuestionItem;
            const area = this.markup.questionItems[this.questionItemIndex].area;
            if (area) {
                this.showArea(area);
            } else {
                this.showBlock(this.questionItemIndex);
                this.itemHidden = false;
            }
            if (
                !this.vctrl.item.rights.editable ||
                !this.vctrl.item.rights.teacher
            ) {
                this.vctrl.doingTask = true;
            }
            return;
        }

        if (this.pluginMode === Mode.QuestionItem) {
            const plugins = this.markup.questionItems[this.questionItemIndex];
            if (!this.hasContent(plugins)) {
                this.error = "You need to provide an answer.";
                return;
            }
            this.error = undefined;

            // Gets all the plugins from the visible question item and compares to choices-array to check which matches.
            const selections = this.getAnswerFromPlugins();
            this.correctMap = this.getCorrectValues();
            // TODO: Fix this so that if there is a dot after the plugin in a paragraph, there is no space between the word and the dot.
            this.correctAnswerString = this.getSentence(
                this.answerArray,
                this.correctMap
            ).join(" ");
            const matchIndex = this.compareChoices(
                this.questionItemIndex,
                selections
            );

            if (!this.teacherRight || this.editMode == null) {
                const area =
                    this.markup.questionItems[this.questionItemIndex].area;
                if (area) {
                    this.hideArea(area);
                } else {
                    this.hideBlock(this.questionItemIndex);
                    this.itemHidden = true;
                }
            }

            if (matchIndex === undefined) {
                this.error =
                    "You have no choices defined, got no matches or using match objects";
            } else {
                // Get the choice in matchIndex and the feedbacks assigned to it.
                if (matchIndex !== undefined) {
                    const choice = plugins.choices[matchIndex];
                    const feedbackLevels = choice.levels;
                    if (choice.correct) {
                        this.printFeedback(
                            feedbackLevels[feedbackLevels.length - 1]
                        );
                        this.correctAnswer = true;
                        // TODO: Might be useful, if the incorrect answers were presented to the user again. Maybe add an attribute for it.
                        this.isAnsweredArray[this.questionItemIndex] = true;
                        this.streak++;
                    } else {
                        this.printFeedback(
                            feedbackLevels[this.currentFeedbackLevel++]
                        );
                        this.correctAnswer = false;
                        this.isAnsweredArray[this.questionItemIndex] = true;
                        this.streak = 0;
                    }
                    this.pluginMode = Mode.Feedback;
                }
            }

            const success = await this.savePlugins(plugins);
            if (!success) {
                this.error = "Error saving plugins";
                return;
            }

            this.setButtonText("Continue");
            return;
        }

        if (this.pluginMode === Mode.Feedback) {
            const result = await this.save();
            if (!result.saved) {
                this.error = "Error saving feedback";
                return;
            }

            this.printFeedback("");

            // Whether to give a question item in a random index or give consecutive ones.
            let questionIndex;
            if (this.markup.shuffle) {
                questionIndex = this.getRandomQuestion(this.isAnsweredArray);
                if (questionIndex !== undefined) {
                    this.questionItemIndex = questionIndex;
                }
            } else {
                this.questionItemIndex++;
                questionIndex = this.questionItemIndex;
            }

            if (
                questionIndex === undefined ||
                this.streak === this.markup.correctsInRow ||
                this.isAnsweredArray.every((x) => x) ||
                this.currentFeedbackLevel === this.feedbackMax ||
                this.questionItemIndex >= this.markup.questionItems.length
            ) {
                this.pluginMode = Mode.EndTask;
                if (
                    !this.vctrl.item.rights.editable ||
                    !this.vctrl.item.rights.teacher
                ) {
                    this.vctrl.doingTask = false;
                }
                this.itemHidden = true;
                this.handleAnswer();
                return;
            }

            this.setPluginWords();

            this.pluginMode = Mode.QuestionItem;
            const area = this.markup.questionItems[this.questionItemIndex].area;
            if (area) {
                this.showArea(area);
            } else {
                this.showBlock(this.questionItemIndex);
                this.itemHidden = false;
            }
            this.setButtonText("OK");
            return;
        }
    }

    /**
     * Checks if the plugins given have any content to return.
     *
     * @param plugins Plugins to check.
     */
    hasContent(plugins: IQuestionItemT): boolean {
        if (!plugins) {
            return false;
        }
        for (const p of plugins.pluginNames) {
            const plugin = this.vctrl.getTimComponentByName(p);
            if (plugin && plugin.getContent() === undefined) {
                return false;
            }
        }
        return true;
    }

    /**
     * Returns a random array index from the question items that are not correctly answered to, excluding the current
     * question item if it is not the only possibility.
     *
     * @param arr Array of the correctness state of question items.
     * @returns{number} Index of the question to be shown.
     */
    getRandomQuestion(arr: boolean[]): number | undefined {
        const falses: number[] = [];
        for (let i = 0; i < arr.length; i++) {
            if (!arr[i] || arr.length === 1) {
                falses.push(i);
            }
        }

        if (falses.length === 0) {
            return;
        }

        if (falses.length === 1) {
            return falses[0];
        }

        if (this.questionItemIndex) {
            for (let i = 0; i < falses.length; i++) {
                if (falses[i] === this.questionItemIndex) {
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
     * @param index The index of the question item.
     * @param answer Array of the answers the user has selected.
     * @returns(number) The index of the choice that matches user selections.
     */
    compareChoices(index: number, answer: string[]): number | undefined {
        const choices = this.markup.questionItems[index].choices;
        if (choices.length === 0) {
            return;
        }

        for (let i = 0; i < choices.length; i++) {
            const match = choices[i].match;
            if (StringArray.is(match)) {
                if (this.checkMatchStringArray(match, answer)) {
                    return i;
                }
            } else {
                // If the match is a MatchObjectArray instead of a string array.
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
     * @param match Array defined in the markup for question item's choices.
     * @param answer Array of selections the user has made as the answers for the question item.
     * @returns{boolean} Whether a match was found or not.
     */
    checkMatchStringArray(match: string[], answer: string[]): boolean {
        if (match.length === 0) {
            return true;
        }

        if (match.length !== answer.length) {
            return false;
        }
        for (let i = 0; i < answer.length; i++) {
            // TODO: Keyword not really needed with RegExp, but leave it in anyway for now if users happen to use it.
            const kw = match[i].match(keywordPlaceHolder);
            if (kw && kw.length > 0) {
                const word = kw[0].split(":")[1].replace("|", "");
                if (!answer[i].toLowerCase().includes(word.toLowerCase())) {
                    return false;
                }
                continue;
            }
            if (match[i] === answer[i]) {
                continue;
            }
            const re = new RegExp(`\\b${match[i]}\\b`);
            if (!re.test(answer[i])) {
                return false;
            }
        }
        return true;
    }

    /**
     * TODO: This function needs to be implemented at some point if the matches are ever used as MatchElement-types
     * @param match MatchElement defined in the markup for question item's choices.
     * @param answer Array of selections the user has made as the answers for the question item.
     */
    checkMatchObjectArray(
        match: IMatchElementT[],
        answer: string[]
    ): number | undefined {
        return;
    }

    /**
     * Prints the feedback the user should see for their answer.
     *
     * @param feedback What feedback should be presented to the user.
     */
    printFeedback(feedback: string) {
        const answer = this.userAnswer.join(" ");
        this.feedback = feedback;
        this.feedback = this.feedback.replace(answerPlaceHolder, answer);
        this.feedback = this.feedback.replace(
            correctPlaceHolder,
            this.correctAnswerString
        );
        const placere = this.feedback.match(partPlaceHolderRegExp);
        if (placere) {
            this.replacePlaceHolder(placere, this.partArray);
        }

        for (const placeholder of answerRegExpArray) {
            const answerre = this.feedback.match(placeholder);
            if (answerre) {
                this.replacePlaceHolder(answerre, this.userAnswer);
            }
        }

        for (const placeholder of matchRegExpArray) {
            const matchre = this.feedback.match(placeholder);
            if (matchre) {
                this.replacePlaceHolder(matchre, this.userSelections);
            }
        }
    }

    /**
     * Replace the placeholders found in feedback with words from the given array.
     *
     * @param re Array of RegExp-matches.
     * @param wordarray Words that should be replaced by the placeholders.
     */
    replacePlaceHolder(re: RegExpMatchArray, wordarray: string[]) {
        for (const placeholder of re) {
            const index = placeholder.match(/[0-9]+/);
            let replacement = "";
            if (index) {
                const indexText = wordarray[parseInt(index.toString(), 10)];
                if (indexText) {
                    replacement = indexText;
                }
            }
            const replacementArray = replacement.split(" ");
            const word = placeholder.match(/:[0-9]+\]/);
            const words = placeholder.match(/:[0-9]+-[0-9]+/);
            const indices = placeholder.match(/[0-9]+-[0-9]+/);

            if (word) {
                const wordString = word.toString();
                const wordIndex = parseInt(wordString.split(":")[1], 10);
                if (replacementArray[wordIndex]) {
                    this.feedback = this.feedback.replace(
                        placeholder,
                        replacementArray[wordIndex]
                    );
                } else {
                    this.feedback = this.feedback.replace(placeholder, "");
                }
            }

            if (words) {
                const result = this.replaceMultipleWords(
                    replacementArray,
                    words
                );
                this.feedback = this.feedback.replace(placeholder, result);
            }

            if (indices) {
                const result = this.replaceMultipleWords(wordarray, indices);
                this.feedback = this.feedback.replace(placeholder, result);
            }
            if (!word && !words && !indices) {
                this.feedback = this.feedback.replace(placeholder, replacement);
            }
        }
    }

    /**
     * Returns the strings between the given indices from a string array.
     *
     * @param array String array to go through.
     * @param indices The indices that should be returned.
     * @returns{string} A string from the given indices in the array.
     */
    replaceMultipleWords(array: string[], indices: string[]): string {
        const wordString = indices.toString().split("-");
        const start = parseInt(wordString[0].replace(":", ""), 10);
        const end = parseInt(wordString[1], 10);
        let result = "";
        for (let i = start; i <= end; i++) {
            if (array[i]) {
                result += ` ${array[i]}`;
            }
        }
        return result.trim();
    }

    /**
     * Gets the user's answer from the currently visible question item of those this feedback-plugin is assigned to.
     * TODO: Maybe add getting answers in an area if needed.
     *
     * @returns(string[]) The user's selections to the question item.
     */
    getAnswerFromPlugins(): string[] {
        const plugins =
            this.markup.questionItems[this.questionItemIndex].pluginNames;
        const timComponent = this.vctrl.getTimComponentByName(plugins[0]);

        if (timComponent) {
            const content = timComponent.getPar()?.getContent();
            if (!content) {
                // log("Not in paragraph, skipping getting answers");
                return [];
            }
            // Add additional nodes to be accepted if the need arises.
            const treeWalker = document.createTreeWalker(
                content,
                NodeFilter.SHOW_ALL,
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
                        // Note, currently hiding stuff from question item answers by putting them inside a <div>,
                        // need to be modified if div is ever changed to be accepted.
                        return NodeFilter.FILTER_REJECT;
                    },
                }
            );

            const answer: string[] = [];
            const parts: string[] = [];
            const selections: string[] = [];
            const values = new Map<string, string>();

            while (treeWalker.nextNode()) {
                const node = treeWalker.currentNode;
                if (node.nodeName === "#text" && node.textContent !== null) {
                    const textNodeContent = node.textContent.trim();
                    if (textNodeContent !== "") {
                        parts.push(textNodeContent);
                    }
                    const words = textNodeContent.split(" ");
                    for (const word of words) {
                        if (word !== "") {
                            answer.push(word);
                        }
                    }
                }

                if (node.nodeName === "TIM-PLUGIN-LOADER") {
                    if (node instanceof Element) {
                        const name = node
                            .getAttribute("task-id")!
                            .split(".")[1];
                        if (name && plugins.includes(name)) {
                            const plugin =
                                this.vctrl.getTimComponentByName(name);
                            if (plugin) {
                                if (plugin.getContentArray) {
                                    const pluginNodeArrayContent =
                                        plugin.getContentArray();
                                    if (pluginNodeArrayContent !== undefined) {
                                        let contentString = "";
                                        if (pluginNodeArrayContent.length > 0) {
                                            for (const c of pluginNodeArrayContent) {
                                                parts.push(c);
                                                contentString += ` ${c}`;
                                            }
                                        }
                                        contentString = contentString.trim();
                                        values.set(name, contentString);
                                        selections.push(contentString);
                                        answer.push(name);
                                    }
                                } else {
                                    const pluginNodeContent =
                                        plugin.getContent();
                                    if (pluginNodeContent !== undefined) {
                                        values.set(
                                            name,
                                            pluginNodeContent.trim()
                                        );
                                        answer.push(name);
                                        parts.push(pluginNodeContent);
                                        selections.push(pluginNodeContent);
                                    }
                                }
                            }
                        }
                    }
                }
            }
            this.selectionMap = values;
            this.answerArray = answer;
            this.partArray = parts;
            this.userSelections = selections;
            this.userAnswer = this.getSentence(answer, values);
        }
        return this.userSelections;
    }

    /**
     * Goes through the sentence it is given and replaces words with the ones in the dictionary it is given.
     *
     * @param sentence The sentence where words are being replaced.
     * @param choices Dictionary that tells which word you want to replace and with what.
     * @returns{string[]} Array with the words replaced.
     */
    getSentence(sentence: string[], choices: Map<string, string>): string[] {
        const temp = [];
        const wordlists =
            this.markup.questionItems[this.questionItemIndex].words;
        let i = 0;
        let j = 0;
        for (const [k, v] of choices) {
            const re = new RegExp(v);
            let value = v;
            if (j < wordlists.length) {
                const word = wordlists[j].filter((x) => re.test(`\\b${x}\\b`));
                if (word.length > 0) {
                    value = word[0];
                }
            }

            // TODO: Might be removable if users rather use regexp to match long sentences.
            const kw = v.match(keywordPlaceHolder);
            if (kw && kw.length > 0 && j < wordlists.length) {
                const keyword = kw[0].split(":")[1].replace("|", "");
                const word = wordlists[j].filter((x) => x.includes(keyword));
                if (word.length > 0) {
                    value = word[0];
                }
            }
            j++;
            while (sentence[i] !== k) {
                temp.push(sentence[i]);
                i++;
            }
            const choice = value.split(" ");
            for (const c of choice) {
                temp.push(c);
            }
            i++;
            while (!choices.has(sentence[i]) && sentence[i]) {
                temp.push(sentence[i]);
                i++;
            }
        }
        return temp;
    }

    /**
     * Goes through the plugins of the current question item's correct answer and forms a map from them.
     *
     * @returns{Map<string, string>} Correct answer for every plugin in the question item.
     */
    getCorrectValues(): Map<string, string> {
        const values = new Map<string, string>();
        const item = this.markup.questionItems[this.questionItemIndex];
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
        return values;
    }

    /**
     * Sets the list of words used in a dropdown plugin and resets a drag source if one is defined. Goes through every
     * question item and every plugin inside the question item. It is assumed for now that the list of words and list
     * of plugins are in the same order and that there are the same amount of them both.
     */
    setPluginWords() {
        const items = this.markup.questionItems;
        for (const item of items) {
            let i = 0;
            for (const plugin of item.pluginNames) {
                const timComponent = this.vctrl.getTimComponentByName(plugin);
                if (timComponent?.setPluginWords) {
                    // TODO: This doesn't properly reset selected dropdown value
                    if (item.words.length === 0) {
                        timComponent.setPluginWords([]);
                    } else {
                        timComponent.setPluginWords(item.words[i]);
                    }
                    i++;
                } else {
                    this.error = `No plugin with such a name (${plugin}) or missing setPluginWords-method`;
                }
            }
            if (item.dragSource) {
                const timComponent = this.vctrl.getTimComponentByName(
                    item.dragSource
                );
                if (timComponent) {
                    timComponent.resetField();
                } else {
                    this.error = `No drag source with such a name (${item.dragSource})`;
                }
            }
        }
    }

    editModeChanged = (newVal: EditMode | null) => {
        if (!this.attrsall.preview && this.editMode != newVal) {
            this.editMode = newVal;
            const instructions = document.querySelectorAll(".par.instruction");
            if (!this.edited) {
                this.showParagraph(instructions[0]);
                const items = this.markup.questionItems;
                for (const item of items) {
                    if (item.pluginNames.length > 0) {
                        const plugin = this.vctrl.getTimComponentByName(
                            item.pluginNames[0]
                        );
                        if (plugin) {
                            this.showParagraph(plugin.getPar()?.getContent());
                        }
                    }
                }
                this.edited = true;
            } else {
                const items = this.markup.questionItems;
                for (const item of items) {
                    if (item.pluginNames.length > 0) {
                        const plugin = this.vctrl.getTimComponentByName(
                            item.pluginNames[0]
                        );
                        if (plugin) {
                            this.hideParagraph(plugin.getPar()?.getContent());
                        }
                    }
                }
                if (!this.instrHidden) {
                    this.showParagraph(instructions[0]);
                } else {
                    this.hideParagraph(instructions[0]);
                }
                if (!this.itemHidden) {
                    this.showBlock(this.questionItemIndex);
                }
                this.edited = false;
            }
        }
    };

    /**
     * Returns the content inside this plugin.
     * @returns {string} The content inside this plugin.
     *
     * TODO: Make the function return something if needed.
     */
    getContent() {
        return undefined;
    }

    getAttributeType() {
        return FeedbackAll;
    }

    isUnSaved() {
        return false; // TODO
    }
}

@NgModule({
    declarations: [FeedbackPluginComponent],
    imports: [CommonModule, HttpClientModule, TimUtilityModule, PurifyModule],
})
export class FeedbackModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

registerPlugin("feedback-runner", FeedbackModule, FeedbackPluginComponent);
