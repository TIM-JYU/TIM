import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule, ViewChild} from "@angular/core";
import * as t from "io-ts";
import $ from "jquery";
import moment from "moment";
import type {IPreviewParams} from "tim/document/question/answer-sheet.component";
import {
    AnswerSheetModule,
    fixQuestionJson,
    getPointsTable,
    makePreview,
    minimizeJson,
} from "tim/document/question/answer-sheet.component";
import {numOrStringToNumber, TimStorage, to, toPromise} from "tim/util/utils";
import {
    KEY_DOWN,
    KEY_ENTER,
    KEY_LEFT,
    KEY_R,
    KEY_RIGHT,
    KEY_S,
    KEY_UP,
} from "tim/util/keycodes";
import type {
    IAskedJsonJson,
    IAskedQuestion,
    IColumn,
    IExplCollection,
    IHeader,
    IQuestionParagraph,
    IQuestionUI,
    IRow,
    MatrixType,
    QuestionType,
} from "tim/lecture/lecturetypes";
import type {IGenericPluginMarkup} from "tim/plugin/attributes";
import {$http} from "tim/util/ngimport";
import {FormsModule, NgForm} from "@angular/forms";
import {QuestionMatrixComponent} from "tim/document/question/question-matrix.component";
import {HttpClient} from "@angular/common/http";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {deleteQuestionWithConfirm} from "tim/document/question/fetchQuestion";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import type {IParResponse} from "tim/document/editing/edittypes";
import {CommonModule} from "@angular/common";

/**
 * Controller for creating and editing questions
 * @author Matias Berg
 * @author Bek Eljurkaev
 * @author Minna Lehtomäki
 * @author Juhani Sihvonen
 * @author Hannu Viinikainen
 * @licence MIT
 * @copyright 2015 Timppa project authors
 */

interface IAnswerField {
    label: string;
    value: MatrixType;
}

export interface IExtendedColumn extends IColumn {
    points: string;
}

export interface IExtendedRow extends IRow {
    expl: string;
    id: number;
    text: string;
    type: string;
    value: string;
    columns: IExtendedColumn[];
    locked?: boolean; // if locked, do not let randomization hide or move the row
}

export type IQuestionDialogParams = INewQuestionParams | IEditQuestionParams;

export interface INewQuestionParams {
    qst: boolean;
    par_id_next: string | null;
    docId: number;
}

export type IEditQuestionParams = IQuestionParagraph | IAskedQuestion;

export type IQuestionDialogResult = IQuestionEditResult | IPointsUpdate;

export interface IQuestionEditResult {
    data: IParResponse;
    ask: boolean;
    deleted: boolean;
    type: "edit";
}

export interface IPointsUpdate {
    type: "points";
}

function isNewQuestion(
    params: IQuestionDialogParams
): params is INewQuestionParams {
    return (params as INewQuestionParams).par_id_next !== undefined;
}

function isAskedQuestion(
    params: IQuestionDialogParams
): params is IAskedQuestion {
    return (params as IAskedQuestion).asked_id != null;
}

// noinspection TypeScriptUnresolvedVariable
@Component({
    selector: "tim-edit-question-dialog",
    template: `
        <tim-dialog-frame *ngIf="!isAskedQuestion()" [autoHeight]="false" [initialAutoHeight]="true">
            <ng-container header>
                {{ getTitle() }}
            </ng-container>
            <ng-container body>
                <form #f (keydown)="formKeyDown($event)">
                    <div class="form-group form-horz-flex" timErrorState>
                        <label for="question">Question text</label>
                        <textarea class="form-control" required name="question" id="question"
                                  [(ngModel)]="question.questionText"
                                  (ngModelChange)="changeQuestionTitle(question.questionText)"></textarea>
                        <tim-error-message></tim-error-message>
                    </div>
                    <div class="form-group form-horz-flex" timErrorState>
                        <label for="title">
                            Question title</label>
                        <input class="form-control"
                               required name="title" id="title" type="text" [(ngModel)]="question.questionTitle"
                               (focus)="titleFocused($event)"
                               (ngModelChange)="titleIsChanged()"/>
                        <tim-error-message></tim-error-message>
                    </div>

                    <div class="form-group form-horz-flex">
                        <label for="qtype">Question type</label>
                        <select class="form-control"
                                name="type"
                                id="qtype"
                                [(ngModel)]="question.questionType"
                                required
                                (ngModelChange)="createMatrix(question.questionType)">
                            <option value="" disabled selected> -- select question type --</option>
                            <option value="radio-vertical">Multiple choice (radio button)</option>
                            <option value="checkbox-vertical">Multiple choice (checkbox)</option>
                            <option value="true-false">True/False</option>
                            <option value="matrix">Many rows and columns</option>
                            <option value="likert">5 choices likert scale</option>
                            <option value="textarea">One text area</option>
                        </select>
                    </div>
                    <label for="defaultPoints">Default points for answers</label>
                    <input [(ngModel)]="defaultPoints"
                           id="defaultPoints"
                           name="defaultPoints"
                           class="form-control"
                           placeholder="Default point value for fields"
                           type="number"
                           step="0.1"/>
                    <label> Randomize rows <input type="checkbox"
                                                  name="randomization"
                                                  [(ngModel)]="randomization"/></label>
                    <div class="form-group" *ngIf="randomization">
                        Number of rows to select (in addition to locked rows)
                        <input [(ngModel)]="randomizedRows"
                               id="randomizedRows"
                               name="randomizedRows"
                               class="form-control"
                               type="number"
                               step="1"/>
                    </div>
                    <div class="form-group"
                         *ngIf="question.questionType == 'matrix' || question.questionType == 'likert'">
                        <label for="answerType">Answer type</label>
                        <select id="answerType"
                                required
                                name="answerType"
                                class="form-control"
                                [(ngModel)]="question.matrixType">
                            <option value="" disabled selected> -- select answer type --</option>
                            <option *ngFor="let item of answerFieldTypes" [value]="item.value">{{item.label}}</option>
                        </select>
                    </div>
                    <tim-question-matrix *ngIf="question.questionType" [qctrl]="this"></tim-question-matrix>
                    <div class="checkbox">
                        <label><input type="checkbox" [(ngModel)]="qst" name="documentQuestion"/> Lecture
                            question</label>
                    </div>
                    <div class="form-group form-horz-flex"
                         *ngIf="!qst">
                        <label for="answerLimit">Answer limit</label>
                        <input [(ngModel)]="pluginMarkup.answerLimit"
                               id="answerLimit"
                               name="answerLimit"
                               class="form-control"
                               placeholder="Maximum number of allowed answer attempts"
                               type="number"
                               min="1"
                               step="1"/>
                    </div>
                    <div class="form-horz-flex" *ngIf="qst">
                        <label>
                            Duration
                        </label>
                        <tim-duration-picker [(amount)]="ui.durationAmount"
                                             [(type)]="ui.durationType"></tim-duration-picker>
                    </div>
                </form>
                <bootstrap-panel title="Preview">
                    <tim-answer-sheet [questiondata]="previewParams"></tim-answer-sheet>
                </bootstrap-panel>
                <tim-alert *ngIf="customError">{{ customError }}</tim-alert>
            </ng-container>
            <ng-container footer>
                <button *ngIf="!isNew()"
                        class="btn btn-danger"
                        (click)="deleteQuestion()">Delete</button>
                <button class="timButton saveButton" (click)="createQuestion(false)" title="Ctrl-S">Save</button>
                <button class="timButton" (click)="dismiss()">Cancel</button>
            </ng-container>
        </tim-dialog-frame>
        <tim-dialog-frame *ngIf="isAskedQuestion()">
            <ng-container header>
                {{ getTitle() }}
            </ng-container>
            <ng-container body>
                <form>
                    <h5 [innerHtml]="question.questionText"></h5>
                    <div *ngIf="question.questionType">
                        <tim-question-matrix [qctrl]="this"></tim-question-matrix>
                    </div>
                    <tim-alert *ngIf="customError">{{ customError }}</tim-alert>
                </form>
            </ng-container>
            <ng-container footer>
                <button class="timButton" (click)="createQuestion(false)" title="Ctrl-S">Save</button>
                <button class="timButton" (click)="dismiss()">Cancel</button>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class QuestionEditDialogComponent extends AngularDialogComponent<
    IQuestionDialogParams,
    IQuestionDialogResult
> {
    protected dialogName = "editQuestion";
    answerFieldTypes: IAnswerField[];
    question: IAskedJsonJson;
    ui: IQuestionUI;
    rows: IExtendedRow[];
    columnHeaders: IHeader[];
    private titleChanged: boolean = false;
    private oldMarkupJson: string;
    private oldHeaders?: string[];
    previewParams: IPreviewParams;
    customError: string | undefined;
    @ViewChild("f") f?: NgForm;
    @ViewChild(QuestionMatrixComponent)
    questionMatrix!: QuestionMatrixComponent;
    qst = false;
    pluginMarkup: IGenericPluginMarkup = {};
    defaultPoints?: number;
    randomization = false;
    randomizedRows?: number;
    private timeLimit = new TimStorage("timelimit", t.number);

    constructor(private http: HttpClient) {
        super();

        this.question = {
            answerFieldType: "text",
            defaultPoints: 0,
            headers: [],
            questionText: "",
            questionTitle: "",
            questionType: "",
            rows: [],
            timeLimit: 30,
        };
        this.ui = {
            durationAmount: 30,
            durationType: "seconds",
        };

        this.rows = [];
        this.columnHeaders = [];
        this.setTime();
        this.answerFieldTypes = [
            {label: "Text area", value: "textArea"},
            {label: "Radio Button horizontal", value: "radiobutton-horizontal"},
            {label: "Checkbox", value: "checkbox"},
        ];
        this.oldMarkupJson = "";
        this.setEmptyMarkup();
        this.previewParams = makePreview(this.question, {
            enabled: false,
            showCorrectChoices: true,
            showExplanations: true,
        });
    }

    public getTitle() {
        return isNewQuestion(this.data) ? "Create a question" : "Edit question";
    }

    isNew() {
        return isNewQuestion(this.data);
    }

    ngOnInit() {
        if (isNewQuestion(this.data)) {
            this.newQuestion(this.data);
        } else {
            this.editQuestion(this.data);
        }
    }

    private putBackQuotations(x: string) {
        const ox = x.replace(/<br>/g, "\n");
        return ox.replace(/&quot;/g, '"');
    }

    private setTime() {
        this.ui = {durationType: "seconds", durationAmount: 30};
        const timeLimit = this.timeLimit.get() ?? 30;
        if (timeLimit > 0) {
            this.ui.durationAmount = timeLimit;
        }
    }

    private setEmptyMarkup(qst = false) {
        this.question = {
            answerFieldType: "text",
            expl: {},
            headers: [],
            questionText: "",
            questionTitle: "",
            questionType: "",
            rows: [],
        };
        this.qst = qst;
        this.pluginMarkup.answerLimit = qst ? 1 : undefined;
    }

    private newQuestion(data: INewQuestionParams) {
        this.setEmptyMarkup(data.qst);
        this.titleChanged = false;
        if (!this.qst) {
            this.ui.durationAmount = undefined; // default no time
        }
    }

    isAskedQuestion() {
        return isAskedQuestion(this.data);
    }

    private editQuestion(data: IEditQuestionParams) {
        const json = isAskedQuestion(data) ? data.json.json : data.markup;
        if (!isAskedQuestion(data)) {
            this.qst = data.qst;
            this.pluginMarkup = data.markup;
        }

        if (json.randomizedRows) {
            this.randomization = true;
            this.randomizedRows = json.randomizedRows;
        }

        if (json.defaultPoints) {
            this.question.defaultPoints = json.defaultPoints;
            this.defaultPoints = json.defaultPoints;
        }
        if (json.questionTitle) {
            this.question.questionTitle = this.putBackQuotations(
                json.questionTitle
            );
        }
        if (this.question.questionTitle === "Untitled") {
            this.question.questionTitle = "";
            this.titleChanged = true;
        }
        if (json.questionText) {
            this.question.questionText = this.putBackQuotations(
                json.questionText
            );
        }
        if (json.questionType) {
            this.question.questionType = json.questionType;
        }
        if (json.matrixType) {
            this.question.matrixType = json.matrixType;
        }
        if (json.answerFieldType) {
            this.question.answerFieldType = json.answerFieldType;
        }

        const r = fixQuestionJson(json);
        const jsonHeaders = r.headers;
        const jsonRows = r.rows;

        const columnHeaders: IHeader[] = [];
        if (jsonHeaders) {
            for (let i = 0; i < jsonHeaders.length; i++) {
                columnHeaders.push({
                    id: i,
                    type: jsonHeaders[i].type,
                    text: this.putBackQuotations(jsonHeaders[i].text),
                });
            }
        }
        this.columnHeaders = columnHeaders;
        const pointsTable = getPointsTable(
            isAskedQuestion(data) ? data.json.json.points : data.markup.points
        );
        const expl: IExplCollection | undefined | null = isAskedQuestion(data)
            ? data.json.json.expl
            : data.markup.expl;

        let locks: number[] = [];
        if (!isAskedQuestion(data) && data.markup.doNotMove) {
            if (typeof data.markup.doNotMove === "number") {
                locks.push(data.markup.doNotMove);
            } else {
                locks = data.markup.doNotMove;
            }
        }

        const rows: IExtendedRow[] = [];
        for (let i = 0; i < jsonRows.length; i++) {
            const row = jsonRows[i];
            rows[i] = {
                id: row.id,
                text: this.putBackQuotations(row.text),
                type: row.type,
                columns: [],
                expl: "",
                value: "",
                locked: locks.includes(row.id),
            };

            const idString = "" + (i + 1); // rows[i].id.toString();
            if (expl && idString in expl) {
                rows[i].expl = expl[idString];
            }

            const jsonColumns = jsonRows[i].columns;

            const columns: IExtendedColumn[] = [];
            for (let j = 0; j < jsonColumns.length; j++) {
                let columnPoints = "";

                if (
                    this.question.questionType === "matrix" ||
                    this.question.questionType === "true-false"
                ) {
                    if (this.question.matrixType !== "textArea") {
                        if (pointsTable.length > i) {
                            const index = (j + 1).toString();
                            if (index in pointsTable[i]) {
                                columnPoints = pointsTable[i][index];
                            }
                        }
                    }
                } else {
                    if (pointsTable.length > 0) {
                        if (idString in pointsTable[0]) {
                            columnPoints = pointsTable[0][idString];
                        }
                    }
                }
                columns[j] = {
                    id: j,
                    points: columnPoints,
                };
            }
            rows[i].columns = columns;
        }
        this.rows = rows;

        // TODO: timeLimit should only be set for lecture questions, but it is probably beneficial to keep it anyway --
        //       it doesn't affect document tasks, and having it in the markup limits the amount of manual work when
        //       converting lecture questions to doc tasks and vice versa.
        if (json.timeLimit && json.timeLimit > 0) {
            this.ui.durationType = "seconds";
            this.ui.durationAmount = json.timeLimit;
        } else {
            this.ui.durationAmount = undefined;
        }
    }

    private moveToElement(event: Event, dir: number) {
        event.preventDefault();
        const activeObj = document.activeElement;
        if (!activeObj) {
            return 0;
        }
        const id = activeObj.id;
        if (!id || !id.startsWith("r")) {
            return 0;
        }
        const edits = this.questionMatrix.textareas.map((x) => x.nativeElement);
        const ind = parseInt(id.substr(1), 10) + dir - 1;
        if (ind < 0) {
            return 0;
        }
        if (ind >= edits.length) {
            this.addRow(-1);
            edits[ind - 2].focus();
            edits[ind - 1].focus();
            const idStr = (ind + 1).toString();
            edits.find((e) => e.id === idStr)!.focus();
            return 0;
        }
        edits[ind].focus();
        return 0;
    }

    formKeyDown(event: KeyboardEvent) {
        if (event.ctrlKey || event.metaKey) {
            switch (event.keyCode) {
                case KEY_LEFT:
                    return;
                case KEY_UP:
                    return this.moveToElement(event, -1);
                case KEY_RIGHT:
                    return;
                case KEY_ENTER:
                case KEY_DOWN:
                    return this.moveToElement(event, +1);
                case KEY_S:
                    event.preventDefault();
                    this.createQuestion(false);
                    break;
                case KEY_R:
                    event.preventDefault();
                    this.createQuestion(true);
                    break;
            }
        }
    }

    /**
     * A function for creating a matrix.
     * @param type The answer type of the matrix.
     */
    createMatrix(type: QuestionType) {
        if (!this.oldHeaders) {
            this.oldHeaders = [];
        }

        for (let i = 0; i < this.columnHeaders.length; i++) {
            if (this.columnHeaders[i].text) {
                this.oldHeaders[i] = this.columnHeaders[i].text;
            }
        }

        const oldRows = 1;
        const oldCols = 1;

        const constHeaders: Record<string, string[] | undefined> = {
            "true-false": ["True", "False"],
            likert: ["1", "2", "3", "4", "5"],
        };
        let rowsCount = 0;
        let columnsCount = 0;
        if (type === "matrix" || type === "true-false") {
            rowsCount = Math.max(2, oldRows);
            columnsCount = Math.max(2, oldCols);
        } else if (type === "textarea") {
            rowsCount = Math.max(1, oldRows);
            columnsCount = Math.max(1, oldCols);
        } else if (type === "likert") {
            rowsCount = Math.max(2, oldRows);
            columnsCount = Math.max(5, oldCols);
        } else {
            rowsCount = Math.max(4, oldRows);
            columnsCount = 1;
        }

        if (this.rows.length < 1) {
            for (let i = 0; i < rowsCount; i++) {
                this.addRow(i);
            }
        }

        if (
            type === "radio-vertical" ||
            type === "true-false" ||
            type === "likert"
        ) {
            this.question.answerFieldType = "radio";
        } else if (type === "checkbox-vertical") {
            this.question.answerFieldType = "checkbox";
        } else if (type === "matrix") {
            this.question.answerFieldType = "matrix";
        } else if (type === "textarea") {
            this.question.answerFieldType = "text";
        }

        for (const r of this.rows) {
            if (r.columns.length > columnsCount) {
                r.columns.splice(columnsCount, r.columns.length);
            }
            while (r.columns.length < columnsCount) {
                this.addCol(this.rows[0].columns.length);
            }
        }

        const ty = type;
        if (type === "textarea" || type === "likert") {
            type = "matrix";
        }

        this.columnHeaders = [];
        if (type === "matrix" || type === "true-false") {
            for (let i = 0; i < this.rows[0].columns.length; i++) {
                let text = "";
                const ch = constHeaders[ty];
                if (ch && i < ch.length) {
                    text = ch[i];
                }
                if (i < this.oldHeaders.length && this.oldHeaders[i]) {
                    text = this.oldHeaders[i];
                }
                this.columnHeaders.push({
                    id: i,
                    text,
                    type: "header",
                });
            }
        }

        if (ty === "likert") {
            this.question.matrixType = "radiobutton-horizontal";
        }

        if (ty === "textarea") {
            this.question.matrixType = "textArea";
        }

        this.question.questionType = type;
    }

    /**
     * A function to add a column to an existing matrix.
     * @param loc The index in the matrix where to add the new column.
     */
    addCol(loc: number) {
        let location = loc;
        if (loc === -1) {
            location = this.rows[0].columns.length;
            loc = this.rows[0].columns.length;
        }
        this.columnHeaders.splice(loc, 0, {type: "header", id: loc, text: ""});
        // add new column to columns
        for (const item of this.rows) {
            item.columns.splice(loc, 0, {
                id: location,
                points: "",
            });
        }
    }

    private createColumnsForRow(location: number): IExtendedColumn[] {
        const columns: IExtendedColumn[] = [];
        if (this.rows.length > 0) {
            for (let j = 0; j < this.rows[0].columns.length; j++) {
                columns[j] = {
                    id: j,
                    points: "",
                };
            }
        }
        return columns;
    }

    /**
     * The function adds a row to an existing matrix
     * @param loc The index in the matrix where to add the new row.
     */
    addRow(loc: number) {
        let location = loc;
        if (loc === -1) {
            location = this.rows.length;
            loc = this.rows.length;
        }

        const columns = this.createColumnsForRow(location);
        this.rows.splice(loc, 0, {
            id: location,
            text: "",
            type: "question",
            value: "",
            columns,
            expl: "",
        });

        for (let i = 0; i < this.rows.length; i++) {
            this.rows[i].id = i + 1;
        }
    }

    /**
     * A function to delete a row from a matrix.
     * @param indexToBeDeleted The index of the row to be deleted.
     */
    delRow(indexToBeDeleted: number) {
        if (this.rows.length > 1) {
            if (indexToBeDeleted === -1) {
                this.rows.splice(-1, 1);
            } else {
                this.rows.splice(indexToBeDeleted, 1);
            }
        }

        for (let i = 0; i < this.rows.length; i++) {
            this.rows[i].id = i + 1;
        }
    }

    /**
     * A function to delete a column from a matrix.
     * @param indexToBeDeleted Index of the column to be deleted.
     */
    delCol(indexToBeDeleted: number) {
        for (const r of this.rows) {
            if (indexToBeDeleted === -1) {
                r.columns.splice(-1, 1);
            } else {
                r.columns.splice(indexToBeDeleted, 1);
            }
        }
        if (indexToBeDeleted === -1) {
            this.columnHeaders.splice(-1, 1);
        } else {
            this.columnHeaders.splice(indexToBeDeleted, 1);
        }
    }

    /**
     * A function to reset the question values.
     */
    private clearQuestion() {
        this.question = {
            answerFieldType: "text",
            headers: [],
            questionText: "",
            questionTitle: "",
            questionType: "",
            rows: [],
            timeLimit: 30,
        };
        this.setTime();

        this.rows = [];
        this.columnHeaders = [];
    }

    /**
     * The function replaces linebreaks with HTML code.
     * @param val The input string
     * @returns {*} The reformatted line.
     */
    private replaceLinebreaksWithHTML(val: string) {
        const output = val.replace(/(?:\r\n|\r|\n)/g, "<br>");
        // output = output.replace(/"/g, '&quot;');
        // return output.replace(/\\/g, "\\\\");
        // var output = val.replace(/(?:\r\n)/g, '\n');
        return output;
    }

    /**
     * Function for checking if the row headings are empty.
     * @param rows The array of rows to be checked.
     * @returns {boolean} Whether or not the row headings are empty.
     */
    private rowHeadingsEmpty(rows: IRow[]) {
        if (rows.length < 2) {
            return false;
        }
        for (const r of rows) {
            if (r.text === "" || r.text == null) {
                return true;
            }
        }
        return false;
    }

    /**
     * Formats a string of form {id}:{point value} based on column point value or defaultPoints attribute
     */
    private createPointVal(id: number, colVal: string): string {
        return (
            id.toString() +
            ":" +
            (numOrStringToNumber(colVal) ?? this.question.defaultPoints ?? 0)
        );
    }

    /**
     * Creates a string of points. Rows are separated by | and answers in the same row separated by ;
     * @returns {string}
     */
    private createPoints() {
        let points = "";
        let separator = "";
        let separator2 = "";
        let n = 0;
        if (
            this.question.questionType === "matrix" ||
            this.question.questionType === "true-false"
        ) {
            if (this.question.matrixType !== "textArea") {
                for (const r of this.rows) {
                    points += separator;
                    separator2 = "";
                    for (const currentColumn of r.columns) {
                        if (currentColumn.points !== "") {
                            points += separator2;
                            const id = currentColumn.id + 1;
                            points += this.createPointVal(
                                id,
                                currentColumn.points
                            );
                            separator2 = ";";
                            n++;
                        }
                    }
                    separator = "|";
                }
            }
        } else {
            for (const r of this.rows) {
                points += separator;
                const currentColumn = r.columns[0];
                if (currentColumn.points !== "") {
                    points += separator2;
                    const id = r.id;
                    points += this.createPointVal(id, currentColumn.points);
                    separator2 = ";";
                    n++;
                }
            }
        }
        if (n) {
            return points;
        }
        return null;
    }

    /**
     * Creates a dict with explanations for question rows
     * @returns {{}}
     */
    private createExplanation(): IExplCollection {
        const expl: IExplCollection = {};
        for (const row of this.rows) {
            if (row.expl?.trim()) {
                expl[row.id] = row.expl.trim();
            }
        }
        return expl;
    }

    /**
     * Creates question json
     * @returns {{questionText: string, title: string, questionType: *, answerFieldType: string, matrixType: string,
     * timeLimit: string, data: {headers: Array, rows: Array}}}
     */
    private createJson(): IAskedJsonJson | undefined {
        if (this.question.questionTitle == "") {
            this.question.questionTitle = "Untitled";
        }
        this.customError = undefined;
        if (this.rows.length > 0) {
            if (
                this.question.questionType ===
                    "radio-vertical" /* || this.question.type === "checkbox-vertical"*/ &&
                this.rows.length < 2
            ) {
                this.customError = "You must have at least two choices.";
            }
        } else if (this.question.questionType !== "") {
            this.customError = "You must have at least one row.";
        }
        let timeLimit: number | undefined;
        if (this.ui.durationAmount != undefined) {
            timeLimit = moment
                .duration(this.ui.durationAmount, this.ui.durationType)
                .asSeconds();
            if (timeLimit <= 0) {
                this.customError =
                    "Please enter a duration greater than zero, or for unending question, leave the duration box empty.";
            }
        } else {
            timeLimit = undefined;
        }

        if (this.customError != null || this.f?.invalid) {
            if (this.question.questionTitle === "Untitled") {
                this.question.questionTitle = "";
            }
            return;
        }
        if (this.question.questionType === "matrix") {
            if (this.question.matrixType === "radiobutton-horizontal") {
                this.question.answerFieldType = "radio";
            }

            if (this.question.matrixType === "textArea") {
                this.question.answerFieldType = "text";
            }
            if (this.question.matrixType === "checkbox") {
                this.question.answerFieldType = "checkbox";
            }
        }

        if (
            this.question.questionText == null ||
            this.question.questionTitle == null ||
            this.question.answerFieldType == null
        ) {
            return;
        }

        this.question.questionText = this.replaceLinebreaksWithHTML(
            this.question.questionText
        );
        this.question.questionTitle = this.replaceLinebreaksWithHTML(
            this.question.questionTitle
        );

        const headersJson: IHeader[] = [];
        if (
            this.question.questionType === "matrix" ||
            this.question.questionType === "true-false" ||
            this.question.questionType === ""
        ) {
            for (const h of this.columnHeaders) {
                const header = {
                    type: h.type,
                    id: h.id,
                    text: this.replaceLinebreaksWithHTML(h.text) || "",
                };
                headersJson.push(header);
            }
        }

        const rowsJson: IRow[] = [];
        for (const r of this.rows) {
            const row: IRow = {
                id: r.id,
                type: r.type,
                text: this.replaceLinebreaksWithHTML(r.text),
                columns: [],
            };
            const columnsJson: IColumn[] = [];
            for (const c of r.columns) {
                const column: IColumn = {
                    id: c.id,
                };
                columnsJson.push(column);
            }
            row.columns = columnsJson;
            rowsJson.push(row);
        }

        const minimized = minimizeJson({
            headers: headersJson,
            rows: rowsJson,
        });

        const questionjson = {
            answerFieldType: this.question.answerFieldType,
            headers: minimized.headers,
            matrixType: this.question.matrixType,
            questionText: this.question.questionText,
            questionTitle: this.question.questionTitle,
            questionType: this.question.questionType,
            rows: minimized.rows,
            timeLimit,
        };

        this.question = questionjson;
        const points = this.createPoints();
        if (points) {
            this.question.points = points;
        } else {
            delete this.question.points;
        }
        this.question.expl = this.createExplanation();
        return questionjson;
    }

    private async ngDoCheck() {
        this.createJson();
        const currJson = JSON.stringify(this.question);
        if (currJson !== this.oldMarkupJson) {
            this.oldMarkupJson = currJson;
            await this.updatePreview();
        }
    }

    private async updatePreview() {
        const mdStr = JSON.stringify(this.question, null, 4);
        const response = await toPromise(
            this.http.post<{md: IAskedJsonJson}>("/qst/getQuestionMD/", {
                text: mdStr,
            })
        );
        if (!response.ok) {
            throw Error("getQuestionMD failed");
        }
        this.previewParams = makePreview(response.result.md, {
            enabled: false,
            showCorrectChoices: true,
            showExplanations: true,
        });
    }

    private getLockedRowIds(): number[] {
        return this.rows.reduce((arr: number[], row) => {
            if (row.locked) {
                arr.push(row.id);
            }
            return arr;
        }, []);
    }

    /**
     * Validates and saves the question into the database.
     */
    async createQuestion(ask: boolean) {
        const questionjson = this.createJson();
        const p = this.data;
        if (!questionjson) {
            return;
        }

        this.timeLimit.set(questionjson.timeLimit ?? 30);

        if (isAskedQuestion(p)) {
            await this.updatePoints(p);
            this.close({type: "points"});
            return;
        }

        this.question.defaultPoints = this.defaultPoints;
        if (!this.randomization) {
            this.question.randomizedRows = undefined;
        } else {
            this.question.randomizedRows =
                this.randomizedRows ?? this.rows.length; // TODO: better alternative for "all"
            this.question.doNotMove = this.getLockedRowIds();
        }

        let route = "/postParagraphQ/";
        let params;
        if (isNewQuestion(p)) {
            route = "/newParagraphQ/";
            params = {par_next: p.par_id_next, isTask: this.qst}; // TODO: should be possible to input taskId in the dialog
        } else {
            params = {par: p.parId, taskId: p.taskId, isTask: this.qst};
        }
        const docId = p.docId;

        // Change undefined to null. This avoids "null" being saved in the markup for answerLimit.
        this.pluginMarkup.answerLimit =
            this.pluginMarkup.answerLimit ?? undefined;

        const r = await toPromise(
            this.http.post<IParResponse>(route, {
                docId,
                ...params,
                question: {
                    ...this.pluginMarkup, // this also retains the other attributes that are not visible in edit dialog
                    ...this.question,
                },
            })
        );
        if (r.ok) {
            const data = r.result;
            this.close({data, deleted: false, ask, type: "edit"});
        } else {
            await showMessageDialog(r.result.error.error);
        }
    }

    /**
     * Calls /updatePoints/ to update questions points according to form
     */
    private async updatePoints(a: IAskedQuestion) {
        const points = this.createPoints();
        const expl = this.createExplanation();
        await to(
            $http({
                method: "POST",
                url: "/updatePoints/",
                params: {
                    asked_id: a.asked_id,
                    expl,
                    points,
                },
            })
        );
    }

    async deleteQuestion() {
        if (isNewQuestion(this.data)) {
            await showMessageDialog(
                "Not editing a question - there is nothing to delete."
            );
            return;
        }
        if (isAskedQuestion(this.data)) {
            await showMessageDialog(
                "Editing an asked question - cannot delete now."
            );
            return;
        }
        const docId = this.data.docId;
        const parId = this.data.parId;
        const data = await deleteQuestionWithConfirm(docId, parId);
        if (data != null) {
            this.close({data, deleted: true, ask: false, type: "edit"});
        }
    }

    explFocus($event: Event) {
        if ($event.target) {
            $($event.target).parent().addClass("explFocus");
        }
    }

    explBlur($event: Event) {
        if ($event.target) {
            $($event.target).parent().removeClass("explFocus");
        }
    }

    /**
     * Changes the question title field to match the question if user hasn't defined title
     * @param question of question
     */
    changeQuestionTitle(question: string) {
        if (!this.titleChanged) {
            this.question.questionTitle = question;
        }
    }

    titleIsChanged() {
        this.titleChanged = true;
    }

    titleFocused(event: FocusEvent) {
        (event.target as HTMLInputElement).select();
    }
}

@NgModule({
    declarations: [QuestionEditDialogComponent, QuestionMatrixComponent],
    imports: [
        CommonModule,
        TimUtilityModule,
        DialogModule,
        AnswerSheetModule,
        FormsModule,
    ],
})
export class QuestionEditDialogModule {}
