import angular, {IFormController, IRootElementService} from "angular";
import $ from "jquery";
import {
    fixQuestionJson,
    getPointsTable,
    IPreviewParams,
    makePreview,
    minimizeJson,
} from "tim/directives/dynamicAnswerSheet";
import {markAsUsed, setsetting} from "tim/utils";
import {QuestionMatrixController} from "../components/questionMatrix";
import {DialogController, registerDialogComponent, showDialog, showMessageDialog} from "../dialog";
import {IParResponse} from "../edittypes";
import {
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
} from "../lecturetypes";
import {$http, $timeout, $window} from "../ngimport";
import {ParCompiler} from "../services/parCompiler";

markAsUsed(QuestionMatrixController);

/**
 * Controller for creating and editing questions
 * @module questionController
 * @author Matias Berg
 * @author Bek Eljurkaev
 * @author Minna Lehtomäki
 * @author Juhani Sihvonen
 * @author Hannu Viinikainen
 * @licence MIT
 * @copyright 2015 Timppa project authors
 */

function cleanParId(id: string) {
    const i = id.lastIndexOf(".");
    if (i < 0) {
        return id;
    }
    return id.substring(i + 1);
}

interface IAnswerField {
    label: string;
    value: MatrixType;
}

export interface IExtendedColumn extends IColumn {
    id: number;
    rowId: number;
    points: string;
}

interface IExtendedRow extends IRow {
    expl: string;
    id: number;
    text: string;
    type: string;
    value: string;
    columns: IExtendedColumn[];
}

export type IQuestionDialogParams = INewQuestionParams | IEditQuestionParams;

export interface INewQuestionParams {
    qst: boolean;
    par_id_next: string;
    docId: number;
}

export type IEditQuestionParams = IQuestionParagraph | IAskedQuestion;

export interface IQuestionDialogResult {
    data: IParResponse;
    ask: boolean;
    deleted: boolean;
}

function isNewQuestion(params: IQuestionDialogParams): params is INewQuestionParams {
    return (params as INewQuestionParams).par_id_next != null;
}

function isAskedQuestion(params: IQuestionDialogParams): params is IAskedQuestion {
    return (params as IAskedQuestion).asked_id != null;
}

export async function fetchQuestion(docId: number, parId: string, edit: boolean = true): Promise<IQuestionParagraph> {
    const response = await $http<IQuestionParagraph>({
        url: "/getQuestionByParId",
        method: "GET",
        params: {par_id: parId, doc_id: docId, edit},
    });
    return response.data;
}

export async function fetchAskedQuestion(askedId: number): Promise<IAskedQuestion> {
    const response = await $http<IAskedQuestion>({
        url: "/getAskedQuestionById",
        method: "GET",
        params: {asked_id: askedId},
    });
    return response.data;
}

export async function fetchAndEditQuestion(docId: number, parId: string, edit: boolean = true) {
    return await showQuestionEditDialog(await fetchQuestion(docId, parId, edit));
}

export async function deleteQuestionWithConfirm(docId: number, parId: string): Promise<IParResponse | null> {
    const confirmDi = $window.confirm("Are you sure you want to delete this question?");
    if (confirmDi) {
        const response = await $http.post<IParResponse>(`/deleteParagraph/${docId}`, {par: parId});
        return response.data;
    }
    return null;
}

export class QuestionController extends DialogController<{params: IQuestionDialogParams}, IQuestionDialogResult, "timEditQuestion"> {
    private static $inject = ["$element"];
    private answerFieldTypes: IAnswerField[];
    private dateTimeOptions: EonasdanBootstrapDatetimepicker.SetOptions;
    private settings: {timelimit: number | null};
    private question: IAskedJsonJson;
    private ui: IQuestionUI;
    private rows: IExtendedRow[];
    private columns: Array<{}>;
    private columnHeaders: IHeader[];
    private titleChanged: boolean;
    private oldMarkupJson: string;
    private element: IRootElementService;
    private oldHeaders: string[];
    private answer: string;
    private previewParams: IPreviewParams;
    private customError: string | null;
    private f?: IFormController;
    private qst = false;

    constructor(element: IRootElementService) {
        super();
        this.element = element;

        this.dateTimeOptions = {
            format: "D.M.YYYY HH:mm:ss",
            showTodayButton: true,
        };

        this.settings = $window.sessionsettings;

        this.question = {
            answerFieldType: "text",
            headers: [],
            matrixType: "",
            questionText: "",
            questionTitle: "",
            questionType: "",
            rows: [],
            timeLimit: 30,
        };
        this.ui = {
            endTimeSelected: true,
            timeLimitFields: {hours: 0, minutes: 0, seconds: 30},
        };

        this.rows = [];
        this.columns = [];
        this.columnHeaders = [];
        this.setTime();
        this.answerFieldTypes = [
            {label: "Text area", value: "textArea"},
            {label: "Radio Button horizontal", value: "radiobutton-horizontal"},
            {label: "Checkbox", value: "checkbox"},
        ];
        this.oldMarkupJson = "";
        this.setEmptyMarkup();
        this.previewParams = makePreview(this.question);
    }

    private $onInit() {
        if (isNewQuestion(this.resolve.params)) {
            this.newQuestion(this.resolve.params);
        } else {
            this.editQuestion(this.resolve.params);
        }
    }

    private async $postLink() {
        await $timeout();
        this.addKeyListeners();
        // ParCompiler.processAllMath($element.parent());
        ParCompiler.processAllMathDelayed(this.element.parent(), 1000);

        /*
         this.questionForm.addEventListener( "keydown", function(event) {
         // $("#question-form").keypress(function(event) {
         var c = String.fromCharCode(event.keyCode);
         if ( (event.which == 115 && event.ctrlKey) || (event.which == 19) ) { // ctrl-s
         event.preventDefault();
         }
         });
         */
    }

    private putBackQuotations(x: string) {
        const ox = x.replace(/<br>/g, "\n");
        return ox.replace(/&quot;/g, '"');
    }

    private setTime() {
        this.ui.timeLimitFields = {hours: 0, minutes: 0, seconds: 30};
        if (this.settings && this.settings.timelimit && this.settings.timelimit > 0) {
            let time = this.settings.timelimit;
            if (time > 3600) {
                this.ui.timeLimitFields.hours = Math.floor(time / 3600);
            } else {
                this.ui.timeLimitFields.hours = 0;
            }
            if (time > 60) {
                this.ui.timeLimitFields.minutes = Math.floor(time / 60);
                time = time % 60;
            } else {
                this.ui.timeLimitFields.minutes = 0;
            }
            if (time > 0) {
                this.ui.timeLimitFields.seconds = time;
            } else {
                this.ui.timeLimitFields.seconds = 0;
            }
        }
    }

    private setEmptyMarkup(qst = false) {
        this.question = {
            answerFieldType: "text",
            expl: {},
            headers: [],
            matrixType: "",
            questionText: "",
            questionTitle: "",
            questionType: "",
            rows: [],
            timeLimit: 0,
        };
        this.qst = qst;
    }

    private newQuestion(data: INewQuestionParams) {
        this.setEmptyMarkup(data.qst);
        this.titleChanged = false;
        if (this.qst) {
            this.ui.endTimeSelected = false; // default no time
        }
    }

    private isAskedQuestion() {
        return isAskedQuestion(this.resolve.params);
    }

    private editQuestion(data: IEditQuestionParams) {
        const json = isAskedQuestion(data) ? data.json.json : data.markup;

        this.titleChanged = false;

        if (json.questionTitle) {
            this.question.questionTitle = this.putBackQuotations(json.questionTitle);
        }
        if (this.question.questionTitle === "Untitled") {
            this.question.questionTitle = "";
            this.titleChanged = true;
        }
        if (json.questionText) {
            this.question.questionText = this.putBackQuotations(json.questionText);
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
        const pointsTable = getPointsTable(isAskedQuestion(data) ? data.json.json.points : data.markup.points);
        const expl: IExplCollection | undefined = isAskedQuestion(data) ? data.json.json.expl : data.markup.expl;

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
            };

            const idString = "" + (i + 1); // rows[i].id.toString();
            if (expl && idString in expl) {
                rows[i].expl = expl[idString];
            }

            const jsonColumns = jsonRows[i].columns;

            const columns: IExtendedColumn[] = [];
            for (let j = 0; j < jsonColumns.length; j++) {
                let columnPoints = "";

                if (this.question.questionType === "matrix" || this.question.questionType === "true-false") {
                    if (this.question.matrixType !== "textArea") {
                        if (pointsTable.length > i) {
                            if ((j + 1).toString() in pointsTable[i]) {
                                columnPoints = pointsTable[i][(j + 1).toString()];
                            }
                        }
                    }
                } else {
                    if (pointsTable.length > 0) {
                        if ((i + 1).toString() in pointsTable[0]) {
                            columnPoints = pointsTable[0][(i + 1).toString()];
                        }
                    }
                }
                columns[j] = {
                    id: j,
                    rowId: i,
                    text: jsonColumns[j].text,
                    points: columnPoints,
                    type: jsonColumns[j].type,
                    answerFieldType: jsonColumns[j].answerFieldType,
                };
            }
            rows[i].columns = columns;
        }
        this.rows = rows;

        if (json.timeLimit && json.timeLimit > 0) {
            this.ui.endTimeSelected = true;
        } else {
            this.ui.endTimeSelected = false;
        }
    }

    private moveToElement(event: Event, dir: number) {
        event.preventDefault();
        const activeObj = document.activeElement;
        const id = activeObj.id;
        if (!id || id[0] !== "r") {
            return 0;
        }
        const edits = this.element.find(".questiontext");
        const ind = parseInt(id.substr(1)) + dir - 1;
        if (ind < 0) {
            return 0;
        }
        if (ind >= edits.length) {
            this.addRow(-1);
            edits[ind - 2].focus();
            edits[ind - 1].focus();
            this.element.find("#r" + (ind + 1)).focus();
            return 0;
        }
        edits[ind].focus();
        return 0;
    }

    private addKeyListeners() {
        const questionForm = this.element.find("form")[0];

        questionForm.addEventListener("keydown", (event: KeyboardEvent) => {
            if (event.ctrlKey || event.metaKey) {
                switch (event.keyCode) {
                    case 37: // left
                        return;
                    case 38: // up
                        return this.moveToElement(event, -1);
                    case 39: // right
                        return;
                    case 13: // down
                    case 40: // down
                        return this.moveToElement(event, +1);
                }

                switch (String.fromCharCode(event.which).toLowerCase()) {
                    case "s":
                        event.preventDefault();
                        this.createQuestion(false);
                        break;
                    case "r":
                        event.preventDefault();
                        this.createQuestion(true);
                        break;
                    case "g":
                }
            }
        });
    }

    /**
     * A function for creating a matrix.
     * @param type The answer type of the matrix.
     */
    private createMatrix(type: QuestionType) {

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

        const constHeaders: {[key: string]: string[] | undefined} = {
            "true-false": ["True", "False"],
            "likert": ["1", "2", "3", "4", "5"],
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

        if (type === "radio-vertical" || type === "true-false" || type === "likert") {
            this.question.answerFieldType = "radio";
        } else if (type === "checkbox-vertical") {
            this.question.answerFieldType = "checkbox";
        } else if (type === "matrix") {
            this.question.answerFieldType = "matrix";
        } else if (type === "textarea") {
            this.question.answerFieldType = "text";
        }

        for (let i = 0; i < this.rows.length; i++) {
            if (this.rows[i].columns.length > columnsCount) {
                this.rows[i].columns.splice(columnsCount, this.rows[i].columns.length);
            }
            while (this.rows[i].columns.length < columnsCount) {
                this.addCol(this.rows[0].columns.length);
            }
        }

        const t = type;
        if (type === "textarea" || type === "likert") {
            type = "matrix";
        }

        this.columnHeaders = [];
        if (type === "matrix" || type === "true-false") {
            for (let i = 0; i < this.rows[0].columns.length; i++) {
                let text = "";
                const ch = constHeaders[t];
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

        if (t === "likert") {
            this.question.matrixType = "radiobutton-horizontal";
        }

        if (t === "textarea") {
            this.question.matrixType = "textArea";
        }

        this.question.questionType = type;
    }

    /**
     * A function to add a column to an existing matrix.
     * @param loc The index in the matrix where to add the new column.
     */
    private addCol(loc: number) {
        let location = loc;
        if (loc === -1) {
            location = this.rows[0].columns.length;
            loc = this.rows[0].columns.length;
        }
        this.columnHeaders.splice(loc, 0, {type: "header", id: loc, text: ""});
        // add new column to columns
        for (let i = 0; i < this.rows.length; i++) {
            this.rows[i].columns.splice(loc, 0, {
                id: location,
                rowId: i,
                text: "",
                points: "",
                type: "answer",
                answerFieldType: this.question.answerFieldType,
            });
        }
    }

    private createColumnsForRow(location: number): IExtendedColumn[] {
        const columns: IExtendedColumn[] = [];
        if (this.rows.length > 0) {
            for (let j = 0; j < this.rows[0].columns.length; j++) {
                columns[j] = {
                    answerFieldType: this.question.answerFieldType,
                    id: j,
                    points: "",
                    rowId: location,
                    text: "",
                    type: "answer",
                };
            }
        }
        return columns;
    }

    /**
     * The function adds a row to an existing matrix
     * @param loc The index in the matrix where to add the new row.
     */
    private addRow(loc: number) {
        let location = loc;
        if (loc === -1) {
            location = this.rows.length;
            loc = this.rows.length;
        }

        const columns = this.createColumnsForRow(location);
        this.rows.splice(loc, 0,
            {
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
    private delRow(indexToBeDeleted: number) {
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
    private delCol(indexToBeDeleted: number) {
        for (let i = 0; i < this.rows.length; i++) {
            if (indexToBeDeleted === -1) {
                this.rows[i].columns.splice(-1, 1);
            } else {
                this.rows[i].columns.splice(indexToBeDeleted, 1);
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
            matrixType: "",
            questionText: "",
            questionTitle: "",
            questionType: "",
            rows: [],
            timeLimit: 30,
        };
        this.setTime();

        this.rows = [];
        this.answer = "";
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
        for (let i = 0; i < rows.length; i++) {
            if (rows[i].text === "" || rows[i].text == null) {
                return true;
            }
        }
        return false;
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
        if (this.question.questionType === "matrix" || this.question.questionType === "true-false") {
            if (this.question.matrixType !== "textArea") {
                for (let i = 0; i < this.rows.length; i++) {
                    points += separator;
                    separator2 = "";
                    for (let j = 0; j < this.rows[i].columns.length; j++) {
                        const currentColumn = this.rows[i].columns[j];
                        if (currentColumn.points !== "" && currentColumn.points != "0") {
                            points += separator2;
                            const id = currentColumn.id + 1;
                            points += id.toString() + ":" + parseFloat(currentColumn.points) || 0;
                            separator2 = ";";
                            n++;
                        }
                    }
                    separator = "|";
                }
            }
        } else {
            for (let i = 0; i < this.rows.length; i++) {
                points += separator;
                const currentColumn = this.rows[i].columns[0];
                if (currentColumn.points !== "" && currentColumn.points != "0") {
                    points += separator2;
                    const id = this.rows[i].id;
                    points += id.toString() + ":" + parseFloat(currentColumn.points) || 0;
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
        let n = 0;
        for (let i = 0; i < this.rows.length; i++) {
            const row = this.rows[i];
            if (row.expl && row.expl.trim()) {
                expl[row.id] = row.expl.trim();
                n++;
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
        this.customError = null;
        if (this.rows.length > 0) {
            if ((this.question.questionType === "radio-vertical" /*|| this.question.type === "checkbox-vertical"*/) && this.rows.length < 2) {
                this.customError = "You must have at least two choices.";
            }
        } else if (this.question.questionType !== "") {
            this.customError = "You must have at least one row.";
        }
        let timeLimit: number | undefined;
        if (this.ui.endTimeSelected) {
            if (!this.ui.timeLimitFields.hours) {
                this.ui.timeLimitFields.hours = 0;
            }
            if (!this.ui.timeLimitFields.minutes) {
                this.ui.timeLimitFields.minutes = 0;
            }
            if (!this.ui.timeLimitFields.seconds) {
                this.ui.timeLimitFields.seconds = 0;
            }
            timeLimit = this.ui.timeLimitFields.seconds;
            if (this.ui.timeLimitFields.hours) {
                timeLimit = timeLimit + (this.ui.timeLimitFields.hours * 60 * 60);
            }
            if (this.ui.timeLimitFields.minutes) {
                timeLimit = timeLimit + (this.ui.timeLimitFields.minutes * 60);
            }
            if (timeLimit <= 0) {
                this.customError = "Please enter a duration greater than zero or for unending question uncheck the duration box.";
            }
        } else {
            timeLimit = undefined;
        }

        if (this.customError != null || (this.f && this.f.$invalid)) {
            if (this.question.questionTitle === "Untitled") {
                this.question.questionTitle = "";
            }
            return;
        }
        if (this.question.questionType === "matrix") {

            if (this.question.matrixType === "radiobutton-horizontal" || this.question.matrixType === "radiobutton-vertical") {
                this.question.answerFieldType = "radio";
            }

            if (this.question.matrixType === "textArea") {
                this.question.answerFieldType = "text";
            }
            if (this.question.matrixType === "checkbox") {
                this.question.answerFieldType = "checkbox";
            }
        }

        if (this.question.questionText == null || this.question.questionTitle == null || this.question.answerFieldType == null) {
            return;
        }

        this.question.questionText = this.replaceLinebreaksWithHTML(this.question.questionText);
        this.question.questionTitle = this.replaceLinebreaksWithHTML(this.question.questionTitle);

        const headersJson: IHeader[] = [];
        if (this.question.questionType === "matrix" || this.question.questionType === "true-false" || this.question.questionType === "") {
            for (let i = 0; i < this.columnHeaders.length; i++) {
                const header = {
                    type: this.columnHeaders[i].type,
                    id: this.columnHeaders[i].id,
                    text: this.replaceLinebreaksWithHTML(this.columnHeaders[i].text) || "",
                };
                headersJson.push(header);
            }
        }

        const rowsJson: IRow[] = [];
        for (let i = 0; i < this.rows.length; i++) {
            const row: IRow = {
                id: this.rows[i].id,
                type: this.rows[i].type,
                text: this.replaceLinebreaksWithHTML(this.rows[i].text),
                columns: [],
            };
            const columnsJson: IColumn[] = [];
            for (let j = 0; j < this.rows[i].columns.length; j++) {
                const column: IColumn = {
                    id: this.rows[i].columns[j].id,
                    type: this.rows[i].columns[j].type,
                    rowId: row.id,
                    text: "",
                    answerFieldType: "text",
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

    private async $doCheck() {
        this.createJson();
        const currJson = JSON.stringify(this.question);
        if (currJson !== this.oldMarkupJson) {
            this.oldMarkupJson = currJson;
            await this.updatePreview();
        }
    }

    private async updatePreview() {
        const mdStr = JSON.stringify(this.question, null, 4);
        const response = await $http.post<{md: IAskedJsonJson}>("/qst/getQuestionMD/", angular.extend({
            text: mdStr,
        }));
        this.previewParams = makePreview(response.data.md);
    }

    /**
     * Validates and saves the question into the database.
     */
    private async createQuestion(ask: boolean) {
        const questionjson = this.createJson();
        const p = this.resolve.params;
        if (!questionjson) {
            return;
        }

        // Without timeout 'timelimit' won't be saved in settings session variable. Thread issue?
        this.settings.timelimit = questionjson.timeLimit || null;
        setTimeout(() => {
            let v = questionjson.timeLimit || "";
            if (!v) {
                v = 0;
            }
            setsetting("timelimit", "" + v);
        }, 1000);

        if (isAskedQuestion(p)) {
            await this.updatePoints(p);
            return;
        }

        let route = "/postParagraphQ/";
        let params;
        if (isNewQuestion(p)) {
            route = "/newParagraphQ/";
            params = {par_next: p.par_id_next, isTask: this.qst}; // TODO: should be possible to input taskId in the dialog
        } else {
            params = {par: p.parId, taskId: p.taskId, isTask: p.qst};
        }
        const docId = p.docId;
        const response = await $http.post<IParResponse>(route, angular.extend({
            docId,
            ...params,
            question: this.question,
        }));
        const data = response.data;
        this.close({data, deleted: false, ask});
    }

    /**
     * Calls /updatePoints/ to update questions points according to form
     */
    private async updatePoints(a: IAskedQuestion) {
        const points = this.createPoints();
        const expl = this.createExplanation();
        await $http({
            method: "POST",
            url: "/updatePoints/",
            params: {
                asked_id: a.asked_id,
                expl,
                points,
            },
        });
    }

    private async deleteQuestion() {
        if (isNewQuestion(this.resolve.params)) {
            await showMessageDialog("Not editing a question - there is nothing to delete.");
            return;
        }
        if (isAskedQuestion(this.resolve.params)) {
            await showMessageDialog("Editing an asked question - cannot delete now.");
            return;
        }
        const docId = this.resolve.params.docId;
        const parId = this.resolve.params.parId;
        const data = await deleteQuestionWithConfirm(docId, parId);
        if (data != null) {
            this.close({data, deleted: true, ask: false});
        }
    }

    private explFocus($event: Event) {
        $($event.target).parent().addClass("explFocus");
    }

    private explBlur($event: Event) {
        $($event.target).parent().removeClass("explFocus");
    }

    /**
     * Changes the question title field to match the question if user hasn't defined title
     * @param question of question
     */
    private changeQuestionTitle(question: string) {
        if (!this.titleChanged) {
            this.question.questionTitle = question;
        }
    }

    private titleIsChanged() {
        this.titleChanged = true;
    }
}

registerDialogComponent("timEditQuestion",
    QuestionController,
    {templateUrl: "/static/templates/question.html"},
    "qctrl");

export async function showQuestionEditDialog(params: IQuestionDialogParams): Promise<IQuestionDialogResult> {
    return showDialog<QuestionController>("timEditQuestion", {
        params: () => params,
    });
}
