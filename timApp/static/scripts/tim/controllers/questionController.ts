import angular, {IRootElementService} from "angular";
import $ from "jquery";
import {fixQuestionJson, getPointsTable, IPreviewParams, minimizeJson} from "tim/directives/dynamicAnswerSheet";
import {markAsUsed, setsetting} from "tim/utils";
import {DialogController, registerDialogComponent, showDialog} from "../dialog";
import {ParCompiler} from "../services/parCompiler";
import {$http, $log, $timeout, $window} from "../ngimport";
import {
    FieldType,
    IAskedJsonJson,
    IAskedJsonJsonJson,
    IColumn,
    IExplCollection,
    IHeader,
    IQuestionUI,
    IRow,
    IUnprocessedHeadersCompat,
    QuestionType,
} from "../lecturetypes";
import {QuestionMatrixController} from "../components/questionMatrix";
import {IParResponse} from "../IParResponse";

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

function cleanParId(id) {
    const i = id.lastIndexOf(".");
    if (i < 0) {
        return id;
    }
    return id.substring(i + 1);
}

interface IAnswerField {
    label: string;
    value: FieldType;
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

export interface IEditQuestionParams {
    asked_id?: number;
    markup: IAskedJsonJson;
    par_id?: string;
    par_id_next?: string;
    docId: number;
}

export interface IQuestionDialogResult {
    data: IParResponse;
    ask: boolean;
    deleted: boolean;
}

function isNewQuestion(params: IQuestionDialogParams): params is INewQuestionParams {
    return (params as INewQuestionParams).qst !== undefined;
}

export class QuestionController extends DialogController<{params: IQuestionDialogParams}, IQuestionDialogResult> {
    private static $inject = ["$element"];
    private answerFieldTypes: IAnswerField[];
    private error_message: string;
    private asked_id: number;
    private dateTimeOptions: EonasdanBootstrapDatetimepicker.SetOptions;
    private settings: {timelimit: number};
    private question: IAskedJsonJsonJson & IQuestionUI;
    private rows: IExtendedRow[];
    private columns: {}[];
    private columnHeaders: IHeader[];
    private new_question: boolean;
    private par_id: string;
    private par_id_next: string;
    private titleChanged: boolean;
    private markup: IAskedJsonJson;
    private oldMarkupJson: string;
    private element: IRootElementService;
    private oldHeaders: string[];
    private answer: string;
    private previewParams: IPreviewParams;

    constructor(element: IRootElementService) {
        super();
        this.element = element;
        this.asked_id = null;

        this.dateTimeOptions = {
            format: "D.M.YYYY HH:mm:ss",
            showTodayButton: true,
        };

        this.settings = $window.sessionsettings;

        this.question = {
            headers: [],
            rows: [],
            questionTitle: "",
            questionText: "",
            matrixType: "",
            answerFieldType: "",
            timeLimit: 30,
            timeLimitFields: {hours: 0, minutes: 0, seconds: 30},
            endTimeSelected: true,
            questionType: null,
        };

        this.rows = [];
        this.columns = [];
        this.columnHeaders = [];
        this.setTime();
        this.error_message = "";
        this.answerFieldTypes = [
            {label: "Text area", value: "textArea"},
            {label: "Radio Button horizontal", value: "radiobutton-horizontal"},
            {label: "Checkbox", value: "checkbox"},
        ];
        this.oldMarkupJson = "";
        this.previewParams = null;
    }

    $onInit() {
        if (isNewQuestion(this.resolve.params)) {
            this.newQuestion(this.resolve.params);
        } else {
            this.editQuestion(this.resolve.params);
        }
    }

    async $postLink() {
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

    putBackQuotations(x: string) {
        const ox = x.replace(/<br>/g, "\n");
        return ox.replace(/&quot;/g, '"');
    }

    setTime() {
        this.question.timeLimitFields = {hours: 0, minutes: 0, seconds: 30};
        if (this.settings && this.settings.timelimit && this.settings.timelimit > 0) {
            let time = this.settings.timelimit;
            if (time > 3600) {
                this.question.timeLimitFields.hours = Math.floor(time / 3600);
            } else {
                this.question.timeLimitFields.hours = 0;
            }
            if (time > 60) {
                this.question.timeLimitFields.minutes = Math.floor(time / 60);
                time = time % 60;
            } else {
                this.question.timeLimitFields.minutes = 0;
            }
            if (time > 0) {
                this.question.timeLimitFields.seconds = time;
            } else {
                this.question.timeLimitFields.seconds = 0;
            }
        }
    }

    newQuestion(data: INewQuestionParams) {
        this.new_question = true;
        this.par_id = "NEW_PAR";
        this.markup = {qst: data.qst} as any;
        this.par_id_next = data.par_id_next;
        this.titleChanged = false;
        if (this.markup.qst) {
            this.question.endTimeSelected = false; // default no time
        }
    }

    editQuestion(data: {asked_id?: number, markup: IAskedJsonJson, par_id?: string, par_id_next?: string}) {
        const par_id = data.par_id;
        const par_id_next = data.par_id_next;
        const asked_id = data.asked_id;
        const json = data.markup.json;
        this.markup = data.markup;

        this.asked_id = null;
        this.new_question = false;
        this.titleChanged = false;
        if (asked_id) {
            this.asked_id = data.asked_id;
        } else {
            this.par_id = par_id;
            this.par_id_next = par_id_next;
        }

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
            this.question.answerFieldType = (json.answerFieldType);
        }

        const jsonData: IUnprocessedHeadersCompat = json.data || json;  // compatibility for old
        const r = fixQuestionJson(jsonData);
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
        const pointsTable = getPointsTable(data.markup.points);

        const rows = [];

        for (let i = 0; i < jsonRows.length; i++) {
            const row = jsonRows[i];

            rows[i] = {
                id: row.id,
                text: this.putBackQuotations(row.text),
                type: row.type,
                // value: row.value,  // TODO: mikä on value?
            };

            const idString = "" + (i + 1); // rows[i].id.toString();
            if (data.markup.expl && idString in data.markup.expl) {
                rows[i].expl = data.markup.expl[idString];
            }

            const jsonColumns = jsonRows[i].columns;

            const columns = [];
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
            this.question.endTimeSelected = true;
        } else {
            this.question.endTimeSelected = false;
        }
    }

    moveToElement(event: Event, dir: number) {
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

    addKeyListeners() {
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
     * @memberof module:questionController
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

        const constHeaders = {
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

        // this.columnHeaders = columnHeaders;
    }

    /**
     * A function to add a column to an existing matrix.
     * @memberof module:questionController
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

    createColumnsForRow(location: number): IExtendedColumn[] {
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
     * @memberof module:questionController
     * @param loc The index in the matrix where to add the new row.
     */
    addRow(loc: number) {
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
     * @memberof module:questionController
     * @param indexToBeDeleted The index of the row to be deleted.
     */
    delRow(indexToBeDeleted: number) {
        this.error_message = "";
        if (this.rows.length > 1) {
            if (indexToBeDeleted === -1) {
                this.rows.splice(-1, 1);
            } else {
                this.rows.splice(indexToBeDeleted, 1);
            }
        } else {
            this.errorize("", "You cannot have an empty table.");
        }

        for (let i = 0; i < this.rows.length; i++) {
            this.rows[i].id = i + 1;
        }
    }

    /**
     * A function to delete a column from a matrix.
     * @memberof module:questionController
     * @param indexToBeDeleted Index of the column to be deleted.
     */
    delCol(indexToBeDeleted: number) {
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
     * @memberof module:questionController
     */
    clearQuestion() {
        this.question = {
            headers: [],
            rows: [],
            questionTitle: "",
            questionText: "",
            matrixType: "",
            answerFieldType: "",
            endTimeSelected: true,
            timeLimit: 30,
            timeLimitFields: {hours: 0, minutes: 0, seconds: 30},
            questionType: "",
        };
        this.setTime();

        this.rows = [];
        this.answer = "";
        this.columnHeaders = [];
    }

    /**
     * The function replaces linebreaks with HTML code.
     * @memberof module:questionController
     * @param val The input string
     * @returns {*} The reformatted line.
     */
    replaceLinebreaksWithHTML(val: string) {
        const output = val.replace(/(?:\r\n|\r|\n)/g, "<br>");
        // output = output.replace(/"/g, '&quot;');
        //return output.replace(/\\/g, "\\\\");
        // var output = val.replace(/(?:\r\n)/g, '\n');
        return output;
    }

    /**
     * The function to highlight the source of the errors for a given ID.
     * @memberof module:questionController
     * @param elemId ID of the element to be errorized.
     * @param errorText Description of the occured error.
     */
    errorize(elemId: string, errorText: string) {
        angular.element("#" + elemId).css("border", "1px solid red");
        if (errorText.length > 0) {
            this.error_message += errorText + "<br />";
        }
    }

    /**
     * The function to highlight the source of the errors for a given class.
     * @memberof module:questionController
     * @param elemClass Class of the element to be errorized.
     * @param errorText Description of the occured error.
     */
    errorizeClass(elemClass: string, errorText: string) {
        angular.element("." + elemClass).css("border", "1px solid red");
        if (errorText.length > 0) {
            this.error_message += errorText + "<br />";
        }
    }

    /**
     * Removes border of a given element.
     * @memberof module:questionController
     * @param element ID of the field whose border will be removed.
     */
    defInputStyle(element) {
        if (element !== null || !element.isDefined) {
            angular.element("#" + element).css("border", "");
        }
    }

    /**
     * Calls defInputStyle for all the form elements.
     * @memberof module:questionController
     */
    removeErrors() {
        this.error_message = "";
        const elementsToRemoveErrorsFrom = [
            "questionName",
            "questionTiming",
            "questionStart",
            "questionTimer",
            "qType",
            "matrix",
            "durationSec",
            "durationHour",
            "durationMin",
            "durationDiv",
        ];
        for (let i = 0; i < elementsToRemoveErrorsFrom.length; i++) {
            if (elementsToRemoveErrorsFrom[i] !== undefined) {
                this.defInputStyle(elementsToRemoveErrorsFrom[i]);
            }
        }
        angular.element(".rowHeading").css("border", "");
    }

    /**
     * Function for checking if the row headings are empty.
     * @memberof module:questionController
     * @param rows The array of rows to be checked.
     * @returns {boolean} Whether or not the row headings are empty.
     */
    rowHeadingsEmpty(rows) {
        if (rows.length < 2) {
            return false;
        }
        for (let i = 0; i < rows.length; i++) {
            if (rows[i].text === "" || rows[i].text === null) {
                return true;
            }
        }
        return false;
    }

    /**
     * Checks if a value is a positive number and makes the appropriate errors if this is not the case.
     * @memberof module:questionController
     * @param element The value to be checked.
     * @param val The id of the value, which is used in case the number is not positive.
     */
    isPositiveNumber(element, val: string) {
        if (element === "" || isNaN(element) || element < 0) {
            this.errorize(val, "Number has to be positive.");
        }
    }

    /**
     * Creates a string of points. Rows are separated by | and answers in the same row separated by ;
     * @returns {string}
     */
    createPoints() {
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
    createExplanation(): IExplCollection {
        const expl = {};
        let n = 0;
        for (let i = 0; i < this.rows.length; i++) {
            const row = this.rows[i];
            if (row.expl && row.expl.trim()) {
                expl[row.id] = row.expl.trim();
                n++;
            }
        }
        if (n) {
            return expl;
        }
        return null;
    }

    /**
     * Creates question json
     * @returns {{questionText: string, title: string, questionType: *, answerFieldType: string, matrixType: string,
     * timeLimit: string, data: {headers: Array, rows: Array}}}
     */
    createJson(): IAskedJsonJsonJson {
        if (this.asked_id) {
            this.updatePoints();
            return this.markup.json;
        }
        this.removeErrors();
        if (this.question.questionTitle == "") {
            this.question.questionTitle = "Untitled";
        }
        if (this.question.questionText === undefined || this.question.questionText.trim().length === 0 || this.question.questionTitle === undefined || this.question.questionTitle.trim().length === 0) {
            this.errorize("questionName", "Question is required.");
        }
        if (this.question.questionType === undefined) {
            this.errorize("qType", "Question type must be selected.");
        } else if (this.question.questionType === "matrix" && (this.question.matrixType === undefined || this.question.matrixType === "")) {
            this.errorize("check", "Answer type must be selected.");
        } else if (/*(this.question.type === "radio-vertical" ||
             this.question.type === "checkbox-vertical" ||
             this.question.type === "true-false") && */
            this.rowHeadingsEmpty(this.rows)) {
            this.errorizeClass("rowHeading", "All rows must be filled in.");
        }
        if (this.rows.length > 0) {
            if ((this.question.questionType === "radio-vertical" /*|| this.question.type === "checkbox-vertical"*/) && this.rows.length < 2) {
                this.errorize("matrix", "You must have at least two choices.");
            }
        } else if (this.question.questionType !== undefined) {
            this.errorize("matrix", "You must have at least one row.");
        }
        let timeLimit: number = null;
        if (this.question.endTimeSelected) {
            if (!this.question.timeLimitFields.hours) {
                this.question.timeLimitFields.hours = 0;
            }
            if (!this.question.timeLimitFields.minutes) {
                this.question.timeLimitFields.minutes = 0;
            }
            if (!this.question.timeLimitFields.seconds) {
                this.question.timeLimitFields.seconds = 0;
            }
            this.isPositiveNumber(this.question.timeLimitFields.hours, "durationHour");
            this.isPositiveNumber(this.question.timeLimitFields.minutes, "durationMin");
            this.isPositiveNumber(this.question.timeLimitFields.seconds, "durationSec");
            timeLimit = this.question.timeLimitFields.seconds;
            if (this.question.timeLimitFields.hours) {
                timeLimit = timeLimit + (this.question.timeLimitFields.hours * 60 * 60);
            }
            if (this.question.timeLimitFields.minutes) {
                timeLimit = timeLimit + (this.question.timeLimitFields.minutes * 60);
            }
            if (timeLimit <= 0) {
                this.errorize("durationDiv", "Please enter a duration greater then zero or for unending question uncheck the duration box.");
            }
        } else {
            timeLimit = null;
        }

        if (this.error_message !== "") {
            if (this.question.questionTitle === "Untitled") {
                this.question.questionTitle = "";
            }
            return;
        }
        this.removeErrors();
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
            const row = {
                id: this.rows[i].id,
                type: this.rows[i].type,
                text: this.replaceLinebreaksWithHTML(this.rows[i].text),
                columns: null,
            };
            const columnsJson = [];
            for (let j = 0; j < this.rows[i].columns.length; j++) {
                const column = {
                    id: this.rows[i].columns[j].id,
                    type: this.rows[i].columns[j].type,
                    rowId: row.id,
                };
                columnsJson.push(column);
            }
            row.columns = columnsJson;
            rowsJson.push(row);
        }

        const minimized = minimizeJson({
            answerFieldType: this.question.answerFieldType,
            headers: headersJson,
            questionType: this.question.questionType,
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
            timeLimit: timeLimit,
            timeLimitFields: this.question.timeLimitFields,
        };

        this.markup.json = questionjson;
        const md = this.markup;
        const points = this.createPoints();
        if (points) {
            md.points = points;
        } else {
            delete md.points;
        }
        const expl = this.createExplanation();
        if (expl) {
            md.expl = expl;
        } else {
            delete md.expl;
        }
        return questionjson;
    }

    async $doCheck() {
        const questionjson = this.createJson();
        const currJson = JSON.stringify(this.markup);
        if (currJson !== this.oldMarkupJson) {
            this.oldMarkupJson = currJson;
            await this.updatePreview(questionjson);
        }
    }

    async updatePreview(questionjson: IAskedJsonJsonJson) {
        const mdStr = JSON.stringify(this.markup, null, 4);
        const route = "/qst/qetQuestionMD/";
        const docId = this.resolve.params.docId;
        const response = await $http.post<{md: IAskedJsonJson}>(route, angular.extend({
            docId,
            text: mdStr,
        }));
        const markup = response.data.md;
        this.previewParams = {
            answerTable: [],
            previousAnswer: null,
            markup: markup,
            points: markup.points,
            expl: markup.expl,
            preview: false,
            result: true,
            answclass: "qstAnswerSheet",
            noDisable: true,
        };
    }

    /**
     * Validates and saves the question into the database.
     * @memberof module:questionController
     */
    async createQuestion(ask: boolean) {
        const questionjson = this.createJson();
        if (!questionjson) {
            return;
        }
        const docId = this.resolve.params.docId;
        const mdStr = JSON.stringify(this.markup, null, 4);

        // var yaml = JSON2YAML(questionjson);
        // $log.info(yaml);

        // Without timeout 'timelimit' won't be saved in settings session variable. Thread issue?
        this.settings.timelimit = questionjson.timeLimit || null;
        setTimeout(() => {
            let v = questionjson.timeLimit || "";
            if (!v) {
                v = 0;
            }
            setsetting("timelimit", "" + v);
        }, 1000);
        this.dismiss();
        return;

        let route = "/postParagraphQ/";
        if (this.new_question) {
            route = "/newParagraphQ/";
        }
        const response = await $http.post<IParResponse>(route, angular.extend({
            docId,
            text: mdStr,
            par: cleanParId(this.par_id),
            par_next: this.par_id_next,
        }));
        const data = response.data;
        $log.info("The question was successfully added to database");
        this.close({data: data, deleted: false, ask: ask});
    }

    /**
     * Calls /updatePoints/ to update questions points according to form
     */
    updatePoints() {
        const points = this.createPoints();
        const expl = this.createExplanation();
        $http({
            method: "POST",
            url: "/updatePoints/",
            params: {
                asked_id: this.asked_id,
                expl,
                points,
            },
        })
            .then(() => {
                $log.info("Points successfully updated.");
            }, () => {
                $log.info("There was some error when updating points.");
            });
        this.dismiss();
    }

    async deleteQuestion() {
        const confirmDi = $window.confirm("Are you sure you want to delete this question?");
        if (confirmDi) {
            const response = await $http.post<IParResponse>("/deleteParagraph/" + this.resolve.params.docId, {par: this.par_id});
            const data = response.data;
            $log.info("Deleted question done!");
            this.close({data: data, deleted: true, ask: false});
        }
    }

    explFocus($event) {
        $($event.target).parent().addClass("explFocus");
    }

    explBlur($event) {
        $($event.target).parent().removeClass("explFocus");
    }

    /**
     * Changes the question title field to match the question if user hasn't defined title
     * @param question of question
     */
    changeQuestionTitle(question: string) {
        if (!this.question.questionTitle && !this.titleChanged) {
            this.question.questionTitle = question;
        }
    }

    titleIsChanged() {
        this.titleChanged = true;
    }
}

registerDialogComponent("timEditQuestion",
    QuestionController,
    {templateUrl: "/static/templates/question.html"},
    "qctrl");

export async function showQuestionEditDialog(params: IQuestionDialogParams): Promise<IQuestionDialogResult> {
    return showDialog<QuestionController, {params: IQuestionDialogParams}, IQuestionDialogResult>("timEditQuestion", {
        params: () => params,
    });
}
