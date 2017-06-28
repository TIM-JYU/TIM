import angular, {IPromise, IRootElementService, IScope} from "angular";
import $ from "jquery";
import {timApp} from "tim/app";
import {ParCompiler} from "../services/parCompiler";
import {$compile, $interval, $rootScope} from "../ngimport";

/**
 * Created by localadmin on 25.5.2015.
 * Directive for dynamic answer sheet. Sheet to answer lecture questions.
 * If preview parameter is used, inputs are disable and there is no progressbar or answer button
 * @module dynamicAnswerSheet
 * @author Matias Berg
 * @author Bek Eljurkaev
 * @author Minna Lehtomäki
 * @author Juhani Sihvonen
 * @author Hannu Viinikainen
 * @author Vesa Lappalainen
 * @licence MIT
 * @copyright 2015 Timppa project authors
 */

function uncheckRadio(e) {
    // set this to click-method if you want a radio that can be uncheked.  for the radio there
    // must be also property form that has other radios.
    const elem = $(this);
    const form = elem.prop("form");
    if (!form) {
        return;
    }
    const gn = elem.prop("name");
    if (elem.prop("previousValue") === true) {
        elem.prop("checked", false);
    } else {
        $("input[name=" + gn + "]", form).prop("previousValue", false);
        // elem.prop('previousValue', false);
    }
    elem.prop("previousValue", elem.prop("checked"));
}

export function getJsonAnswers(answer) {
    // Converts answer string to JSON table
    if (answer.length > 0 && answer[0] === "[") {
        return JSON.parse(answer);
    }
    const singleAnswers = [];
    const allAnswers = answer.split("|");

    for (let i = 0; i < allAnswers.length; i++) {
        singleAnswers.push(allAnswers[i].split(","));
    }
    return singleAnswers;
}

function deletePar(s) {
    if (!s.startsWith("<p>")) {
        return s;
    }
    const rs = s.substring(3);
    if (!rs.endsWith("</p>")) {
        return s;
    }
    return rs.substring(0, rs.length - 4);
}

export function getPointsTable(markupPoints) {
    // Format of markupPoints: 1:1.1;2:1.2;3:1.3||2:3.2
    const pointsTable = [];
    if (markupPoints && markupPoints !== "") {
        const points = markupPoints.split("|");
        for (let i = 0; i < points.length; i++) {
            const rowPoints = points[i].split(";");
            const rowPointsDict = {};
            for (let k = 0; k < rowPoints.length; k++) {
                if (rowPoints[k] !== "") {
                    const colPoints = rowPoints[k].split(":", 2);
                    rowPointsDict[colPoints[0]] = colPoints[1];
                }
            }
            pointsTable.push(rowPointsDict);
        }
    }
    return pointsTable;
}

export function minimizeJson(json) {
    // remove not needed fields from json, call when saving the question
    const result: any = {};
    if (json.headers) {
        result.headers = [];
        for (let i = 0; i < json.headers.length; i++) {
            let header = json.headers[i];
            if (header.id == i && header.type === "header") {
                header = header.text;
            }
            result.headers.push(header);
        }
    }

    const ir = -1;
    let allText = true;
    const rows = json.rows;
    const rrows = [];

    for (let i = 0; i < rows.length; i++) {
        let row = rows[i];
        if (row.id == i + 1 && (!row.type || row.type === "question")) {
            row = row.text; // { text: row.text};
        } else {
            allText = false;
        }
        rrows.push(row);
    }
    // rrows.push({}); // push empty object to force Python json yaml dump to put rows in separate lines. Remember to remove it

    // if ( allText ) rrows = rrows.join("\n"); // oletuksena menee samalle riville kaikki json text muunnoksessa.

    result.rows = rrows;
    result.questionText = json.questionText;
    result.questionTitle = json.questionTitle;
    result.questionType = json.questionType;
    result.answerFieldType = json.answerFieldType;
    result.matrixType = json.matrixType;
    if (json.timeLimit) {
        result.timeLimit = json.timeLimit;
    }
    return result;
}

function fixLineBreaks(s) {
    // var result = s.replace(" < "," &lt; ");
    //result = result.replace(" > "," &gt; ");
    const parts = s.split("!!");
    const result = parts[0];
    return result;
    //return s.replace("\n","<br />");
}

export function fixQuestionJson(json) {
    // fill all missing fields from question json, call before use json
    const columnHeaders = [];
    if (json.data) {
        json.headers = json.data.headers;
        json.rows = json.data.rows;
    }
    if (json.headers) {
        let headers = json.headers;
        if (Array !== headers.constructor) { // if just on text string
            const jrows = headers.split("\n");
            headers = [];
            let ir = -1;
            for (let i = 0; i < jrows.length; i++) {
                const jrow = jrows[i];
                if (jrow) {
                    ir++;
                    headers.push({text: jrow.toString(), type: "header", id: ir});
                }
            }
        }

        json.headers = headers;

        for (let i = 0; i < json.headers.length; i++) {
            let header = json.headers[i];
            if (typeof (header.text) === "undefined") {
                header = {text: header.toString(), type: "header", id: i};
            }
            if (!header.id) {
                header.id = i;
            }
            if (!header.type) {
                header.type = "header";
            }
            json.headers[i] = header;
        }
    }

    if (!json.answerFieldType) {
        json.answerFieldType = "radio";
    }

    if (json.questionType === "true-false" && (!json.headers || json.headers.length == 0)) {
        json.headers[0] = {type: "header", id: 0, text: "True"};
        json.headers[1] = {type: "header", id: 1, text: "False"};
    }

    let rows = json.rows;
    if (Array !== rows.constructor) { // if just on text string
        const jrows = rows.split("\n");
        rows = [];
        for (let i = 0; i < jrows.length; i++) {
            const jrow = jrows[i];
            if (jrow) {
                rows.push({text: jrow.toString(), type: "question", id: i});
            }
        }
    }

    let ir = -1;
    const n = rows.length - 1;
    if (n >= 0) {
        const last = rows[n];  // remove ending object if needed
        if (typeof last === "object" && $.isEmptyObject(last)) {
            rows.splice(n, 1); // trust that null is also object!
        }
    }

    for (let i = 0; i < rows.length; i++) {
        let row = rows[i];
        if (typeof (row.text) === "undefined") {
            row = {text: row.toString(), type: "question"};
        }
        // if (!row.text) continue;
        ir++;
        if (!row.id) {
            row.id = ir + 1;
        }
        if (!row.columns) {
            row.columns = [{}];
            let nh = 0;
            if (json.headers) {
                nh = json.headers.length;
            }
            for (let ic = 1; ic < nh; ic++) {
                row.columns.push({});
            }
        }
        rows[i] = row;
    }
    json.rows = rows;
}

type JSONData = {
    headers: {text: string}[],
    text: string,
    rows: {
        type: string,
        text: string,
        columns: {id: number}[],
    }[],
    columns?: {Value: string}[],
};

class AnswerSheetController {
    private static $inject = ["$scope", "$element"];

    private promise: IPromise<any>;
    private timeLeft;
    private barFilled;
    private element: IRootElementService;
    private buttonText: string;
    private preview: boolean;
    private gid: string;
    private result: {};
    private previousAnswer: {};
    private json: {data: JSONData, timeLimit: number, questionText: string, questionType: string, answerFieldType: string} & JSONData;
    private markup: string;
    private answerTable: string[][];
    private expl: string;
    private askedTime: number;
    private endTime: number;
    private htmlSheet: JQuery;
    private scope: IScope;
    private progressElem: JQuery;
    private isText: boolean;
    private progressText: JQuery;
    private $parent: any; // TODO require questioncontroller

    constructor(scope: IScope, element: IRootElementService) {
        this.element = element;
        this.scope = scope;

        /**
         * Use time parameter to either close question/points window or extend question end time.
         * If time is null, question/points is closed.
         * Else time is set as questions new end time.
         */
        scope.$on("update_end_time", function(event, time) {
            if (time !== null) {
                this.endTime = time - this.$parent.vctrl.clockOffset;
                this.progressElem.attr("max", this.endTime - this.askedTime);
            } else {
                if (!this.$parent.lctrl.isLecturer) {
                    $interval.cancel(this.promise);
                    this.element.empty();
                    scope.$emit("closeQuestion");
                }
            }
        });
    }

    cg() {
        return "group"; // + this.gid;
    }

    /**
     * Creates question answer/preview form.
     */
    createAnswer(params) {
        this.result = params.result;
        this.gid = params.tid || "";
        this.gid = this.gid.replace(".", "_");
        this.buttonText = params.markup.button || params.markup.buttonText || "Answer";

        const answclass = params.answclass || "answerSheet";
        this.previousAnswer = params.previousAnswer;

        let disabled = "";
        // If showing preview or question result, inputs are disabled
        if (params.preview || this.preview || this.result) {
            disabled = " disabled ";
        }
        if (params.noDisable) {
            disabled = "";
        }

        this.element.empty();
        this.json = params.markup.json;
        this.markup = params.markup;
        const data = this.json.data || this.json; // compability to old format
        const json = this.json;

        fixQuestionJson(data);

        this.answerTable = params.answerTable || [];

        // If user has answer to question, create table of answers and select inputs according to it
        if (this.previousAnswer) {
            this.answerTable = getJsonAnswers(this.previousAnswer);
        }
        const answerTable = this.answerTable;
        const pointsTable = getPointsTable(params.points || params.markup.points);

        this.expl = params.expl || params.markup.expl || params.markup.xpl;
        this.askedTime = params.askedTime - params.clockOffset;
        this.endTime = params.askedTime + this.json.timeLimit * 1000 - params.clockOffset;

        // var htmlSheet = $('<div>', {class: answclass});
        const htmlSheet = $("<form>", {class: answclass});
        this.htmlSheet = htmlSheet;
        params.htmlSheet = htmlSheet;
        if (this.json.timeLimit && this.endTime && !this.preview && !this.result) {
            const progress = $("<progress>", {
                max: (this.endTime - this.askedTime),
                id: "progressBar",
            });
            htmlSheet.append(progress);
            htmlSheet.append($("<span>", {
                class: "progresslabel",
                id: "progressLabel",
                text: this.json.timeLimit + " s",
            }));
        }
        const h5 = $("<h5>");
        h5.append(fixLineBreaks(json.questionText));

        htmlSheet.append(h5);
        // htmlSheet.append($('<h5>', {text: json.questionText}));
        if (params.markup.userpoints !== undefined) {
            htmlSheet.append($("<p>", {text: "Points: " + params.markup.userpoints}));
        }

        const table = $("<table>", {id: "answer-sheet-table", class: "table table-borderless"});

        let totalBorderless = true;

        if (data.headers &&
            data.headers.length > 0 && !(data.headers[0].text === "" && data.headers.length === 1)) {
            const tr = $("<tr>", {class: "answer-heading-row"});
            if (data.headers.length > 0) {
                tr.append($("<th>"));
            }
            angular.forEach(data.headers, function(header) {
                const th = $("<th>");
                th.append(fixLineBreaks(header.text));
                totalBorderless = false;
                tr.append(th);
                // tr.append($('<th>', {text: header.text || header}));
            });
            if (this.result && this.expl) {
                tr.append($("<th>", {}));
            }
            table.append(tr);
        }

        let ir = -1;
        angular.forEach(data.rows, function(row) {
            ir++;
            let pointsRow = {};
            if (params.result && pointsTable.length > ir && pointsTable[ir]) {
                pointsRow = pointsTable[ir];
            }
            const rtext = fixLineBreaks(row.text);
            const tr = $("<tr>");
            if (json.questionType === "matrix" || json.questionType === "true-false") {
                const td = $("<td>");
                td.append(rtext);
                if (rtext && ir > 0) {
                    totalBorderless = false;
                }
                tr.append(td);
                //tr.append($('<td>', {text: row.text}));
            }
            let header = 0;
            //TODO: Needs correct JSON to be made better way
            for (let ic = 0; ic < row.columns.length; ic++) {
                let group;
                group = this.cg() + ir;

                if (json.questionType === "matrix" || json.questionType === "true-false") {
                    const value = "" + (ic + 1); // (row.columns[ic].id + 1).toString();

                    let colTDPoints;
                    let colPtsClass = "qst-normal";
                    if (value in pointsRow) {
                        const colPoints = pointsRow[value];
                        colTDPoints = $("<p>", {class: "qst-points"}).append(colPoints);
                        if (colPoints > 0) {
                            colPtsClass = "qst-correct";
                        }
                    }
                    row.columns[ic].id = ic;

                    if (json.answerFieldType === "text") {
                        this.isText = true;
                        let text = "";
                        if (answerTable && ir < answerTable.length && ic < answerTable[ir].length) {
                            text = answerTable[ir][ic];
                        }
                        const textArea = $("<textarea>", {
                            id: "textarea-answer",
                            name: group,
                        });
                        textArea.text(text);
                        if (disabled !== "") {
                            textArea.attr("disabled", "disabled");
                        }
                        if (data.headers && data.headers.length === 1 && data.headers[0].text === "" && data.rows.length === 1) {
                            // textArea.attr('style', 'height:200px');
                        }
                        tr.append($("<td>", {class: "answer-button"}).append($("<label>").append(textArea)));
                        header++;
                    } else {
                        // group = this.cg() + rtext.replace(/[^a-zA-Z0-9]/g, "");
                        let checked = false;
                        if (answerTable && ir < answerTable.length) {
                            checked = (answerTable[ir].indexOf(value) >= 0);
                        }
                        const input = $("<input>", {
                            type: json.answerFieldType,
                            name: group,
                            value: ic + 1, // parseInt(row.columns[ic].id) + 1,
                            checked,
                        });
                        if (json.answerFieldType === "radio") {
                            input.prop("form", htmlSheet as any);
                            input.click(uncheckRadio);  // TODO: Tähän muutoskäsittely ja jokaiseen tyyppiin tämä
                        }
                        if (disabled !== "") {
                            input.attr("disabled", "disabled");
                        }

                        const td = $("<td>", {class: "answer-button"});
                        const ispan = $("<span>", {class: colPtsClass});
                        ispan.append(input);
                        td.append($("<label>").append(ispan));

                        if (colTDPoints) {
                            td.append(colTDPoints);
                        }
                        tr.append(td);
                        header++;
                    }
                } else {
                    const value = "" + (ir + 1); // (row.id + 1).toString();
                    let colTDPoints;
                    let colPtsClass = "qst-normal";
                    let pointsRow = {};
                    if (params.result && pointsTable.length > 0 && pointsTable[0]) pointsRow = pointsTable[0];
                    if (value in pointsRow) {
                        const colPoints = pointsRow[value];
                        colTDPoints = $("<p>", {class: "qst-points"}).append(colPoints);
                        if (colPoints > 0) {
                            colPtsClass = "qst-correct";
                        }
                    }
                    row.columns[ic].id = ic;

                    const type = row.type || "question";
                    group = this.cg() + type.replace(/[^a-zA-Z0-9]/g, "");
                    let checked = false;
                    if (answerTable && answerTable.length > 0) {
                        checked = (answerTable[0].indexOf(value) >= 0);
                    }
                    const input = $("<input>", {
                        type: json.answerFieldType,
                        name: group,
                        value: ir + 1, //parseInt(row.id) + 1,
                        checked,
                    });
                    if (json.answerFieldType === "radio") {
                        input.prop("form", htmlSheet as any);
                        input.click(uncheckRadio);
                    }
                    if (disabled !== "") {
                        input.attr("disabled", "disabled");
                    }
                    const label = $("<label>");
                    const ispan = $("<span>", {class: colPtsClass});
                    ispan.append(input);
                    label.append(ispan).append(" " + deletePar("" + rtext));
                    const td = $("<td>", {class: "answer-button2"});
                    td.append(label);
                    if (colTDPoints) {
                        td.append(colTDPoints);
                    }
                    tr.append(td);
                }
            }
            // If showing question results, add question rows explanation
            if (this.result && this.expl) {
                let expl = "";
                const ir1 = (ir + 1).toString();
                if (ir1 in this.expl) {
                    expl = this.expl[ir1];
                }
                const tde = $("<td>", {class: "explanation"});
                tde.append(expl);
                // tr.append($('<td>', {class: 'explanation', text: expl}));
                tr.append(tde);
            }
            table.append(tr);
        });

        htmlSheet.append($("<div>").append(table));

        if (totalBorderless) {
            table.addClass("total-borderless");
        }

        this.element.append(htmlSheet);
        $compile(this.scope as any); // TODO this seems wrong

        if (!this.preview && !this.result) {
            const $table = this.element.find("#answer-sheet-table");
            window.setTimeout(function() {
                const $table = this.element.find("#answer-sheet-table");
                let $input = null;
                if (this.json.answerFieldType !== "text") {
                    $input = $table.find("input:first");
                } else {
                    $input = $table.find("textarea:first");
                }
                if (params.isAsking) {
                    $input[0].focus();
                }
            }, 0);
            //
            if (!this.isText) {
                $table.on("keyup.send", this.answerWithEnter);
            }
            const now = new Date().valueOf();
            this.timeLeft = this.endTime - now;
            this.barFilled = 0;
            if (this.endTime && this.json.timeLimit) {
                const timeBetween = 500;
                const maxCount = this.timeLeft / timeBetween + 5 * timeBetween;
                this.progressElem = $("#progressBar");
                this.progressText = $("#progressLabel");
                this.start(timeBetween, maxCount);
            }
        }
        ParCompiler.processAllMath(this.htmlSheet);
    }

    start(timeBetween, maxCount) {
        this.promise = $interval(this.updateBar, timeBetween, maxCount);
    }

    // createAnswer ends

    /**
     * Updates progressbar and time left text
     * @memberof module:dynamicAnswerSheet
     */
    updateBar() {
        //TODO: Problem with inactive tab.
        const now = new Date().valueOf();
        if (!this.endTime || !this.promise) {
            return;
        }
        this.timeLeft = this.endTime - now;
        this.barFilled = (this.endTime - this.askedTime) - (this.endTime - now);
        this.progressElem.attr("value", (this.barFilled));
        this.progressText.text(Math.max((this.timeLeft / 1000), 0).toFixed(0) + " s");
        this.progressElem.attr("content", (Math.max((this.timeLeft / 1000), 0).toFixed(0) + " s"));
        if (this.barFilled >= this.progressElem.attr("max")) {
            $interval.cancel(this.promise);
            if (!this.$parent.lctrl.isLecturer && !this.$parent.lctrl.questionEnded) {
                this.answerToQuestion();
            } else {
                this.progressText.text("Time's up");
            }
            this.$parent.questionEnded = true;
        }
    }

    endQuestion() {
        if (!this.promise) {
            return;
        }
        $interval.cancel(this.promise);
        const max = this.progressElem.attr("max");
        this.progressElem.attr("value", max);
        this.progressText.text("Time's up");
    }

    answerWithEnter = (e) => {
        if (e.keyCode === 13) {
            $("#answer-sheet-table").off("keyup.send");
            this.$parent.answer();
        }
    }

    getAnswers() {
        const answers = [];
        const data = this.json.data || this.json;
        if (angular.isDefined(data.rows)) {
            let groupName; // data.rows[ir].text.replace(/[^a-zA-Z0-9]/g, '');
            if (this.json.questionType === "matrix" || this.json.questionType === "true-false") {
                for (let ir = 0; ir < data.rows.length; ir++) {
                    groupName = this.cg() + ir;
                    const answer = [];
                    let matrixInputs;
                    // groupName = this.cg() + ir; // data.rows[ir].text.replace(/[^a-zA-Z0-9]/g, '');

                    if (this.json.answerFieldType === "text") {
                        matrixInputs = $("textarea[name=" + groupName + "]", this.htmlSheet);
                        for (let ic = 0; ic < matrixInputs.length; ic++) {
                            const v = matrixInputs[ic].value || "";
                            answer.push(v);
                        }

                        answers.push(answer);
                        continue;
                    }

                    matrixInputs = $("input[name=" + groupName + "]:checked", this.htmlSheet);

                    for (let k = 0; k < matrixInputs.length; k++) {
                        const v = matrixInputs[k].value || "";
                        answer.push(v);
                    }
                    if (matrixInputs.length <= 0) {
                        answer.push("");
                    }
                    answers.push(answer);
                }
            } else {
                answers.push([]);
                const type = data.rows[0].type || "question";
                groupName = this.cg() + type.replace(/[^a-zA-Z0-9]/g, "");
                const checkedInputs = $("input[name=" + groupName + "]:checked", this.htmlSheet) as any as HTMLInputElement[];
                for (let j = 0; j < checkedInputs.length; j++) {
                    answers[0].push(checkedInputs[j].value);
                }

                if (checkedInputs.length <= 0) {
                    answers[0].push(""); // was "undefined"
                }
            }
        }

        if (angular.isDefined(data.columns)) {
            angular.forEach(data.columns, function(column) {
                const groupName = this.cg() + column.Value.replace(/ /g, "");
                answers.push($("input[name=" + groupName + "]:checked").val());
            });
        }
        return answers;
    }

    /**
     * Function to create question answer and send it to server.
     * @memberof module:dynamicAnswerSheet
     */
    answerToQuestion() {

        const answers = this.getAnswers();

        if (!this.$parent.lctrl.isLecturer) {
            this.element.empty();
            $interval.cancel(this.promise);
        }
        this.scope.$emit("answerToQuestion", {answer: answers, askedId: this.$parent.askedId});
    }

    /**
     * Closes question window and clears updateBar interval.
     * If user is lecturer, also closes answer chart window.
     * @memberof module:dynamicAnswerSheet
     */
    closeQuestion() {
        $interval.cancel(this.promise);
        this.element.empty();
        this.scope.$emit("closeQuestion");
        if (this.$parent.isLecturer) {
            $rootScope.$broadcast("closeAnswerSheetForGood");
        }
    }

    closePreview() {
        this.element.empty();
    }
}

timApp.component("dynamicAnswerSheet", {
    bindings: {
        control: "=",
        preview: "@",
    },
    controller: AnswerSheetController,
    transclude: true,
});
