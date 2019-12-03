import {IChangesObject, IController, IOnChangesObject} from "angular";
import $ from "jquery";
import {timApp} from "tim/app";
import {ParCompiler} from "../../editor/parCompiler";
import {
    AnswerFieldType,
    AnswerTable,
    IAskedJsonJson,
    IExplCollection,
    IHeader,
    IProcessedHeaders,
    IRow,
    IUnprocessedHeaders,
} from "../../lecture/lecturetypes";
import {Binding} from "../../util/utils";

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

function deletePar(s: string) {
    if (!s.startsWith("<p>")) {
        return s;
    }
    const rs = s.substring(3);
    if (!rs.endsWith("</p>")) {
        return s;
    }
    return rs.substring(0, rs.length - 4);
}

export function getPointsTable(markupPoints?: string): Array<{[points: string]: string}> {
    // Format of markupPoints: 1:1.1;2:1.2;3:1.3||2:3.2
    const pointsTable: Array<{[points: string]: string}> = [];
    if (markupPoints && markupPoints !== "") {
        const points = markupPoints.split("|");
        for (const p of points) {
            const rowPoints = p.split(";");
            const rowPointsDict: {[points: string]: string} = {};
            for (const rp of rowPoints) {
                if (rp !== "") {
                    const colPoints = rp.split(":", 2);
                    rowPointsDict[colPoints[0]] = colPoints[1];
                }
            }
            pointsTable.push(rowPointsDict);
        }
    }
    return pointsTable;
}

export function minimizeJson(json: IProcessedHeaders): IUnprocessedHeaders {
    // remove not needed fields from json, call when saving the question
    const result: IUnprocessedHeaders = {
        headers: [],
        rows: [],
    };
    if (json.headers) {
        result.headers = [];
        for (let i = 0; i < json.headers.length; i++) {
            let header: string | IHeader = json.headers[i];
            if (header.id === i && header.type === "header") {
                header = header.text;
            }
            result.headers.push(header);
        }
    }

    let allText = true;
    const rows = json.rows;

    for (let i = 0; i < rows.length; i++) {
        let row: string | IRow = rows[i];
        if (row.id === i + 1 && (!row.type || row.type === "question")) {
            row = row.text; // { text: row.text};
        } else {
            allText = false;
        }
        result.rows.push(row);
    }
    // rrows.push({}); // push empty object to force Python json yaml dump to put rows in separate lines. Remember to remove it

    // if ( allText ) rrows = rrows.join("\n"); // oletuksena menee samalle riville kaikki json text muunnoksessa.

    return result;
}

function fixLineBreaks(s: string) {
    // var result = s.replace(" < "," &lt; ");
    // result = result.replace(" > "," &gt; ");
    const parts = s.split("!!");
    const result = parts[0];
    return result;
    // return s.replace("\n","<br />");
}

export function fixQuestionJson(json: IUnprocessedHeaders): IProcessedHeaders {
    // fill all missing fields from question json, call before use json
    // row ids are by default 1-based and header ids 0-based
    const fixed: IProcessedHeaders = {headers: [], rows: []};
    const headers = json.headers;
    const rows = json.rows;
    if (headers) {
        for (let i = 0; i < headers.length; i++) {
            const header = headers[i];
            if (typeof header === "string") {
                fixed.headers.push({text: header, type: "header", id: i});
            } else {
                fixed.headers.push(header);
            }
        }
    }

    const defaultFieldType: AnswerFieldType = "text";
    const blankColumn = {text: "", type: "question", answerFieldType: defaultFieldType, id: 0, rowId: 0};
    let ir = -1;
    for (const r of rows) {
        ir++;
        if (typeof r === "string") {
            fixed.rows.push({
                columns: [blankColumn],
                id: ir + 1,
                text: r,
                type: "question",
            });
        } else {
            fixed.rows.push(r);
        }
    }

    for (const row of fixed.rows) {
        let nh = 0;
        if (fixed.headers) {
            nh = fixed.headers.length;
        }
        for (let ic = 1; ic < nh; ic++) {
            row.columns.push(blankColumn);
        }
    }
    return fixed;
}

export interface IPreviewParams {
    answerTable: AnswerTable;
    enabled: boolean;
    markup: IAskedJsonJson;
    showExplanations: boolean;
    showCorrectChoices: boolean;
    userpoints?: number;
}

function createArray(...args: number[]) {
    if (args.length === 0) {
        return [];
    }
    const [length, ...rest] = args;
    const arr = new Array(length || 0);
    let i = length;

    if (args.length > 1) {
        while (i--) {
            arr[length - 1 - i] = createArray(...rest);
        }
    }
    return arr;
}

export function makePreview(markup: IAskedJsonJson, {
    answerTable = [],
    enabled = false,
    showExplanations = false,
    showCorrectChoices = false,
    userpoints,
}: Partial<IPreviewParams> = {
    answerTable: [],
    enabled: false,
    showCorrectChoices: false,
    showExplanations: false,
}): IPreviewParams {
    return {
        answerTable,
        enabled,
        markup,
        showCorrectChoices,
        showExplanations,
        userpoints,
    };
}

type MatrixElement = string | number;

class AnswerSheetController implements IController {
    static $inject = ["$element"];
    private element: JQLite;
    private questiondata?: Binding<IPreviewParams, "<">;
    private json!: IAskedJsonJson; // TODO decide if undefined should be valid
    private processed!: IProcessedHeaders; // TODO decide if undefined should be valid
    private answerMatrix: MatrixElement[][] = [];
    private expl?: IExplCollection;
    private pointsTable: Array<{[p: string]: string}> = [];
    private userpoints?: number;
    private disabled?: boolean;
    private onAnswerChange!: Binding<() => ((at: AnswerTable) => void) | undefined, "&">;

    constructor(element: JQLite) {
        this.element = element;
    }

    $onInit() {

    }

    $onChanges(onChangesObj: IOnChangesObject) {
        const qdata = onChangesObj.questiondata as IChangesObject<IPreviewParams> | undefined;
        const adata = onChangesObj.answertable as IChangesObject<AnswerTable> | undefined;
        if (qdata) {
            this.createAnswer();
            ParCompiler.processAllMathDelayed(this.element);

            // Do a fake update; without this "no answer" is not registered properly if the user never clicks
            // the answer sheet.
            this.signalUpdate();
        } else if (adata) {
            this.answerMatrix = this.answerMatrixFromTable(adata.currentValue);
        }
    }

    $doCheck() {
    }

    private getHeader() {
        return this.fixText(this.json.questionText);
    }

    private fixText(s: string) {
        return fixLineBreaks(s);
    }

    private getPoints(rowIndex: number, colIndex: number): string | null {
        if (!this.questiondata) {
            return null;
        }
        if (!this.questiondata.showCorrectChoices) {
            return null;
        }
        if (this.isVertical()) {
            [rowIndex, colIndex] = [colIndex, rowIndex];
        }
        if (this.pointsTable.length <= rowIndex) {
            return null;
        }
        const rowPoints = this.pointsTable[rowIndex];
        if (!rowPoints) {
            return null;
        }
        const idxStr = "" + (colIndex + 1);
        if (idxStr in rowPoints) {
            return rowPoints[idxStr];
        }
        return null;
    }

    private getLabelText(row: IRow): string {
        if (this.isMatrix()) {
            return "";
        }
        return row.text;
    }

    private getInputClass(rowIndex: number, colIndex: number) {
        const pts = this.getPoints(rowIndex, colIndex);
        return pts != null && parseFloat(pts) > 0 ? "qst-correct" : "qst-normal";
    }

    private canShowExpl(): boolean {
        if (!this.questiondata) {
            return false;
        }
        return (this.questiondata.showExplanations) && this.expl != null;
    }

    private getExpl(rowIndex: number): string | null {
        if (!this.canShowExpl() || !this.expl) {
            return null;
        }
        const key = "" + (rowIndex + 1);
        if (key in this.expl) {
            return this.expl[key];
        }
        return null;
    }

    private isMatrix() {
        return this.json.questionType === "matrix" || this.json.questionType === "true-false";
    }

    private isVertical() {
        return !this.isMatrix();
    }

    private getTableClass(): string | null {
        let totalBorderless = true;
        const data = this.processed;
        if (this.hasHeaders() || (data.rows.length > 1 && data.rows[0].columns.length > 1)) {
            totalBorderless = false;
        }
        return totalBorderless ? "total-borderless" : null;
    }

    private hasHeaders(): boolean {
        const data = this.processed;
        return data.headers &&
            data.headers.length > 0 && !(data.headers[0].text === "" && data.headers.length === 1);
    }

    private getGroupName(rowIndex: number) {
        if (this.isMatrix()) {
            return "group" + rowIndex;
        } else {
            return "groupquestion";
        }
    }

    private getInputValue(rowIndex: number, colIndex: number) {
        if (this.isMatrix()) {
            return colIndex + 1;
        } else {
            return rowIndex + 1;
        }
    }

    /**
     * Creates question answer/preview form.
     */
    createAnswer() {
        const params = this.questiondata;
        if (!params) {
            return;
        }

        this.disabled = !params.enabled;

        this.json = params.markup;
        this.processed = fixQuestionJson(this.json);
        this.answerMatrix = this.answerMatrixFromTable(params.answerTable);
        this.pointsTable = getPointsTable(params.markup.points);
        this.expl = params.markup.expl;
        this.userpoints = params.userpoints;
    }

    private questionHasPoints() {
        return this.questiondata && this.questiondata.markup.points != null;
    }

    private answerMatrixFromTable(table: AnswerTable): MatrixElement[][] {
        const data = this.processed;
        if (data.rows.length === 0) {
            return [];
        }
        const arr: MatrixElement[][] = createArray(data.rows.length, data.rows[0].columns.length);
        if (this.isMatrix()) {
            if (this.isText()) {
                for (let i = 0; i < arr.length; i++) {
                    for (let j = 0; j < arr[i].length; j++) {
                        arr[i][j] = (table[i] || [])[j] || "";
                    }
                }
                return arr;
            } else {
                for (let i = 0; i < table.length; i++) {
                    for (const val of table[i]) {
                        if (!val) {
                            continue;
                        }
                        const value = parseInt(val, 10);
                        if (this.isCheckbox()) {
                            arr[i][value - 1] = 1;
                        } else {
                            arr[i][0] = value;
                        }
                    }
                }
            }
        } else {
            if (table.length === 0) {
                return arr;
            }
            if (table.length !== 1) {
                throw new Error(`Expected answertable of length 1, got ${table.length}`);
            }
            const row = table[0];
            for (const val of row) {
                if (!val) {
                    continue;
                }
                const value = parseInt(val, 10);
                if (this.isCheckbox()) {
                    arr[value - 1][0] = 1;
                } else if (this.isRadio()) {
                    arr[0][0] = value;
                } else {
                    throw new Error(`Unexpected input type: ${this.json.answerFieldType}`);
                }
            }
        }
        return arr;
    }

    private tableFromAnswerMatrix(matrix: MatrixElement[][]): AnswerTable {
        let table: AnswerTable = [];
        if (this.isMatrix()) {
            if (this.isText()) {
                table = matrix.map((row) => row.map((elem) => (elem || "").toString()));
            } else {
                for (const row of matrix) {
                    const tableRow: string[] = [];
                    let rowi = 1;
                    for (const val of row) {
                        if (val) {
                            tableRow.push("" + (this.isCheckbox() ? rowi : val));
                        }
                        rowi++;
                    }
                    // TODO: if row is empty, should we push an empty string (old code did so)?
                    table.push(tableRow);
                }
            }
        } else {
            const tableRow: string[] = [];
            let rowi = 1;
            for (const row of matrix) {
                const val = row[0];
                if (val) {
                    tableRow.push("" + (this.isCheckbox() ? rowi : val));
                }
                rowi++;
            }
            // TODO: if row is empty, should we push an empty string (old code did so)?
            table.push(tableRow);
        }
        return table;
    }

    private isText() {
        return this.json.answerFieldType === "text";
    }

    private isRadio() {
        return this.json.answerFieldType === "radio";
    }

    private isCheckbox() {
        return this.json.answerFieldType === "checkbox";
    }

    private signalUpdate() {
        const c = this.onAnswerChange();
        if (c) {
            c(this.tableFromAnswerMatrix(this.answerMatrix));
        }
    }
}

timApp.component("dynamicAnswerSheet", {
    bindings: {
        onAnswerChange: "&",
        questiondata: "<",
    },
    controller: AnswerSheetController,
    template: `
<form ng-if="$ctrl.json">
    <h5 ng-bind-html="$ctrl.getHeader()"></h5>
    <p ng-if="$ctrl.userpoints != null && $ctrl.questionHasPoints()">Points received: {{ $ctrl.userpoints }}</p>
    <table class="table" ng-class="$ctrl.getTableClass()">
        <tbody>
        <tr ng-if="$ctrl.hasHeaders()" class="answer-heading-row">
            <th ng-if="$ctrl.isMatrix()"></th>
            <th ng-repeat="h in $ctrl.processed.headers" ng-bind-html="$ctrl.fixText(h.text)"></th>
            <th ng-if="$ctrl.canShowExpl()"></th>
        </tr>
        <tr ng-repeat="row in $ctrl.processed.rows track by $index" ng-init="rowi = $index">
            <td ng-if="$ctrl.isMatrix()" ng-bind-html="$ctrl.fixText(row.text)"></td>
            <td ng-repeat="col in row.columns track by $index" ng-init="coli = $index">
                <label>
                    <input ng-if="$ctrl.isRadio()"
                           ng-class="$ctrl.getInputClass(rowi, coli)"
                           ng-disabled="$ctrl.disabled"
                           ng-change="$ctrl.signalUpdate()"
                           type="radio"
                           ng-model="$ctrl.answerMatrix[$ctrl.isMatrix() ? rowi : 0][0]"
                           name="{{$ctrl.getGroupName(rowi)}}"
                           ng-value="$ctrl.getInputValue(rowi, coli)">
                    <input ng-if="$ctrl.isCheckbox()"
                           ng-class="$ctrl.getInputClass(rowi, coli)"
                           ng-disabled="$ctrl.disabled"
                           ng-change="$ctrl.signalUpdate()"
                           type="checkbox"
                           ng-model="$ctrl.answerMatrix[rowi][coli]"
                           name="{{$ctrl.getGroupName(rowi)}}"
                           ng-true-value="1"
                           ng-false-value="0">
                    <textarea
                            class="form-control"
                            ng-if="$ctrl.isText()"
                            ng-disabled="$ctrl.disabled"
                            ng-change="$ctrl.signalUpdate()"
                            ng-model="$ctrl.answerMatrix[rowi][coli]">
</textarea>
                    <span ng-bind-html="$ctrl.getLabelText(row, col)"></span></label>
                <p ng-if="(p = $ctrl.getPoints(rowi, coli)) != null" class="qst-points" ng-bind="p"></p>
            </td>
            <td ng-if="p = $ctrl.getExpl(rowi)" ng-bind-html="p" class="explanation"></td>
        </tr>
        </tbody>
    </table>
</form>
`,
});
