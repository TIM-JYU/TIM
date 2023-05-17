import type {OnChanges, SimpleChanges} from "@angular/core";
import {
    Component,
    ElementRef,
    EventEmitter,
    Input,
    NgModule,
    NgZone,
    Output,
} from "@angular/core";
import {ParCompiler} from "tim/editor/parCompiler";
import type {
    AnswerFieldType,
    AnswerTable,
    IAskedJsonJson,
    IExplCollection,
    IHeader,
    IProcessedHeaders,
    IRow,
    IUnprocessedHeaders,
} from "tim/lecture/lecturetypes";
import {RowCodec} from "tim/lecture/lecturetypes";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {PurifyModule} from "tim/util/purify.module";
import {HTMLSanitizeModule} from "tim/util/htmlsanitize.module";
import {CommonModule} from "@angular/common";

/**
 * Directive for dynamic answer sheet. Sheet to answer lecture questions.
 * If preview parameter is used, inputs are disable and there is no progressbar or answer button
 * @author Matias Berg
 * @author Bek Eljurkaev
 * @author Minna Lehtomäki
 * @author Juhani Sihvonen
 * @author Hannu Viinikainen
 * @author Vesa Lappalainen
 * @licence MIT
 * @copyright 2015 Timppa project authors
 */

export function getPointsTable(
    markupPoints?: string
): Array<Record<string, string>> {
    // Format of markupPoints: 1:1.1;2:1.2;3:1.3||2:3.2
    const pointsTable: Array<Record<string, string>> = [];
    if (markupPoints && markupPoints !== "") {
        const points = markupPoints.split("|");
        for (const p of points) {
            const rowPoints = p.split(";");
            const rowPointsDict: Record<string, string> = {};
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

    const rows = json.rows;

    for (let i = 0; i < rows.length; i++) {
        let row: string | IRow = rows[i];
        if (row.id === i + 1 && (!row.type || row.type === "question")) {
            row = row.text; // { text: row.text};
        } else {
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
    return parts[0];
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
    const blankColumn = {
        text: "",
        type: "question",
        answerFieldType: defaultFieldType,
        id: 0,
        rowId: 0,
    };
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
        } else if (RowCodec.is(r)) {
            fixed.rows.push(r);
        } else {
            console.warn("Skipping invalid row:", r);
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

export function makePreview(
    markup: IAskedJsonJson,
    {
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
    }
): IPreviewParams {
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

@Component({
    selector: "tim-answer-sheet",
    template: `
        <form *ngIf="json">
            <h5 [innerHtml]="getHeader() | purify"></h5>
            <p *ngIf="userpoints != null && questionHasPoints()">Points received: {{ userpoints }}</p>
            <table class="table" [ngClass]="getTableClass()">
                <colgroup *ngIf="processed.rows.length > 0">
                    <col *ngIf="isMatrix()">
                    <col *ngFor="let _ of processed.rows[0].columns">
                </colgroup>
                <thead *ngIf="customHeader" [innerHTML]="customHeader | htmlSanitize"></thead>
                <tbody>
                <tr *ngIf="hasHeaders()" class="answer-heading-row">
                    <th *ngIf="isMatrix()"></th>
                    <th *ngFor="let h of processed.headers" [innerHtml]="fixText(h.text) | purify"></th>
                    <th *ngIf="canShowExpl()"></th>
                </tr>
                <tr *ngFor="let row of processed.rows; let rowi = index" [ngClass]="getTableRowClass()">
                    <td *ngIf="isMatrix()" [innerHtml]="fixText(row.text) | purify" class="qst-row_text"></td>
                    <td *ngFor="let col of row.columns; let coli = index;" class="qst-td">
                        <ng-template #points>
                            &ngsp;<span [innerHtml]="getLabelText(row) | purify"></span>
                            <p *ngIf="getPoints(rowi, coli) as p" class="qst-points" [innerText]="p"></p>
                        </ng-template>
                        <div *ngIf="isRadio()" [ngClass]="radioClass(rowi, coli)">
                            <label>
                                <input
                                        [attr.disabled]="disabled || null"
                                        type="radio"
                                        [(ngModel)]="answerMatrix[isMatrix() ? rowi : 0][0]"
                                        (ngModelChange)="signalUpdate()"
                                        [name]="getGroupName(rowi)"
                                        [value]="getInputValue(rowi, coli)">
                                <ng-container *ngTemplateOutlet="points"></ng-container>
                            </label>
                        </div>
                        <div *ngIf="isCheckbox()" [ngClass]="checkBoxClass(rowi, coli)">
                            <label>
                                <input
                                        [attr.disabled]="disabled || null"
                                        type="checkbox"
                                        [checked]="answerMatrix[rowi][coli] === 1"
                                        (change)="checkBoxChanged(rowi, coli, $event)"
                                        [name]="getGroupName(rowi)">
                                <ng-container *ngTemplateOutlet="points"></ng-container>
                            </label>
                        </div>
                        <div *ngIf="isText()" class="qst-text">
                            <label>
                            <textarea
                                    class="form-control"
                                    [attr.disabled]="disabled || null"
                                    [(ngModel)]="answerMatrix[rowi][coli]"
                                    [ngModelOptions]="{standalone: true}"
                                    (ngModelChange)="signalUpdate()"></textarea>
                                <ng-container *ngTemplateOutlet="points"></ng-container>
                            </label>
                        </div>
                        <div *ngIf="isInputText()" [ngClass]="inputTextClass(rowi, coli)">
                            <label>
                                <input
                                        [ngClass]="getInputClass(rowi, coli, -1)"
                                        [attr.disabled]="disabled || null"
                                        [(ngModel)]="answerMatrix[rowi][coli]"
                                        [ngModelOptions]="{standalone: true}"
                                        (ngModelChange)="signalUpdate()"
                                        type="text"
                                        [size]="json.size || 3">
                                <ng-container *ngTemplateOutlet="points"></ng-container>
                            </label>
                        </div>
                    </td>
                    <td *ngIf="getExpl(rowi) as p" [innerHtml]="p | purify" class="explanation"></td>
                </tr>
                </tbody>
            </table>
        </form>
    `,
})
export class AnswerSheetComponent implements OnChanges {
    private readonly element: JQLite;
    @Input() questiondata?: IPreviewParams;
    @Input() customHeader?: string;
    json!: IAskedJsonJson; // TODO decide if undefined should be valid
    processed!: IProcessedHeaders; // TODO decide if undefined should be valid
    answerMatrix: MatrixElement[][] = [];
    private expl?: IExplCollection | null;
    private pointsTable: Array<Record<string, string>> = [];
    private defaultPoints?: number;
    userpoints?: number;
    disabled = false;
    @Output() onAnswerChange: EventEmitter<AnswerTable> = new EventEmitter();
    private customDomUpdateInProgress = false;

    constructor(element: ElementRef, private zone: NgZone) {
        this.element = $(element.nativeElement);
    }

    ngOnChanges(onChangesObj: SimpleChanges) {
        const qdata = onChangesObj.questiondata;
        const adata = onChangesObj.answertable;
        if (qdata) {
            this.createAnswer();
            // Do a fake update; without this "no answer" is not registered properly if the user never clicks
            // the answer sheet.
            this.signalUpdate();
        } else if (adata) {
            this.answerMatrix = this.answerMatrixFromTable(adata.currentValue);
        }
    }

    async ngAfterViewInit() {
        await ParCompiler.processAllMath(this.element);
    }

    getHeader() {
        return this.fixText(this.json.questionText);
    }

    fixText(s: string) {
        return fixLineBreaks(s);
    }

    getPoints(rowIndex: number, colIndex: number): string | null {
        if (!this.questiondata) {
            return null;
        }
        if (!this.questiondata.showCorrectChoices) {
            return null;
        }
        if (this.isVertical()) {
            [rowIndex, colIndex] = [colIndex, rowIndex];
        }
        // Return a non-null value as long as a point value was explicitly given in point array or defaultPoints
        if (this.pointsTable.length <= rowIndex) {
            return this.defaultPoints != null
                ? this.defaultPoints.toString()
                : null;
        }
        const rowPoints = this.pointsTable[rowIndex];
        if (!rowPoints) {
            return this.defaultPoints != null
                ? this.defaultPoints.toString()
                : null;
        }
        const idxStr = "" + (colIndex + 1);
        if (idxStr in rowPoints) {
            return rowPoints[idxStr];
        } else if (this.defaultPoints != null) {
            return this.defaultPoints.toString();
        }
        return null;
    }

    getLabelText(row: IRow): string {
        if (this.isMatrix()) {
            return "";
        }
        return row.text;
    }

    getInputClass(rowIndex: number, colIndex: number, userCheck: number) {
        const pts = this.getPoints(rowIndex, colIndex);
        let userChk = "";
        if (userCheck >= 0 && this.questiondata?.showCorrectChoices) {
            userChk = "qst-userCheck" + userCheck;
        }
        return pts != null && parseFloat(pts) > 0
            ? "qst-correct " + userChk
            : "qst-normal " + userChk;
    }

    radioClass(rowi: number, coli: number) {
        const ans = this.answerMatrix[this.isMatrix() ? rowi : 0][0];
        const lastPos = (this.isMatrix() ? coli : rowi) + 1;
        const className = this.getInputClass(
            rowi,
            coli,
            ans === lastPos ? 1 : 0
        );
        return `qst-radio ${className}`;
    }

    checkBoxClass(rowi: number, coli: number) {
        const ans = this.answerMatrix[rowi][coli];
        const className = this.getInputClass(rowi, coli, ans === 1 ? 1 : 0);
        return `qst-checkBox ${className}`;
    }

    inputTextClass(rowi: number, coli: number) {
        const className = this.getInputClass(rowi, coli, -1);
        return `qst-input-text ${className}`;
    }

    canShowExpl(): boolean {
        if (!this.questiondata) {
            return false;
        }
        return (
            this.questiondata.showExplanations &&
            this.expl != null &&
            Object.keys(this.expl).length !== 0
        );
    }

    getExpl(rowIndex: number): string | null {
        if (!this.canShowExpl() || !this.expl) {
            return null;
        }
        const key = "" + (rowIndex + 1);
        if (key in this.expl) {
            return this.expl[key];
        }
        return null;
    }

    isMatrix() {
        return (
            this.json.questionType === "matrix" ||
            this.json.questionType === "true-false"
        );
    }

    private isVertical() {
        return !this.isMatrix();
    }

    getTableClass(): string {
        let totalBorderless = true;
        const data = this.processed;
        if (
            this.hasHeaders() ||
            (data.rows.length > 1 && data.rows[0].columns.length > 1)
        ) {
            totalBorderless = false;
        }
        return totalBorderless ? "total-borderless" : "";
    }

    getTableRowClass(): string {
        if (this.isMatrix()) {
            return "qst-tr qst-matrix";
        }
        return "qst-tr";
    }

    hasHeaders(): boolean {
        const data = this.processed;
        return (
            data.headers &&
            data.headers.length > 0 &&
            !(data.headers[0].text === "" && data.headers.length === 1)
        );
    }

    getGroupName(rowIndex: number) {
        if (this.isMatrix()) {
            return "group" + rowIndex;
        } else {
            return "groupquestion";
        }
    }

    getInputValue(rowIndex: number, colIndex: number) {
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
        this.defaultPoints = params.markup.defaultPoints;
        this.expl = params.markup.expl;
        this.userpoints = params.userpoints;
    }

    questionHasPoints() {
        return this.questiondata?.markup.points != null;
    }

    answerMatrixFromTable(table: AnswerTable): MatrixElement[][] {
        const data = this.processed;
        if (data.rows.length === 0) {
            return [];
        }
        const arr: MatrixElement[][] = createArray(
            data.rows.length,
            data.rows[0].columns.length
        );
        if (this.isMatrix()) {
            if (this.isText() || this.isInputText()) {
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
                throw new Error(
                    `Expected answertable of length 1, got ${table.length}`
                );
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
                    throw new Error(
                        `Unexpected input type: ${this.json.answerFieldType}`
                    );
                }
            }
        }
        return arr;
    }

    public tableFromAnswerMatrix(matrix: MatrixElement[][]): AnswerTable {
        let table: AnswerTable = [];
        if (this.isMatrix()) {
            if (this.isText() || this.isInputText()) {
                table = matrix.map((row) =>
                    row.map((elem) => (elem || "").toString())
                );
            } else {
                for (const row of matrix) {
                    const tableRow: string[] = [];
                    let rowi = 1;
                    for (const val of row) {
                        if (val) {
                            tableRow.push(
                                "" + (this.isCheckbox() ? rowi : val)
                            );
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

    isText() {
        return this.json.answerFieldType === "text";
    }

    isInputText() {
        return this.json.answerFieldType === "inputText";
    }

    isRadio() {
        return this.json.answerFieldType === "radio";
    }

    isCheckbox() {
        return this.json.answerFieldType === "checkbox";
    }

    signalUpdate() {
        console.log("signalupdate");
        this.onAnswerChange.emit(this.tableFromAnswerMatrix(this.answerMatrix));
    }

    checkBoxChanged(rowi: number, coli: number, event: Event) {
        this.answerMatrix[rowi][coli] = (event.target as HTMLInputElement)
            .checked
            ? 1
            : 0;
        this.signalUpdate();
    }
}

@NgModule({
    declarations: [AnswerSheetComponent],
    imports: [
        CommonModule,
        HttpClientModule,
        FormsModule,
        TimUtilityModule,
        HTMLSanitizeModule,
        PurifyModule,
    ],
    exports: [AnswerSheetComponent],
})
export class AnswerSheetModule {}
