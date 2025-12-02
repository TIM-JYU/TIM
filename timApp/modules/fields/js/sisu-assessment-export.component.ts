import * as t from "io-ts";
import {Component, Input, NgModule, ViewChild} from "@angular/core";
import type {IncludeUsersOption} from "tim/plugin/attributes";
import type {IUser} from "tim/user/IUser";
import {sortLang} from "tim/user/IUser";
import {
    copyToClipboard,
    StringOrNumber,
    timeout,
    to2,
    toPromise,
} from "tim/util/utils";
import moment from "moment";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {DataViewModule} from "tim/plugin/dataview/data-view.module";
import type {DataModelProvider} from "tim/plugin/dataview/data-view.component";
import {DataViewComponent} from "tim/plugin/dataview/data-view.component";
import {BsDatepickerModule, BsLocaleService} from "ngx-bootstrap/datepicker";
import {defineLocale} from "ngx-bootstrap/chronos";
import {fiLocale} from "ngx-bootstrap/locale";
import {computeHiddenRowsFromFilters} from "tim/plugin/timTable/filtering";
import {CommonModule} from "@angular/common";

export const GroupType = t.union([t.string, t.array(t.string)]);

interface IAssessment {
    completionCredits?: unknown;
    completionDate: unknown;
    gradeId: unknown;
    privateComment?: unknown;
    sentGrade?: unknown;
    sentCredit?: unknown;
    user: IUser;
}

interface IAssessmentError {
    message: string;
    assessment: IAssessment;
}

interface IAssessmentExt extends IAssessment {
    error?: string;
}

interface IGradeResponse {
    sent_assessments: IAssessment[];
    assessment_errors: IAssessmentError[];
    default_selection: number[];
}

function getAssessments(data: IGradeResponse) {
    return [
        ...data.sent_assessments,
        ...data.assessment_errors.map((a) => ({
            ...a.assessment,

            // Don't show error message for missing grades because it's obvious.
            // But don't set it to null either so that the isRowSelectable check still works.
            error: a.assessment.gradeId ? a.message : "",
        })),
    ].sort(sortAssessments);
}

async function getFakeData(opts: ISendGradeOptions): Promise<IGradeResponse> {
    await timeout(500);
    const d = opts.completionDate;
    return {
        sent_assessments: [
            {
                completionDate: "",
                gradeId: "4",
                user: {
                    id: -1,
                    name: "akuankka1",
                    real_name: "Ankka Aku 1",
                    email: "akuankka1@example.com",
                },
            },
            {
                completionDate: "",
                gradeId: "3",
                user: {
                    id: -2,
                    name: "akuankka2",
                    real_name: "Ankka Aku 2",
                    email: "akuankka2@example.com",
                },
            },
            {
                completionDate: "",
                gradeId: null,
                user: {
                    id: -3,
                    name: "akuankka3",
                    real_name: "Ankka Aku 3",
                    email: "akuankka3@example.com",
                },
            },
        ]
            .filter(
                (u) =>
                    opts.filterUsers === undefined ||
                    opts.filterUsers.includes(u.user.name)
            )
            .map((u) =>
                d
                    ? {
                          ...u,
                          completionDate: moment(d).format("YYYY-MM-DD"),
                      }
                    : u
            ),
        default_selection: opts.dryRun ? [-1, -2] : [],
        assessment_errors: [
            {
                assessment: {
                    completionDate: "",
                    gradeId: "0",
                    user: {
                        id: -4,
                        name: "akuankka4",
                        real_name: "Ankka Aku 4",
                        email: "akuankka4@example.com",
                    },
                },
                message: "Sisu: Ilmoittautumista toteutukseen ei löytynyt",
            },
        ].filter(
            (u) =>
                opts.filterUsers === undefined ||
                opts.filterUsers.includes(u.assessment.user.name)
        ),
    };
}

interface ISendGradeOptions {
    completionDate?: Date;
    dryRun: boolean;
    filterUsers?: string[];
    partial: boolean;
}

function sortAssessments(a: IAssessmentExt, b: IAssessmentExt): number {
    return (a.user.real_name ?? "").localeCompare(
        b.user.real_name ?? "",
        sortLang
    );
}

const failGrades = new Set(["0", "HYL"]);

function unknownToStr(x: unknown) {
    if (StringOrNumber.is(x)) {
        return x.toString();
    }
    if (x == null) {
        return "";
    }
    return "invalid type";
}

const colGetters: ((a: IAssessmentExt) => string)[] = [
    (a) => a.user.real_name ?? "",
    (a) => a.user.name,
    (a) => unknownToStr(a.gradeId),
    (a) => unknownToStr(a.sentGrade),
    (a) => unknownToStr(a.completionDate),
    (a) => unknownToStr(a.completionCredits),
    (a) => unknownToStr(a.sentCredit),
    (a) => unknownToStr(a.privateComment),
    (a) => a.error ?? "",
];

// TODO(refactor): Extract all generic stuff to a base class.
class AssessmentTableModel implements DataModelProvider {
    private hiddenRows = new Set<number>();
    private hiddenCols = new Set<number>();
    readonly checkedRows = new Set<number>();
    private readonly colFilters: string[][];
    private selectedFilter = false;

    constructor(
        public assessments: IAssessmentExt[],
        private getDataView: () => DataViewComponent | undefined
    ) {
        this.colFilters = [Array(colGetters.length).fill("")];
    }

    classForCell(_rowIndex: number, _columnIndex: number): string {
        return "";
    }

    getCellContents(rowIndex: number, columnIndex: number): string {
        return colGetters[columnIndex](this.assessments[rowIndex]);
    }

    getColumnHeaderContents(columnIndex: number): string {
        switch (columnIndex) {
            case 0:
                return "Full name";
            case 1:
                return "Username";
            case 2:
                return "Grade";
            case 3:
                return "Old g";
            case 4:
                return "Compl. date";
            case 5:
                return "Credit";
            case 6:
                return "Old c";
            case 7:
                return "Comment";
            case 8:
                return "Error?";
            default:
                throw Error(`unexpected column index: ${columnIndex}`);
        }
    }

    getColumnWidth(_columnIndex: number): [number, boolean] {
        return [0, true];
    }

    getDimension(): {rows: number; columns: number} {
        return {columns: colGetters.length, rows: this.assessments.length};
    }

    getRowContents(rowIndex: number): string[] {
        return Array.of(0, 1, 2, 3, 4, 5, 6, 7, 8).map((n) =>
            this.getCellContents(rowIndex, n)
        );
    }

    getRowHeight(_rowIndex: number): number | undefined {
        return undefined;
    }

    getSortSymbolInfo(_columnIndex: number): {
        symbol: string;
        style: Record<string, string>;
    } {
        return {style: {}, symbol: ""};
    }

    handleChangeCheckbox(_rowIndex: number): void {
        this.getDataView()?.updateVisible();
        this.getDataView()?.updateAllSelected();
    }

    handleChangeFilter() {
        this.hiddenRows = computeHiddenRowsFromFilters(
            this.assessments,
            (i) => this.isRowChecked(i),
            this.colFilters,
            this.selectedFilter,
            (r, i) => colGetters[i](r)
        );
        this.getDataView()?.updateVisible();
        this.getDataView()?.updateAllSelected();
    }

    async handleClickCell(_rowIndex: number, _columnIndex: number) {}

    clearFilters(): void {}

    handleClickHeader(_columnIndex: number): void {}

    isPreview(): boolean {
        return false;
    }

    isRowChecked(rowIndex: number): boolean {
        return this.checkedRows.has(rowIndex);
    }

    setRowChecked(rowIndex: number, checked: boolean): void {
        if (!this.isRowSelectable(rowIndex)) {
            return;
        }
        this.doSetRowChecked(rowIndex, checked);

        // We need to hide the row if it got unchecked and the selected filter is enabled.
        if (!checked && this.selectedFilter) {
            this.handleChangeFilter();
        }
    }

    private doSetRowChecked(rowIndex: number, checked: boolean) {
        if (checked) {
            this.checkedRows.add(rowIndex);
        } else {
            this.checkedRows.delete(rowIndex);
        }
    }

    isRowSelectable(rowIndex: number) {
        return this.assessments[rowIndex].gradeId != null;
    }

    setRowFilter(
        filterRowIndex: number,
        columnIndex: number,
        value: string
    ): void {
        this.colFilters[filterRowIndex][columnIndex] = value;
    }

    getRowFilter(filterRowIndex: number, columnIndex: number): string {
        return this.colFilters[filterRowIndex][columnIndex] ?? "";
    }

    getFilterRowCount(): number {
        return this.colFilters.length;
    }

    async addFilterRow() {}

    async deleteFilterRow() {}

    setSelectAll(state: boolean): void {
        for (let i = 0; i < this.assessments.length; ++i) {
            if (this.isRowSelectable(i) && this.showRow(i)) {
                this.doSetRowChecked(i, state);
            }
        }
        this.handleChangeFilter();
    }

    setSelectedFilter(state: boolean): void {
        this.selectedFilter = state;
    }

    showColumn(colIndex: number): boolean {
        return !this.hiddenCols.has(colIndex);
    }

    showRow(rowIndex: number): boolean {
        return !this.hiddenRows.has(rowIndex);
    }

    stylingForCell(
        _rowIndex: number,
        _columnIndex: number
    ): Record<string, string> {
        return {};
    }

    stylingForRow(_rowIndex: number): Record<string, string> {
        return {};
    }

    getSelectedRows() {
        return this.assessments.filter((_a, i) => this.isRowChecked(i));
    }

    setShowColumn(col: number, show: boolean) {
        if (show) {
            this.hiddenCols.delete(col);
        } else {
            this.hiddenCols.add(col);
        }
    }
}

// TODO virtual scrolling mode does not seem to work for dataview in this use case.
//  Specific bugs include at least:
//  * The method getCellContents(...) is called with undefined rowIndex, causing an exception
//  * Giving explicit error column size causes another exception inside dataview (in method getHeaderColumnWidth):
//     TypeError: cache.getCell(...) is undefined

// The noWrap="true" is a workaround for a Firefox issue with dataview.
// The issue can (sometimes) be reproduced as follows:
//
//  * Ensure the table has at least one row with an error message.
//  * Type something to some column filter so that the table becomes empty.
//  * Clear the column filter.
//
// Assuming the bug gets reproduced, the error column becomes slightly narrower than what it was before
// filtering, and word wrapping occurs in at least one of the error cells. That means that the row
// height gets larger, but the corresponding checkbox row height does not get updated, so the checkboxes
// seem to be visually out of sync with the data. (The row numbers and checkboxes are a separate table
// element in dataview component.)
@Component({
    selector: "tim-sisu-assessment-export",
    template: `
        <div>
            <button class="timButton"
                    [disabled]="loading"
                    (click)="togglePreviewAssessments()">
                {{ model ? 'Sulje esikatselu' : 'Esikatsele arviointeja' }}
            </button>
            <tim-loading *ngIf="loading && !model"></tim-loading>
        </div>
        <div *ngIf="model">
            <p>
    <span class="red">
        Sisussa vahvistettua arvosanaa ei voi enää muuttaa (ei edes hylättyjä).
    </span>
            </p>
            <p>Taulukosta voi valita lähetettäväksi vain niitä arviointeja, joissa on arvosana.</p>
            <p *ngIf="testOnly"><i>Tämä plugin on vain demo. Arvosanojen lähettäminen ei oikeasti tee mitään.</i></p>
            <!--suppress TypeScriptValidateTypes -->
            <tim-data-view [modelProvider]="model"
                           [selectedIndices]="model.checkedRows //noinspection UnresolvedReference"
                           [headerStyle]="{backgroundColor: 'rgb(240, 240, 240)', fontWeight: 'bold', whiteSpace: 'nowrap'}"
                           [tableStyle]="{fontSize: 'smaller'}"
                           [virtualScrolling]="{enabled: false}"
                           tableMaxHeight="60vh"
                           [noWrap]="true"
                           [cbFilter]="initialSelectedFilter">
            </tim-data-view>
            <p>{{ numSelectedAssessments() }} arviointia valittu.</p>
            <!--suppress TypeScriptUnresolvedReference -->
            <p class="red" *ngIf="notSendableButChanged && notSendableButChanged.length > 0">
                Taulukossa on {{ notSendableButChanged.length }} kpl arviointeja, joiden arvosana tai
                opintopistemäärä on
                muuttunut
                (tai joita ei ole TIMistä vielä lähetetty Sisuun) mutta jotka on jo vahvistettu Sisussa.
                Näitä ei voi päivittää Sisun kautta, mutta voit ottaa
                <a (click)="copyNotSendableButChanged()">tästä CSV-tiedoston</a> ja pyytää kansliaa päivittämään
                kyseiset arvioinnit.
            </p>
            <div class="form-inline">
                Suorituspäivä:
                <input type="text"
                       class="form-control"
                       bsDatepicker
                       [(bsValue)]="completionDate">
                &ngsp;
                <button class="timButton"
                        [disabled]="loading || numSelectedAssessments() === 0"
                        (click)="sendAssessments()">
                    Lähetä valitut Sisuun
                </button>
            </div>
            <tim-loading *ngIf="loading"></tim-loading>
            <ng-container *ngIf="okAssessments != null && !loading">
                <p>
                    {{ okAssessments }} arviointia lähetettiin Sisuun.
                    <span *ngIf="errAssessments != null && errAssessments > 0">{{ errAssessments }} virheellistä arviointia torjuttiin.</span>
                </p>
                <p>
                    Käy tarkistamassa ja vahvistamassa arvioinnit kurssin <a
                        href="https://sisu.jyu.fi/teacher/role/teacher/teaching/course-unit-realisations/view/{{destCourse}}/ng-evaluation/confirmation">Tarkista
                    ja vahvista</a> -näkymästä.
                </p>
            </ng-container>
        </div>
    `,
})
export class SisuAssessmentExportComponent {
    completionDate = new Date();
    okAssessments?: number;
    errAssessments?: number;
    loading: boolean = false;
    @Input() destCourse!: string;
    @Input() docId!: number;
    @Input() group?: t.TypeOf<typeof GroupType>;
    @Input() includeUsers?: t.TypeOf<typeof IncludeUsersOption>;
    @Input() testOnly?: boolean;
    @ViewChild(DataViewComponent) dataView?: DataViewComponent;
    notSendableButChanged?: IAssessmentExt[];
    notSendable?: IAssessmentExt[];
    model?: AssessmentTableModel;
    initialSelectedFilter = false;

    constructor(
        private http: HttpClient,
        private localeService: BsLocaleService
    ) {
        defineLocale("fi", fiLocale);
        localeService.use("fi");
    }

    async callSendGrades(opts: ISendGradeOptions) {
        if (!this.destCourse) {
            return;
        }
        this.loading = true;
        const groups: string[] | undefined = t.string.is(this.group)
            ? [this.group]
            : this.group;
        const r = this.testOnly
            ? await to2(getFakeData(opts))
            : await toPromise(
                  this.http.post<IGradeResponse>("/sisu/sendGrades", {
                      completionDate: opts.completionDate,
                      destCourse: this.destCourse,
                      docId: this.docId,
                      dryRun: opts.dryRun,
                      filterUsers: opts.filterUsers,
                      groups: groups,
                      partial: opts.partial,
                      includeUsers: this.includeUsers,
                  })
              );
        this.loading = false;
        if (!r.ok) {
            await showMessageDialog(r.result.error.error);
            return;
        }
        return r.result;
    }

    async sendAssessments() {
        if (!this.model) {
            return;
        }
        const data = await this.callSendGrades({
            partial: true,
            dryRun: false,
            completionDate: this.completionDate,
            filterUsers: this.model.getSelectedRows().map((r) => r.user.name),
        });
        if (!data) {
            return;
        }
        this.okAssessments = data.sent_assessments.length;
        this.errAssessments = data.assessment_errors.length;
        const all = getAssessments(data);
        const indexMap = new Map<string, number>();
        this.model.assessments.forEach((a, i) => indexMap.set(a.user.name, i));
        for (const a of all) {
            const index = indexMap.get(a.user.name);
            if (index !== undefined) {
                this.model.assessments[index] = a;
            } else {
                console.warn(
                    `sendAssessments returned a user that did not exist in preview: ${a.user.name}`
                );
            }
        }
        this.dataView!.endReset();
    }

    numSelectedAssessments() {
        if (!this.model) {
            return 0;
        }
        return this.model.getSelectedRows().length;
    }

    async togglePreviewAssessments() {
        if (this.model) {
            this.model = undefined;
            return;
        }
        const data = await this.callSendGrades({partial: true, dryRun: true});
        if (!data) {
            return;
        }
        const assessments = getAssessments(data);
        const defaults = new Set(data.default_selection);
        const gradeHasChanged = (a: IAssessmentExt) =>
            StringOrNumber.is(a.sentGrade) &&
            StringOrNumber.is(a.gradeId) &&
            a.sentGrade &&
            a.gradeId &&
            a.sentGrade.toString() !== a.gradeId.toString();
        const changedGrades = assessments.filter(gradeHasChanged);
        const hasChangedGrades = changedGrades.length > 0;
        const creditHasChanged = (a: IAssessmentExt) =>
            StringOrNumber.is(a.sentCredit) &&
            StringOrNumber.is(a.completionCredits) &&
            a.sentCredit &&
            a.completionCredits &&
            a.sentCredit.toString() !== a.completionCredits.toString();
        const changedCredits = assessments.filter(creditHasChanged);
        const hasChangedCredits = changedCredits.length > 0;
        const alreadyConfirmed = (a: IAssessmentExt) =>
            a.error?.startsWith("Sisu: Aikaisempi vahvistettu suoritus");
        this.notSendableButChanged = assessments
            .filter(
                (a) =>
                    alreadyConfirmed(a) &&
                    (gradeHasChanged(a) ||
                        creditHasChanged(a) ||
                        a.sentGrade == null)
            )
            .filter(
                (a) =>
                    StringOrNumber.is(a.gradeId) &&
                    !failGrades.has(a.gradeId.toString())
            );
        this.notSendable = assessments.filter(alreadyConfirmed);

        this.model = new AssessmentTableModel(
            assessments,
            () => this.dataView!
        );
        const indexMap = assessments.reduce(
            (p, c, i) => p.set(c.user.id, i),
            new Map<number, number>()
        );
        for (const d of defaults) {
            const i = indexMap.get(d);
            if (i != null) {
                this.model.checkedRows.add(i);
            }
        }
        this.initialSelectedFilter = this.model.checkedRows.size > 0;
        this.model.setShowColumn(3, hasChangedGrades);
        this.model.setShowColumn(6, hasChangedCredits);
        this.model.setShowColumn(7, false);
    }

    copyNotSendableButChanged() {
        if (!this.notSendableButChanged) {
            return;
        }
        this.copyListToClipboard(this.notSendableButChanged);
    }

    copyNotSendable() {
        if (!this.notSendable) {
            return;
        }
        this.copyListToClipboard(this.notSendable);
    }

    copyListToClipboard(list: readonly IAssessmentExt[]) {
        let s = "real_name;username;grade\n";
        for (const a of [...list].sort(sortAssessments)) {
            const gradeToPrint = StringOrNumber.is(a.gradeId) ? a.gradeId : "";
            s += `${a.user.real_name ?? "(null)"};${
                a.user.name
            };${gradeToPrint}\n`;
        }
        copyToClipboard(s);
        void showMessageDialog("CSV copied to clipboard.");
    }

    protected readonly unknownToStr = unknownToStr;
}

@NgModule({
    declarations: [SisuAssessmentExportComponent],
    imports: [
        CommonModule,
        TimUtilityModule,
        HttpClientModule,
        BsDatepickerModule.forRoot(),
        DataViewModule,
    ],
    exports: [SisuAssessmentExportComponent],
})
export class SisuAssessmentExportModule {}
