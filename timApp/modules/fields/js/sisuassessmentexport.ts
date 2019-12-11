import angular from "angular";
import * as t from "io-ts";
import {IncludeUsersOption} from "tim/plugin/attributes";
import moment from "tim/plugin/reexports/moment";
import {showMessageDialog} from "tim/ui/dialog";
import {IUser} from "tim/user/IUser";
import {withComparatorFilters} from "tim/util/comparatorfilter";
import {$http, $timeout} from "tim/util/ngimport";
import {Binding, copyToClipboard, StringOrNumber, to} from "tim/util/utils";

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

export const Sisu = angular.module("sisuModule", []);

async function getFakeData(opts: ISendGradeOptions): Promise<{data: IGradeResponse}> {
    await $timeout(500);
    const d = opts.completionDate;
    return {
        data: {
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
            ].filter((u) => opts.filterUsers === undefined || opts.filterUsers.includes(u.user.name))
                .map((u) => (d ? {
                    ...u,
                    completionDate: d.format("YYYY-MM-DD"),
                } : u)),
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
            ].filter((u) => opts.filterUsers === undefined || opts.filterUsers.includes(u.assessment.user.name)),
        },
    };
}

interface ISendGradeOptions {
    completionDate?: moment.Moment;
    dryRun: boolean;
    filterUsers?: string[];
    partial: boolean;
}

function sortAssessments(a: IAssessmentExt, b: IAssessmentExt): number {
    return (a.user.real_name || "").localeCompare(b.user.real_name || "", "fi");
}

const failGrades = new Set(["0", "HYL"]);

class SisuAssessmentExportController {
    private assessments?: IAssessmentExt[];
    private gridOptions?: uiGrid.IGridOptionsOf<IAssessmentExt>;
    private dateOptions: EonasdanBootstrapDatetimepicker.SetOptions = {
        format: "D.M.YYYY",
        defaultDate: moment(),
        allowInputToggle: true,
    };
    private completionDate?: moment.Moment;
    private grid?: uiGrid.IGridApiOf<IAssessmentExt>;
    private okAssessments?: number;
    private errAssessments?: number;
    private loading: boolean = false;
    private destCourse!: Binding<string, "<">;
    private docId!: Binding<number, "<">;
    private group?: Binding<t.TypeOf<typeof GroupType>, "<">;
    private includeUsers?: Binding<t.TypeOf<typeof IncludeUsersOption>, "<">;
    private testOnly?: Binding<boolean, "<">;
    private notSendableButChanged?: IAssessmentExt[];
    private notSendable?: IAssessmentExt[];

    async callSendGrades(opts: ISendGradeOptions) {
        if (!this.destCourse) {
            return;
        }
        this.loading = true;
        const groups: string[] | undefined = t.string.is(this.group) ? [this.group] : this.group;
        const r = this.testOnly ? await to(getFakeData(opts)) : await to($http.post<IGradeResponse>("/sisu/sendGrades", {
            completionDate: opts.completionDate,
            destCourse: this.destCourse,
            docId: this.docId,
            dryRun: opts.dryRun,
            filterUsers: opts.filterUsers,
            groups: groups,
            partial: opts.partial,
            includeUsers: this.includeUsers,
        }));
        this.loading = false;
        if (!r.ok) {
            await showMessageDialog(r.result.data.error);
            return;
        }
        return r.result.data;
    }

    async sendAssessments() {
        if (!this.grid || !this.assessments) {
            return;
        }
        const data = await this.callSendGrades({
            partial: true,
            dryRun: false,
            completionDate: this.completionDate,
            filterUsers: this.grid.selection.getSelectedRows().map((r) => r.user.name),
        });
        if (!data) {
            return;
        }
        this.okAssessments = data.sent_assessments.length;
        this.errAssessments = data.assessment_errors.length;
        const all = getAssessments(data);
        const indexMap = new Map<string, number>();
        this.assessments.forEach((a, i) => indexMap.set(a.user.name, i));
        const state = this.grid.saveState.save();
        for (const a of all) {
            const index = indexMap.get(a.user.name);
            if (index !== undefined) {
                this.assessments[index] = a;
            } else {
                console.warn(`sendAssessments returned a user that did not exist in preview: ${a.user.name}`);
            }
        }
        await $timeout();
        this.grid.saveState.restore(this.grid.grid.appScope!, state);
    }

    numSelectedAssessments() {
        if (!this.grid) {
            return 0;
        }
        return this.grid.selection.getSelectedRows().length;
    }

    async previewAssessments() {
        this.assessments = undefined;
        const data = await this.callSendGrades({partial: true, dryRun: true});
        if (!data) {
            return;
        }
        this.assessments = getAssessments(data);
        const defaults = new Set(data.default_selection);
        const hasNumericGrades = this.assessments.find(
            (a) => t.number.is(a.gradeId) || (t.string.is(a.gradeId) && !isNaN(parseInt(a.gradeId, 10))),
        );
        const hasNonNumericGrades = this.assessments.find(
            (a) => t.string.is(a.gradeId) && a.gradeId === "HYV",
        );
        let defaultFilter = ">0";
        if (hasNonNumericGrades) {
            defaultFilter = "HYV";
            if (hasNumericGrades) {
                defaultFilter = ".";
            }
        }
        const gradeHasChanged = (a: IAssessmentExt) => StringOrNumber.is(a.sentGrade) && StringOrNumber.is(a.gradeId) && a.sentGrade && a.gradeId && a.sentGrade.toString() !== a.gradeId.toString();
        const changedGrades = this.assessments.filter(
            gradeHasChanged,
        );
        const hasChangedGrades = changedGrades.length > 0;
        const creditHasChanged = (a: IAssessmentExt) =>
            (StringOrNumber.is(a.sentCredit) &&
                StringOrNumber.is(a.completionCredits) &&
                a.sentCredit && a.completionCredits &&
                a.sentCredit.toString() !== a.completionCredits.toString());
        const changedCredits = this.assessments.filter(
            creditHasChanged,
        );
        const hasChangedCredits = changedCredits.length > 0;
        const alreadyConfirmed = (a: IAssessmentExt) => a.error && a.error === "Sisu: Aikaisempi vahvistettu suoritus";
        this.notSendableButChanged = this.assessments.filter(
            (a) => alreadyConfirmed(a) &&
                (gradeHasChanged(a) ||
                    creditHasChanged(a) ||
                    a.sentGrade == null
                )
        ).filter((a) => StringOrNumber.is(a.gradeId) && !failGrades.has(a.gradeId.toString()));
        this.notSendable = this.assessments.filter(alreadyConfirmed);
        this.gridOptions = {
            onRegisterApi: async (grid) => {
                this.grid = grid;
                await $timeout();
                for (const row of grid.core.getVisibleRows(grid.grid)) {
                    if (defaults.has(row.entity.user.id)) {
                        row.setSelected(true);
                    }
                }
            },
            columnDefs: withComparatorFilters([
                {
                    field: "user.real_name",
                    name: "Full name",
                    allowCellFocus: false,
                    width: 160,
                },
                {
                    field: "user.name",
                    name: "Username",
                    allowCellFocus: false,
                    width: 140,
                },
                {
                    field: "gradeId",
                    name: "Grade",
                    allowCellFocus: false,
                    width: 65,
                    filter: {term: defaultFilter},
                },
                {
                    field: "sentGrade",
                    name: "Old g",
                    allowCellFocus: false,
                    width: 65,
                    visible: hasChangedGrades,
                },
                {
                    field: "completionDate",
                    name: "Compl. date",
                    allowCellFocus: false,
                    width: 105,
                    filter: {term: ""}, // The grade or credit may have changed, so we cannot have any filter here!
                },
                {
                    field: "completionCredits",
                    name: "Credits",
                    allowCellFocus: false,
                    width: 70,
                },
                {
                    field: "sentCredit",
                    name: "Old c",
                    allowCellFocus: false,
                    width: 65,
                    visible: hasChangedCredits,
                },
                {
                    field: "privateComment",
                    name: "Comment",
                    allowCellFocus: false,
                    cellTooltip: true,
                    visible: false,
                },
                {
                    field: "error",
                    name: "Error?",
                    allowCellFocus: false,
                    sort: {direction: "asc"},
                    cellTooltip: true,
                },
            ]),
            isRowSelectable: (row) => {
                const a = (row as unknown as uiGrid.IGridRowOf<IAssessmentExt>).entity;
                return a.gradeId != null;
            },
            data: this.assessments,
            enableColumnMenus: false,
            enableFiltering: true,
            enableFullRowSelection: false,
            enableGridMenu: true,
            enableHorizontalScrollbar: false,
            enableSorting: true,
        };
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
            s += `${a.user.real_name};${a.user.name};${a.gradeId}\n`;
        }
        copyToClipboard(s);
        void showMessageDialog("CSV copied to clipboard.");
    }
}

Sisu.component("sisuAssessmentExport", {
    bindings: {
        destCourse: "<",
        docId: "<",
        group: "<",
        includeUsers: "<",
        testOnly: "<",
    },
    controller: SisuAssessmentExportController,
    template: `
<div>
    <button class="timButton"
            ng-disabled="$ctrl.loading"
            ng-click="$ctrl.previewAssessments()">
        Esikatsele arviointeja
    </button>
    <tim-loading ng-if="$ctrl.loading && !$ctrl.assessments"></tim-loading>
</div>
<div ng-if="$ctrl.assessments">
    <p>
    <span class="red">
        Arviointien lähettäminen Sisuun tyhjentää Sisusta KAIKKI vahvistamattomat arvioinnit (myös ne, joita ei
        tässä lähetyksessä lähetetä).
        Tästä syystä uutta Sisuun lähetystä EI SAA tehdä ennen kuin aikaisemmin lähetetyt on Sisussa vahvistettu.
        Sisussa vahvistettua arvosanaa ei voi enää muuttaa (ei edes hylättyjä).
    </span>
    </p>
    <p>Taulukosta voi valita lähetettäväksi vain niitä arviointeja, joissa on arvosana.</p>
    <p ng-if="$ctrl.testOnly"><i>Tämä plugin on vain demo. Arvosanojen lähettäminen ei oikeasti tee mitään.</i></p>
    Suorituspäivä:
    <div class="input-group date" datetimepicker ng-model="$ctrl.completionDate"
         data-options="$ctrl.dateOptions">
        <input type="text"
               class="form-control"/>
        <span class="input-group-addon">
                    <span class="glyphicon glyphicon-calendar"></span>
                    </span>
    </div>
    <div style="font-size: small"
         ui-grid="$ctrl.gridOptions"
         ui-grid-selection
         ui-grid-save-state
         ui-grid-auto-resize
         ui-grid-cellNav>
    </div>
    <p>{{ $ctrl.numSelectedAssessments() }} arviointia valittu.</p>
    <p class="red" ng-if="$ctrl.notSendableButChanged.length > 0">
        Taulukossa on {{$ctrl.notSendableButChanged.length}} kpl arviointeja, joiden arvosana on muuttunut
        (tai joita ei ole TIMistä vielä lähetetty Sisuun) mutta jotka on jo vahvistettu Sisussa.
        Näitä ei voi päivittää Sisun kautta, mutta voit ottaa
        <a ng-click="$ctrl.copyNotSendableButChanged()">tästä CSV-tiedoston</a> ja pyytää kansliaa päivittämään
        kyseiset arvioinnit.
    </p>
<!--    <p class="red" ng-if="$ctrl.notSendable.length > 0">-->
<!--        Taulukossa on yhteensä {{$ctrl.notSendable.length}} kpl arviointeja, jotka on jo vahvistettu Sisussa.-->
<!--        Voit ladata listan näistä <a ng-click="$ctrl.copyNotSendable()">tästä</a>.-->
<!--    </p>-->
    <button class="timButton"
            ng-disabled="$ctrl.loading || $ctrl.numSelectedAssessments() === 0"
            ng-click="$ctrl.sendAssessments()">
        Lähetä valitut Sisuun
    </button>
    <tim-loading ng-if="$ctrl.loading"></tim-loading>
    <p ng-if="$ctrl.okAssessments != null && !$ctrl.loading">
        {{ $ctrl.okAssessments }} arviointia lähetettiin Sisuun.
        <span ng-if="$ctrl.errAssessments > 0">{{ $ctrl.errAssessments }} virheellistä arviointia torjuttiin.</span>
        Voit vahvistaa arvioinnit
        <a href="https://sisu.jyu.fi/teacher/role/teacher/teaching/course-unit-realisations/view/{{$ctrl.destCourse}}/evaluation/verification">
        Sisussa</a>.
    </p>
</div>
    `,
});
