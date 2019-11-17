import angular from "angular";
import * as t from "io-ts";
import {IncludeUsersOption} from "tim/plugin/attributes";
import moment from "tim/plugin/reexports/moment";
import {showMessageDialog} from "tim/ui/dialog";
import {IUser} from "tim/user/IUser";
import {withComparatorFilters} from "tim/util/comparatorfilter";
import {$http, $timeout} from "tim/util/ngimport";
import {Binding, to} from "tim/util/utils";

export const GroupType = t.union([t.string, t.array(t.string)]);

interface IAssessment {
    completionCredits?: unknown;
    completionDate: unknown;
    gradeId: unknown;
    privateComment?: unknown;
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
    ];
}

export const Sisu = angular.module("sisuModule", []);

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

    async callSendGrades(opts: {
        completionDate?: moment.Moment,
        dryRun: boolean,
        filterUsers?: string[],
        partial: boolean,
    }) {
        if (!this.destCourse) {
            return;
        }
        this.loading = true;
        const groups: string[] | undefined = t.string.is(this.group) ? [this.group] : this.group;
        const r = await to($http.post<IGradeResponse>("/sisu/sendGrades", {
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
                    width: 200,
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
                    filter: {term: ">0"},
                },
                {
                    field: "completionDate",
                    name: "Completion date",
                    allowCellFocus: false,
                    width: 140,
                    filter: {term: "="},
                },
                {
                    field: "completionCredits",
                    name: "Credits",
                    allowCellFocus: false,
                    width: 70,
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
}

Sisu.component("sisuAssessmentExport", {
    bindings: {
        destCourse: "<",
        docId: "<",
        group: "<",
        includeUsers: "<",
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
