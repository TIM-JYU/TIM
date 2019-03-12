import {IAngularEvent, IController, IRootElementService, IScope} from "angular";
import * as allanswersctrl from "tim/answer/allAnswersController";
import {timApp} from "tim/app";
import uiGrid, {IFilterOptions, IGridColumnOf, IGridRowOf} from "ui-grid";
import {ViewCtrl} from "../document/viewctrl";
import {DialogController, registerDialogComponent, showDialog, showMessageDialog} from "../ui/dialog";
import {IUser} from "../user/IUser";
import {$http, $timeout} from "../util/ngimport";
import {Binding, getURLParameter, markAsUsed, Require, to} from "../util/utils";
import {showAllAnswers} from "./allAnswersController";
import {IRelevanceResponse} from "../item/relevanceEdit";

markAsUsed(allanswersctrl);

interface IFixedFilterOptions extends IFilterOptions {
    rawTerm?: boolean;
}

export interface IExportOptions {
    totalPointField: string;
    velpPointField: string;
    taskPointField: string;
}

function filterFn(term: string, cellValue: any, row: IGridRowOf<any>, column: IGridColumnOf<any>) {
    try {
        return new RegExp(term, "i").test(cellValue);
    } catch {
        return false;
    }
}

export class UserListController implements IController {
    private static $inject = ["$scope", "$element"];
    private gridOptions?: uiGrid.IGridOptions & {gridMenuCustomItems: any};
    private scope: IScope;
    private gridApi?: uiGrid.IGridApiOf<IUser>;
    private instantUpdate: boolean = false;
    private columns!: Array<uiGrid.IColumnDefOf<IUser>>; // $onInit
    private onUserChange!: Binding<(params: {$USER: IUser, $UPDATEALL: boolean}) => void, "&">;
    private viewctrl!: Require<ViewCtrl>;
    private element: IRootElementService;

    constructor(scope: IScope, element: IRootElementService) {
        this.scope = scope;
        this.element = element;
    }

    $onInit() {
        this.scope.$watch(
            () => this.element[0].offsetHeight + this.element[0].offsetWidth,
            (sum) => {
                const grid = this.element.find(".grid");
                grid.css("width", (this.element[0].offsetWidth - 5) + "px");
                grid.css("height", (this.element[0].offsetHeight - 30) + "px");
            },
        );

        let anyAnnotations = false;
        let smallFieldWidth = 59;

        for (let i = 0; i < this.viewctrl.users.length; ++i) {
            if (this.viewctrl.users[i].velped_task_count > 0) {
                anyAnnotations = true;
                smallFieldWidth = 40;
                break;
            }
        }

        this.columns = [
            {
                field: "real_name",
                name: "Full name",
                cellTooltip: true,
                headerTooltip: true,
            },
            {
                field: "name",
                name: "Username",
                cellTooltip: true,
                headerTooltip: true,
                maxWidth: 100,
            },
            {
                field: "task_count",
                name: "Tasks",
                cellTooltip: true,
                headerTooltip: true,
                maxWidth: smallFieldWidth,
            },
            {
                field: "task_points",
                name: "Task points",
                cellTooltip: true,
                headerTooltip: true,
                maxWidth: smallFieldWidth,
                visible: anyAnnotations,
            },
            {
                field: "velped_task_count",
                name: "Velped tasks",
                cellTooltip: true,
                headerTooltip: true,
                maxWidth: smallFieldWidth,
                visible: anyAnnotations,
            },
            {
                field: "velp_points",
                name: "Velp points",
                cellTooltip: true,
                headerTooltip: true,
                maxWidth: smallFieldWidth,
                visible: anyAnnotations,
            },
            {
                field: "total_points",
                name: "Points",
                cellTooltip: true,
                headerTooltip: true,
                maxWidth: smallFieldWidth,
            },
        ];
        for (const c of this.columns) {
            const f: IFixedFilterOptions = {
                condition: filterFn,
                rawTerm: true, // required for RegExp to work
            };
            c.filter = f;
        }
        this.instantUpdate = false;

        this.gridOptions = {
            exporterMenuPdf: false,
            multiSelect: false,
            enableFullRowSelection: true,
            enableRowHeaderSelection: false,
            noUnselect: true,
            enableFiltering: true,
            enableColumnMenus: false,
            enableGridMenu: true,
            data: this.viewctrl.users,
            enableSorting: true,
            columnDefs: this.columns,
            onRegisterApi: (gridApi) => {
                this.gridApi = gridApi;

                gridApi.selection.on.rowSelectionChanged(this.scope, (row) => {
                    this.fireUserChange(row, this.instantUpdate);
                });
                if (this.gridOptions && this.gridOptions.data) {
                    gridApi.grid.modifyRows(this.gridOptions.data as any[]);
                    gridApi.selection.selectRow(this.gridOptions.data[0]);
                    const userName = getURLParameter("user");
                    if (userName) {
                        const foundUser = this.findUserByName(userName);
                        if (foundUser) {
                            this.gridApi.selection.selectRow(foundUser);
                        } else {
                            void showMessageDialog(`User ${userName} not found from answerers.`);
                            gridApi.selection.selectRow(this.gridOptions.data[0]);
                        }
                    } else {
                        gridApi.selection.selectRow(this.gridOptions.data[0]);
                    }
                }
                gridApi.cellNav.on.navigate(this.scope, (newRowCol, oldRowCol) => {
                    gridApi.selection.selectRow(newRowCol.row.entity);
                });
            },
            gridMenuCustomItems: [
                {
                    title: "Export to Korppi",
                    action: ($event: IAngularEvent) => {
                        $timeout(async () => {
                            const options = await showKorppiExportDialog();
                            this.exportKorppi(options);
                        });
                    },
                    order: 10,
                },
                {
                    title: "Enable instant update",
                    action: ($event: IAngularEvent) => {
                        this.instantUpdate = true;
                    },
                    shown: () => {
                        return !this.instantUpdate;
                    },
                    leaveOpen: true,
                    order: 20,
                },
                {
                    title: "Disable instant update",
                    action: ($event: IAngularEvent) => {
                        this.instantUpdate = false;
                    },
                    shown: () => {
                        return this.instantUpdate;
                    },
                    leaveOpen: true,
                    order: 30,
                },
                {
                    title: "Answers as plain text",
                    action: async ($event: IAngularEvent) => {
                        await showAllAnswers({
                            url: "/allDocumentAnswersPlain/" + this.viewctrl.item.id,
                            identifier: this.viewctrl.item.id.toString(),
                            allTasks: true,
                        });
                    },
                    order: 40,
                },
                { // Feedback report output TODO: Should only be visible if page is a feedback giving test
                    title: "Feedback answer report",
                    action: async ($event: IAngularEvent) => {
                        // TODO: remove "link" later as just for demo testing
                        const minne = 'http://192.168.99.100/feedback/test';
                        window.location.assign(minne);
                        // gets r as result json of said form TODO: form change to csv when csv done
                        // const r = await to($http.get<{'answers0': string,
                        //     'taskids1': string,
                        //     'answers1': string,
                        //     'taskids2': string,
                        //     'answers2': string}>(`/feedback/test`));
                        // console.log(r.result.data);
                        // TODO: we want a showAllAnswers type selection dialog after which a "save as csv"
                    },
                    order: 50,
                },
            ],
            rowTemplate: "<div ng-dblclick=\"grid.appScope.fireUserChange(row, true)\" ng-repeat=\"(colRenderIndex, col) in colContainer.renderedColumns track by col.colDef.name\" class=\"ui-grid-cell\" ng-class=\"{ 'ui-grid-row-header-cell': col.isRowHeader }\" ui-grid-cell></div>",
        };
    }

    fireUserChange(row: uiGrid.IGridRowOf<IUser>, updateAll: boolean) {
        this.onUserChange({$USER: row.entity, $UPDATEALL: updateAll});
    }

    exportKorppi(options: IExportOptions) {
        if (!options.taskPointField && !options.velpPointField && !options.totalPointField) {
            return;
        }
        if (!this.gridApi) {
            throw new Error("gridApi was not initialized");
        }
        const data = this.gridApi.core.getVisibleRows(this.gridApi.grid);
        let dataKorppi = "";

        const fields = ["total_points", "task_points", "velp_points"];
        const fieldNames: {[index: string]: string} = {};
        fieldNames[fields[0]] = options.totalPointField;
        fieldNames[fields[1]] = options.taskPointField;
        fieldNames[fields[2]] = options.velpPointField;
        let filename;
        for (let i = 0; i < fields.length; ++i) {
            const fieldName = fieldNames[fields[i]];
            if (fieldName) {
                filename = (filename || fieldName + ".txt");
                if (dataKorppi !== "") {
                    dataKorppi += "\n";
                }
                for (let j = 0; j < data.length; j++) {
                    const entity = data[j].entity as any;
                    if (entity[fields[i]] != null) {
                        dataKorppi += entity.name + ";" + fieldName + ";" + entity[fields[i]] + "\n";
                    }
                }
            }
        }

        if (!filename) {
            filename = "korppi_" + this.viewctrl.docId + ".txt";
        }
        // from https://stackoverflow.com/a/33542499
        const blob = new Blob([dataKorppi], {type: "text/plain"});
        if (window.navigator.msSaveOrOpenBlob) {
            window.navigator.msSaveBlob(blob, filename);
        } else {
            const elem = window.document.createElement("a");
            elem.href = window.URL.createObjectURL(blob);
            elem.download = filename;
            document.body.appendChild(elem);
            elem.click();
            document.body.removeChild(elem);
        }
    }

    private findUserByName(userName: string) {
        return this.viewctrl.users.find((u) => u.name === userName);
    }
}

timApp.component("timUserList", {
    bindings: {
        onUserChange: "&",
        users: "<", // unused?
    },
    controller: UserListController,
    require: {
        viewctrl: "^timView",
    },
    template: `<div
     class="userlist"
     ng-if="$ctrl.users"
     ui-grid="$ctrl.gridOptions"
     ui-grid-selection
     ui-grid-exporter
     ui-grid-auto-resize
     ui-grid-cellNav>
</div>
<div ng-if="!$ctrl.users">
    No answerers.
</div>`,
});

export class KorppiExportCtrl extends DialogController<{}, IExportOptions, "timKorppiExport"> {
    private static $inject = ["$element", "$scope"];
    private options: IExportOptions = {totalPointField: "", velpPointField: "", taskPointField: ""};

    protected getTitle() {
        return "Export to Korppi";
    }

    ok() {
        this.close(this.options);
    }
}

registerDialogComponent("timKorppiExport", KorppiExportCtrl, {templateUrl: "/static/templates/korppiExport.html"});

function showKorppiExportDialog() {
    return showDialog<KorppiExportCtrl>("timKorppiExport", {}).result;
}
