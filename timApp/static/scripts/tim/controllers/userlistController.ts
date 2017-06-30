import {IRootElementService, IScope} from "angular";
import {timApp} from "tim/app";
import {$timeout, $uibModal} from "../ngimport";

interface IUser {
    name: string;
    velped_task_count: number;
}

export class UserListController {
    private static $inject = ["$scope", "$element"];
    private gridOptions: uiGrid.IGridOptions & {gridMenuCustomItems: any};
    private scope: IScope;
    private gridApi: uiGrid.IGridApiOf<IUser>;
    private instantUpdate: boolean;
    private users: IUser[];
    private docId: number;
    private columns: Array<uiGrid.IColumnDefOf<IUser>>;
    private onUserChange: (user: IUser, updateAll: boolean) => void;

    constructor(scope: IScope, element: IRootElementService) {
        this.scope = scope;
        scope.$watch(
            () => element[0].offsetHeight + element[0].offsetWidth,
            (sum) => {
                const grid = element.find(".grid");
                grid.css("width", (element[0].offsetWidth - 5) + "px");
                grid.css("height", (element[0].offsetHeight - 30) + "px");
            },
        );

        let anyAnnotations = false;
        let smallFieldWidth = 59;

        for (let i = 0; i < this.users.length; ++i) {
            if (this.users[i].velped_task_count > 0) {
                anyAnnotations = true;
                smallFieldWidth = 40;
                break;
            }
        }

        this.columns = [
            {field: "real_name", name: "Full name", cellTooltip: true, headerTooltip: true},
            {field: "name", name: "Username", cellTooltip: true, headerTooltip: true, maxWidth: 100},
            {field: "task_count", name: "Tasks", cellTooltip: true, headerTooltip: true, maxWidth: smallFieldWidth},
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
            {field: "total_points", name: "Points", cellTooltip: true, headerTooltip: true, maxWidth: smallFieldWidth},
        ];
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
            data: this.users,
            enableSorting: true,
            columnDefs: this.columns,
            onRegisterApi: (gridApi) => {
                this.gridApi = gridApi;

                gridApi.selection.on.rowSelectionChanged(scope, (row) => {
                    this.fireUserChange(row, this.instantUpdate);
                });
                gridApi.grid.modifyRows(this.gridOptions.data as any[]);
                gridApi.selection.selectRow(this.gridOptions.data[0]);
                gridApi.cellNav.on.navigate(scope, (newRowCol, oldRowCol) => {
                    this.gridApi.selection.selectRow(newRowCol.row.entity);
                });
            },
            gridMenuCustomItems: [
                {
                    title: "Export to Korppi",
                    action($event) {
                        $timeout(() => {
                            const instance = $uibModal.open({
                                animation: false,
                                ariaLabelledBy: "modal-title",
                                ariaDescribedBy: "modal-body",
                                templateUrl: "/static/templates/korppiExport.html",
                                controller: "KorppiExportCtrl",
                                controllerAs: "$ctrl",
                                size: "md",
                            });
                            instance.result.then(this.exportKorppi, () => {

                            });
                        });
                    },
                    order: 10,
                },
                {
                    title: "Enable instant update",
                    action($event) {
                        this.instantUpdate = true;
                    },
                    shown() {
                        return !this.instantUpdate;
                    },
                    leaveOpen: true,
                    order: 20,
                },
                {
                    title: "Disable instant update",
                    action($event) {
                        this.instantUpdate = false;
                    },
                    shown() {
                        return this.instantUpdate;
                    },
                    leaveOpen: true,
                    order: 30,
                },
                {
                    title: "Answers as plain text",
                    action($event) {
                        $uibModal.open({
                            animation: false,
                            ariaLabelledBy: "modal-title",
                            ariaDescribedBy: "modal-body",
                            component: "AllAnswers",
                            size: "md",
                            resolve: {
                                options() {
                                    return {
                                        url: "/allDocumentAnswersPlain/" + this.docId,
                                        identifier: this.docId,
                                        allTasks: true,
                                    };
                                },
                            },
                        });
                    },
                    order: 40,
                },
            ],
            rowTemplate: "<div ng-dblclick=\"grid.appScope.fireUserChange(row, true)\" ng-repeat=\"(colRenderIndex, col) in colContainer.renderedColumns track by col.colDef.name\" class=\"ui-grid-cell\" ng-class=\"{ 'ui-grid-row-header-cell': col.isRowHeader }\" ui-grid-cell></div>",
        };
    }

    fireUserChange(row, updateAll) {
        this.onUserChange(row.entity, updateAll);
    }

    exportKorppi(options) {
        if (!options.taskPointField && !options.velpPointField && !options.totalPointField) {
            return;
        }
        const data = this.gridApi.core.getVisibleRows(this.gridApi.grid);
        let dataKorppi = "";

        const fields = ["task_points", "velp_points", "total_points"];
        const fieldNames = {};
        fieldNames[fields[0]] = options.taskPointField;
        fieldNames[fields[1]] = options.velpPointField;
        fieldNames[fields[2]] = options.totalPointField;

        for (let i = 0; i < fields.length; ++i) {
            if (fieldNames[fields[i]]) {
                if (dataKorppi !== "") {
                    dataKorppi += "\n";
                }
                for (let j = 0; j < data.length; j++) {
                    if (data[j].entity[fields[i]] !== null) {
                        dataKorppi += data[j].entity.name + ";" + fieldNames[fields[i]] + ";" + data[j].entity[fields[i]] + "\n";
                    }
                }
            }
        }

        const filename = "korppi_" + this.docId + ".txt";
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
}

timApp.component("timUserList", {
    bindings: {
        onUserChange: "&",
        users: "<",
    },
    controller: UserListController,
    template: `<div ng-if="$ctrl.users"
     ui-grid="$ctrl.gridOptions"
     ui-grid-selection
     ui-grid-exporter
     ui-grid-auto-resize
     ui-grid-cellNav class="grid">
</div>
<div ng-if="!$ctrl.users">
    No answerers.
</div>`,
});

timApp.controller("KorppiExportCtrl", ["$uibModalInstance", function($uibModalInstance) {
    "use strict";
    const $ctrl = this;
    $ctrl.options = {};

    $ctrl.ok = function() {
        $uibModalInstance.close($ctrl.options);
    };

    $ctrl.cancel = function() {
        $uibModalInstance.dismiss("cancel");
    };
}]);
