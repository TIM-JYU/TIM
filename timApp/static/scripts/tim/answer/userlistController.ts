import {IAngularEvent, IController, IScope} from "angular";
import {timApp} from "tim/app";
import uiGrid from "ui-grid";
import {documentglobals} from "tim/util/globals";
import {showKorppiExportDialog} from "tim/answer/showKorppiExportDialog";
import {showFeedbackAnswers} from "tim/answer/showFeedbackAnswers";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import {showAllAnswersDialog} from "tim/answer/showAllAnswersDialog";
import {ViewCtrl} from "../document/viewctrl";
import {IUser, IUserListEntry, sortByRealName, sortLang} from "../user/IUser";
import {withComparatorFilters} from "../util/comparatorfilter";
import {$timeout} from "../util/ngimport";
import {
    Binding,
    copyToClipboard,
    getURLParameter,
    getViewName,
    Require,
} from "../util/utils";

export interface IExportOptions {
    totalPointField: string;
    velpPointField: string;
    taskPointField: string;
    copy: boolean;
}

function numericSort(a: number | null, b: number | null) {
    if (a === b) {
        return 0;
    }
    if (a === null) {
        return -1;
    }
    if (b === null) {
        return 1;
    }
    return Math.sign(a - b);
}

export class UserListController implements IController {
    static $inject = ["$scope", "$element"];
    private gridOptions?: uiGrid.IGridOptionsOf<IUserListEntry> & {
        gridMenuCustomItems: unknown;
    };
    private gridApi?: uiGrid.IGridApiOf<IUserListEntry>;
    private instantUpdate: boolean = false;
    private onUserChange!: Binding<
        (params: {$USER: IUser; $UPDATEALL: boolean}) => void,
        "&"
    >;
    private viewctrl!: Require<ViewCtrl>;
    private preventedChange = false;

    constructor(private scope: IScope, private element: JQLite) {}

    $onInit() {
        this.scope.$watch(
            () => this.element[0].offsetHeight + this.element[0].offsetWidth,
            (sum) => {
                const grid = this.element.find(".grid");
                grid.css("width", this.element[0].offsetWidth - 5 + "px");
                grid.css("height", this.element[0].offsetHeight - 30 + "px");
            }
        );

        let anyAnnotations = false;
        let smallFieldWidth = 59;
        // check if server gave student ids for users
        let includeStudentId = this.viewctrl.users.some(a => a.user.hasOwnProperty('student_id'));

        function nameCompare(a: IUserListEntry, b: IUserListEntry) {
            return sortByRealName(a.user, b.user);
        }

        this.viewctrl.users.sort(nameCompare);

        for (const u of this.viewctrl.users) {
            if (u.velped_task_count > 0) {
                anyAnnotations = true;
                smallFieldWidth = 40;
                break;
            }
        }

        const columns: Array<uiGrid.IColumnDefOf<
            IUserListEntry
        >> = withComparatorFilters([
            {
                field: "user.real_name",
                name: "Full name",
                cellTooltip: true,
                headerTooltip: true,
                sortingAlgorithm: (a: string, b: string) =>
                    a.localeCompare(b, sortLang),
            },
            {
                field: "user.name",
                name: "Username",
                cellTooltip: true,
                headerTooltip: true,
                maxWidth: 100,
            },
            // only include Student # tab if server gave them
            ...(includeStudentId ? [{
                field: "user.student_id",
                name: "Student #",
                cellTooltip: true,
                headerTooltip: true,
                visible: false,
                maxWidth: 70,
            }] : []),
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
                sortingAlgorithm: numericSort,
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
                sortingAlgorithm: numericSort,
            },
            {
                field: "total_points",
                name: "Points",
                cellTooltip: true,
                headerTooltip: true,
                maxWidth: smallFieldWidth,
                sortingAlgorithm: numericSort,
            },
        ]);
        const formMode = this.viewctrl.docSettings.form_mode ?? false;
        this.instantUpdate = formMode || getViewName() === "review";

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
            columnDefs: columns,
            onRegisterApi: (gridApi) => {
                this.gridApi = gridApi;

                gridApi.selection.on.rowSelectionChanged(this.scope, (row) => {
                    this.fireUserChange(row, this.instantUpdate);
                });
                if (this.gridOptions?.data) {
                    gridApi.grid.modifyRows(
                        this.gridOptions.data as IUserListEntry[]
                    );
                    const firstRow = this.gridOptions.data[0] as IUserListEntry;
                    const userName = getURLParameter("user");
                    let foundUser: IUserListEntry | undefined;
                    if (userName) {
                        foundUser = this.viewctrl.findUserByName(userName);
                        if (!foundUser) {
                            void showMessageDialog(
                                `User ${userName} not found from answerers.`
                            );
                        }
                    }
                    const currSel = documentglobals().current_list_user;
                    if (!foundUser && currSel) {
                        foundUser = this.viewctrl.findUserByName(currSel.name);
                    }
                    this.gridApi.selection.selectRow(
                        foundUser ? foundUser : firstRow
                    );
                }
                gridApi.cellNav.on.navigate(
                    this.scope,
                    (newRowCol, oldRowCol) => {
                        // TODO: check if simple way to cancel this event here
                        //  or make unsavitimcomponents checks at keyboardpress/click events before on.navigate gets called
                        if (this.preventedChange) {
                            this.preventedChange = false;
                            return;
                        }

                        if (oldRowCol && oldRowCol.row === newRowCol.row) {
                            return;
                        }
                        const unsavedTimComponents = this.viewctrl.checkUnSavedTimComponents(
                            true
                        );
                        if (
                            unsavedTimComponents &&
                            !window.confirm(
                                "You have unsaved changes. Change user anyway?"
                            )
                        ) {
                            this.preventedChange = true;
                            if (oldRowCol) {
                                gridApi.cellNav.scrollToFocus(
                                    oldRowCol.row.entity,
                                    oldRowCol.col.colDef
                                );
                            } else {
                                if (
                                    this.gridOptions?.data &&
                                    this.gridOptions.columnDefs
                                ) {
                                    gridApi.cellNav.scrollToFocus(
                                        this.gridOptions
                                            .data[0] as IUserListEntry,
                                        this.gridOptions.columnDefs[0]
                                    );
                                }
                            }
                            return;
                        }
                        gridApi.selection.selectRow(newRowCol.row.entity);
                    }
                );
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
                    title: "Answers as plain text/JSON",
                    action: async ($event: IAngularEvent) => {
                        await showAllAnswersDialog({
                            url:
                                "/allDocumentAnswersPlain/" +
                                this.viewctrl.item.id,
                            identifier: this.viewctrl.item.id.toString(),
                            allTasks: true,
                        });
                    },
                    order: 40,
                },
                {
                    title: "Create Feedback Report",
                    action: async ($event: IAngularEvent) => {
                        let iusers: IUser[];
                        iusers = [];
                        if (this.gridApi) {
                            const selectedUser = this.gridApi.selection.getSelectedRows()[0];
                            iusers.push(selectedUser.user);

                            const visibleRows = this.gridApi.core.getVisibleRows();

                            for (const row of visibleRows) {
                                // Create string array of visible item.
                                if (row.entity !== selectedUser) {
                                    iusers.push(row.entity.user);
                                }
                            }
                            if (visibleRows.length <= 0) {
                                iusers = [];
                            }
                        }
                        await showFeedbackAnswers({
                            url: "/feedback/report/" + this.viewctrl.item.id,
                            users: iusers,
                            identifier: this.viewctrl.item.id.toString(),
                            allTasks: true,
                        });
                    },
                    order: 50,
                },
            ],
            rowTemplate: `
<div ng-dblclick="grid.appScope.fireUserChange(row, true)"
     ng-repeat="(colRenderIndex, col) in colContainer.renderedColumns track by col.colDef.name"
     class="ui-grid-cell"
     ng-class="{ 'ui-grid-row-header-cell': col.isRowHeader }" ui-grid-cell>
</div>`,
        };
    }

    fireUserChange(row: uiGrid.IGridRowOf<IUserListEntry>, updateAll: boolean) {
        this.onUserChange({$USER: row.entity.user, $UPDATEALL: updateAll});
    }

    exportKorppi(options: IExportOptions) {
        if (
            !options.taskPointField &&
            !options.velpPointField &&
            !options.totalPointField
        ) {
            return;
        }
        if (!this.gridApi) {
            throw new Error("gridApi was not initialized");
        }
        const data = this.gridApi.core.getVisibleRows();
        let dataKorppi = "";

        const fields = ["total_points", "task_points", "velp_points"] as const;
        const fieldNames = new Map<string, string>();
        fieldNames.set(fields[0], options.totalPointField);
        fieldNames.set(fields[1], options.taskPointField);
        fieldNames.set(fields[2], options.velpPointField);
        let filename;
        for (const f of fields) {
            const fieldName = fieldNames.get(f);
            if (fieldName) {
                filename = filename ?? fieldName + ".txt";
                if (dataKorppi !== "") {
                    dataKorppi += "\n";
                }
                for (const d of data) {
                    const entity = d.entity;
                    if (entity[f] != null) {
                        dataKorppi +=
                            entity.user.name +
                            ";" +
                            fieldName +
                            ";" +
                            entity[f] +
                            "\n";
                    }
                }
            }
        }

        if (!filename) {
            filename = "korppi_" + this.viewctrl.docId + ".txt";
        }

        if (options.copy) {
            copyToClipboard(dataKorppi);
            return;
        }
        // from https://stackoverflow.com/a/33542499

        const blob = new Blob([dataKorppi], {type: "text/plain"});
        if (window.navigator.msSaveBlob) {
            window.navigator.msSaveBlob(blob, filename);
        } else {
            const elem = window.document.createElement("a");
            elem.href = window.URL.createObjectURL(blob);
            elem.download = filename;
            document.body.appendChild(elem);
            elem.click();
            document.body.removeChild(elem);
        }
        /*
        const opened = window.open("text/plain", "replace");
        if ( !opened ) { return; }
        opened.document.write(dataKorppi);
        opened.close();
        */
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
