import type {OnInit} from "@angular/core";
import {refreshUI} from "tim/util/refresh";
import {Component, ElementRef, HostListener} from "@angular/core";
import {documentglobals} from "tim/util/globals";
import {showKorppiExportDialog} from "tim/answer/showKorppiExportDialog";
import {showFeedbackAnswers} from "tim/answer/showFeedbackAnswers";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import {showAllAnswersDialog} from "tim/answer/showAllAnswersDialog";
import {showImportAnswersDialog} from "tim/answer/showImportAnswersDialog";
import type {ViewCtrl} from "tim/document/viewctrl";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import type {IUser, IUserListEntry} from "tim/user/IUser";
import {sortByRealName, sortLang} from "tim/user/IUser";
import {ComparatorFilter} from "tim/util/comparatorfilter";
import {
    copyToClipboard,
    getURLParameter,
    getUrlParams,
    getViewName,
    to2,
} from "tim/util/utils";

export interface IExportOptions {
    totalPointField: string;
    velpPointField: string;
    taskPointField: string;
    copy: boolean;
}

type CellValue = string | number | null | undefined;

type ColumnField =
    | "real_name"
    | "name"
    | "student_id"
    | "task_count"
    | "task_points"
    | "velped_task_count"
    | "velp_points"
    | "total_points";

interface IColumn {
    field: ColumnField;
    name: string;
    visible: boolean;
    narrow: boolean;
    value: (e: IUserListEntry) => CellValue;
    compare: (a: CellValue, b: CellValue) => number;
}

interface IMenuItem {
    title: string;
    action: () => void;
    shown?: () => boolean;
    leaveOpen?: boolean;
}

type SortDirection = "asc" | "desc";

function numericSort(a: CellValue, b: CellValue) {
    if (a === b) {
        return 0;
    }
    if (a == null) {
        return -1;
    }
    if (b == null) {
        return 1;
    }
    return Math.sign((a as number) - (b as number));
}

function localeSort(a: CellValue, b: CellValue) {
    return String(a ?? "").localeCompare(String(b ?? ""), sortLang);
}

function alphaSort(a: CellValue, b: CellValue) {
    if (a == null || b == null) {
        return numericSort(a == null ? null : 1, b == null ? null : 1);
    }
    const strA = String(a).toLowerCase();
    const strB = String(b).toLowerCase();
    return strA === strB ? 0 : strA < strB ? -1 : 1;
}

function toggleUrlParam(param: string, value: string) {
    const urlParams = getUrlParams();
    urlParams.set(param, value);
    window.location.search = urlParams.toString();
}

function downloadText(filename: string, text: string, type: string) {
    // from https://stackoverflow.com/a/33542499
    const blob = new Blob([text], {type});
    const elem = window.document.createElement("a");
    elem.href = window.URL.createObjectURL(blob);
    elem.download = filename;
    document.body.appendChild(elem);
    elem.click();
    document.body.removeChild(elem);
}

function csvCell(value: CellValue) {
    if (value == null) {
        return "";
    }
    if (typeof value === "number") {
        return value.toString();
    }
    return `"${value.replace(/"/g, '""')}"`;
}

/**
 * The list of users with answers that is shown in the teacher, answers and review views.
 * Selecting a user changes the user whose answers are shown in the document.
 */
@Component({
    selector: "tim-user-list",
    template: `
        <div class="userFixed"
             tim-draggable-fixed
             save="%%PAGEID%%studentList"
             [click]="true"
             [caption]="viewctrl.users.length + ' users with answers'">
            <div class="draggable-content user-list-content">
        <div class="userlist">
            <div class="user-list-menu">
                <button type="button"
                        class="user-list-menu-button"
                        title="Menu"
                        [attr.aria-expanded]="menuOpen"
                        (click)="menuOpen = !menuOpen">
                    <i class="glyphicon glyphicon-menu-hamburger"></i>
                </button>
                <ul class="dropdown-menu dropdown-menu-right user-list-menu-items" *ngIf="menuOpen">
                    <ng-container *ngFor="let item of menuItems">
                        <li *ngIf="!item.shown || item.shown()">
                            <a (click)="runMenuItem(item)">{{ item.title }}</a>
                        </li>
                    </ng-container>
                    <li role="separator" class="divider"></li>
                    <li class="dropdown-header">Columns:</li>
                    <li *ngFor="let col of columns">
                        <a (click)="toggleColumn(col)">
                            <i class="glyphicon"
                               [ngClass]="col.visible ? 'glyphicon-ok' : 'glyphicon-remove'"></i>
                            {{ col.name }}
                        </a>
                    </li>
                </ul>
            </div>
            <div class="user-list-table" tabindex="0" (keydown)="onKeyDown($event)">
                <table>
                    <thead>
                    <tr class="user-list-header-row">
                        <th *ngFor="let col of visibleColumns"
                            [ngClass]="'col-' + col.field"
                            [class.narrow]="col.narrow"
                            [title]="col.name">
                            <a class="sort-header" (click)="sortBy(col)">
                                <span>{{ col.name }}</span>
                                <i *ngIf="sortColumn === col"
                                   class="glyphicon"
                                   [ngClass]="sortDirection === 'asc' ? 'glyphicon-triangle-top' : 'glyphicon-triangle-bottom'"></i>
                            </a>
                        </th>
                    </tr>
                    <tr class="user-list-filter-row">
                        <th *ngFor="let col of visibleColumns" [class.narrow]="col.narrow">
                            <input type="text"
                                   [attr.aria-label]="'Filter for column ' + col.name"
                                   [(ngModel)]="filters[col.field]"
                                   (ngModelChange)="updateVisibleRows()">
                        </th>
                    </tr>
                    </thead>
                    <tbody>
                    <tr *ngFor="let row of visibleRows; trackBy: trackRow"
                        class="user-list-row"
                        [class.selected]="row === selected"
                        (click)="tryChangeUser(row)"
                        (dblclick)="fireUserChange(row)">
                        <td *ngFor="let col of visibleColumns"
                            [ngClass]="'col-' + col.field"
                            [class.narrow]="col.narrow"
                            [title]="col.value(row) ?? ''">{{ col.value(row) }}</td>
                    </tr>
                    </tbody>
                </table>
            </div>
        </div>
            </div>
        </div>
    `,
    styleUrls: ["./user-list.component.scss"],
    standalone: false,
})
export class UserListComponent implements OnInit {
    columns: IColumn[] = [];
    visibleColumns: IColumn[] = [];
    filters: Partial<Record<ColumnField, string>> = {};
    sortColumn?: IColumn;
    sortDirection: SortDirection = "asc";
    visibleRows: IUserListEntry[] = [];
    selected?: IUserListEntry;
    menuOpen = false;
    menuItems: IMenuItem[] = [];
    viewctrl: ViewCtrl = vctrlInstance!;

    constructor(private element: ElementRef<HTMLElement>) {}

    ngOnInit() {
        const rows = this.viewctrl.users;
        let anyAnnotations = false;
        // check if server gave student ids for users
        const includeStudentId = rows.some(
            (a) => a.user.student_id !== undefined
        );

        rows.sort((a, b) => sortByRealName(a.user, b.user));

        if (getViewName() !== "review") {
            anyAnnotations = rows.some((u) => u.velped_task_count > 0);
        }

        this.columns = [
            {
                field: "real_name",
                name: "Full Name",
                visible: true,
                narrow: false,
                value: (e) => e.user.real_name,
                compare: localeSort,
            },
            {
                field: "name",
                name: "Username",
                visible: true,
                narrow: false,
                value: (e) => e.user.name,
                compare: alphaSort,
            },
            // only include Student # tab if server gave them
            ...(includeStudentId
                ? [
                      {
                          field: "student_id" as const,
                          name: "Student #",
                          visible: false,
                          narrow: false,
                          value: (e: IUserListEntry) => e.user.student_id,
                          compare: alphaSort,
                      },
                  ]
                : []),
            {
                field: "task_count",
                name: "Tasks",
                visible: true,
                narrow: true,
                value: (e) => e.task_count,
                compare: numericSort,
            },
            {
                field: "task_points",
                name: "Task Points",
                visible: anyAnnotations,
                narrow: true,
                value: (e) => e.task_points,
                compare: numericSort,
            },
            {
                field: "velped_task_count",
                name: "Velped Tasks",
                visible: anyAnnotations,
                narrow: true,
                value: (e) => e.velped_task_count,
                compare: numericSort,
            },
            {
                field: "velp_points",
                name: "Velp Points",
                visible: anyAnnotations,
                narrow: true,
                value: (e) => e.velp_points,
                compare: numericSort,
            },
            {
                field: "total_points",
                name: "Points",
                visible: getViewName() !== "review",
                narrow: true,
                value: (e) => e.total_points,
                compare: numericSort,
            },
        ];
        this.menuItems = this.createMenuItems();
        this.updateVisibleColumns();
        this.updateVisibleRows();

        this.viewctrl.userList = this;

        if (rows.length > 0) {
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
            this.selected = foundUser ?? rows[0];
            this.fireUserChange(this.selected);
            this.scrollToSelected();
        }
    }

    trackRow(_index: number, row: IUserListEntry) {
        return row.user.id;
    }

    private createMenuItems(): IMenuItem[] {
        return [
            {
                title: "Export to Korppi",
                action: async () => {
                    const options = await to2(showKorppiExportDialog());
                    if (options.ok) {
                        this.exportKorppi(options.result);
                    }
                },
            },
            {
                title: "Enable instant update",
                action: () => {
                    this.viewctrl.instantUpdateTasks = true;
                },
                shown: () => !this.viewctrl.instantUpdateTasks,
                leaveOpen: true,
            },
            {
                title: "Disable instant update",
                action: () => {
                    this.viewctrl.instantUpdateTasks = false;
                },
                shown: () => this.viewctrl.instantUpdateTasks,
                leaveOpen: true,
            },
            {
                title: "Sync selected users in tasks",
                action: () => {
                    this.viewctrl.syncAnswerBrowsers = true;
                },
                shown: () => !this.viewctrl.syncAnswerBrowsers,
                leaveOpen: true,
            },
            {
                // TODO: better desc
                title: "De-sync selected users in tasks",
                action: () => {
                    this.viewctrl.syncAnswerBrowsers = false;
                },
                shown: () => this.viewctrl.syncAnswerBrowsers,
                leaveOpen: true,
            },
            {
                title: "Answers as plain text/JSON",
                action: () => {
                    void to2(
                        showAllAnswersDialog({
                            url:
                                "/allDocumentAnswersPlain/" +
                                this.viewctrl.item.id,
                            identifier: this.viewctrl.item.id.toString(),
                            allTasks: true,
                        })
                    );
                },
            },
            {
                title: "Import answers as JSON",
                action: async () => {
                    const r = await showImportAnswersDialog();
                    if (r.ok && r.result) {
                        window.location.reload();
                    }
                },
            },
            {
                title: "Create Feedback Report",
                action: () => {
                    let iusers: IUser[] = [];
                    if (this.selected) {
                        iusers.push(this.selected.user);
                    }
                    for (const row of this.visibleRows) {
                        if (row !== this.selected) {
                            iusers.push(row.user);
                        }
                    }
                    if (this.visibleRows.length <= 0) {
                        iusers = [];
                    }
                    void to2(
                        showFeedbackAnswers({
                            url: "/feedback/report/" + this.viewctrl.item.id,
                            users: iusers,
                            identifier: this.viewctrl.item.id.toString(),
                            allTasks: true,
                        })
                    );
                },
            },
            {
                title: "Hide answerer names",
                shown: () => !documentglobals().hideNamesRequested,
                action: () => toggleUrlParam("hide_names", "true"),
            },
            {
                title: "Show answerer names",
                shown: () => documentglobals().hideNamesRequested,
                action: () => toggleUrlParam("hide_names", "false"),
            },
            {
                title: "Show only valid answers",
                shown: () => !documentglobals().showValidAnswersOnly,
                action: () => toggleUrlParam("valid_answers_only", "true"),
            },
            {
                title: "Show all answers",
                shown: () => documentglobals().showValidAnswersOnly,
                action: () => toggleUrlParam("valid_answers_only", "false"),
            },
            {
                title: "Clear all filters",
                action: () => {
                    this.filters = {};
                    this.updateVisibleRows();
                },
            },
            {
                title: "Export all data as csv",
                action: () => this.exportCsv(this.viewctrl.users),
            },
            {
                title: "Export visible data as csv",
                action: () => this.exportCsv(this.visibleRows),
            },
            {
                title: "Export selected data as csv",
                action: () =>
                    this.exportCsv(this.selected ? [this.selected] : []),
            },
        ];
    }

    runMenuItem(item: IMenuItem) {
        if (!item.leaveOpen) {
            this.menuOpen = false;
        }
        item.action();
    }

    @HostListener("document:click", ["$event"])
    onDocumentClick(e: MouseEvent) {
        if (!this.menuOpen || !(e.target instanceof Node)) {
            return;
        }
        const menu =
            this.element.nativeElement.querySelector(".user-list-menu");
        // A menu item may have removed itself from the DOM while handling the click.
        if (menu && !menu.contains(e.target) && e.target.isConnected) {
            this.menuOpen = false;
        }
    }

    toggleColumn(col: IColumn) {
        col.visible = !col.visible;
        this.updateVisibleColumns();
    }

    private updateVisibleColumns() {
        this.visibleColumns = this.columns.filter((c) => c.visible);
    }

    /**
     * Cycles the sorting of the column: ascending, descending, no sorting.
     */
    sortBy(col: IColumn) {
        if (this.sortColumn !== col) {
            this.sortColumn = col;
            this.sortDirection = "asc";
        } else if (this.sortDirection === "asc") {
            this.sortDirection = "desc";
        } else {
            this.sortColumn = undefined;
        }
        this.updateVisibleRows();
    }

    updateVisibleRows() {
        const activeFilters = this.columns
            .map((col) => ({col, term: this.filters[col.field] ?? ""}))
            .filter((f) => f.term !== "")
            .map((f) => ({
                ...f,
                comparator: ComparatorFilter.makeNumFilter(f.term),
            }));
        const rows = this.viewctrl.users.filter((row) =>
            activeFilters.every((f) => {
                const value = f.col.value(row);
                if (value == null) {
                    return f.term === "=";
                }
                return f.comparator.test(value.toString());
            })
        );
        const col = this.sortColumn;
        if (col) {
            const dir = this.sortDirection === "asc" ? 1 : -1;
            // Array.sort is stable, so equal values keep the order by name.
            rows.sort((a, b) => dir * col.compare(col.value(a), col.value(b)));
        }
        this.visibleRows = rows;
    }

    fireUserChange(row: IUserListEntry) {
        void this.viewctrl.changeUser(row.user);
        // The document view runs outside the Angular zone.
        refreshUI();
    }

    /**
     * Selects the user if it is allowed, i.e. there are no unsaved changes or the user accepts losing them.
     */
    tryChangeUser(row: IUserListEntry) {
        if (row === this.selected) {
            return;
        }
        const unsavedTimComponents = this.viewctrl.checkUnSavedComponents(true);
        if (
            unsavedTimComponents &&
            !window.confirm("You have unsaved changes. Change user anyway?")
        ) {
            return;
        }
        this.selected = row;
        this.fireUserChange(row);
        this.scrollToSelected();
    }

    onKeyDown(e: KeyboardEvent) {
        if (e.target instanceof HTMLInputElement) {
            return;
        }
        let step;
        if (e.key === "ArrowDown") {
            step = 1;
        } else if (e.key === "ArrowUp") {
            step = -1;
        } else {
            return;
        }
        e.preventDefault();
        if (this.visibleRows.length === 0) {
            return;
        }
        const current = this.selected
            ? this.visibleRows.indexOf(this.selected)
            : -1;
        const next =
            current < 0
                ? 0
                : Math.min(
                      Math.max(current + step, 0),
                      this.visibleRows.length - 1
                  );
        this.tryChangeUser(this.visibleRows[next]);
    }

    private scrollToSelected() {
        window.setTimeout(() => {
            this.element.nativeElement
                .querySelector("tr.selected")
                ?.scrollIntoView({block: "nearest"});
        });
    }

    /**
     * Change selected user externally without notifying about the change.
     * @param user user to select
     */
    changeUserWithoutFiring(user: IUserListEntry) {
        this.selected = user;
        this.scrollToSelected();
    }

    private exportCsv(rows: IUserListEntry[]) {
        const cols = this.visibleColumns;
        const lines = [
            cols.map((c) => csvCell(c.name)).join(","),
            ...rows.map((row) =>
                cols.map((c) => csvCell(c.value(row))).join(",")
            ),
        ];
        downloadText("download.csv", lines.join("\n"), "text/csv");
    }

    exportKorppi(options: IExportOptions) {
        if (
            !options.taskPointField &&
            !options.velpPointField &&
            !options.totalPointField
        ) {
            return;
        }
        const data = this.visibleRows;
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
                for (const entity of data) {
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
        downloadText(filename, dataKorppi, "text/plain");
    }
}
