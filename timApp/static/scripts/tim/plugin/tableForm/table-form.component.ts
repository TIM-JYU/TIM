/**
 * Defines the client-side implementation of a plugin for editing other plugins' answers in a formatted table
 */
import angular from "angular";
import * as t from "io-ts";
import type {ApplicationRef, DoBootstrap, OnInit} from "@angular/core";
import {
    ChangeDetectorRef,
    Component,
    ElementRef,
    NgModule,
    ViewChild,
} from "@angular/core";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {DomSanitizer} from "@angular/platform-browser";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import {showInputDialog} from "tim/ui/showInputDialog";
import {InputDialogKind} from "tim/ui/input-dialog.kind";
import {documentglobals} from "tim/util/globals";
import {PurifyModule} from "tim/util/purify.module";
import type {ViewCtrl} from "tim/document/viewctrl";
import {widenFields} from "tim/util/common";
import {
    GenericPluginMarkup,
    getTopLevelFields,
    IncludeUsersOption,
    nullable,
    withDefault,
} from "tim/plugin/attributes";
import type {
    CellAttrToSave,
    CellToSave,
    DataEntity,
    TimTable,
} from "tim/plugin/timTable/tim-table.component";
import {
    ClearSort,
    colnumToLetters,
    DataViewSettingsType,
    isPrimitiveCell,
    TimTableComponent,
    TimTableModule,
} from "tim/plugin/timTable/tim-table.component";
import {registerPlugin} from "tim/plugin/pluginRegistry";
import {TimMessageSendModule} from "tim/messaging/tim-message-send.component";
import {$http, $httpParamSerializer} from "tim/util/ngimport";
import {
    clone,
    defaultErrorMessage,
    maxContentOrFitContent,
    splitItems,
    to,
    to2,
    toPromise,
} from "tim/util/utils";
import {CommonModule} from "@angular/common";
import type {IAddmemberResponse} from "tim/ui/add-member.component";

const RunScriptModel = t.intersection([
    t.type({
        script: nullable(t.string),
        button: nullable(t.string),
        all: nullable(t.boolean),
        update: nullable(t.boolean),
        interval: nullable(t.number),
    }),
    t.partial({
        onMemberAdd: nullable(t.boolean),
    }),
]);

interface RunScriptModelType extends t.TypeOf<typeof RunScriptModel> {}

interface RunScriptType extends RunScriptModelType {
    handle?: number;
    running?: number;
}

const FilterValue = t.record(
    t.union([t.string, t.number]),
    t.union([t.string, t.number])
);

const Filters = t.partial({
    sort: t.array(t.union([t.string, t.number])),
    values: t.array(FilterValue),
});

// interface RunScriptsType extends t.TypeOf<typeof t.array(t.union([t.string, RunScriptModel]))> {}

const TableFormMarkup = t.intersection([
    t.partial({
        anonNames: nullable(t.boolean),
        autosave: t.boolean,
        hideButtonText: nullable(t.string),

        hiddenColumns: t.array(t.number),
        hiddenRows: t.array(t.number),
        locked: t.boolean,
        lockedFields: t.array(t.string),
        maxWidth: t.string,
        minWidth: t.string,
        maxRows: t.string,
        filterRow: t.union([t.boolean, t.number]),
        filters: Filters,
        allowPasteTable: t.boolean,
        pasteTableChars: t.record(t.string, t.array(t.string)),

        // filters: t.array(t.object),
        toolbarTemplates: t.array(t.UnknownRecord),

        cbColumn: t.boolean,
        nrColumn: t.boolean,
        charRow: t.boolean,
        groups: t.array(t.string),
        report: nullable(t.boolean),
        reportButton: nullable(t.string),
        separator: nullable(t.string),
        sortBy: nullable(
            t.string
        ) /* TODO! Username and task, or task and username -- what about points? */,
        table: nullable(t.boolean),
        removeUsersButtonText: nullable(t.string),
        userListButtonText: nullable(t.string),
        emailUsersButtonText: nullable(t.string),
        forceUpdateButtonText: nullable(t.string),
        fields: t.array(t.string),
        showToolbar: t.boolean,
        tinyFilters: t.boolean,
        hide: t.partial({
            removeMenu: t.boolean,
            insertMenu: t.boolean,
        }),
        sisugroups: t.string,
        // runScripts: t.array(t.union([t.string, RunScriptModel])),
        runScripts: t.array(t.union([t.string, RunScriptModel])),
        dataView: nullable(DataViewSettingsType),
        replyToEmail: nullable(t.string),
        addUsersButton: nullable(t.string),
        notifyOnAdd: t.boolean,
        createMissingUsers: t.boolean,
    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
        autoupdate: withDefault(t.number, 500),
        cols: withDefault(t.number, 20),
        autoUpdateFields: withDefault(t.boolean, true),
        autoUpdateTables: withDefault(t.boolean, true),
        fontSize: withDefault(t.string, "smaller"),
        fixedColor: withDefault(t.string, "fixedColor"),
        includeUsers: withDefault(IncludeUsersOption, "current"),
        saveStyles: withDefault(t.boolean, true),
        removeDocIds: withDefault(t.boolean, true),
        taskBorders: withDefault(t.boolean, false),
        singleLine: withDefault(t.boolean, true),
        usernames: withDefault(t.boolean, true),
        realnames: withDefault(t.boolean, true),
        emails: withDefault(t.boolean, false),
        addedDates: withDefault(t.boolean, false),
        maxCols: withDefault(t.string, maxContentOrFitContent()),
        openButtonText: withDefault(t.string, "Avaa Taulukko/Raporttinäkymä"),
        open: withDefault(t.boolean, true),
        reportFilter: withDefault(t.string, ""),
        downloadAsExcelFile: withDefault(t.string, ""),
    }),
]);

const Rows = t.record(
    t.string,
    t.record(t.string, t.union([t.string, t.null, t.number]))
);
const Styles = t.record(
    t.string,
    t.record(t.string, t.union([t.null, t.record(t.string, t.string)]))
);

interface IRowsType extends t.TypeOf<typeof Rows> {}

interface ITableFormUser {
    id: number;
    real_name: string;
    email: string;
}

const TableFormAll = t.intersection([
    t.partial({
        aliases: t.record(t.string, t.string),
        fields: t.array(t.string),
        users: t.record(
            t.string,
            t.type({id: t.number, email: t.string, real_name: t.string})
        ),
        membership_add: t.record(t.string, nullable(t.string)),
        membership_end: t.record(t.string, nullable(t.string)),
        rows: Rows,
        styles: Styles,
    }),
    getTopLevelFields(TableFormMarkup),
    t.type({}),
]);

const realNameColumn = "A";
const userNameColumn = "B";
const emailColumn = "C";
const membershipAddedColumn = "D";
const membershipEndColumn = "E";
const realNameColIndex = 0;
const userNameColIndex = 1;
const emailColIndex = 2;
const memberShipAddColIndex = 3;
const memberShipEndColIndex = 4;
const sortLang = "fi";

@Component({
    selector: "tableform-runner",
    // changeDetection: ChangeDetectionStrategy.OnPush,
    template: `
        <div class="tableform" *ngIf="showTable">
            <tim-markup-error *ngIf="markupError" [data]="markupError!"></tim-markup-error>
            <h4 *ngIf="header" [innerHtml]="header | purify"></h4>
            <p *ngIf="stem" [innerHtml]="stem | purify"></p>
            <tim-table *ngIf="tableCheck()" [data]="data"
                       [taskid]="getTaskId()"></tim-table>

            <div class="hidden-print">
                <tim-alert *ngIf="actionInfo" severity="info" [closeable]="true"
                           (closing)="resetActionInfoText()">
                    <p class="action-info-content">{{actionInfo}}</p>
                </tim-alert>
                <button class="timButton"
                        *ngIf="(tableCheck() && !autosave && !locked) || saveFailed"
                        (click)="saveText()">
                    {{ buttonText() }}
                </button>
                <button class="timButton"
                        *ngIf="addUsersButton"
                        (click)="addUsers()"
                >
                    {{ addUsersButton }}
                </button>
                <button class="timButton"
                        *ngIf="reportCheck()"
                        (click)="generateReport()">
                    {{ reportButton() }}
                </button>
                <button class="timButton"
                        (click)="closeTable()"
                        *ngIf="hideButtonText">
                    {{ hideButtonText }}
                </button>
                <button class="timButton"
                        (click)="forceUpdateTable()"
                        *ngIf="forceUpdateButtonText">
                    {{ forceUpdateButtonText }}
                </button>
                <button class="timButton"
                        (click)="removeUsers()"
                        *ngIf="removeUsersButtonText && cbCount">
                    {{ removeUsersButtonText }}
                </button>
                <button class="timButton"
                        (click)="listUsernames()"
                        *ngIf="userListButtonText && cbCount">
                    {{ userListButtonText }}
                </button>
                <button class="timButton"
                        (click)="emailUsers()"
                        *ngIf="emailUsersButtonText && cbCount">
                    {{ emailUsersButtonText }}
                </button>
                <ng-container *ngIf="runScripts">
                    <button class="timButton"
                            *ngFor="let s of runScripts"
                            [hidden]="(!s.all && !cbCount) || s.onMemberAdd"
                            (click)="runJsRunner(s)">
                        {{ s.button }}
                    </button>
                </ng-container>
                <ng-container *ngIf="sisugroups && cbCount">
                    <button class="timButton"
                            (click)="orderSisuGroups()"
                            [disabled]="loading">
                        Confirm groups
                    </button>&nbsp;
                    <tim-loading *ngIf="loading"></tim-loading>
                </ng-container>
            </div>
            <div class="csRunDiv tableUsers" style="padding: 1em;" *ngIf="userlist"> <!-- userlist -->
                <tim-close-button (click)="userlist=''"></tim-close-button>
                <p>Separator:
                    <label><input type="radio" name="listsep" [(ngModel)]="listSep" value="-"
                                  (change)="listUsernames()">-</label>&nbsp;
                    <label><input type="radio" name="listsep" [(ngModel)]="listSep" value=","
                                  (change)="listUsernames()">,</label>&nbsp;
                    <label><input type="radio" name="listsep" [(ngModel)]="listSep" value="|"
                                  (change)="listUsernames()">|</label>&nbsp;
                    <label><input type="radio" name="listsep" [(ngModel)]="listSep" value=";"
                                  (change)="listUsernames()">;</label>&nbsp;
                    <label><input type="radio" name="listsep" [(ngModel)]="listSep" value="\n"
                                  (change)="listUsernames()">\\n</label>&nbsp;
                </p>
                <label><input type="checkbox" [(ngModel)]="listName" (change)="listUsernames()">Name</label>&nbsp;
                <label><input type="checkbox" [(ngModel)]="listUsername"
                              (change)="listUsernames()">Username</label>&nbsp;
                <label><input type="checkbox" [(ngModel)]="listEmail" (change)="listUsernames()">Email</label>&nbsp;
                <br>
                <textarea id="userlist" [(ngModel)]="userlist" rows="10" cols="60"></textarea>
                <button class="timButton"
                        (click)="copyList()">
                    Copy
                </button>
            </div>
            <tim-message-send [(recipientList)]="recipientList"
                              [docId]="currentDocumentID()"
                              [storageKey]="this.taskIdFull"
                              [replyToEmail]="markup['replyToEmail']"
            ></tim-message-send>
            <pre *ngIf="result">{{ result }}</pre>
            <pre *ngIf="error" [innerHtml]="error"></pre>
            <p *ngIf="footer" [innerHtml]="footer | purify" class="plgfooter"></p>
        </div>
        <div class="tableOpener" *ngIf="!showTable">
            <button class="timButton"
                    [disabled]="loading"
                    (click)="openTable()">
                {{ openButtonText }}
            </button>
            <tim-loading *ngIf="loading"></tim-loading>
        </div>
    `,
    styleUrls: ["./table-form.component.scss"],
})
export class TableFormComponent
    extends AngularPluginBase<
        t.TypeOf<typeof TableFormMarkup>,
        t.TypeOf<typeof TableFormAll>,
        typeof TableFormAll
    >
    implements OnInit
{
    public viewctrl?: ViewCtrl;
    result?: string;
    error?: string;
    data: TimTable & {userdata: DataEntity} = {
        hide: {edit: false, insertMenu: true, removeMenu: true},
        hiddenRows: [],
        hiddenColumns: [],
        hideSaveButton: true,
        // lockCellCount: true,
        lockedCells: [],
        lockedColumns: [],
        table: {countRow: 0, countCol: 0, columns: []},
        // TODO: give rows (and maybe columns) in data.table
        task: true,
        userdata: {type: "Relative", cells: {}},
        nonUserSpecific: true,
        isPreview: false,
    };
    // TODO: Change row format to properly typed format (maybe userobject:IRowstype) format
    private rows!: IRowsType;
    private styles?: t.TypeOf<typeof Styles>;
    private fields!: string[];
    private lockedFields!: string[];
    private users!: Record<string, ITableFormUser>;
    private membershipAdd!: Record<string, string | null>;
    private membershipEnd!: Record<string, string | null>;
    private aliases!: Record<string, string>;
    private realnames = false;
    private usernames = false;
    // private emails = false;  // TODO: not used?
    showTable = false;
    private tableFetched = false;
    private rowKeys!: string[];
    private userLocations: Record<string, string> = {};
    private taskLocations: Record<string, string> = {};
    private changedCells: string[] = []; // Use same type as data.userdata?
    private clearStylesCells = new Set<string>();
    private isAddingUsers = false;
    actionInfo?: string;

    runScripts?: RunScriptType[];

    userlist: string = "";
    listSep: string = "-";
    listName: boolean = false;
    listUsername: boolean = true;
    listEmail: boolean = false;
    private fixedColor: string = "fixedColor"; // "#f0f0f0";
    cbCount: number = 0;
    @ViewChild(TimTableComponent)
    timTable?: TimTableComponent;
    recipientList = "";
    loading = false;
    saveFailed = false;
    taskIdFull?: string;

    get refreshScripts(): string[] {
        return (
            this.runScripts
                ?.filter((s) => s.update && s.script)
                ?.map((s) => s.script!) ?? []
        );
    }

    currentDocumentID() {
        return documentglobals().curr_item.id;
    }

    getDefaultMarkup() {
        return {};
    }

    get autosave() {
        return this.markup.autosave;
    }

    get locked() {
        return this.markup.locked;
    }

    get hideButtonText() {
        return this.markup.hideButtonText;
    }

    get openButtonText() {
        return this.markup.openButtonText;
    }

    get removeUsersButtonText() {
        return this.markup.removeUsersButtonText;
    }

    get userListButtonText() {
        return this.markup.userListButtonText;
    }

    get emailUsersButtonText() {
        return this.markup.emailUsersButtonText;
    }

    get forceUpdateButtonText() {
        return this.markup.forceUpdateButtonText;
    }

    get xrunScripts() {
        return this.markup.runScripts;
    }

    get sisugroups() {
        return this.markup.sisugroups;
    }

    get addUsersButton() {
        return this.markup.addUsersButton;
    }

    /**
     * Used to define table view & relative save button in angular, true or false.
     */
    buttonText() {
        return this.markup.buttonText ?? "Tallenna taulukko";
    }

    /**
     * Used to define table view & relative save button in angular, true or false.
     */
    reportButton() {
        return this.markup.reportButton ?? "Luo Raportti";
    }

    addHiddenIndex(i: number) {
        if (!this.data.hiddenColumns) {
            this.data.hiddenColumns = [i];
        } else {
            this.data.hiddenColumns.push(i);
        }
    }

    checkToShow(param: boolean | undefined, i: number, def: boolean): boolean {
        if (param == undefined) {
            param = def;
        }
        if (param) {
            return true;
        }

        this.addHiddenIndex(i);
        return false;
    }

    constructor(
        el: ElementRef,
        http: HttpClient,
        domSanitizer: DomSanitizer,
        _cdr: ChangeDetectorRef
    ) {
        super(el, http, domSanitizer);
        // cdr.detach();
    }

    parseRunScripts(
        scripts: (RunScriptModelType | string)[],
        oldScripts: RunScriptType[] | undefined
    ) {
        if (oldScripts) {
            for (const sr of oldScripts) {
                if (sr.handle) {
                    window.clearInterval(sr.handle);
                }
            }
        }
        const runScripts = [];

        for (const r of scripts) {
            let s: string | undefined | null = "";
            const rs: RunScriptType = {
                script: "",
                button: null,
                all: null,
                update: null,
                interval: null,
                handle: undefined,
                running: 0,
                onMemberAdd: null,
            };
            if (typeof r === "string") {
                if (r.length == 0) {
                    continue;
                }
                s = r;
            } else {
                s = r.script;
                rs.button = r.button;
                rs.all = r.all;
                rs.update = r.update;
                rs.interval = r.interval;
                rs.onMemberAdd = r.onMemberAdd;
            }
            let script = "";
            if (s) {
                const parts = s.split("=");
                script = parts[0].replace("!", "").replace("*", "");
                rs.script = script;
                rs.button = rs.button ?? (parts.length > 1 ? parts[1] : script);
                rs.all = rs.all ?? s.startsWith("*"); // compatible with old format:  "*name!=buttontext"
                rs.update = rs.update ?? s.includes("!");
            }
            if (script || rs.interval) {
                runScripts.push(rs);
                if (rs.interval && rs.interval > 0) {
                    // if (rs.interval < 10) rs.interval = 10; // at least 5 sec
                    rs.handle = window.setInterval(() => {
                        if (rs.running) {
                            return;
                        }
                        rs.running = 1;
                        const tt: TimTableComponent | undefined =
                            this.getTimTable();
                        if (!tt) {
                            return;
                        }
                        if (tt.isPreview() || !this.showTable) {
                            return;
                        }
                        void this.runJsRunner(rs);
                        rs.running = 0;
                    }, rs.interval * 1000);
                }
            }
        }
        return runScripts;
    }

    ngOnInit() {
        super.ngOnInit();

        this.taskIdFull = this.getTaskId()?.docTask().toString();

        if (this.markup.runScripts) {
            this.runScripts = this.parseRunScripts(
                this.markup.runScripts,
                this.runScripts
            );
        }

        const tid = this.getTaskId();
        this.viewctrl = vctrlInstance; // TODO: Make an Angular service for getting ViewCtrl.
        if (this.viewctrl && tid) {
            this.viewctrl.addTableForm(this, tid.docTask());
        }
        const table = this.data.table;
        if (this.markup.fontSize) {
            table.fontSize = this.markup.fontSize;
        }
        this.data.taskBorders = this.markup.taskBorders;
        this.fixedColor = this.markup.fixedColor || this.fixedColor;

        this.data.hiddenRows = this.markup.hiddenRows;
        this.data.hiddenColumns = this.markup.hiddenColumns;

        // Initialize hide-attribute
        this.data.hide = {removeMenu: true, insertMenu: true};
        if (this.markup.hide) {
            this.data.hide = this.markup.hide; // TODO: TimTablen oletukset tähän
            const hide = this.markup.hide;
            if (hide.removeMenu === undefined) {
                this.data.hide.removeMenu = true;
            }
            if (hide.insertMenu === undefined) {
                this.data.hide.insertMenu = true;
            }
        }
        if (this.markup.showToolbar !== undefined) {
            this.data.hide.toolbar = !this.markup.showToolbar;
        }

        if (this.markup.tinyFilters) {
            this.data.tinyFilters = this.markup.tinyFilters;
        }

        this.realnames = this.checkToShow(
            this.markup.realnames,
            realNameColIndex,
            true
        );
        this.usernames = this.checkToShow(
            this.markup.usernames,
            userNameColIndex,
            true
        );
        /* TODO: this.emails = */
        this.checkToShow(this.markup.emails, emailColIndex, false);
        this.checkToShow(this.markup.addedDates, memberShipAddColIndex, false);
        this.checkToShow(
            this.markup.includeUsers !== "current",
            memberShipEndColIndex,
            false
        );

        this.rows = this.attrsall.rows ?? {};
        this.rowKeys = Object.keys(this.rows);
        this.styles = this.attrsall.styles;
        this.fields = this.attrsall.fields ?? [];
        this.lockedFields = this.markup.lockedFields ?? [];
        this.users = this.attrsall.users ?? {};
        this.membershipAdd = this.attrsall.membership_add ?? {};
        this.membershipEnd = this.attrsall.membership_end ?? {};
        this.aliases = this.attrsall.aliases ?? {};

        this.setDataMatrix();

        this.data.saveCallBack = (cellsTosave) => this.cellChanged(cellsTosave);
        if (this.markup.saveStyles) {
            this.data.saveStyleCallBack = (cellsTosave) =>
                this.cellChanged(cellsTosave);
        }
        this.data.cbCallBack = (cbs, n, index) => this.cbChanged(cbs, n, index);

        if (this.markup.minWidth) {
            this.data.minWidth = this.markup.minWidth;
        }
        if (this.markup.maxWidth !== undefined) {
            this.data.maxWidth = this.markup.maxWidth;
        }
        if (this.markup.singleLine) {
            this.data.singleLine = this.markup.singleLine;
        }
        if (this.markup.open) {
            this.tableFetched = true;
            this.showTable = this.markup.open;
        }

        this.data.cbColumn = this.markup.cbColumn;
        this.data.nrColumn = this.markup.nrColumn;
        this.data.charRow = this.markup.charRow;
        this.data.filterRow = this.markup.filterRow;
        this.data.filters = this.markup.filters;
        this.data.allowPasteTable = this.markup.allowPasteTable;
        this.data.pasteTableChars = this.markup.pasteTableChars;
        this.data.maxRows = this.markup.maxRows;
        this.data.maxCols = this.markup.maxCols;
        this.data.toolbarTemplates = this.markup.toolbarTemplates;
        this.data.dataView = this.markup.dataView;
        this.data.isPreview = this.isPreview();
        this.data.locked = this.markup.locked;
        // this.cdr.detectChanges();
    }

    /**
     * Returns the TimTableComponent within the tableForm.
     */
    getTimTable() {
        return this.timTable;
    }

    /**
     * Sorts row key values (usernames) by their real name attribute in this.users
     * @param a username to compare with b
     * @param b username to compare with a
     */
    sortByRealName(a: string, b: string) {
        if (!this.users) {
            return 0;
        }
        try {
            return this.users[a].real_name.localeCompare(
                this.users[b].real_name,
                sortLang
            );
        } catch (e) {
            return 0;
        }
    }

    sortByEmail(a: string, b: string) {
        try {
            return this.users[a].email.localeCompare(
                this.users[b].email,
                sortLang
            );
        } catch (e) {
            return 0;
        }
    }

    async tryUpdateTable() {
        const timTable = this.getTimTable();
        if (timTable == null) {
            return;
        }
        if (!timTable.isSomeCellBeingEdited()) {
            // Can safely do a proper update with resetting filters
            await this.forceUpdateTable();
        } else {
            // Only simple update to not break editor
            await this.updateTable();
        }
    }

    /**
     * Clears tableForm rows and fetches new data to be put into rows
     * Basically just a reset
     */
    public async updateTable() {
        if (this.attrsall.markup.sisugroups) {
            return;
        }

        // TODO: Save before reset?
        interface TableFetchResponse {
            aliases: Record<string, string>;
            fields: string[];
            users: Record<string, ITableFormUser>;
            membership_add: Record<string, string | null>;
            membership_end: Record<string, string | null>;
            rows: IRowsType;
            styles: t.TypeOf<typeof Styles>;
        }

        let prom;
        const tid = this.getTaskId();
        if (!tid) {
            this.error = "TaskId is missing.";
            return;
        }
        this.loading = true;
        if (this.isPreview()) {
            prom = $http.get<TableFetchResponse>(
                "/tableForm/fetchTableDataPreview?" +
                    $httpParamSerializer({
                        taskid: tid.docTask(),
                        fields: this.markup.fields,
                        groups: this.markup.groups,
                        removeDocIds: this.markup.removeDocIds,
                        anonNames: this.markup.anonNames ?? false,
                    })
            );
        } else {
            prom = $http.get<TableFetchResponse>(
                "/tableForm/fetchTableData?" +
                    $httpParamSerializer({
                        taskid: tid.docTask(),
                    })
            );
        }
        const r = await to(prom);
        this.loading = false;
        if (!r.ok) {
            this.error = r.result.data.error;
        } else {
            const tableResponse = r.result;
            // TODO: Generic reset function
            this.aliases = tableResponse.data.aliases || {};
            this.membershipAdd = tableResponse.data.membership_add;
            this.membershipEnd = tableResponse.data.membership_end;
            this.rows = tableResponse.data.rows || {};
            this.rowKeys = Object.keys(tableResponse.data.rows);
            this.fields = tableResponse.data.fields || [];
            this.users = tableResponse.data.users;
            this.styles = tableResponse.data.styles || {};
            this.userLocations = {};
            this.taskLocations = {};
            this.data.table.countCol = 0;
            this.data.table.countRow = 0;
            this.data.table.columns = [];
            this.data.userdata.cells = {};
            this.setDataMatrix();
            this.reinitializeTimTable();
        }
    }

    private reinitializeTimTable() {
        const timtab = this.getTimTable();
        if (timtab) {
            timtab.reInitialize(ClearSort.No);
            timtab.c();
        }
    }

    /**
     * Queries new values for given fields and updates the table
     * @param fields to be updated
     */
    public async updateFields(fields: string[]) {
        if (this.attrsall.markup.sisugroups) {
            return;
        }

        try {
            if (!this.tableFetched || !this.viewctrl) {
                return;
            }
            const fieldsToUpdate: string[] = [];
            if (!this.markup.fields) {
                return;
            }
            const ownFields = widenFields(this.markup.fields);
            for (const aliasfield of ownFields) {
                const field = aliasfield.split("=")[0].trim();
                const docField = this.viewctrl.docId + "." + field;
                // TODO: Double .includes call - maybe it's better to search for fieldsToUpdate from something
                //  that already has the docID
                if (fields.includes(field) || fields.includes(docField)) {
                    fieldsToUpdate.push(aliasfield);
                }
            }
            const tid = this.getTaskId();
            if (!tid) {
                return;
            }
            const r = await to(
                $http.get<{
                    rows: IRowsType;
                    styles: t.TypeOf<typeof Styles>;
                    fields: string[];
                }>(
                    "/tableForm/updateFields?" +
                        $httpParamSerializer({
                            fields: fieldsToUpdate,
                            taskid: tid.docTask(),
                        })
                )
            );
            if (!r.ok) {
                return;
            }
            const tableResponse = r.result;
            // TODO if response status != ok
            const rows = tableResponse.data.rows || {};
            const styles = tableResponse.data.styles || {};
            const tableFields = tableResponse.data.fields || [];

            // Find out which columns to update
            const taskColumns: Record<string, string> = {};
            for (const f of tableFields) {
                const extendedField = this.aliases[f] || f;
                for (const [key, value] of Object.entries(this.taskLocations)) {
                    if (value == extendedField) {
                        taskColumns[f] = key;
                        break;
                    }
                }
            }

            // TODO: Check if any value changed.  If not do not call reInitialize
            for (const f of tableFields) {
                for (let y = 0; y < this.rowKeys.length; y++) {
                    if (
                        styles &&
                        !angular.equals(styles, {}) &&
                        styles[this.rowKeys[y]]
                    ) {
                        this.data.userdata.cells[taskColumns[f] + (y + 1)] = {
                            cell: rows[this.rowKeys[y]][f],
                            ...styles[this.rowKeys[y]][f],
                        };
                    } else {
                        if (rows[this.rowKeys[y]]) {
                            this.data.userdata.cells[taskColumns[f] + (y + 1)] =
                                {cell: rows[this.rowKeys[y]][f]};
                        }
                    }
                }
            }
            this.reinitializeTimTable();
        } catch (e) {
            console.log(e);
            this.error = "Error updating fields" + "\n" + e;
        }
    }

    /**
     * Transforms user/task combination defined in this.rows into cell format and sets up the table
     * TODO: generate rows/columns for this.data.table, possibly needed for more easily maintained layout handling
     */
    setDataMatrix() {
        try {
            if (!this.data.lockedCells) {
                this.data.lockedCells = [];
            }
            if (!this.data.lockedColumns) {
                this.data.lockedColumns = [];
            }
            if (this.realnames) {
                this.rowKeys.sort((a, b) => this.sortByRealName(a, b));
                this.data.lockedColumns.push(realNameColumn);
            } else if (this.usernames) {
            } else {
                this.rowKeys.sort((a, b) => this.sortByEmail(a, b));
            }
            this.data.lockedColumns.push(userNameColumn);
            this.data.lockedColumns.push(emailColumn);
            this.data.lockedColumns.push(membershipAddedColumn);
            this.data.lockedColumns.push(membershipEndColumn);
            // }
            if (this.attrsall.markup.sisugroups) {
                // These require unique names, otherwise could just use empty strings in place of "invisibleX".
                this.data.headers = [
                    $localize`Description`,
                    $localize`Sisu name`,
                    "invisible1",
                    "invisible2",
                    "invisible3",
                ];
            } else {
                this.data.headers = [
                    // NOTE: if change these, change also exported names!
                    $localize`User's name`,
                    $localize`Username`,
                    $localize`E-mail`,
                    $localize`Added`,
                    $localize`:@@userGroupRemoved:Removed`,
                ];
            }
            this.data.headersStyle = {
                // backgroundColor: this.fixedColor,
                // "font-weight": "bold",
            };

            if (this.fields) {
                this.data.table.countCol = this.fields.length + 3;
            }
            this.data.table.countRow = Object.keys(this.rows).length;
            let y = 1;
            for (const r of this.rowKeys) {
                this.data.userdata.cells[userNameColumn + y] = {
                    cell: r,
                    class: this.fixedColor,
                };
                this.userLocations[y] = r;
                const userInfo = this.users[r];
                this.data.userdata.cells[realNameColumn + y] = {
                    cell: userInfo.real_name,
                    class: this.fixedColor,
                };
                this.data.userdata.cells[emailColumn + y] = {
                    cell: userInfo.email,
                    class: this.fixedColor,
                };
                this.data.userdata.cells[membershipAddedColumn + y] = {
                    cell: this.membershipAdd[r],
                    class: this.fixedColor,
                };
                this.data.userdata.cells[membershipEndColumn + y] = {
                    cell: this.membershipEnd[r],
                    class: this.fixedColor,
                };
                y++;
            }
            // TODO: Load default cell colors from tableForm's private answer?
            const xOffset = memberShipEndColIndex + 1;
            if (this.fields) {
                for (let x = 0; x < this.fields.length; x++) {
                    const colheader = this.fields[x];
                    const currentCol = colnumToLetters(x + xOffset);
                    const expandedLockedFields = widenFields(this.lockedFields);
                    if (expandedLockedFields.includes(colheader)) {
                        this.data.lockedColumns.push(currentCol);
                    }
                    this.data.headers.push(colheader);

                    let contentalias;
                    if (this.aliases && colheader in this.aliases) {
                        contentalias = this.aliases[colheader];
                    } else {
                        contentalias = colheader;
                    }
                    this.taskLocations[colnumToLetters(x + xOffset)] =
                        contentalias;
                    for (y = 0; y < this.rowKeys.length; y++) {
                        if (this.styles && !angular.equals(this.styles, {})) {
                            this.data.userdata.cells[currentCol + (y + 1)] = {
                                cell: this.rows[this.rowKeys[y]][
                                    this.fields[x]
                                ],
                                ...this.styles[this.rowKeys[y]][this.fields[x]],
                            };
                        } else {
                            this.data.userdata.cells[currentCol + (y + 1)] = {
                                cell: this.rows[this.rowKeys[y]][
                                    this.fields[x]
                                ],
                            };
                        }
                    }
                }
            }
        } catch (e) {
            console.log(e);
            this.error = "Error in setDataMatrix" + "\n" + e;
        }
    }

    /**
     * Closes timTable's editor and saves the cell that is being currently edited
     */
    async saveText() {
        const timTable = this.getTimTable();
        if (timTable == null) {
            return;
        }
        await timTable.saveAndCloseSmallEditor();
        await this.doSaveText();
    }

    /**
     * Returns true value, if table attribute is true.
     * Used to define table view & relative save button in angular, true or false.
     */
    tableCheck() {
        // return (this.markup.table === true);
        if (this.markup.table != undefined) {
            return this.markup.table;
        } else {
            return true;
        }
    }

    /**
     * Returns true value, if report attribute is true.
     * Used to define create report button in angular, true or false.
     */
    reportCheck() {
        return this.markup.report == true;
    }

    /**
     * String to determinate how usernames are filtered in report.
     * Choices are username, username and full name and anonymous. Username as default.
     */
    sortBy() {
        return this.markup.sortBy ?? "username";
    }

    /**
     * Generates report based on the table.
     * Used if report is set to true and create report button is clicked.
     * Used to define table view & relative save button in angular, true or false.
     */
    generateReport() {
        // const dataTable = this.generateCSVTable();
        const taskId = this.pluginMeta.getTaskId();
        if (taskId == undefined) {
            return;
        }

        const reportParams = {
            // TODO: support for relevant attrs (realnames, usernames, emails...)
            // TODO: get relevant user input from timTable (sort, filters, checkboxes...)
            // taskid: this.getTaskId()
            // TODO: use taskid? (less data to transfer because of plug.values, but dependant on task existence)
            docId: taskId.docId,
            fields: this.markup.fields,
            groups: this.markup.groups,
            removeDocIds: this.markup.removeDocIds,
            separator: this.markup.separator ?? ",",
            anonNames: this.markup.anonNames,
            realnames: this.markup.realnames,
            usernames: this.markup.usernames,
            emails: this.markup.emails,
            reportFilter: this.markup.reportFilter,
            downloadAsExcelFile: this.markup.downloadAsExcelFile,
        };
        let filterParams;
        const timTable = this.getTimTable();
        if (timTable != null) {
            const selUsers = timTable.getCheckedRows(0, false);
            const users = TableFormComponent.makeUserArray(
                selUsers,
                userNameColIndex
            );

            if (selUsers.length > 0) {
                filterParams = {userFilter: users};
            } else {
                const filterFields: string[] = [];
                const filterValues: string[] = [];

                const xOffset = memberShipEndColIndex + 1;

                // TODO: change to handle other filter rows also
                timTable.filters[0].forEach((value, index) => {
                    if (!value) {
                        return;
                    }
                    switch (index) {
                        case realNameColIndex:
                            filterFields.push("realname");
                            break;
                        case userNameColIndex:
                            filterFields.push("username");
                            break;
                        case emailColIndex:
                            filterFields.push("email");
                            break;
                        case memberShipAddColIndex:
                            filterFields.push("membership_add");
                            break;
                        case memberShipEndColIndex:
                            filterFields.push("membership_end");
                            break;
                        default:
                            filterFields.push(this.fields[index - xOffset]);
                    }
                    filterValues.push(value);
                });
                filterParams = {filterFields, filterValues};
            }
        }

        const win = window.open(
            "/tableForm/generateReport?" +
                $httpParamSerializer({
                    ...reportParams,
                    ...filterParams,
                }),
            "WINDOWID"
        );
        if (win == null) {
            this.error = "Failed to open report window.";
        }
    }

    /**
     * Make list of users colIndex.  Separate items by separators
     * @param users array of users
     * @param colIndex what index to use for list
     * @param preseparator what comes before evyry item
     * @param midseparator what comes between items
     */
    static makeUserList(
        users: string[][],
        colIndex: number,
        preseparator: string,
        midseparator: string
    ): string {
        let result = "";
        let sep = "";
        for (const r of users) {
            result += sep + preseparator + r[colIndex];
            sep = midseparator;
        }
        return result;
    }

    static makeUserArray(users: string[][], colIndex: number): string[] {
        const result = [];
        for (const r of users) {
            result.push(r[colIndex]);
        }
        return result;
    }

    /**
     * Removes selected users from the group
     */
    async removeUsers() {
        const timTable = this.getTimTable();
        if (timTable == null) {
            return;
        }
        const selUsers = timTable.getCheckedRows(0, true);
        let msg = "";
        for (const r of selUsers) {
            msg += r.join(", ") + "<br>";
        }
        if (msg == "") {
            return;
        }

        if (!this.markup.groups) {
            return;
        }
        const group = this.markup.groups[0];

        const dr = await to2(
            showInputDialog({
                text:
                    $localize`<b>Really remove the following users from group</b> ` +
                    group +
                    "<b>?</b><br>\n<pre>\n" +
                    msg +
                    "\n</pre>",
                title: $localize`Remove users from group ` + group,
                isInput: InputDialogKind.ValidatorOnly,
                validator: async () => {
                    const ulist = TableFormComponent.makeUserList(
                        selUsers,
                        1,
                        "",
                        ","
                    );
                    const r = await to(
                        $http.post<unknown>(`/groups/removemember/${group}`, {
                            names: ulist.split(","),
                        })
                    );
                    if (r.ok) {
                        return {ok: true, result: r.result.data} as const;
                    } else {
                        return {
                            ok: false,
                            result: r.result.data.error,
                        } as const;
                    }
                },
            })
        );
        if (dr.ok) {
            location.reload();
        }
    }

    listUsernames() {
        const timTable = this.getTimTable();
        if (timTable == null) {
            return;
        }
        let preseparator = " - ";
        let midseparator = "\n";
        let sep = this.listSep;
        const colindex = 0;
        const selUsers = timTable.getCheckedRows(0, true);
        const ulist = [];
        let usep = "";
        for (const u of selUsers) {
            const un = u[userNameColIndex];
            let s = "";
            if (this.listName) {
                s = this.users[un].real_name;
                usep = ", ";
            }
            if (this.listUsername) {
                s += usep + un;
                usep = ", ";
            }
            if (this.listEmail) {
                s += usep + this.users[un].email;
                usep = ", ";
            }
            usep = "";
            ulist.push([s]);
        }
        // if ( this.listEmail ) { midseparator = "\n"; preseparator = "";  }
        if (sep == "") {
            sep = "\n";
        } // radio could not give \n?
        if (sep != "-") {
            midseparator = sep;
            preseparator = "";
        }
        this.userlist = TableFormComponent.makeUserList(
            ulist,
            colindex,
            preseparator,
            midseparator
        );
    }

    copyList() {
        const ta = this.element.find("#userlist");
        ta.focus();
        ta.select();
        document.execCommand("copy");
        // TODO: myös iPad toimimaan, ks GeoGebra tai csPlugin jaa tee yleinen copy
    }

    emailUsers() {
        const timTable = this.getTimTable();
        if (timTable == null) {
            return;
        }
        const selUsers = timTable.getCheckedRows(0, true);
        this.recipientList = TableFormComponent.makeUserList(
            selUsers,
            emailColIndex,
            "",
            "\n"
        );
    }

    /**
     * Callback function to be noticed when check boxes are changed in table
     * @param _cbs boolean list of cb-values
     * @param n number of visible checked cbs
     * @param _index index of clicked cb, may be -1 if header row cb clicked
     */
    cbChanged(_cbs: boolean[], n: number, _index: number) {
        this.cbCount = n;
    }

    /**
     * Callback function that gets called when timTable saves a cell
     * Collects information about which cells have changed and which ones want to clear their style attributes
     * @param cellsToSave list of cells that needs to be saved
     */
    async cellChanged(cellsToSave: CellToSave[] | CellAttrToSave[]) {
        if (this.attrsall.markup.sisugroups || this.markup.locked) {
            return;
        }
        for (const c of cellsToSave) {
            const coli = c.col;
            const rowi = c.row;
            const changedStyle = c.key;
            if (changedStyle) {
                if (changedStyle == "CLEAR") {
                    this.clearStylesCells.add(
                        colnumToLetters(coli) + (rowi + 1)
                    );
                } else {
                    this.clearStylesCells.delete(
                        colnumToLetters(coli) + (rowi + 1)
                    );
                }
            }
            this.changedCells.push(colnumToLetters(coli) + (rowi + 1));
        }
        if (this.markup.autosave) {
            await this.doSaveText();
        }
    }

    async openTable() {
        if (!this.tableFetched) {
            await this.updateTable();
            this.tableFetched = true;
        }
        this.showTable = true;
    }

    async forceUpdateTable() {
        this.timTable?.dataViewComponent?.startReset();
        this.timTable?.ngOnInit();
        this.ngOnInit();
        await this.updateTable();
        this.timTable?.clearSortOrder();
        this.timTable?.repeatLastSort();
        this.timTable?.handleChangeFilter();
        this.timTable?.c();
        this.tableFetched = true;
        this.timTable?.dataViewComponent?.endReset();
    }

    closeTable() {
        this.showTable = false;
    }

    /**
     * Transforms the cell format back to row format and saves the table input
     */
    async doSaveText() {
        // this.error = "... saving ...";
        if (this.changedCells.length == 0) {
            return;
        }
        const replyRows: Record<
            number,
            Record<string, string | null | Record<string, unknown>>
        > = {};
        const changedFields = new Set<string>();
        const changedFieldsForTables = new Set<string>();
        try {
            for (const coord of this.changedCells) {
                const alphaRegExp = new RegExp("([A-Z]*)");
                const alpha = alphaRegExp.exec(coord);
                if (alpha == null) {
                    continue;
                }
                const columnPlace = alpha[0];
                const numberPlace = coord.substring(columnPlace.length);
                if (
                    columnPlace === userNameColumn ||
                    columnPlace === realNameColumn || // TODO: Do we need this anymore?
                    columnPlace === emailColumn
                ) {
                    // TODO: Do we need this anymore?
                    continue;
                }
                const cell = this.data.userdata.cells[coord];
                let cellContent;
                let cellStyle: Record<string, unknown> | null = null;
                if (!isPrimitiveCell(cell)) {
                    cellContent = cell.cell;
                    if (this.markup.saveStyles) {
                        cellStyle = clone(cell);
                        delete cellStyle.cell;
                    }
                } else {
                    cellContent = cell;
                }
                if (cellContent === null) {
                    cellContent = "";
                } else if (
                    typeof cellContent === "boolean" ||
                    typeof cellContent === "number"
                ) {
                    cellContent = cellContent.toString();
                }
                // else if (typeof cellContent === "boolean") {
                //     throw new Error("cell was boolean?");

                if (
                    (this.markup.autoUpdateFields ||
                        this.markup.autoUpdateTables) &&
                    this.viewctrl
                ) {
                    const taskWithField =
                        this.taskLocations[columnPlace].split(".");
                    const docTask = taskWithField[0] + "." + taskWithField[1];
                    if (
                        this.viewctrl.selectedUser.name ==
                        this.userLocations[numberPlace]
                    ) {
                        // TODO: Should check for global / useCurrentUser fields here
                        changedFields.add(docTask);
                    }
                    changedFieldsForTables.add(docTask);
                }
                const userId = this.users[this.userLocations[numberPlace]].id;
                try {
                    replyRows[userId][this.taskLocations[columnPlace]] =
                        cellContent;
                } catch (e) {
                    replyRows[userId] = {};
                    replyRows[userId][this.taskLocations[columnPlace]] =
                        cellContent;
                }
                /* TODO: instead of iterating clearStylesCells could decide that absence of any styles
                    (e.g primitivecell) would mean result in null style value being sent
                */
                if (this.clearStylesCells.has(columnPlace + numberPlace)) {
                    const taskWithField =
                        this.taskLocations[columnPlace].split(".");
                    const docTaskStyles =
                        taskWithField[0] + "." + taskWithField[1] + ".styles";
                    replyRows[userId][docTaskStyles] = null;
                } else if (
                    cellStyle != null &&
                    Object.keys(cellStyle).length != 0
                ) {
                    const taskWithField =
                        this.taskLocations[columnPlace].split(".");
                    const docTaskStyles =
                        taskWithField[0] + "." + taskWithField[1] + ".styles";
                    replyRows[userId][docTaskStyles] = cellStyle;
                }
            }
        } catch (e) {
            console.log(e);
            this.error = "Error in doSaveText" + "\n" + e;
        }
        const params = {
            input: {
                nosave: false,
                replyRows: replyRows,
            },
        };
        const url = this.pluginMeta.getAnswerUrl();
        const r = await to(
            $http.put<{web: {result: string; error?: string}}>(url, params)
        );
        this.loading = false;
        if (r.ok) {
            const data = r.result.data;
            this.error = data.web.error;
            this.saveFailed = false;
            // this.result = "Saved";
        } else {
            this.saveFailed = true;
            this.error = r.result.data?.error ?? defaultErrorMessage;
            return;
        }
        const timtab = this.getTimTable();
        if (!timtab) {
            return;
        }
        timtab.confirmSaved();
        if (this.viewctrl) {
            if (this.markup.autoUpdateFields && changedFields.size > 0) {
                await this.viewctrl.updateFields(Array.from(changedFields));
            }
            if (
                this.markup.autoUpdateTables &&
                changedFieldsForTables.size > 0
            ) {
                this.viewctrl.updateAllTables(
                    Array.from(changedFieldsForTables),
                    this.getTaskId()
                );
            }
        }
        this.clearStylesCells.clear();
        this.changedCells = [];
    }

    async orderSisuGroups() {
        const timTable = this.getTimTable();
        if (timTable == null || this.data.headers == null) {
            return;
        }
        const selUsers = timTable.getCheckedRows(0, true);
        const groups = TimTableComponent.makeSmallerMatrix(selUsers, [
            1,
            this.data.headers.indexOf("TIM-nimi"),
        ]);
        const params = groups.map(([sisuid, timname]) => ({
            externalId: sisuid,
            name: timname,
        }));
        this.loading = true;
        const r = await to(
            $http.post<{web: {result: string; error?: string}}>(
                "/sisu/createGroupDocs",
                params
            )
        );
        this.loading = false;
        if (r.ok) {
            timTable.confirmSaved();
            location.reload();
        } else {
            this.error = r.result.data.error;
        }
    }

    async runJsRunner(runner: RunScriptType, users?: string[]) {
        const timTable = this.getTimTable();
        if (timTable == null) {
            return;
        }
        const runnerName = runner.script;
        let jsRunner;
        if (this.viewctrl && runnerName) {
            const selUsers = timTable.getCheckedRows(0, true);
            const userNames =
                users ??
                TableFormComponent.makeUserArray(selUsers, userNameColIndex);
            jsRunner = await this.viewctrl.runJsRunner(runnerName, userNames);
        }
        // JSRunner is able to update all tables automatically
        if (runner.update && !jsRunner?.willAutoRefreshTables()) {
            if (!timTable.isSomeCellBeingEdited()) {
                await this.forceUpdateTable();
            }
        }
    }

    getAttributeType() {
        return TableFormAll;
    }

    async addUsers() {
        if (this.isAddingUsers) {
            return;
        }
        this.isAddingUsers = true;

        let prompt = $localize`Please provide email addresses or usernames.
Separate multiple addresses with commas or write each address on a new line.`;

        if (this.markup.createMissingUsers) {
            prompt +=
                "\n\n" +
                $localize`If the address does not have a TIM account, a new account will be created for them.`;
        }
        if (this.markup.notifyOnAdd) {
            prompt += "\n" + $localize`New members will be notified by email.`;
        }

        const res = await to2(
            showInputDialog<IAddmemberResponse>({
                title: $localize`Add users`,
                text: prompt,
                okText: $localize`Add`,
                inputType: "textarea",
                isInput: InputDialogKind.InputAndValidator,
                defaultValue: "",
                validator: async (input: string) => {
                    if (!this.markup.groups) {
                        return {
                            ok: false,
                            result: $localize`There are not groups defined in this table.`,
                        };
                    }
                    const group = this.markup.groups[0];
                    const r = await toPromise(
                        this.http.post<IAddmemberResponse>(
                            `/groups/addmember/${group}`,
                            {
                                names: splitItems(input),
                                create_missing_users:
                                    !!this.markup.createMissingUsers,
                                notify_new: !!this.markup.notifyOnAdd,
                            }
                        )
                    );
                    if (r.ok) {
                        return r;
                    } else {
                        return {
                            ok: false,
                            result: r.result.error.error,
                        };
                    }
                },
            })
        );
        if (res.ok) {
            const result = res.result;

            let actionMessage = $localize`Updated group.`;

            if (result.added.length > 0) {
                const addedText =
                    $localize`Added ${result.added.length} users:` +
                    "\n\n" +
                    result.added.map((u) => `- ${u}`).join("\n");
                actionMessage += "\n\n" + addedText;
                if (this.markup.notifyOnAdd) {
                    actionMessage +=
                        "\n\n" +
                        $localize`New members will be notified by email.`;
                }
            }
            if (result.already_belongs.length > 0) {
                const alreadyInGroupText =
                    $localize`Users already in group:` +
                    "\n\n" +
                    result.already_belongs.map((u) => `- ${u}`).join("\n");
                actionMessage += "\n\n" + alreadyInGroupText;
            }
            if (result.not_exist.length > 0) {
                const notFoundText =
                    $localize`Users not found in TIM or address was wrong:` +
                    "\n\n" +
                    result.not_exist.map((u) => `- ${u}`).join("\n");
                actionMessage += "\n\n" + notFoundText;
            }

            if (this.runScripts) {
                const onAddMemberScripts = this.runScripts
                    .filter((s) => !!s.onMemberAdd)
                    .map((s) => this.runJsRunner(s, []));
                await Promise.all(onAddMemberScripts);
            }

            this.setActionInfoText(actionMessage, 10000);
            await this.forceUpdateTable();
        }
        this.isAddingUsers = false;
    }

    resetActionInfoText() {
        this.actionInfo = undefined;
    }

    setActionInfoText(text: string, resetMs?: number) {
        this.actionInfo = text;
        if (resetMs) {
            setTimeout(() => {
                this.resetActionInfoText();
            }, resetMs);
        }
    }
}

@NgModule({
    declarations: [TableFormComponent],
    imports: [
        CommonModule,
        HttpClientModule,
        FormsModule,
        TimUtilityModule,
        TimTableModule,
        TimMessageSendModule,
        PurifyModule,
    ],
})
export class TableFormModule implements DoBootstrap {
    ngDoBootstrap(_appRef: ApplicationRef) {}
}

registerPlugin("tableform-runner", TableFormModule, TableFormComponent);
