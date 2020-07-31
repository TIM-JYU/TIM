/**
 * Defines the client-side implementation of a plugin for editing other plugins' answers in a formatted table
 */
import angular from "angular";
import * as t from "io-ts";
import {$http, $httpParamSerializer} from "tim/util/ngimport";
import {clone, maxContentOrFitContent, to} from "tim/util/utils";
import {
    ApplicationRef,
    ChangeDetectorRef,
    Component,
    DoBootstrap,
    ElementRef,
    Input,
    NgModule,
    OnInit,
    StaticProvider,
    ViewChild,
} from "@angular/core";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {createDowngradedModule, doDowngrade} from "tim/downgrade";
import {BrowserModule, DomSanitizer} from "@angular/platform-browser";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import {TaskId} from "tim/plugin/taskid";
import {ViewCtrl} from "../document/viewctrl";
import {InputDialogKind, showInputDialog} from "../ui/inputDialog";
import {Users} from "../user/userService";
import {widenFields} from "../util/common";
import {GenericPluginMarkup, getTopLevelFields, IncludeUsersOption, nullable, withDefault} from "./attributes";
import {
    CellAttrToSave,
    CellEntity,
    CellToSave,
    ClearSort,
    colnumToLetters,
    DataEntity,
    isPrimitiveCell,
    TimTable,
    TimTableComponent,
    TimTableModule,
} from "./timTable";

const TableFormMarkup = t.intersection([
    t.partial({
        anonNames: nullable(t.boolean),
        autosave: t.boolean,
        hideButtonText: nullable(t.string),

        hiddenColumns: t.array(t.number),
        hiddenRows: t.array(t.number),
        lockedFields: t.array(t.string),
        maxWidth: t.string,
        minWidth: t.string,
        maxRows: t.string,
        filterRow: t.boolean,
        toolbarTemplates: t.array(t.object),

        cbColumn: t.boolean,
        nrColumn: t.boolean,
        charRow: t.boolean,
        groups: t.array(t.string),
        report: nullable(t.boolean),
        reportButton: nullable(t.string),
        separator: nullable(t.string),
        sortBy: nullable(t.string), /* TODO! Username and task, or task and username -- what about points? */
        table: nullable(t.boolean),
        removeUsersButtonText: nullable(t.string),
        userListButtonText: nullable(t.string),
        emailUsersButtonText: nullable(t.string),
        fields: t.array(t.string),
        showToolbar: t.boolean,
        hide: t.partial({
            editMenu: t.boolean,
            insertMenu: t.boolean,
        }),
        sisugroups: t.string,
        runScripts: t.array(t.string),

        asDataView: nullable(t.boolean),
    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
        autoupdate: withDefault(t.number, 500),
        cols: withDefault(t.number, 20),
        autoUpdateFields: withDefault(t.boolean, true),
        autoUpdateTables: withDefault(t.boolean, true),
        fontSize: withDefault(t.string, "smaller"),
        fixedColor: withDefault(t.string, "#f0f0f0"),
        includeUsers: withDefault(IncludeUsersOption, "current"),
        saveStyles: withDefault(t.boolean, true),
        removeDocIds: withDefault(t.boolean, true),
        taskBorders: withDefault(t.boolean, false),
        singleLine: withDefault(t.boolean, true),
        usernames: withDefault(t.boolean, true),
        realnames: withDefault(t.boolean, true),
        emails: withDefault(t.boolean, false),
        maxCols: withDefault(t.string, "max-content"),
        openButtonText: withDefault(t.string, "Avaa Taulukko/Raporttinäkymä"),
        open: withDefault(t.boolean, true),
        reportFilter: withDefault(t.string, ""),
    }),
]);

const Rows = t.record(t.string, t.record(t.string, t.union([t.string, t.null, t.number])));
const Styles = t.record(t.string, t.record(t.string, t.union([t.null, t.record(t.string, t.string)])));

interface IRowsType extends t.TypeOf<typeof Rows> {
}

const TableFormAll = t.intersection([
    t.partial({
        aliases: t.record(t.string, t.string),
        fields: t.array(t.string),
        realnamemap: t.record(t.string, t.string),
        emailmap: t.record(t.string, t.string),
        membershipmap: t.record(t.string, nullable(t.string)),
        rows: Rows,
        styles: Styles,
    }),
    getTopLevelFields(TableFormMarkup),
    t.type({}),
]);

const realNameColumn = "A";
const userNameColumn = "B";
const emailColumn = "C";
const membershipColumn = "D";
const realNameColIndex = 0;
const userNameColIndex = 1;
const emailColIndex = 2;
const memberShipColIndex = 3;
const sortLang = "fi";

@Component({
    selector: "tim-email-send",
    template: `
        <div class="csRunDiv tableEmail" style="padding: 1em;" *ngIf="emaillist">
            <tim-close-button (click)="emaillist=''"></tim-close-button>
            <p><textarea [(ngModel)]="emaillist" rows="4" cols="40"></textarea>
            </p>
            <p>
                <label title="Send so that names are not visible (works only non-TIM send)"><input type="checkbox"
                                                                                                   [(ngModel)]="emailbcc">BCC</label>&nbsp;
                <label title="Send also a copy for me"><input type="checkbox" [(ngModel)]="emailbccme">BCC also
                    for me</label>&nbsp;
                <label title="Send using TIM. Every mail is sent as a personal mail."><input type="checkbox"
                                                                                              [(ngModel)]="emailtim">use
                    TIM to send</label>&nbsp;
            </p>
            <p>Subject: <input [(ngModel)]="emailsubject" size="60"></p>
            <p>eMail content:</p>
            <p><textarea [(ngModel)]="emailbody" rows="10" cols="70"></textarea></p>
            <p>
                <button class="timButton"
                        (click)="sendEmail()">
                    Send
                </button>
                <span class="savedtext" *ngIf="emailMsg">Sent!</span>
            </p>
        </div>
    `,
})
export class TimEmailComponent {
    @Input()
    emaillist: string = "";
    emailsubject: string = "";
    emailbody: string = "";
    emailbcc: boolean = false;
    emailbccme: boolean = true;
    emailtim: boolean = true;
    emailMsg: string = "";
    @Input()
    taskid?: TaskId;

    async sendEmailTim() {
        if (!this.taskid) {
            this.emailMsg = "Cannot send email without taskid";
            return;
        }
        this.emailMsg = ""; // JSON.stringify(response);
        const url = `/multiSendEmail/${this.taskid.docTask()}`;
        const response = await to($http.post<string[]>(url, {
            rcpt: this.emaillist.replace(/\n/g, ";"),
            subject: this.emailsubject,
            msg: this.emailbody,
            bccme: this.emailbccme,
        }));
        this.emailMsg = "Sent"; // JSON.stringify(response);
    }

    public async sendEmail() {
        if (this.emailtim) {
            await this.sendEmailTim();
            return;
        }
        // TODO: iPad do not like ;
        let addrs = this.emaillist.replace(/\n/g, ",");
        let bcc = "";
        if (this.emailbcc) {
            bcc = addrs;
            addrs = "";
        }
        if (this.emailbccme) {
            if (bcc) {
                bcc += ",";
            }
            bcc += Users.getCurrent().email;
        }
        window.location.href = "mailto:" + addrs
            + "?" + "subject=" + this.emailsubject
            + "&" + "body=" + this.emailbody
            + "&" + "bcc=" + bcc;
    }
}

@Component({
    selector: "tableform-runner",
    // changeDetection: ChangeDetectionStrategy.OnPush,
    template: `
        <div class="tableform" *ngIf="showTable">
            <tim-markup-error *ngIf="markupError" [data]="markupError"></tim-markup-error>
            <h4 *ngIf="header" [innerHtml]="header"></h4>
            <p *ngIf="stem" [innerHtml]="stem"></p>
            <tim-table *ngIf="tableCheck()" [data]="data"
                       [taskid]="getTaskId()"></tim-table>

            <div class="hidden-print">
                <button class="timButton"
                        *ngIf="tableCheck() && !autosave"
                        (click)="saveText()">
                    {{ buttonText() }}
                </button>
                <button class="timButton"
                        *ngIf="reportCheck()"
                        (click)="generateReport()">
                    {{ reportButton() }}
                </button>
                <button class="timButton"
                        (click)="closeTable()"
                        *ngIf="hideButtonText">
                    {{hideButtonText}}
                </button>
                <button class="timButton"
                        (click)="removeUsers()"
                        *ngIf="removeUsersButtonText && cbCount">
                    {{removeUsersButtonText}}
                </button>
                <button class="timButton"
                        (click)="listUsernames()"
                        *ngIf="userListButtonText && cbCount">
                    {{userListButtonText}}
                </button>
                <button class="timButton"
                        (click)="emailUsers()"
                        *ngIf="emailUsersButtonText && cbCount">
                    {{emailUsersButtonText}}
                </button>
                <ng-container *ngIf="runScripts && cbCount">
                    <button class="timButton"
                            *ngFor="let s of runScripts"
                            (click)="runJsRunner(s)">
                        Run {{s}}
                    </button>
                </ng-container>
                <button class="timButton"
                        (click)="orderSisuGroups()"
                        *ngIf="sisugroups && cbCount">
                    Confirm groups
                </button>
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
            <tim-email-send [emaillist]="emaillist" [taskid]="getTaskId()"></tim-email-send>
            <pre *ngIf="result">{{result}}</pre>
            <pre *ngIf="error" [innerHtml]="error"></pre>
            <p *ngIf="footer" [innerText]="footer" class="plgfooter"></p>
        </div>
        <div class="tableOpener" *ngIf="!showTable">
            <button class="timButton"
                    [disabled]="loading"
                    (click)="openTable()">
                {{openButtonText}}
            </button><tim-loading *ngIf="loading"></tim-loading>
        </div>
    `,
    styleUrls: ["./tableForm.scss"],
})
export class TableFormComponent extends AngularPluginBase<t.TypeOf<typeof TableFormMarkup>, t.TypeOf<typeof TableFormAll>, typeof TableFormAll>
    implements OnInit {
    public viewctrl?: ViewCtrl;
    result?: string;
    error?: string;
    private userfilter = "";
    data: TimTable & { userdata: DataEntity } = {
        hide: {edit: false, insertMenu: true, editMenu: true},
        hiddenRows: [],
        hiddenColumns: [],
        hideSaveButton: true,
        // lockCellCount: true,
        lockedCells: [],
        lockedColumns: [],
        table: {countRow: 0, countCol: 0, columns: []},
        // TODO: give rows (and maybe colums) in data.table
        task: true,
        userdata: {type: "Relative", cells: {}},
        nonUserSpecific: true,

        // saveCallBack: this.singleCellSave
    };
    // TODO: Change row format to properly typed format (maybe userobject:IRowstype) format
    private rows!: IRowsType;
    private styles?: t.TypeOf<typeof Styles>;
    private fields!: string[];
    private lockedFields!: string[];
    private realnamemap!: Record<string, string>;
    private emailmap!: Record<string, string>;
    private membershipmap!: Record<string, string | null>;
    private aliases!: Record<string, string>;
    private realnames = false;
    private usernames = false;
    private emails = false;
    showTable = false;
    private tableFetched = false;
    private rowKeys!: string[];
    private userLocations: Record<string, string> = {};
    private taskLocations: Record<string, string> = {};
    private changedCells: string[] = []; // Use same type as data.userdata?
    private clearStylesCells = new Set<string>();
    userlist: string = "";
    listSep: string = "-";
    listName: boolean = false;
    listUsername: boolean = true;
    listEmail: boolean = false;
    private fixedColor: string = "#f0f0f0";
    cbCount: number = 0;
    @ViewChild(TimTableComponent)
    timTable?: TimTableComponent;
    emaillist = "";
    loading = false;

    getDefaultMarkup() {
        return {};
    }

    get autosave() {
        return this.markup.autosave;
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

    get runScripts() {
        return this.markup.runScripts;
    }

    get sisugroups() {
        return this.markup.sisugroups;
    }

    /**
     * Used to define table view & relative save button in angular, true or false.
     */
    buttonText() {
        return (this.markup.buttonText ?? "Tallenna taulukko");
    }

    /**
     * Used to define table view & relative save button in angular, true or false.
     */
    reportButton() {
        return (this.markup.reportButton ?? "Luo Raportti");
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

    constructor(el: ElementRef, http: HttpClient, domSanitizer: DomSanitizer, private cdr: ChangeDetectorRef) {
        super(el, http, domSanitizer);
        // cdr.detach();
    }

    ngOnInit() {
        super.ngOnInit();
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
        this.data.hide = {editMenu: true, insertMenu: true};
        if (this.markup.hide) {
            this.data.hide = this.markup.hide; // TODO: TimTablen oletukset tähän
            const hide = this.markup.hide;
            if (hide.editMenu === undefined) {
                this.data.hide.editMenu = true;
            }
            if (hide.insertMenu === undefined) {
                this.data.hide.insertMenu = true;
            }
        }
        if (this.markup.showToolbar !== undefined) {
            this.data.hide.toolbar = this.markup.showToolbar;
        }

        this.userfilter = "";
        this.realnames = this.checkToShow(this.markup.realnames, realNameColIndex, true);
        this.usernames = this.checkToShow(this.markup.usernames, userNameColIndex, true);
        this.emails = this.checkToShow(this.markup.emails, emailColIndex, false);
        this.checkToShow(this.markup.includeUsers !== "current", memberShipColIndex, false);

        this.rows = this.attrsall.rows ?? {};
        this.rowKeys = Object.keys(this.rows);
        this.styles = this.attrsall.styles;
        this.fields = this.attrsall.fields ?? [];
        this.lockedFields = this.markup.lockedFields ?? [];
        this.realnamemap = this.attrsall.realnamemap ?? {};
        this.emailmap = this.attrsall.emailmap ?? {};
        this.membershipmap = this.attrsall.membershipmap ?? {};
        this.aliases = this.attrsall.aliases ?? {};

        this.setDataMatrix();

        this.data.saveCallBack = (cellsTosave, colValuesAreSame) => this.cellChanged(cellsTosave, colValuesAreSame);
        if (this.markup.saveStyles) {
            this.data.saveStyleCallBack = (cellsTosave, colValuesAreSame) => this.cellChanged(cellsTosave, colValuesAreSame);
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
        this.data.maxRows = this.markup.maxRows;
        this.data.maxCols = this.markup.maxCols;
        this.data.toolbarTemplates = this.markup.toolbarTemplates;
        this.data.asDataView = this.markup.asDataView ?? false;
        // this.cdr.detectChanges();
    }

    /**
     * Returns the TimTableComponent within the tableForm.
     */
    getTimTable() {
        return this.timTable;
    }

    /**
     * Sorts row key values (usernames) by their real name attribute in this.realnamemap
     * @param a username to compare with b
     * @param b username to compare with a
     */
    sortByRealName(a: string, b: string) {
        if (!this.realnamemap) {
            return 0;
        }
        try {
            return this.realnamemap[a].localeCompare(this.realnamemap[b], sortLang);
        } catch (e) {
            return 0;
        }
    }

    sortByEmail(a: string, b: string) {
        if (!this.emailmap) {
            return 0;
        }
        try {
            return this.emailmap[a].localeCompare(this.emailmap[b], sortLang);
        } catch (e) {
            return 0;
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
        type TableFetchResponse = ({
            aliases: Record<string, string>,
            fields: string[],
            realnamemap: Record<string, string>,
            membershipmap: Record<string, string>,
            emailmap: Record<string, string>,
            rows: IRowsType,
            styles: t.TypeOf<typeof Styles>,
        });
        let prom;
        const tid = this.getTaskId();
        if (!tid) {
            this.error = "TaskId is missing.";
            return;
        }
        this.loading = true;
        if (this.isPreview()) {
            prom = $http.get<TableFetchResponse>("/tableForm/fetchTableDataPreview?" + $httpParamSerializer({
                taskid: tid.docTask(),
                fields: this.markup.fields,
                groups: this.markup.groups,
                removeDocIds: this.markup.removeDocIds,
            }));
        } else {
            prom = $http.get<TableFetchResponse>("/tableForm/fetchTableData?" + $httpParamSerializer({
                taskid: tid.docTask(),
            }));
        }
        const r = await to(prom);
        this.loading = false;
        if (!r.ok) {
            this.error = r.result.data.error;
        } else {
            const tableResponse = r.result;
            // TODO: Generic reset function
            this.aliases = tableResponse.data.aliases || {};
            this.membershipmap = tableResponse.data.membershipmap;
            this.rows = tableResponse.data.rows || {};
            this.rowKeys = Object.keys(tableResponse.data.rows);
            this.fields = tableResponse.data.fields || [];
            this.realnamemap = tableResponse.data.realnamemap || {};
            this.emailmap = tableResponse.data.emailmap || {};
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
                // TODO: Double .includes call - maybe it's better to search for fieldsToUpdate from somethign
                //  that already has the docID
                if (fields.includes(field) || fields.includes(docField)) {
                    fieldsToUpdate.push(aliasfield);
                }
            }
            const tid = this.getTaskId();
            if (!tid) {
                return;
            }
            const r = await to($http.get <{
                rows: IRowsType,
                styles: t.TypeOf<typeof Styles>,
                fields: string[],
            }>("/tableForm/updateFields?" + $httpParamSerializer({
                fields: fieldsToUpdate,
                taskid: tid.docTask(),
            })));
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
                    if (styles && !angular.equals(styles, {})) {
                        this.data.userdata.cells[taskColumns[f] + (y + 1)] = {
                            cell: rows[this.rowKeys[y]][f],
                            ...styles[this.rowKeys[y]][f],
                        };
                    } else {
                        this.data.userdata.cells[taskColumns[f] + (y + 1)] = {cell: rows[this.rowKeys[y]][f]};
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
            if (this.emailmap) {
                this.data.lockedColumns.push(emailColumn);
            }
            if (this.attrsall.markup.sisugroups) {
                // These require unique names, otherwise could just use empty strings in place of "invisibleX".
                this.data.headers = ["Kuvaus", "Sisu-nimi", "invisible1", "invisible2"];
            } else {
                this.data.headers = ["Henkilön nimi", "Käyttäjänimi", "eMail", "Poistunut?"];
            }
            this.data.headersStyle = {
                "backgroundColor": this.fixedColor,
                "font-weight": "bold",
            };

            if (this.fields) {
                this.data.table.countCol = this.fields.length + 3;
            }
            this.data.table.countRow = Object.keys(this.rows).length;
            let y = 1;
            for (const r of this.rowKeys) {
                this.data.userdata.cells[userNameColumn + y] = {cell: r, backgroundColor: this.fixedColor};
                this.userLocations[y] = r;
                for (const [map, col] of [
                    [this.realnamemap, realNameColumn],
                    [this.emailmap, emailColumn],
                    [this.membershipmap, membershipColumn],
                ] as const) {
                    if (map) {
                        this.data.userdata.cells[col + y] = {
                            cell: map[r],
                            backgroundColor: this.fixedColor,
                        };
                    }
                }
                y++;
            }
            // TODO: Load default cell colors from tableForm's private answer?
            const xOffset = memberShipColIndex + 1;
            if (this.fields) {
                for (let x = 0; x < this.fields.length; x++) {

                    const colheader = this.fields[x];
                    const currentCol = colnumToLetters(x + xOffset);
                    const expandedLockedFields = widenFields(this.lockedFields);
                    if (expandedLockedFields.includes(colheader)) {
                        this.data.lockedColumns.push(currentCol);
                    }
                    this.data.headers.push(colheader);
                    /*
                    this.data.userdata.cells[colnumToLetters(x + xOffset) + 1] = {
                        cell: colheader,
                        backgroundColor: this.fixedColor,
                    };
                    */

                    let contentalias;
                    if (this.aliases && colheader in this.aliases) {
                        contentalias = this.aliases[colheader];
                    } else {
                        contentalias = colheader;
                    }
                    this.taskLocations[colnumToLetters(x + xOffset)] = contentalias;
                    // this.data.lockedCells.push(colnumToLetters(x + xOffset) + 1);
                    // y = 0;
                    // for (const [u, r] of Object.entries(this.rows)) {
                    //     if (r[this.attrsall.fields[x]]) {
                    //         this.data.userdata.cells[colnumToLetters(x + xOffset) + (y + 2)] = r[this.attrsall.fields[x]];
                    //     }
                    //     y++;
                    // }
                    for (y = 0; y < this.rowKeys.length; y++) {
                        // this.data.userdata.cells[colnumToLetters(x + xOffset) + (y + 1)] = this.rows[this.rowKeys[y]][this.attrsall.fields[x]];
                        if (this.styles && !angular.equals(this.styles, {})) {
                            this.data.userdata.cells[currentCol + (y + 1)] = {
                                cell: this.rows[this.rowKeys[y]][this.fields[x]],
                                ...this.styles[this.rowKeys[y]][this.fields[x]],
                            };
                        } else {
                            this.data.userdata.cells[currentCol + (y + 1)] = {cell: this.rows[this.rowKeys[y]][this.fields[x]]};
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
        this.doSaveText([]);
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
        return (this.markup.report == true);
    }

    /**
     * String to determinate how usernames are filtered in report.
     * Choises are username, username and full name and anonymous. Username as default.
     */
    sortBy() {
        return (this.markup.sortBy ?? "username");
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
        const timTable = this.getTimTable();
        if (timTable == null) {
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
            separator: (this.markup.separator ?? ","),
            anonNames: this.markup.anonNames,
            realnames: this.markup.realnames,
            usernames: this.markup.usernames,
            emails: this.markup.emails,
            reportFilter: this.markup.reportFilter,
        };
        let filterParams = {};
        const selUsers = timTable.getCheckedRows(0, false);
        const users = TableFormComponent.makeUserArray(selUsers, userNameColIndex);

        if (selUsers.length > 0) {
            filterParams = {userFilter: users};
        } else {
            const filterFields: string[] = [];
            const filterValues: string[] = [];

            const xOffset = memberShipColIndex + 1;

            timTable.filters.forEach((value, index) => {
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
                    case memberShipColIndex:
                        filterFields.push("membership");
                        break;
                    default:
                        filterFields.push(this.fields[index - xOffset]);
                }
                filterValues.push(value);
            });
            filterParams = {filterFields, filterValues};
        }

        const win = window.open("/tableForm/generateCSV?" + $httpParamSerializer({
            ...reportParams,
            ...filterParams,
        }), "WINDOWID");
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
    static makeUserList(users: string[][], colIndex: number, preseparator: string, midseparator: string): string {
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

        await showInputDialog({
            text: "<b>Really remove the following users from group:</b> " + group + "<br>\n<pre>\n" + msg + "\n</pre>",
            title: "Remove users from group " + group,
            isInput: InputDialogKind.ValidatorOnly,
            validator: async () => {
                const ulist = TableFormComponent.makeUserList(selUsers, 1, "", ",");
                const r = await to($http.post<unknown>(
                    `/groups/removemember/${group}`,
                    {names: ulist.split(",")}));
                if (r.ok) {
                    return {ok: true, result: r.result.data} as const;
                } else {
                    return {ok: false, result: r.result.data.error} as const;
                }
            },
        });
        location.reload();
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
        if (!this.realnamemap) {
            return;
        }
        if (!this.emailmap) {
            return;
        }
        for (const u of selUsers) {
            const un = u[userNameColIndex];
            let s = "";
            if (this.listName) {
                s = this.realnamemap[un];
                usep = ", ";
            }
            if (this.listUsername) {
                s += usep + un;
                usep = ", ";
            }
            if (this.listEmail) {
                s += usep + this.emailmap[un];
                usep = ", ";
            }
            usep = "";
            ulist.push([s]);
        }
        // if ( this.listEmail ) { midseparator = "\n"; preseparator = "";  }
        if (sep == "") {
            sep = "\n";
        }  // radio could not give \n?
        if (sep != "-") {
            midseparator = sep;
            preseparator = "";
        }
        this.userlist = TableFormComponent.makeUserList(ulist, colindex, preseparator, midseparator);
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
        this.emaillist = TableFormComponent.makeUserList(selUsers, emailColIndex, "", "\n");
    }

    /**
     * Callback function to be noticed when check boxes are changed in table
     * @param cbs boolean list of cb-values
     * @param n number of visible checked cbs
     * @param index index of clicked cb, may be -1 if header row cb clicked
     */
    cbChanged(cbs: boolean[], n: number, index: number) {
        this.cbCount = n;
    }

    /**
     * Callback function that gets called when timTable saves a cell
     * @param cellsToSave list of cells that needs to be saved
     * @param colValuesAreSame if all values in on column has same value
     */
    async cellChanged(cellsToSave: CellToSave[] | CellAttrToSave[], colValuesAreSame: boolean) {
        // TODO make better implementation so singleCellSave is not called one by one
        // TODO: maybe done so that push cells to chengedCells and call save
        // TODO: but first check if saved to person or group and to that column by column
        if (this.attrsall.markup.sisugroups) {
            return;
        }

        const globalChangedFields = new Set<string>();

        for (const c of cellsToSave) {
            const coli = c.col;
            const rowi = c.row;
            const content = c.c;
            const changedStyle = c.key;
            if (changedStyle) {
                if (changedStyle == "CLEAR") {
                    this.clearStylesCells.add(colnumToLetters(coli) + (rowi + 1));
                } else {
                    this.clearStylesCells.delete(colnumToLetters(coli) + (rowi + 1));
                }
            }
            if (this.markup.autosave) {
                await this.singleCellSave(rowi, coli, content, globalChangedFields);
            } else {
                this.changedCells.push(colnumToLetters(coli) + (rowi + 1));
            }
        }
        if (this.viewctrl && globalChangedFields.size > 0) {
            if (this.markup.autoUpdateFields) {
                this.viewctrl.updateFields(Array.from(globalChangedFields));
            }
            if (this.markup.autoUpdateTables) {
                this.viewctrl.updateAllTables(Array.from(globalChangedFields));
            }
        }
    }

    /**
     * Calls the actual save function with given cell
     * @param rowi row number
     * @param coli col number
     * @param content unused
     * @param globalChangedFields set where save fields than should be updated
     */
    async singleCellSave(rowi: number, coli: number,
                         content: string,
                         globalChangedFields: Set<string> | undefined = undefined) {
        const cells = [colnumToLetters(coli) + (rowi + 1)];
        await this.doSaveText(cells, globalChangedFields);
    }

    async openTable() {
        if (!this.tableFetched) {
            await this.updateTable();
            this.tableFetched = true;
        }
        this.showTable = true;
    }

    closeTable() {
        this.showTable = false;
    }

    /**
     * Transforms the cell format back to row format and saves the table input
     * @param cells to save
     * @param globalChangedFields set where save fields than should be updated
     */
    async doSaveText(cells: string[], globalChangedFields: Set<string> | undefined = undefined) {
        // this.error = "... saving ...";
        let keys: string[] = [];
        if (cells && cells.length > 0) {
            keys = cells;
        } else {
            // TODO: Force save all?
            // keys = Object.keys(this.data.userdata.cells);
            keys = this.changedCells;
        }
        if (keys.length == 0) {
            return;
        }
        const replyRows: Record<string, Record<string, CellEntity>> = {};
        const styleRows: Record<string, Record<string, string>> = {};
        const changedFields = new Set<string>();
        try {
            for (const coord of keys) {
                const alphaRegExp = new RegExp("([A-Z]*)");
                const alpha = alphaRegExp.exec(coord);
                if (alpha == null) {
                    continue;
                }
                const columnPlace = alpha[0];
                const numberPlace = coord.substring(columnPlace.length);
                if (columnPlace === userNameColumn
                    || columnPlace === realNameColumn  // TODO: Do we need this anymore?
                    || columnPlace === emailColumn) {  // TODO: Do we need this anymore?
                    continue;
                }
                const cell = this.data.userdata.cells[coord];
                let cellContent;
                let cellStyle = null;
                if (!isPrimitiveCell(cell)) {
                    cellContent = cell.cell;
                    if (this.markup.saveStyles) {
                        const cellcopy = clone(cell);
                        delete cellcopy.cell;
                        // cellStyle = JSON.stringify(cellcopy);
                        cellStyle = cellcopy;
                    }
                } else {
                    cellContent = cell;
                }
                if (cellContent === null) {
                    cellContent = "";
                } else if (typeof cellContent === "boolean" || typeof cellContent === "number") {
                    cellContent = cellContent.toString();
                }
                // else if (typeof cellContent === "boolean") {
                //     throw new Error("cell was boolean?");

                // TODO: If attr (auto)updatefields...
                if (true && this.viewctrl) {
                    if (this.viewctrl.selectedUser.name == this.userLocations[numberPlace]) {
                        const taskWithField = this.taskLocations[columnPlace].split(".");
                        const docTask = taskWithField[0] + "." + taskWithField[1];
                        if (globalChangedFields) { // if call from cellChanged update global
                            globalChangedFields.add(docTask);
                        } else {
                            changedFields.add(docTask);
                        }
                    }
                }
                try {
                    replyRows[this.userLocations[numberPlace]][this.taskLocations[columnPlace]] = cellContent;
                } catch (e) {
                    replyRows[this.userLocations[numberPlace]] = {};
                    replyRows[this.userLocations[numberPlace]][this.taskLocations[columnPlace]] = cellContent;
                }
                /* TODO: instead of iterating clearStylesCells could decide that absence of any styles
                    (e.g primitivecell) would mean result in null style value being sent
                */
                if (this.clearStylesCells.has(columnPlace + numberPlace)) {
                    const taskWithField = this.taskLocations[columnPlace].split(".");
                    const docTaskStyles = taskWithField[0] + "." + taskWithField[1] + ".styles";
                    replyRows[this.userLocations[numberPlace]][docTaskStyles] = null;
                } else if (cellStyle != null && Object.keys(cellStyle).length != 0) {
                    const taskWithField = this.taskLocations[columnPlace].split(".");
                    const docTaskStyles = taskWithField[0] + "." + taskWithField[1] + ".styles";
                    replyRows[this.userLocations[numberPlace]][docTaskStyles] = cellStyle;
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
        const r = await to($http.put<{ web: { result: string, error?: string } }>(url, params));
        this.loading = false;
        if (r.ok) {
            const data = r.result.data;
            this.error = data.web.error;
            // this.result = "Saved";
        } else {
            this.error = r.result.data.error; // "Infinite loop or some other error?";
        }
        const timtab = this.getTimTable();
        if (!timtab) {
            return;
        }
        timtab.confirmSaved();
        if (this.viewctrl && changedFields.size > 0) {  // if this.globalChangedFields then this is empty
            if (this.markup.autoUpdateFields) {
                this.viewctrl.updateFields(Array.from(changedFields));
            }
            if (this.markup.autoUpdateTables) {
                this.viewctrl.updateAllTables(Array.from(changedFields));

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
        const groups = TimTableComponent.makeSmallerMatrix(selUsers, [1, this.data.headers.indexOf("TIM-nimi")]);
        const params = groups.map(([sisuid, timname]) => ({externalId: sisuid, name: timname}));
        this.loading = true;
        const r = await to($http.post<{ web: { result: string, error?: string } }>("/sisu/createGroupDocs", params));
        this.loading = false;
        if (r.ok) {
            timTable.confirmSaved();
            location.reload();
        } else {
            this.error = r.result.data.error;
        }
    }

    runJsRunner(runner: string) {
        const timTable = this.getTimTable();
        if (timTable == null) {
            return;
        }
        if (this.viewctrl) {
            const selUsers = timTable.getCheckedRows(0, true);
            const users = TableFormComponent.makeUserArray(selUsers, userNameColIndex);
            this.viewctrl.runJsRunner(runner, users);
        }
    }

    getAttributeType() {
        return TableFormAll;
    }
}

// noinspection AngularInvalidImportedOrDeclaredSymbol
@NgModule({
    declarations: [
        TableFormComponent,
        TimEmailComponent,
    ],
    imports: [
        BrowserModule,
        HttpClientModule,
        FormsModule,
        TimUtilityModule,
        TimTableModule,
    ],
})
export class TableFormModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {
    }
}

const bootstrapFn = (extraProviders: StaticProvider[]) => {
    const platformRef = platformBrowserDynamic(extraProviders);
    return platformRef.bootstrapModule(TableFormModule);
};

const angularJsModule = createDowngradedModule(bootstrapFn);
doDowngrade(angularJsModule, "tableformRunner", TableFormComponent);
export const moduleDefs = [angularJsModule];
