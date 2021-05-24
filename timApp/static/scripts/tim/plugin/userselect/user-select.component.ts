import {
    ApplicationRef,
    Component,
    DoBootstrap,
    ElementRef,
    NgModule,
    ViewChild,
} from "@angular/core";
import {BrowserModule} from "@angular/platform-browser";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import * as t from "io-ts";
import {HttpClientModule, HttpParams} from "@angular/common/http";
import {FormsModule, NgForm} from "@angular/forms";
import {race, Subject, Subscription} from "rxjs";
import {Observable} from "rxjs/internal/Observable";
import {
    debounceTime,
    distinctUntilChanged,
    filter,
    first,
} from "rxjs/operators";
import {Result} from "@zxing/library";
import {BsDropdownModule} from "ngx-bootstrap/dropdown";
import {NoopAnimationsModule} from "@angular/platform-browser/animations";
import {createDowngradedModule, doDowngrade} from "../../downgrade";
import {AngularPluginBase} from "../angular-plugin-base.directive";
import {GenericPluginMarkup, getTopLevelFields, nullable} from "../attributes";
import {
    isMobileDevice,
    templateString,
    timeout,
    TimStorage,
    to2,
} from "../../util/utils";
import {TimUtilityModule} from "../../ui/tim-utility.module";
import {CodeScannerComponent} from "./code-scanner.component";
import {MediaDevicesSupported} from "./util";
import {
    IQueryHandler,
    PrefetchedQueryHandler,
    SearchResult,
    ServerQueryHandler,
    UserResult,
} from "./searchQueryHandlers";

const PluginMarkup = t.intersection([
    GenericPluginMarkup,
    t.type({
        inputMinLength: t.number,
        autoSearchDelay: t.number,
        preFetch: t.boolean,
        maxMatches: t.number,
        selectOnce: t.boolean,
        displayFields: t.array(t.string),
        allowUndo: t.boolean,
        sortBy: t.array(t.string),
        scanner: t.type({
            enabled: t.boolean,
            scanInterval: t.number,
            applyOnMatch: t.boolean,
            continuousMatch: t.boolean,
            waitBetweenScans: t.number,
            beepOnSuccess: t.boolean,
            beepOnFailure: t.boolean,
        }),
        text: t.type({
            apply: nullable(t.string),
            cancel: nullable(t.string),
            success: nullable(t.string),
            undone: nullable(t.string),
            undo: nullable(t.string),
            undoWarning: nullable(t.string),
        }),
    }),
]);

const PluginFields = t.intersection([
    getTopLevelFields(PluginMarkup),
    t.type({}),
]);

async function playBeep(name: string, audio?: HTMLAudioElement) {
    let result = audio;
    // Loading audio may fail, browsers usually throw an exception for Audio function failure
    try {
        if (!result) {
            result = new Audio(`/static/audio/${name}.wav`);
        }
        await result.play();
    } catch (e) {}
    return result;
}

const USER_FIELDS: Record<string, string> = {
    username: $localize`Username`,
    realname: $localize`Real name`,
    useremail: $localize`Email`,
};

class ToggleOption {
    private storageValue!: TimStorage<boolean>;
    private val = false;
    enabled = false;

    constructor(key: string, private toggleName: string) {
        this.storageValue = new TimStorage(key, t.boolean);
        this.val = this.storageValue.get() ?? false;
    }

    get value() {
        return this.val && this.enabled;
    }

    set value(v: boolean) {
        this.val = v;
        this.storageValue.set(this.val);
    }

    get name() {
        return this.toggleName;
    }
}

@Component({
    selector: "user-selector",
    template: `
        <div *ngIf="enableScanner" class="barcode-video">
            <button [disabled]="!queryHandler || !supportsMediaDevices" class="timButton btn-lg"
                    (click)="scanCode = !scanCode">
                <span class="icon-text">
                    <i class="glyphicon glyphicon-qrcode"></i>
                    <span>/</span>
                    <i class="glyphicon glyphicon-barcode"></i><br>
                </span>
                <span i18n>Scan code</span>
            </button>
            <span *ngIf="!supportsMediaDevices" class="label label-default not-supported" i18n>Not supported in this browser</span>
            <tim-code-scanner *ngIf="scanCode" (successfulRead)="onCodeScanned($event)"
                              [scanInterval]="scanInterval" #codeScanner></tim-code-scanner>
        </div>
        <form class="search" (ngSubmit)="searchPress.next()" #searchForm="ngForm">
            <input class="form-control input-lg"
                   placeholder="Search for user"
                   i18n-placeholder
                   name="search-string"
                   type="search"
                   autocomplete="off"
                   [(ngModel)]="searchString"
                   (ngModelChange)="inputTyped.next($event)"
                   [disabled]="!queryHandler || undoing"
                   minlength="{{ inputMinLength }}"
                   required
                   #searchInput>

            <ng-template #submitButton>
                <button class="timButton btn-lg" i18n
                        [disabled]="!queryHandler || !searchForm.form.valid || searching || undoing">
                    Search
                </button>
            </ng-template>

            <ng-container *ngIf="optionToggles.length == 0; else withSearchOptions">
                <ng-container *ngTemplateOutlet="submitButton"></ng-container>
            </ng-container>
            <ng-template #withSearchOptions>
                <div class="btn-group" dropdown>
                    <ng-container *ngTemplateOutlet="submitButton"></ng-container>
                    <button type="button" class="timButton btn-lg dropdown-toggle dropdown-toggle-split" dropdownToggle>
                        <span class="caret"></span>
                        <span class="sr-only">Split button</span>
                    </button>
                    <ul *dropdownMenu class="dropdown-menu" role="menu">
                        <li role="menuitem" *ngFor="let option of optionToggles">
                            <a class="dropdown-item" (click)="option.value = !option.value">
                                <i class="glyphicon glyphicon-ok" style="margin-right: 1rem;" *ngIf="option.value"></i>
                                <span>{{option.name}}</span>
                            </a>
                        </li>
                    </ul>
                </div>
            </ng-template>

        </form>
        <div class="progress" *ngIf="!isInPreview && (searching || !queryHandler)">
            <div class="progress-bar progress-bar-striped active" style="width: 100%;"></div>
        </div>
        <div class="search-result" *ngIf="lastSearchResult">
            <tim-alert *ngIf="lastSearchResult.matches.length == 0" i18n>
                No matches for given keyword
            </tim-alert>
            <form *ngIf="lastSearchResult.matches.length != 0">
                <table *ngIf="lastSearchResult">
                    <thead>
                    <tr>
                        <th i18n>Select</th>
                        <th *ngFor="let fieldName of fieldNames">
                            {{fieldName}}
                        </th>
                    </tr>
                    </thead>
                    <tbody>
                    <tr class="user-row" *ngFor="let match of lastSearchResult.matches"
                        [class.selected-user]="selectedUser == match" (click)="selectedUser = match; onUserSelected()">
                        <td class="select-col">
                            <span class="radio">
                                <label>
                                    <input type="radio"
                                           name="userselect-radios"
                                           [value]="match"
                                           [(ngModel)]="selectedUser"
                                           (ngModelChange)="onUserSelected()"
                                           [disabled]="applying">
                                </label>
                            </span>
                        </td>
                        <td *ngFor="let fieldId of displayFields">{{match.fields[fieldId]}}</td>
                    </tr>
                    </tbody>
                </table>
            </form>
            <div class="more-matches-info"
                 *ngIf="lastSearchResult && lastSearchResult.matches.length < lastSearchResult.allMatchCount"
                 i18n>
                There are {{lastSearchResult.allMatchCount}} matches. Only first {{lastSearchResult.matches.length}} are
                shown.
                Please give more specific keywords to show all results.
            </div>
            <div class="action-buttons" *ngIf="selectedUser">
                <tim-loading *ngIf="applying"></tim-loading>
                <button type="button" class="btn btn-success btn-lg" [disabled]="applying" (click)="apply()">
                    {{applyButtonText}}
                </button>
                <button type="button" class="btn btn-danger btn-lg" [disabled]="applying" (click)="resetView()">
                    {{cancelButtonText}}
                </button>
            </div>
        </div>
        <tim-alert *ngIf="lastAddedUser" severity="success" [closeable]="!undoing" (closing)="resetView()">
            <div class="undoable-message">
                <span class="undoable-text">{{successMessage}}</span>
                <span class="undo-button" *ngIf="allowUndo && !undone">
                    <tim-loading *ngIf="undoing"></tim-loading>
                    <button (click)="verifyUndo = true" class="btn btn-danger"
                            [disabled]="undoing">{{undoButtonLabel}}</button>
                </span>
            </div>
        </tim-alert>
        <tim-alert severity="warning" *ngIf="verifyUndo" [closeable]="!undoing" (closing)="resetView()">
            <div class="undoable-message">
                <span class="undoable-text">{{undoWarningText}}</span>
                <span class="undo-button">
                    <button (click)="undoLast()" class="btn btn-danger">{{undoButtonLabel}}</button>
                </span>
            </div>
        </tim-alert>
        <tim-alert class="small" *ngIf="undoErrors.length > 0" severity="warning">
            <ng-container i18n>
                <p>
                    There were problems while undoing. Someone might have edited the permissions at the same time as
                    you.
                </p>
                <p>You can reapply permissions or fix them manually.</p>
                <p>Detailed error messages:</p>
            </ng-container>
            <div class="alert-details">
                <a (click)="showErrorMessage = !showErrorMessage" i18n><i class="glyphicon glyphicon-chevron-down"></i>Show
                    details</a>
                <ng-container *ngIf="showErrorMessage">
                    <ul *ngFor="let error of undoErrors">
                        <li>{{error}}</li>
                    </ul>
                </ng-container>
            </div>
        </tim-alert>
        <tim-alert class="small" *ngIf="errorMessage" i18n>
            <span>{{errorMessage}}</span>
            <div class="alert-details">
                <p>Please try refreshing the page and try again.</p>
                <ng-container *ngIf="detailedError">
                    <div>
                        <a (click)="showErrorMessage = !showErrorMessage">
                            <i class="glyphicon glyphicon-chevron-down"></i>
                            Show details
                        </a>
                    </div>
                    <pre *ngIf="showErrorMessage">{{detailedError}}</pre>
                </ng-container>
            </div>
        </tim-alert>
        <table class="t9kbd" *ngIf="t9Mode.value">
            <tr>
                <td>
                    <button (click)="applyT9('1')">1<br>&nbsp;</button>
                </td>
                <td>
                    <button (click)="applyT9('2')">2<br>ABC</button>
                <td>
                    <button (click)="applyT9('3')">3<br>DEF</button>
                </td>
            </tr>
            <tr>
                <td>
                    <button (click)="applyT9('4')">4<br>GHI</button>
                </td>
                <td>
                    <button (click)="applyT9('5')">5<br>JKL</button>
                <td>
                    <button (click)="applyT9('6')">6<br>MNO</button>
                </td>
            </tr>
            <tr>
                <td>
                    <button (click)="applyT9('7')">7<br>PQRS</button>
                </td>
                <td>
                    <button (click)="applyT9('8')">8<br>TUV</button>
                <td>
                    <button (click)="applyT9('9')">9<br>WXYZ</button>
                </td>
            </tr>
            <tr>
                <td>
                    <button (click)="applyT9('clr')">clr<br>&nbsp;</button>
                </td>
                <td>
                    <button (click)="applyT9('0')">0<br>space</button>
                <td>
                    <button (click)="applyT9('<=')"><=<br>&nbsp;</button>
                </td>
            </tr>
        </table>
    `,
    styleUrls: ["user-select.component.scss"],
})
export class UserSelectComponent extends AngularPluginBase<
    t.TypeOf<typeof PluginMarkup>,
    t.TypeOf<typeof PluginFields>,
    typeof PluginFields
> {
    @ViewChild("searchForm") searchForm!: NgForm;
    @ViewChild("searchInput") searchInput!: ElementRef<HTMLInputElement>;
    @ViewChild("codeScanner", {read: ElementRef}) codeScanner?: ElementRef<
        HTMLElement
    >;

    showErrorMessage = false;
    errorMessage?: string;
    detailedError?: string;

    searchString: string = "";
    inputMinLength!: number;
    searching: boolean = false;
    applying: boolean = false;
    searchPress: Subject<void> = new Subject();
    inputTyped: Subject<string> = new Subject();
    lastSearchResult?: SearchResult;
    cachedSearchResult?: SearchResult;
    selectedUser?: UserResult;
    lastAddedUser?: UserResult;
    barCodeResult: string = "";
    supportsMediaDevices = true;
    enableScanner = false;
    inputListener?: Subscription;
    beepSuccess?: HTMLAudioElement;
    beepFail?: HTMLAudioElement;
    applyButtonText!: string;
    cancelButtonText!: string;
    scanInterval!: number;
    processScanResults: boolean = true;
    queryHandler!: IQueryHandler;
    isInPreview = false;
    displayFields: string[] = [];
    fieldNames: string[] = [];
    allowUndo!: boolean;
    undoing: boolean = false;
    undone: boolean = false;
    undoErrors: string[] = [];
    scanCode: boolean = false;
    scrollOnce: boolean = false;
    optionToggles: ToggleOption[] = [];
    keyboardMode = new ToggleOption(
        "userSelect_keyboardMode",
        $localize`Keyboard mode`
    );
    t9Mode = new ToggleOption("userSelect_t9Mode", $localize`T9 keyboard`);
    autoApplyOnFullMatch = new ToggleOption(
        "userSelect_autoApplyOnFullMatch",
        $localize`Auto apply on match`
    );
    verifyUndo: boolean = false;

    get successMessage() {
        if (!this.lastAddedUser) {
            return "";
        }
        const template = this.undone
            ? this.markup.text.undone
            : this.markup.text.success;
        if (template) {
            return templateString(template, this.lastAddedUser.fields);
        }
        return this.undone
            ? $localize`Undone permissions for ${this.lastAddedUser?.user.real_name}:INTERPOLATION:.`
            : $localize`Permissions applied to ${this.lastAddedUser?.user.real_name}:INTERPOLATION:.`;
    }

    get undoButtonLabel() {
        return this.markup.text.undo ?? $localize`Undo`;
    }

    get undoWarningText() {
        return (
            this.markup.text.undoWarning ??
            $localize`Are you sure you want to undo the last action?`
        );
    }

    private get searchQueryStrings() {
        // Preliminarily strip whitespace
        const searchString = this.searchString.trim();
        const result = [searchString];

        // When reading Finnish personal identity numbers, code scanner can sometimes misread chars +/-
        // Same can of course happen to a normal person doing a query by hand
        // Therefore, we add alternative search string where we swap +/- with each other
        const mistakePidPattern = /^(\d{6})([+-])(\d{3}[a-zA-Z])$/i;
        const pidMatch = mistakePidPattern.exec(searchString);
        if (pidMatch) {
            const [, pidStart, pidMark, pidEnd] = pidMatch;
            result.push(`${pidStart}${pidMark == "-" ? "+" : "-"}${pidEnd}`);
        }
        return result;
    }

    async undoLast() {
        if (!this.lastAddedUser) {
            return;
        }
        this.undoing = true;
        this.verifyUndo = false;

        // Pass possible urlmacros
        const params = new HttpParams({
            fromString: window.location.search.replace("?", "&"),
        });
        const result = await to2(
            this.http
                .post<{distributionErrors: string[]}>(
                    "/userSelect/undo",
                    {
                        username: this.lastAddedUser.user.name,
                        par: this.getPar().par.getJsonForServer(),
                    },
                    {params}
                )
                .toPromise()
        );

        if (result.ok) {
            this.undoErrors = result.result.distributionErrors;
            // Don't allow to reapply undo on fail to prevent further potential data races
            this.undone = true;
        } else {
            this.undoErrors = [result.result.error.error];
        }

        this.undoing = false;
    }

    onUserSelected() {
        if (
            !this.markup.selectOnce ||
            this.keyboardMode.value ||
            !this.lastSearchResult ||
            !this.selectedUser
        ) {
            return;
        }
        this.lastSearchResult.matches = [this.selectedUser];
        // Force to not show "there are more matches" text
        this.lastSearchResult.allMatchCount = 1;
        const el = this.codeScanner?.nativeElement ?? this.getRootElement();
        el.scrollIntoView();
    }

    async onCodeScanned(result: Result) {
        if (!this.processScanResults) {
            return;
        }
        this.processScanResults = false;
        if (!this.markup.scanner.continuousMatch) {
            this.scanCode = false;
        }
        this.searchString = result.getText();

        await this.search(
            this.markup.scanner.applyOnMatch,
            this.markup.scanner.beepOnSuccess,
            this.markup.scanner.beepOnFailure
        );

        if (
            this.markup.scanner.continuousMatch &&
            this.markup.scanner.waitBetweenScans > 0
        ) {
            await timeout(this.markup.scanner.waitBetweenScans * 1000);
        }
        this.processScanResults = true;
    }

    ngOnInit() {
        super.ngOnInit();
        this.isInPreview = this.isPreview();
        this.supportsMediaDevices = MediaDevicesSupported;
        this.initToggleOptions();

        this.applyButtonText =
            this.markup.text.apply ?? $localize`Set permission`;
        this.cancelButtonText = this.markup.text.cancel ?? $localize`Cancel`;
        this.allowUndo = this.markup.allowUndo;

        this.enableScanner = this.markup.scanner.enabled;
        this.inputMinLength = this.markup.inputMinLength;
        this.listenSearchInput();
        this.inputTyped.subscribe((val) => {
            if (
                !this.markup.preFetch ||
                (this.markup.preFetch &&
                    (val.length == 0 || !this.searchForm.form.valid))
            ) {
                this.selectedUser = undefined;
                this.lastSearchResult = undefined;
            }
        });
        this.scanInterval = this.markup.scanner.scanInterval;

        void this.initQueryHandler();
    }

    async apply() {
        if (!this.selectedUser) {
            return;
        }
        this.applying = true;
        this.resetError();

        // Pass possible urlmacros
        const params = new HttpParams({
            fromString: window.location.search.replace("?", "&"),
        });
        const result = await to2(
            this.http
                .post<{distributionErrors: string[]}>(
                    "/userSelect/apply",
                    {
                        par: this.getPar().par.getJsonForServer(),
                        username: this.selectedUser.user.name,
                    },
                    {params}
                )
                .toPromise()
        );

        if (result.ok) {
            this.lastAddedUser = this.selectedUser;
            this.resetView();
        } else {
            this.setError(
                $localize`Could not apply the permission.`,
                result.result.error.error
            );
        }

        this.applying = false;
    }

    resetView() {
        this.selectedUser = undefined;
        this.lastSearchResult = undefined;
        this.searchString = "";
        this.verifyUndo = false;
        if (!isMobileDevice()) {
            this.searchInput.nativeElement.focus();
        }
        if (this.markup.selectOnce) {
            const el = this.codeScanner?.nativeElement ?? this.getRootElement();
            el.scrollIntoView();
        }
    }

    applyT9(s: string) {
        if (s === "clr") {
            this.resetView();
            return;
        }
        if (s === "<=") {
            // bs
            if (this.searchString.length > 0) {
                this.searchString = this.searchString.substr(
                    0,
                    this.searchString.length - 1
                );
            }
        } else {
            this.searchString += s;
        }
        this.inputTyped.next(this.searchString);
    }

    getAttributeType() {
        return PluginFields;
    }

    // Specify full return type for better IDE type checking
    getDefaultMarkup(): t.TypeOf<typeof PluginMarkup> {
        return {
            scanner: {
                enabled: false,
                scanInterval: 1.5,
                applyOnMatch: false,
                continuousMatch: false,
                waitBetweenScans: 0,
                beepOnSuccess: false,
                beepOnFailure: false,
            },
            text: {
                apply: null,
                cancel: null,
                success: null,
                undone: null,
                undo: null,
                undoWarning: null,
            },
            inputMinLength: 3,
            autoSearchDelay: 0,
            preFetch: false,
            maxMatches: 10,
            selectOnce: false,
            allowUndo: false,
            sortBy: [],
            displayFields: ["username", "realname"],
        };
    }

    private async search(
        applyOnMatch: boolean = false,
        beepOnSuccess: boolean = false,
        beepOnFailure: boolean = false
    ) {
        // Cache the search query strings for later check
        const searchQueryStrings = this.searchQueryStrings;
        const scanOk = await this.doSearch();
        if (
            scanOk &&
            this.lastSearchResult?.allMatchCount != 0 &&
            beepOnSuccess
        ) {
            this.beepSuccess = await playBeep("beep_ok", this.beepSuccess);
        }
        if (
            (!scanOk || this.lastSearchResult?.allMatchCount == 0) &&
            beepOnFailure
        ) {
            this.beepFail = await playBeep("beep_fail", this.beepFail);
        }
        if (
            this.lastSearchResult &&
            applyOnMatch &&
            this.lastSearchResult.matches.length == 1 &&
            this.isFullMatch(
                this.lastSearchResult.matches[0],
                searchQueryStrings
            )
        ) {
            await this.apply();
        }
    }

    private isFullMatch(user: UserResult, keywords: string[]) {
        return Object.values(user.fields).some((v) =>
            keywords.some((q) =>
                typeof v == "string" && typeof q == "string"
                    ? v.toLowerCase() == q.toLowerCase()
                    : q === v
            )
        );
    }

    private async doSearch() {
        this.undone = false;
        this.searching = true;
        this.lastSearchResult = undefined;
        this.lastAddedUser = undefined;
        this.lastAddedUser = undefined;
        this.displayFields = [];
        this.fieldNames = [];
        this.resetError();

        const result = await this.queryHandler.searchUser(
            this.searchQueryStrings,
            this.markup.maxMatches,
            this.t9Mode.value
        );
        if (result.ok) {
            this.lastSearchResult = result.result;

            if (
                this.markup.displayFields.some(
                    (f) => USER_FIELDS[f] === undefined
                )
            ) {
                this.displayFields = this.markup.displayFields;
            } else {
                this.displayFields = [
                    ...this.markup.displayFields,
                    ...result.result.fieldNames,
                ];
            }
            this.fieldNames = this.displayFields.map(
                (f) => USER_FIELDS[f] ?? f
            );

            this.lastSearchResult.matches = this.lastSearchResult.matches.map(
                (ur) => ({
                    ...ur,
                    fields: {
                        ...ur.fields,
                        username: ur.user.name,
                        realname: ur.user.real_name ?? undefined,
                        useremail: ur.user.email ?? undefined,
                    },
                })
            );

            if (this.markup.sortBy.length > 0) {
                this.lastSearchResult.matches = this.lastSearchResult.matches.sort(
                    (a, b) => this.compareUsers(a, b)
                );
            }

            if (this.lastSearchResult.matches.length == 1) {
                this.selectedUser = this.lastSearchResult.matches[0];
            }
        } else {
            this.setError(
                $localize`Could not search for user.`,
                result.result.errorMessage
            );
        }
        this.searching = false;
        this.listenSearchInput();
        return result.ok;
    }

    private compareUsers(firstUser: UserResult, secondUser: UserResult) {
        for (const fieldName of this.markup.sortBy) {
            const a = firstUser.fields[fieldName];
            const b = secondUser.fields[fieldName];

            let index = 0;
            if (typeof a == "string" && typeof b == "string") {
                index = a.localeCompare(b);
            } else if (typeof a == "number" && typeof b == "number") {
                index = a - b;
            } else if (a !== undefined && b !== undefined) {
                index = a.toString().localeCompare(b.toString());
            }

            if (index != 0) {
                return index;
            }
        }
        return 0;
    }

    private initToggleOptions() {
        const addOption = (to: ToggleOption) => {
            to.enabled = true;
            this.optionToggles.push(to);
        };

        if (this.markup.selectOnce) {
            addOption(this.keyboardMode);
            addOption(this.t9Mode);
        }
        if (this.markup.scanner.applyOnMatch) {
            addOption(this.autoApplyOnFullMatch);
        }
    }

    private resetError() {
        this.undoErrors = [];
        this.showErrorMessage = false;
        this.errorMessage = undefined;
        this.detailedError = undefined;
    }

    private setError(message: string, details?: string) {
        this.errorMessage = message;
        this.detailedError = details;
    }

    private listenSearchInput() {
        if (this.inputListener && !this.inputListener.closed) {
            return;
        }
        const observables: Observable<unknown>[] = [this.searchPress];
        if (this.markup.autoSearchDelay > 0 || this.markup.preFetch)
            observables.push(
                this.inputTyped.pipe(
                    debounceTime(
                        this.markup.preFetch
                            ? 0
                            : this.markup.autoSearchDelay * 1000
                    ),
                    distinctUntilChanged(),
                    filter(() => this.searchForm.form.valid)
                )
            );
        this.inputListener = race(...observables)
            .pipe(first())
            .subscribe(() => this.search(this.autoApplyOnFullMatch.value));
    }

    private async initQueryHandler() {
        if (this.isInPreview) {
            return;
        }

        // FIXME: Fetch par info via task because for some reason getPar() is not properly initialized in editor or right after saving the par
        const task = this.pluginMeta.getTaskId();
        let par;
        if (task?.docId && task.blockHint) {
            par = {doc_id: task.docId, par_id: task.blockHint};
        } else {
            par = this.getPar().par.getJsonForServer();
        }

        const queryHandler = this.markup.preFetch
            ? new PrefetchedQueryHandler(this.http, par)
            : new ServerQueryHandler(this.http, par);
        const result = await to2(queryHandler.initialize());
        if (!result.ok) {
            this.setError(
                $localize`Failed to contact the server.`,
                result.result.error.error
            );
        } else {
            this.queryHandler = queryHandler;
        }
    }
}

@NgModule({
    declarations: [UserSelectComponent, CodeScannerComponent],
    imports: [
        BrowserModule,
        HttpClientModule,
        FormsModule,
        TimUtilityModule,
        NoopAnimationsModule,
        BsDropdownModule.forRoot(),
    ],
})
export class UserSelectModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef): void {}
}

export const moduleDefs = [
    doDowngrade(
        createDowngradedModule((extraProviders) =>
            platformBrowserDynamic(extraProviders).bootstrapModule(
                UserSelectModule
            )
        ),
        "userSelector",
        UserSelectComponent
    ),
];
