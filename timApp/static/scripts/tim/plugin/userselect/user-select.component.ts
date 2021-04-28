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
import {BrowserMultiFormatReader, NotFoundException} from "@zxing/library";
import {createDowngradedModule, doDowngrade} from "../../downgrade";
import {AngularPluginBase} from "../angular-plugin-base.directive";
import {GenericPluginMarkup, getTopLevelFields, nullable} from "../attributes";
import {
    formatString,
    isMobileDevice,
    timeout,
    TimStorage,
    to2,
} from "../../util/utils";
import {IUser} from "../../user/IUser";
import {TimUtilityModule} from "../../ui/tim-utility.module";

interface UserResult {
    user: IUser;
    fields: Record<string, string | number | undefined>;
}

interface SearchResult {
    matches: UserResult[];
    allMatchCount: number;
    fieldNames: string[];
}

const PluginMarkup = t.intersection([
    GenericPluginMarkup,
    t.type({
        inputMinLength: t.number,
        autoSearchDelay: t.number,
        scanner: t.type({
            enabled: t.boolean,
            scanInterval: t.number,
            applyOnMatch: t.boolean,
            continuousMatch: t.boolean,
            waitBetweenScans: t.number,
            beepOnSuccess: t.boolean,
        }),
        text: t.type({
            apply: nullable(t.string),
            cancel: nullable(t.string),
            success: nullable(t.string),
        }),
    }),
]);

const PluginFields = t.intersection([
    getTopLevelFields(PluginMarkup),
    t.type({}),
]);

@Component({
    selector: "user-selector",
    template: `
        <div *ngIf="enableScanner" class="barcode-video">
            <video [class.hidden]="!codeReaderStream" #barcodeOutput></video>
            <button [disabled]="!supportsMediaDevices" class="timButton btn-lg"
                    (click)="startCodeReader()">
                <span class="icon-text">
                    <i class="glyphicon glyphicon-qrcode"></i>
                    <span>/</span>
                    <i class="glyphicon glyphicon-barcode"></i><br>
                </span>
                <span>Scan code</span>
            </button>
            <span *ngIf="!supportsMediaDevices" class="label label-default not-supported" i18n>Not supported in this browser</span>
            <span *ngIf="!hasCameras" class="label label-danger not-supported" i18n>No cameras found</span>
            <span *ngIf="supportsMediaDevices && !supportsConstraint('torch')" class="label label-default not-supported"
                  i18n>Flashlight is not supported</span>
            <div class="input-group" *ngIf="availableCameras.length > 1">
                <span class="input-group-addon" i18n>Selected camera</span>
                <select class="form-control" [(ngModel)]="selectedCamera" (ngModelChange)="cameraSelected()">
                    <option *ngFor="let camera of availableCameras" [ngValue]="camera.id">{{camera.name}}</option>
                </select>
            </div>
        </div>
        <form class="search" (ngSubmit)="searchPress.next()" #searchForm="ngForm">
            <input class="form-control input-lg"
                   placeholder="Search for user"
                   i18n-placeholder
                   name="search-string"
                   type="text"
                   [(ngModel)]="searchString"
                   (ngModelChange)="inputTyped.next($event)"
                   minlength="{{ inputMinLength }}"
                   required
                   #searchInput>
            <input class="timButton btn-lg" type="submit" value="Search" i18n-value
                   [disabled]="!searchForm.form.valid || search">
        </form>
        <div class="search-result" *ngIf="search || lastSearchResult">
            <div class="progress" *ngIf="search">
                <div class="progress-bar progress-bar-striped active" style="width: 100%;"></div>
            </div>
            <form>
                <table *ngIf="lastSearchResult">
                    <thead>
                    <tr>
                        <th i18n>Select</th>
                        <th i18n>Username</th>
                        <th i18n>Full name</th>
                        <th *ngFor="let fieldName of lastSearchResult.fieldNames">
                            {{fieldName}}
                        </th>
                    </tr>
                    </thead>
                    <tbody *ngIf="lastSearchResult.matches.length == 0">
                    <tr>
                        <td [colSpan]="3 + lastSearchResult.fieldNames.length">
                            <em i18n>No matches for given keyword</em>
                        </td>
                    </tr>
                    </tbody>
                    <tbody *ngIf="lastSearchResult.matches.length > 0">

                    <tr class="user-row" *ngFor="let match of lastSearchResult.matches"
                        [class.selected-user]="selectedUser == match.user" (click)="selectedUser = match.user">
                        <td class="select-col">
                            <span class="radio">
                                <label>
                                    <input type="radio"
                                           name="userselect-radios"
                                           [value]="match.user"
                                           [(ngModel)]="selectedUser"
                                           [disabled]="applying">
                                </label>
                            </span>
                        </td>
                        <td>{{match.user.name}}</td>
                        <td>{{match.user.real_name}}</td>
                        <td *ngFor="let fieldName of lastSearchResult.fieldNames">{{match.fields[fieldName]}}</td>
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
        <tim-alert *ngIf="lastAddedUser" severity="success">
            {{successMessage}}
        </tim-alert>
        <tim-alert *ngIf="errorMessage" i18n>
            <span>{{errorMessage}}</span>
            <div style="margin-top: 1rem;">
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

        <ng-template i18n="@@userSelectErrorNoSearchResult">Could not search for the user.</ng-template>
        <ng-template i18n="@@userSelectScanError">Could not scan the bar code.</ng-template>
        <ng-template i18n="@@userSelectApplyError">Could not apply the permission.</ng-template>
        <ng-template i18n="@@userSelectButtonApply">Set permission</ng-template>
        <ng-template i18n="@@userSelectButtonCancel">Cancel</ng-template>
        <ng-template i18n="@@userSelectTextSuccess">Permissions applied to {{0}}.</ng-template>
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
    @ViewChild("barcodeOutput") barcodeOutput!: ElementRef<HTMLVideoElement>;

    showErrorMessage = false;
    errorMessage?: string;
    detailedError?: string;

    searchString: string = "";
    inputMinLength!: number;
    search: boolean = false;
    applying: boolean = false;
    searchPress: Subject<void> = new Subject();
    inputTyped: Subject<string> = new Subject();
    lastSearchResult?: SearchResult;
    selectedUser?: IUser;
    lastAddedUser?: string;
    barCodeResult: string = "";
    videoAspectRatio: number = 1;
    videoWidth: number = 50;
    supportsMediaDevices = true;
    hasCameras = true;
    enableScanner = false;
    codeReader!: BrowserMultiFormatReader;
    codeReaderStream?: MediaStream;
    inputListener?: Subscription;
    beepAudio?: HTMLAudioElement;
    availableCameras: {id: string; name: string}[] = [];
    selectedCamera?: string;
    private supportedCameraConstraints: Record<string, boolean> = {};
    private selectedCameraStorage = new TimStorage(
        "codeScannerCamera",
        t.string
    );

    applyButtonText!: string;
    cancelButtonText!: string;

    get successMessage() {
        if (this.markup.text.success) {
            return formatString(this.markup.text.success, this.lastAddedUser!);
        }
        return $localize`:@@userSelectTextSuccess:Permissions applied to ${this.lastAddedUser}:INTERPOLATION:.`;
    }

    private async playBeep() {
        try {
            if (!this.beepAudio) {
                this.beepAudio = new Audio("/static/audio/beep.wav");
            }
            await this.beepAudio.play();
        } catch (e) {}
    }

    ngOnInit() {
        super.ngOnInit();
        void this.initMediaDevices();

        this.applyButtonText =
            this.markup.text.apply ??
            $localize`:@@userSelectButtonApply:Set permission`;
        this.cancelButtonText =
            this.markup.text.cancel ??
            $localize`:@@userSelectButtonCancel:Cancel`;

        this.enableScanner = this.markup.scanner.enabled;
        this.inputMinLength = this.markup.inputMinLength;
        this.initSearch();
        this.inputTyped.subscribe(() => {
            this.selectedUser = undefined;
            this.lastSearchResult = undefined;
        });
    }

    async cameraSelected() {
        this.selectedCameraStorage.set(this.selectedCamera!);
        if (!this.codeReader) {
            return;
        }
        await this.startCodeReader();
    }

    initMediaDevices() {
        this.codeReader = new BrowserMultiFormatReader(
            undefined,
            this.markup.scanner.scanInterval * 1000
        );
        this.supportsMediaDevices = this.codeReader.isMediaDevicesSuported;
        if (this.supportsMediaDevices) {
            // There can be more constraints than what TS lib lists
            this.supportedCameraConstraints = navigator.mediaDevices.getSupportedConstraints() as Record<
                string,
                boolean
            >;
            this.selectedCamera = this.selectedCameraStorage.get();
        }
    }

    async hasVideoDevice() {
        try {
            await navigator.mediaDevices.getUserMedia({video: true});
            return true;
        } catch (e) {
            return false;
        }
    }

    async startCodeReader() {
        // Ask for permission first
        this.hasCameras = await this.hasVideoDevice();
        if (!this.hasCameras) {
            return;
        }

        this.resetError();
        this.resetView();
        await this.resetCodeReader();
        try {
            const codeReader = new BrowserMultiFormatReader(
                undefined,
                this.markup.scanner.scanInterval * 1000
            );
            this.availableCameras = (
                await codeReader.listVideoInputDevices()
            ).map((d) => ({
                id: d.deviceId,
                name: d.label,
            }));
            this.selectedCamera =
                this.selectedCameraStorage.get() ?? this.availableCameras[0].id;

            // Reset camera if it's missing
            if (
                !this.availableCameras.find((c) => c.id == this.selectedCamera)
            ) {
                this.selectedCamera = this.availableCameras[0].id;
                this.selectedCameraStorage.set(this.selectedCamera);
            }

            const stream = await navigator.mediaDevices.getUserMedia({
                video: {
                    deviceId: this.selectedCamera,
                },
            });
            this.codeReader = codeReader;
            this.codeReaderStream = stream;

            this.videoAspectRatio =
                stream.getVideoTracks()[0].getSettings().aspectRatio ?? 1;

            const decoder = codeReader.decodeOnceFromStream(
                stream,
                this.barcodeOutput.nativeElement
            );

            // Set focus mode separately since apparently it can cause torch to not be enabled later
            await this.setAdvancedCameraConstraints({
                focusMode: "auto",
            });
            // https://github.com/zxing-js/library/issues/267
            await this.setAdvancedCameraConstraints({
                torch: true,
            });

            const result = await decoder;
            this.searchString = result.getText();
            if (this.markup.scanner.beepOnSuccess) {
                await this.playBeep();
            }
            await this.doSearch();
            if (this.lastSearchResult && this.markup.scanner.applyOnMatch) {
                if (this.lastSearchResult.matches.length == 1) {
                    await this.apply();
                    if (this.markup.scanner.continuousMatch) {
                        if (this.markup.scanner.waitBetweenScans > 0) {
                            await timeout(
                                this.markup.scanner.waitBetweenScans * 1000
                            );
                        }
                        // noinspection ES6MissingAwait: Run the code in a new context without deepening the call stack
                        this.startCodeReader();
                        return;
                    }
                }
            }
        } catch (e) {
            const err = $localize`:@@userSelectErrorNoSearchResult:Could not search for the user.`;
            // Simply reset if no code found
            if (e instanceof NotFoundException) {
                await this.resetCodeReader();
                return;
            }
            if (e instanceof Error) {
                this.setError(err, `${e.name}: ${e.message}`);
            } else {
                this.setError(err);
            }
        }
        await this.resetCodeReader();
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
                .post(
                    "/userSelect/apply",
                    {
                        par: this.getPar().par.getJsonForServer(),
                        username: this.selectedUser.name,
                    },
                    {params}
                )
                .toPromise()
        );

        if (result.ok) {
            this.lastAddedUser =
                this.selectedUser.real_name ?? this.selectedUser.name;
            this.resetView();
        } else {
            this.setError(
                $localize`:@@userSelectApplyError:Could not apply the permission.`,
                result.result.error.error
            );
        }

        this.applying = false;
    }

    resetView() {
        this.selectedUser = undefined;
        this.lastSearchResult = undefined;
        this.searchString = "";
        if (!isMobileDevice()) {
            this.searchInput.nativeElement.focus();
        }
    }

    async doSearch() {
        this.search = true;
        this.lastSearchResult = undefined;
        this.lastAddedUser = undefined;
        this.lastAddedUser = undefined;
        this.resetError();
        await this.resetCodeReader();

        const params = new HttpParams({
            fromString: window.location.search.replace("?", "&"),
        });
        const result = await to2(
            this.http
                .post<SearchResult>(
                    "/userSelect/search",
                    {
                        par: this.getPar().par.getJsonForServer(),
                        search_string: this.searchString,
                    },
                    {params}
                )
                .toPromise()
        );

        if (result.ok) {
            this.lastSearchResult = result.result;
            if (this.lastSearchResult.matches.length > 0) {
                this.selectedUser = this.lastSearchResult.matches[0].user;
            }
        } else {
            this.setError(
                $localize`:@@userSelectScanError:Could not scan the bar code.`,
                result.result.error.error
            );
        }
        this.search = false;
        this.initSearch();
    }

    getAttributeType() {
        return PluginFields;
    }

    getDefaultMarkup() {
        return {
            scanner: {
                enabled: false,
                scanInterval: 1.5,
                applyOnMatch: false,
                continuousMatch: false,
                waitBetweenScans: 0,
                beepOnSuccess: false,
            },
            text: {
                apply: null,
                cancel: null,
                success: null,
            },
            inputMinLength: 3,
            autoSearchDelay: 0,
        };
    }

    supportsConstraint(name: string) {
        return (
            this.supportsMediaDevices && this.supportedCameraConstraints[name]
        );
    }

    private async setAdvancedCameraConstraints(
        constraints: Record<string, unknown>
    ) {
        if (!this.codeReaderStream) {
            return;
        }
        const cleanConstraints: Record<string, unknown> = {};
        for (const k in constraints) {
            if (
                Object.hasOwnProperty.call(constraints, k) &&
                this.supportsConstraint(k)
            ) {
                cleanConstraints[k] = constraints[k];
            }
        }
        if (!cleanConstraints) {
            return;
        }
        // Browser *should* ignore unknown constraints, but on some browsers it appears to throw nonetheless
        try {
            await this.codeReaderStream.getVideoTracks()[0].applyConstraints({
                advanced: [cleanConstraints],
            });
        } catch (e) {
            // Swallow the error; the torch is just not supported
        }
    }

    private async resetCodeReader() {
        try {
            await this.setAdvancedCameraConstraints({torch: false});
            this.codeReader.reset();
        } catch (e) {}
        this.codeReaderStream = undefined;
    }

    private resetError() {
        this.showErrorMessage = false;
        this.errorMessage = undefined;
        this.detailedError = undefined;
    }

    private setError(message: string, details?: string) {
        this.errorMessage = message;
        this.detailedError = details;
    }

    private initSearch() {
        if (this.inputListener && !this.inputListener.closed) {
            return;
        }
        const observables: Observable<unknown>[] = [this.searchPress];
        if (this.markup.autoSearchDelay > 0)
            observables.push(
                this.inputTyped.pipe(
                    debounceTime(this.markup.autoSearchDelay * 1000),
                    distinctUntilChanged(),
                    filter(() => this.searchForm.form.valid)
                )
            );
        this.inputListener = race(...observables)
            .pipe(first())
            .subscribe(() => this.doSearch());
    }
}

@NgModule({
    declarations: [UserSelectComponent],
    imports: [BrowserModule, HttpClientModule, FormsModule, TimUtilityModule],
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
