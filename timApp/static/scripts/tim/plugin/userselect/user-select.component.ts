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
import {BrowserMultiFormatReader} from "@zxing/library";
import {createDowngradedModule, doDowngrade} from "../../downgrade";
import {AngularPluginBase} from "../angular-plugin-base.directive";
import {GenericPluginMarkup, getTopLevelFields} from "../attributes";
import {to2} from "../../util/utils";
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
            <video [class.hidden]="!codeReader" #barcodeOutput></video>
            <button [disabled]="!hasMediaDevices || codeReader" class="timButton btn-lg" (click)="initCodeReader()">
                <span class="icon-text">
                    <i class="glyphicon glyphicon-qrcode"></i>
                    <span>/</span>
                    <i class="glyphicon glyphicon-barcode"></i><br>
                </span>
                <span>Scan code</span>
            </button>
            <span *ngIf="!hasMediaDevices" class="label label-default not-supported">Not supported on this device</span>
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
                <button type="button" class="btn btn-success btn-lg" [disabled]="applying" (click)="apply()" i18n>Apply
                </button>
                <button type="button" class="btn btn-danger btn-lg" [disabled]="applying" (click)="resetView()" i18n>
                    Cancel
                </button>
            </div>
        </div>
        <tim-alert *ngIf="lastAddedUser" severity="success" i18n>
            Permissions applied successfully to <strong>{{lastAddedUser}}</strong>.
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
    hasMediaDevices = false;
    enableScanner = false;
    codeReader?: BrowserMultiFormatReader;
    inputListener?: Subscription;

    ngOnInit() {
        super.ngOnInit();
        void this.initMediaDevices();
        this.enableScanner = this.markup.scanner.enabled;
        this.inputMinLength = this.markup.inputMinLength;
        this.initSearch();
        this.inputTyped.subscribe(() => {
            this.selectedUser = undefined;
            this.lastSearchResult = undefined;
        });
    }

    async initMediaDevices() {
        this.hasMediaDevices = "mediaDevices" in navigator;
        if (this.hasMediaDevices) {
            const devices = await navigator.mediaDevices.enumerateDevices();
            this.hasMediaDevices =
                devices.filter((d) => d.kind == "videoinput").length > 0;
        }
    }

    async initCodeReader() {
        this.resetError();
        this.resetView(false);
        try {
            this.codeReader = new BrowserMultiFormatReader(
                undefined,
                this.markup.scanner.scanInterval * 1000
            );
            const stream = await navigator.mediaDevices.getUserMedia({
                video: {
                    facingMode: "environment",
                },
            });

            this.videoAspectRatio =
                stream.getVideoTracks()[0].getSettings().aspectRatio ?? 1;

            const decoder = this.codeReader.decodeOnceFromStream(
                stream,
                this.barcodeOutput.nativeElement
            );

            // Trick: set torch constraints for the stream after reading starts
            // See https://github.com/zxing-js/library/issues/267
            await stream.getVideoTracks()[0].applyConstraints({
                advanced: [
                    {
                        torch: true,
                        fillLightMode: "torch",
                    } as Record<string, unknown>,
                ],
            });

            const result = await decoder;
            this.searchString = result.getText();
            await this.doSearch();
            if (this.lastSearchResult && this.markup.scanner.applyOnMatch) {
                if (this.lastSearchResult.matches.length == 1) {
                    await this.apply();
                }
            }
        } catch (e) {
            const err = $localize`:@@userSelectErrorNoSearchResult:Could not search for the user.`;
            if (e instanceof Error) {
                this.setError(err, `${e.name}: ${e.message}`);
            } else {
                this.setError(err);
            }
        }
        this.resetCodeReader();
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

    resetView(refocus: boolean = true) {
        this.selectedUser = undefined;
        this.lastSearchResult = undefined;
        this.searchString = "";
        if (refocus) {
            this.searchInput.nativeElement.focus();
        }
    }

    async doSearch() {
        this.search = true;
        this.lastSearchResult = undefined;
        this.lastAddedUser = undefined;
        this.lastAddedUser = undefined;
        this.resetError();
        this.resetCodeReader();

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
            },
            inputMinLength: 3,
            autoSearchDelay: 0,
        };
    }

    private resetCodeReader() {
        if (!this.codeReader) {
            return;
        }
        try {
            this.codeReader.reset();
        } catch (e) {}
        this.codeReader = undefined;
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
