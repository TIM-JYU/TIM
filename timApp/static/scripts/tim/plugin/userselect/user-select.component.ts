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
import {HttpClientModule} from "@angular/common/http";
import {FormsModule, NgForm} from "@angular/forms";
import {race, Subject} from "rxjs";
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
import {
    GenericPluginMarkup,
    getTopLevelFields,
    withDefault,
} from "../attributes";
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
        inputMinLength: withDefault(t.number, 3),
        autoSearchDelay: withDefault(t.number, 0),
    }),
]);

const PluginFields = t.intersection([
    getTopLevelFields(PluginMarkup),
    t.type({}),
]);

@Component({
    selector: "user-selector",
    template: `
        <div class="barcode-video">
            <video [class.hidden]="!readBarcode" #barcodeOutput></video>
            <button [disabled]="readBarcode" class="timButton btn-lg" (click)="initCodeReader()">
                <span class="icon-text">
                    <i class="glyphicon glyphicon-qrcode"></i>
                    <span>/</span>
                    <i class="glyphicon glyphicon-barcode"></i><br>
                </span>
                <span>Scan code</span>
            </button>
        </div>
            <p>{{barCodeResult}}</p>    
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
        <tim-alert *ngIf="applied" severity="success" i18n>
            Permissions applied successfully.
        </tim-alert>
        <tim-alert *ngIf="errorMessage" i18n>
            <span>Could not search for the user.</span>
            <div style="margin-top: 1rem;">
                <p>Please try refreshing the page and try again.</p>
                <div><a (click)="showErrorMessage = !showErrorMessage"><i class="glyphicon glyphicon-chevron-down"></i>
                    Show details</a></div>
                <pre *ngIf="showErrorMessage">{{errorMessage}}</pre>
            </div>
        </tim-alert>
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
    searchString: string = "";
    inputMinLength!: number;
    search: boolean = false;
    applying: boolean = false;
    applied: boolean = false;
    searchPress: Subject<void> = new Subject();
    inputTyped: Subject<string> = new Subject();
    lastSearchResult?: SearchResult;
    selectedUser?: IUser;
    barCodeResult: string = "";
    videoAspectRatio: number = 1;
    videoWidth: number = 50;
    readBarcode: boolean = false;

    ngOnInit() {
        super.ngOnInit();
        this.inputMinLength = this.markup.inputMinLength;
        this.initSearch();
        this.inputTyped.subscribe(() => {
            this.selectedUser = undefined;
            this.lastSearchResult = undefined;
        });
    }

    async initCodeReader() {
        this.readBarcode = true;
        const scanTime = 1500;
        const reader = new BrowserMultiFormatReader(undefined, scanTime);
        const stream = await navigator.mediaDevices.getUserMedia({
            video: {
                facingMode: "environment",
            },
        });

        this.videoAspectRatio =
            stream.getVideoTracks()[0].getSettings().aspectRatio ?? 1;

        const decoder = reader.decodeOnceFromStream(
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

        reader.reset();

        this.readBarcode = false;
    }

    async apply() {
        if (!this.selectedUser) {
            return;
        }
        this.applying = true;
        this.resetError();

        const result = await to2(
            this.http
                .post("/userSelect/apply", {
                    par: this.getPar().par.getJsonForServer(),
                    username: this.selectedUser.name,
                })
                .toPromise()
        );

        if (result.ok) {
            this.applied = true;
            this.resetView();
        } else {
            this.errorMessage = result.result.error.error;
        }

        this.applying = false;
    }

    resetView() {
        this.selectedUser = undefined;
        this.lastSearchResult = undefined;
        this.searchString = "";
        this.searchInput.nativeElement.focus();
    }

    async doSearch() {
        this.search = true;
        this.lastSearchResult = undefined;
        this.applied = false;
        this.resetError();
        const result = await to2(
            this.http
                .post<SearchResult>("/userSelect/search", {
                    par: this.getPar().par.getJsonForServer(),
                    search_string: this.searchString,
                })
                .toPromise()
        );

        if (result.ok) {
            this.lastSearchResult = result.result;
            if (this.lastSearchResult.matches.length > 0) {
                this.selectedUser = this.lastSearchResult.matches[0].user;
            }
        } else {
            this.errorMessage = result.result.error.error;
        }
        this.search = false;
        this.initSearch();
    }

    getAttributeType() {
        return PluginFields;
    }

    getDefaultMarkup() {
        return {};
    }

    private resetError() {
        this.showErrorMessage = false;
        this.errorMessage = undefined;
    }

    private initSearch() {
        const observables: Observable<unknown>[] = [this.searchPress];
        if (this.markup.autoSearchDelay > 0)
            observables.push(
                this.inputTyped.pipe(
                    debounceTime(this.markup.autoSearchDelay * 1000),
                    distinctUntilChanged(),
                    filter(() => this.searchForm.form.valid)
                )
            );
        race(...observables)
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
