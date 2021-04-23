import {
    ApplicationRef,
    Component,
    DoBootstrap,
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
        <h3>Search user</h3>
        <form class="search" (ngSubmit)="searchPress.next()" #searchForm="ngForm">
            <input class="form-control input-lg"
                   placeholder="Search for user"
                   name="search-string"
                   type="text"
                   [(ngModel)]="searchString"
                   (ngModelChange)="inputTyped.next($event)"
                   minlength="{{ inputMinLength }}" required>
            <input class="timButton btn-lg" type="submit" value="Search" [disabled]="!searchForm.form.valid || search">
        </form>
        <div class="search-result" *ngIf="search || lastSearchResult">
            <div class="progress" *ngIf="search">
                <div class="progress-bar progress-bar-striped active" style="width: 100%;"></div>
            </div>

            <table *ngIf="lastSearchResult">
                <thead>
                <td>Select</td>
                <td>Username</td>
                <td>Full name</td>
                <td *ngFor="let fieldName of lastSearchResult.fieldNames">
                    {{fieldName}}
                </td>
                </thead>
                <tbody *ngIf="lastSearchResult.matches.length == 0">
                <tr>
                    <td [colSpan]="3 + lastSearchResult.fieldNames.length">
                        <em>No matches for given keyword</em>
                    </td>
                </tr>
                </tbody>
                <tbody *ngIf="lastSearchResult.matches.length > 0">
                <tr class="user-row" *ngFor="let match of lastSearchResult.matches"
                    [class.selected-user]="selectedUser == match.user" (click)="selectedUser = match.user">
                    <td>
                            <span class="radio">
                                <label>
                                    <input type="radio" name="{{taskId}}-userselect-radios" [value]="match.user"
                                           [(ngModel)]="selectedUser">
                                </label>
                            </span>
                    </td>
                    <td>{{match.user.name}}</td>
                    <td>{{match.user.real_name}}</td>
                    <td *ngFor="let fieldName of lastSearchResult.fieldNames">{{match.fields[fieldName]}}</td>
                </tr>
                </tbody>
            </table>

            <div class="small"
                 *ngIf="lastSearchResult && lastSearchResult.matches.length < lastSearchResult.allMatchCount">
                There are {{lastSearchResult.allMatchCount}} matches. Only first {{lastSearchResult.matches.length}} are
                shown.
                Please give more specific keywords to show all results.
            </div>

            <div class="action-buttons" *ngIf="selectedUser">
                <tim-loading></tim-loading>
                <button type="button" class="btn btn-success btn-lg">Apply</button>
                <button type="button" class="btn btn-danger btn-lg">Cancel</button>
            </div>
        </div>
        <tim-alert *ngIf="errorMessage">
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

    showErrorMessage = false;
    errorMessage?: string;
    searchString: string = "";
    inputMinLength: number = 3;
    search: boolean = false;
    searchPress: Subject<void> = new Subject();
    inputTyped: Subject<string> = new Subject();
    lastSearchResult?: SearchResult;
    taskId: string = "";
    selectedUser?: IUser;

    ngOnInit() {
        super.ngOnInit();
        this.inputMinLength = this.markup.inputMinLength;
        this.initSearch();
        this.taskId = this.getTaskId()?.docTask().toString() ?? "";
    }

    async doSearch() {
        this.search = true;
        this.lastSearchResult = undefined;
        this.errorMessage = undefined;
        this.selectedUser = undefined;
        const result = await to2(
            this.http
                .get<SearchResult>("/userSelect/search", {
                    params: {
                        task_id: this.taskId,
                        search_string: this.searchString,
                    },
                })
                .toPromise()
        );

        if (result.ok) {
            this.lastSearchResult = result.result;
            if (this.lastSearchResult.matches.length > 0) {
                this.selectedUser = this.lastSearchResult.matches[0].user;
            }
        } else {
            this.showErrorMessage = false;
            this.errorMessage = result.result.error.error;
        }
        this.search = false;
        this.initSearch();
    }

    getAttributeType(): typeof PluginFields {
        return PluginFields;
    }

    getDefaultMarkup(): Partial<t.TypeOf<typeof PluginMarkup>> {
        return {};
    }

    private initSearch() {
        const observables: Observable<unknown>[] = [this.searchPress];
        if (this.markup.autoSearchDelay > 0)
            observables.push(
                this.inputTyped.pipe(
                    filter(() => this.searchForm.form.valid),
                    distinctUntilChanged(),
                    debounceTime(this.markup.autoSearchDelay * 1000)
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
