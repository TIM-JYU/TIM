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

interface UserResult {
    user: IUser;
    field: Record<string, string | number | undefined>;
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
            <input class="form-control"
                   placeholder="Search for user"
                   name="search-string"
                   type="text"
                   [(ngModel)]="searchString"
                   (ngModelChange)="inputTyped.next($event)"
                   minlength="{{ inputMinLength }}" required>
            <input class="timButton" type="submit" value="Search" [disabled]="!searchForm.form.valid || search">
        </form>
    `,
    styleUrls: ["user-select.component.scss"],
})
export class UserSelectComponent extends AngularPluginBase<
    t.TypeOf<typeof PluginMarkup>,
    t.TypeOf<typeof PluginFields>,
    typeof PluginFields
> {
    @ViewChild("searchForm") searchForm!: NgForm;

    searchString: string = "";
    inputMinLength: number = 3;
    search: boolean = false;
    searchPress: Subject<void> = new Subject();
    inputTyped: Subject<string> = new Subject();

    ngOnInit() {
        super.ngOnInit();
        this.inputMinLength = this.markup.inputMinLength;
        this.initSearch();
    }

    async doSearch() {
        this.search = true;

        const result = await to2(
            this.http
                .get<UserResult[]>("/userSelect/search", {
                    params: {
                        task_id: this.getTaskId()?.docTask().toString() ?? "",
                        search_string: this.searchString,
                    },
                })
                .toPromise()
        );

        console.log(result);
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
    imports: [BrowserModule, HttpClientModule, FormsModule],
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
