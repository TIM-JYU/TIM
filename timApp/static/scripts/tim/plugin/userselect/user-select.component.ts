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
import {createDowngradedModule, doDowngrade} from "../../downgrade";
import {AngularPluginBase} from "../angular-plugin-base.directive";
import {
    GenericPluginMarkup,
    getTopLevelFields,
    withDefault,
} from "../attributes";
import {to2} from "../../util/utils";

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
        <form class="search" (ngSubmit)="doSearch()" #searchForm="ngForm">
            <input name="search-string" type="text" [(ngModel)]="searchString" minlength="{{ inputMinLength }}" required
                   (input)="startSearchTimer()">
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
    private currentTimeoutTimer = 0;

    ngOnInit() {
        super.ngOnInit();
        this.inputMinLength = this.markup.inputMinLength;
    }

    startSearchTimer() {
        if (this.markup.autoSearchDelay > 0) {
            if (this.currentTimeoutTimer) {
                window.clearTimeout(this.currentTimeoutTimer);
                this.currentTimeoutTimer = 0;
            }
            this.currentTimeoutTimer = window.setTimeout(() => {
                this.currentTimeoutTimer = 0;
                if (this.searchForm.form.valid) {
                    this.searchForm.ngSubmit.emit();
                }
            }, this.markup.autoSearchDelay * 1000);
        }
    }

    async doSearch() {
        if (this.currentTimeoutTimer) {
            window.clearTimeout(this.currentTimeoutTimer);
            this.currentTimeoutTimer = 0;
        }
        this.search = true;

        const result = await to2(
            this.http
                .get("/userSelect/search", {
                    params: {
                        task_id: this.getTaskId()?.docTask().toString() ?? "",
                        search_string: this.searchString,
                    },
                })
                .toPromise()
        );

        console.log(result);
        this.search = false;
    }

    getAttributeType(): typeof PluginFields {
        return PluginFields;
    }

    getDefaultMarkup(): Partial<t.TypeOf<typeof PluginMarkup>> {
        return {};
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
