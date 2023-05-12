/**
 * Defines the client-side implementation of data import plugin.
 */
import * as t from "io-ts";
import type {ApplicationRef, DoBootstrap} from "@angular/core";
import {Component, NgModule} from "@angular/core";
import {HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {registerPlugin} from "tim/plugin/pluginRegistry";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import type {IUser} from "tim/user/IUser";
import {TimStorage} from "tim/util/utils";
import {
    GenericPluginMarkup,
    Info,
    nullable,
    withDefault,
} from "tim/plugin/attributes";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import {CommonModule} from "@angular/common";

const ImportDataMarkup = t.intersection([
    t.partial({
        beforeOpen: t.string,
        buttonBottom: t.boolean,
        message: t.string,
        width: t.number,
        height: t.number,
        tool: t.boolean,
        fields: t.array(t.string),
        createMissingUsers: t.boolean,
        addUsersToGroup: t.string,
        aplus: t.type({
            course: t.number,
        }),
    }),
    GenericPluginMarkup,
    t.type({
        open: withDefault(t.boolean, false),
        lang: withDefault(t.string, "fi"),
        upload: withDefault(t.boolean, false),
        useurl: withDefault(t.boolean, false),
        useseparator: withDefault(t.boolean, false),
        usefields: withDefault(t.boolean, false),
        uploadstem: withDefault(t.string, "File to upload:"),
        loadButtonText: withDefault(t.string, "Load from URL"),
        urlstem: withDefault(t.string, "URL: "),
        separatorstem: withDefault(t.string, "Separator: "),
        separator: withDefault(t.string, ";"),
        url: withDefault(t.string, ""),
        beforeOpen: withDefault(t.string, "+ Open import"),
        placeholder: withDefault(t.string, "Put here content to import"),
        showInView: withDefault(t.boolean, false),
        useurltoken: withDefault(t.boolean, false),
    }),
]);
const ImportDataAll = t.intersection([
    t.partial({}),
    t.type({
        info: Info,
        markup: ImportDataMarkup,
        preview: t.boolean,
        state: nullable(
            t.partial({
                url: t.string,
                separator: t.string,
                fields: t.array(t.string),
            })
        ),
    }),
]);

interface ISchacPersonalUniqueCode {
    code: string;
    codetype: string;
    org: string;
}

interface IUserInfo {
    username?: string;
    full_name?: string;
    email?: string;
    unique_codes: ISchacPersonalUniqueCode[];
}

interface IFieldSaveResult {
    fields_changed: number;
    fields_ignored: number;
    fields_unchanged: number;
    users_created: IUser[];
    users_missing: IUserInfo[];
}

// noinspection TypeScriptUnresolvedVariable
@Component({
    selector: "importdata-runner",
    template: `
        <button *ngIf="!isOpen" class="timButton" [innerHtml]="markup.beforeOpen" (click)="isOpen = true"></button>
        <tim-plugin-frame [markupError]="markupError" *ngIf="isVisible() && isOpen">
            <tim-plugin-header *ngIf="header" header>
                <span [innerHTML]="header"></span>
                <tim-close-button (click)="isOpen = false"></tim-close-button>
            </tim-plugin-header>
            <ng-container body>
                <tim-close-button *ngIf="!header" (click)="isOpen = false"></tim-close-button>
                <p *ngIf="stem" class="stem" [innerHtml]="stem"></p>
                <div *ngIf="useurl || useurltoken" class="form">
                    <div *ngIf="useurl" class="form-group form-group-sm">
                        <label for="url" class="small">{{markup.urlstem}}</label>
                        <input id="url"
                               [disabled]="!!markup.aplus"
                               class="form-control"
                               [(ngModel)]="url"
                               size="50">
                    </div>
                    <div *ngIf="useurltoken" class="form-group form-group-sm">
                        <label for="urltoken" class="small">Auth token:</label>
                        <input id="urltoken" class="form-control" [(ngModel)]="urlToken">
                    </div>
                    <button [disabled]="fetchingData || !url"
                            *ngIf="!markup.aplus"
                            class="timButton btn-sm"
                            (click)="pickFromWWW()">{{markup.loadButtonText}}
                    </button>
                    <tim-loading *ngIf="fetchingData"></tim-loading>
                </div>
                <div *ngIf="markup.upload" class="form-inline small">
                    <div class="form-group small"> {{markup.uploadstem}}
                        <input type="file" (change)="onFileSelect($event)">
                    </div>
                </div>
                <p class="form-inline small" *ngIf="markup.useseparator">{{markup.separatorstem}}<input
                        [(ngModel)]="separator" size="5"/></p>
                <p *ngIf="markup.usefields">Fields:</p>
                <p><textarea class="form-control"
                             *ngIf="markup.usefields"
                             [(ngModel)]="fields"
                             placeholder="fields"
                             rows="7"
                             cols="30"></textarea></p>
                <p><textarea class="form-control"
                             *ngIf="!markup.aplus"
                             [(ngModel)]="importText"
                             (change)="importTextChanged()"
                             [placeholder]="markup.placeholder"
                             rows="10"
                             cols="50"></textarea></p>
                <div class="checkbox" *ngIf="showCreateMissingUsers()">
                    <label>
                        <input [(ngModel)]="createMissingUsers"
                               type="checkbox"> Create missing users
                    </label>
                </div>
                <button class="timButton" [disabled]="isRunning" (click)="doImport()">
                    {{buttonText()}}
                </button>
                <tim-loading *ngIf="isRunning"></tim-loading>
                <div *ngIf="error">
                    <tim-close-button (click)="error = undefined"></tim-close-button>
                    <tim-alert severity="danger">
                        <span [innerHTML]="error"></span>
                    </tim-alert>
                </div>
                <pre *ngIf="result">{{result}}</pre>
                <ul *ngIf="importResult">
                    <li>Changed values: {{importResult.fields_changed}}</li>
                    <li>Unchanged values: {{importResult.fields_unchanged}}</li>
                    <li *ngIf="importResult.users_created.length > 0">
                        Created users: {{importResult.users_created.length}}
                    </li>
                    <li *ngIf="importResult.users_missing.length > 0">
                        Missing users:
                        <ul>
                            <li *ngFor="let u of importResult.users_missing">
                                {{u.full_name || 'Unnamed user'}} ({{u.username || u.email}})
                            </li>
                        </ul>
                    </li>
                </ul>
            </ng-container>
            <p footer *ngIf="footer" [textContent]="footer"></p>
        </tim-plugin-frame>
    `,
    styleUrls: ["./import-data.component.scss"],
})
export class ImportDataComponent extends AngularPluginBase<
    t.TypeOf<typeof ImportDataMarkup>,
    t.TypeOf<typeof ImportDataAll>,
    typeof ImportDataAll
> {
    isRunning = false;
    importText: string = "";
    error?: string;
    result?: string;
    importResult?: IFieldSaveResult;
    isOpen: boolean = false;
    url: string = "";
    separator: string = ";";
    fields: string = "";
    urlToken: string = "";
    fetchingData = false;
    createMissingUsers = false;
    private storage = new TimStorage(
        "importData",
        t.type({importToken: t.string, importUrl: t.string})
    );
    useurl = false;
    useurltoken = false;

    getDefaultMarkup() {
        return {};
    }

    buttonText() {
        return super.buttonText() ?? "Import";
    }

    ngOnInit() {
        super.ngOnInit();
        this.isOpen = this.markup.open;
        const aa = this.attrsall;
        const state = aa.state;
        this.separator = state?.separator ?? this.markup.separator;
        const stored = this.storage.get();
        this.url = state?.url ?? stored?.importUrl ?? this.markup.url;
        this.urlToken = stored?.importToken ?? "";
        this.fields = (state?.fields ?? this.markup.fields ?? []).join("\n");
        this.useurltoken = this.markup.useurltoken || !!this.markup.aplus;
        this.useurl = this.markup.useurl || !!this.markup.aplus;
        if (this.markup.aplus) {
            this.url = `https://plus.cs.aalto.fi/api/v2/courses/${this.markup.aplus.course}/aggregatedata/`;
        }
    }

    getAttributeType() {
        return ImportDataAll;
    }

    isVisible() {
        const pn = window.location.pathname;
        return (
            this.markup.showInView ||
            pn.startsWith("/teacher/") ||
            pn.startsWith("/answers/")
        );
    }

    async pickFromWWW() {
        this.error = undefined;
        this.result = "";

        this.fetchingData = true;
        const r = await this.httpGet<{
            data: string;
            status_code: number;
        }>("/getproxy", {
            url: this.url,
            auth_token: this.urlToken,
        });
        this.fetchingData = false;
        if (!r.ok) {
            this.error = r.result.error.error;
            return;
        }
        if (r.result.status_code >= 400) {
            this.error = r.result.data;
            return;
        }
        this.storage.set({importToken: this.urlToken, importUrl: this.url});
        this.importText = r.result.data;
    }

    onFileSelect(e: Event) {
        const fileInput = e.target as HTMLInputElement;
        const file = fileInput.files?.item(0);
        if (!file) {
            return;
        }
        const reader = new FileReader();
        reader.onload = () => {
            this.importText = reader.result as string;
        };
        reader.readAsText(file);
    }

    async doImport() {
        this.isRunning = true;
        const params = {
            input: this.markup.aplus
                ? {
                      // If this is A+ import, the server will fetch the data from A+.
                      // We only need to send the access token.
                      token: this.urlToken,
                  }
                : {
                      data: this.importText,
                      separator: this.separator,
                      url: this.url,
                      fields: this.fields ? this.fields.split("\n") : undefined,
                      createMissingUsers: this.createMissingUsers,
                  },
        };

        this.result = undefined;
        this.error = undefined;
        const r = await this.postAnswer<{
            web: {
                result?: string;
                error?: string;
                fieldresult?: IFieldSaveResult;
            };
        }>(params);
        this.isRunning = false;
        if (!r.ok) {
            this.error = r.result.error.error;
            return;
        }
        if (r.result.errors) {
            this.error = r.result.errors.join("\n");
            return;
        }
        this.importResult = r.result.web.fieldresult;
        if (r.result.web.error) {
            this.error = r.result.web.error;
            return;
        }
        if (r.result.web.result) {
            this.result = r.result.web.result;
            return;
        }
    }

    importTextChanged() {
        this.importResult = undefined;
    }

    showCreateMissingUsers() {
        return (
            (this.importResult?.users_missing.length ?? 0) > 0 &&
            this.markup.createMissingUsers
        );
    }
}

@NgModule({
    declarations: [ImportDataComponent],
    imports: [CommonModule, HttpClientModule, FormsModule, TimUtilityModule],
})
export class ImportDataModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

registerPlugin("importdata-runner", ImportDataModule, ImportDataComponent);
