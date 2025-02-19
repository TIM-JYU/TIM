import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule, ViewChild} from "@angular/core";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {toPromise} from "tim/util/utils";
import {PurifyModule} from "tim/util/purify.module";
import {CommonModule} from "@angular/common";
import {itemglobals} from "tim/util/globals";
import {EditorModule} from "../../../../../modules/cs/js/editor/module";
import {AceEditorComponent} from "../../../../../modules/cs/js/editor/ace";

export interface IEditJsonEventOptions {
    filterParams: Record<string, string>;
}

@Component({
    selector: "tim-edit-json-event-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container header i18n>
                Edit event JSON
            </ng-container>
            <ng-container body>
                <p i18n>Below are all events visible in this calendar in JSON format. You can add or edit events en masse.</p>
                <p i18n>Adding <code>"delete": true</code> to an event will delete it.</p>
                <tim-loading *ngIf="loading"></tim-loading>
                <tim-alert *ngIf="wasSaved" severity="success" i18n>Saved!</tim-alert>
                <tim-alert *ngIf="error">{{error}}</tim-alert>
                <tim-alert *ngIf="htmlError"><div [innerHTML]="htmlError | purify"></div></tim-alert>
                <cs-ace-editor [disabled]="loading" [minRows]="20" [maxRows]="30" languageMode="json" #editor></cs-ace-editor>
            </ng-container>
            <div footer class="dialog-button-panel">
                <button class="timButton" (click)="saveEvents()" i18n>Save</button>
                <button class="btn btn-default" [class.btn-danger]="!wasSaved" (click)="close(wasSaved)">
                    <ng-container *ngIf="wasSaved" i18n>Close</ng-container>
                    <ng-container *ngIf="!wasSaved" i18n>Close without saving</ng-container>
                </button>
            </div>
        </tim-dialog-frame>
    `,
})
export class EditJsonEventDialogComponent extends AngularDialogComponent<
    IEditJsonEventOptions,
    boolean
> {
    protected dialogName = "EventJsonEdit";
    @ViewChild("editor", {static: true}) editor!: AceEditorComponent;

    wasSaved = false;
    loading = true;
    error?: string;
    htmlError?: string;

    constructor(private http: HttpClient) {
        super();
    }

    ngOnInit() {
        void this.loadEvents();
    }

    private async loadEvents() {
        this.error = undefined;
        const res = await toPromise(
            this.http.get("/calendar/export", {
                params: {
                    format: "template-json",
                    ...this.data.filterParams,
                },
            })
        );
        if (res.ok) {
            this.editor.content = JSON.stringify(res.result, null, 4);
        } else {
            this.error = $localize`Could not load events. More info: ${res.result.error.error}`;
        }
        this.loading = false;
    }

    async saveEvents() {
        this.loading = true;
        this.wasSaved = false;
        this.error = undefined;
        this.htmlError = undefined;

        let jsonData: unknown;
        try {
            jsonData = JSON.parse(this.editor.content);
        } catch (e) {
            this.loading = false;
            this.error = $localize`Could not parse JSON. Error info: ${e}`;
            return;
        }

        const res = await toPromise(
            this.http.post<{
                web?: {error?: string};
            }>("/calendar/import", {
                events: jsonData,
                origin_doc_id: itemglobals().curr_item.id,
            })
        );

        if (res.ok) {
            if (res.result.web?.error) {
                this.htmlError = $localize`Error: ${res.result.web.error}`;
            } else {
                this.wasSaved = true;
            }
        } else {
            this.error = $localize`Could not save events. More info: ${res.result.error.error}`;
        }

        this.loading = false;
    }
}

@NgModule({
    declarations: [EditJsonEventDialogComponent],
    imports: [
        CommonModule,
        DialogModule,
        TimUtilityModule,
        EditorModule,
        HttpClientModule,
        PurifyModule,
    ],
})
export class EditJsonEventDialogModule {}
