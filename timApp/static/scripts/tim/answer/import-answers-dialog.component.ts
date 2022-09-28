import {BrowserModule} from "@angular/platform-browser";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {Component, NgModule, ViewChild} from "@angular/core";
import {FormsModule} from "@angular/forms";
import {TimUtilityModule} from "../ui/tim-utility.module";
import {PurifyModule} from "../util/purify.module";
import {EditorModule} from "../../../../modules/cs/js/editor/module";
import {DialogModule} from "../ui/angulardialog/dialog.module";
import {AngularDialogComponent} from "../ui/angulardialog/angular-dialog-component.directive";
import {AceEditorComponent} from "../../../../modules/cs/js/editor/ace";
import {documentglobals} from "../util/globals";
import {ViewCtrl} from "../document/viewctrl";
import {vctrlInstance} from "../document/viewctrlinstance";
import {toPromise} from "../util/utils";

export interface IEditJsonEventOptions {
    filterParams: Record<string, string>;
}

interface IImportResults {
    imported: number;
    skipped_duplicates: number;
    missing_users: string[];
}

@Component({
    selector: "tim-import-answers-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container header i18n>
                Import answers as JSON
            </ng-container>
            <ng-container body>
                <p i18n>Import answers as JSON using the text box below. Use the following format for importing:</p>
                <pre>
[
    {{'{'}}
        "content": "Content of the answer. Export answers as JSON to see the content format.",
        "email": "User's email address to use for importing. Export answers as JSON to see the email format.",
        "points": 0,
        "task": "task1",
        "time": "2020-01-01T00:00:00Z",
        "valid": true
    {{'}'}}
]</pre>
                <tim-loading *ngIf="loading"></tim-loading>
                <tim-alert *ngIf="wasSaved" severity="success">
                    <ng-container i18n>Saved!</ng-container>
                    <ng-container *ngIf="importResults" i18n> Imported {{importResults.imported}} answers, {{importResults.skipped_duplicates}} duplicates were skipped.</ng-container>
                </tim-alert>
                <tim-alert *ngIf="error">{{error}}</tim-alert>
                <tim-alert *ngIf="htmlError">
                    <div [innerHTML]="htmlError | purify"></div>
                </tim-alert>
                <p class="text-danger" i18n>Note: Answer format depends on plugin type. Export answers as JSON first to see the correct format!</p>
                <cs-ace-editor [disabled]="loading || !isTeacher" [minRows]="20" [maxRows]="25" languageMode="json"
                               #editor></cs-ace-editor>
                <p i18n>Additionally, specify the user group name for which the answers are imported.</p>
                <div class="form-inline">
                    <label>
                        Group for which to import answers:
                        <input class="form-control" type="text" [(ngModel)]="importGroup">
                    </label>
                </div>
                <p *ngIf="!isTeacher" class="text-danger" i18n>You must be in teacher mode to be able to import
                    answers!</p>
            </ng-container>
            <div footer class="dialog-button-panel">
                <button class="timButton" (click)="importAnswers()" [disabled]="!isTeacher || loading || !importGroup || !importGroup.trim()" i18n>Import
                </button>
                <button class="btn btn-default" [class.btn-danger]="!wasSaved" [disabled]="loading" (click)="close(wasSaved)">
                    <ng-container *ngIf="wasSaved" i18n>Close and refresh page</ng-container>
                    <ng-container *ngIf="!wasSaved" i18n>Close without importing</ng-container>
                </button>
            </div>
        </tim-dialog-frame>
    `,
})
export class ImportAnswersDialogComponent extends AngularDialogComponent<
    undefined,
    boolean
> {
    protected dialogName = "EventJsonEdit";
    @ViewChild("editor", {static: true}) editor!: AceEditorComponent;

    wasSaved = false;
    loading = false;
    error?: string;
    htmlError?: string;
    importGroup?: string;
    viewCtrl: ViewCtrl;
    isTeacher: boolean;
    importResults?: IImportResults;

    constructor(private http: HttpClient) {
        super();
        this.viewCtrl = vctrlInstance!;
        this.isTeacher = this.viewCtrl.teacherMode;
    }

    ngOnInit() {
        this.importGroup = documentglobals().group;
    }

    async importAnswers() {
        this.loading = true;
        this.wasSaved = false;
        this.error = undefined;
        this.importResults = undefined;
        let jsonData: unknown;
        try {
            jsonData = JSON.parse(this.editor.content);
        } catch (e) {
            this.loading = false;
            this.error = $localize`Could not parse JSON. Error info: ${e}`;
            return;
        }
        const jd = jsonData as Record<string, unknown>[];
        for (const answer of jd) {
            answer.doc = documentglobals().curr_item.path;
        }

        const res = await toPromise(
            this.http.post<IImportResults>("/importAnswers", {
                exported_answers: jd,
                allow_missing_users: true,
                match_email_case: false,
                group: this.importGroup,
            })
        );

        if (res.ok) {
            this.wasSaved = true;
            this.importResults = res.result;
        } else {
            this.error = $localize`Could not import answers. More info: ${res.result.error.error}`;
        }

        this.loading = false;
    }
}

@NgModule({
    declarations: [ImportAnswersDialogComponent],
    imports: [
        BrowserModule,
        DialogModule,
        TimUtilityModule,
        EditorModule,
        HttpClientModule,
        PurifyModule,
        FormsModule,
    ],
})
export class EditJsonEventDialogModule {}
