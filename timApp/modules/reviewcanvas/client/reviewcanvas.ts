/**
 * Defines the client-side implementation of the review canvas.
 */

import * as t from "io-ts";
import {
    GenericPluginMarkup,
    getTopLevelFields,
    nullable,
    withDefault,
} from "tim/plugin/attributes";
import {defaultErrorMessage, defaultTimeout, valueDefu} from "tim/util/utils";
import {
    ApplicationRef,
    Component,
    DoBootstrap,
    NgModule,
    OnDestroy,
    OnInit,
} from "@angular/core";
import {BrowserModule} from "@angular/platform-browser";
import {HttpClientModule, HttpHeaders} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {Subject, Subscription} from "rxjs";
import {debounceTime, distinctUntilChanged} from "rxjs/operators";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {createDowngradedModule, doDowngrade} from "tim/downgrade";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import {CsUtilityModule} from "../../cs/js/util/module";
import {IFile} from "../../cs/js/util/file-select";
import {Set} from "../../cs/js/util/set";

const FileSubmission = t.intersection([
    t.type({
        source: t.string,
        path: t.string,
    }),
    t.partial({
        content: nullable(t.string),
        type: t.string,
    }),
]);
export type IFileSubmission = t.TypeOf<typeof FileSubmission>;

const UploadedFile = t.type({
    path: t.string,
    type: t.string,
});
interface IUploadedFile extends t.TypeOf<typeof UploadedFile> {}

interface IUploadResponse {
    file: string;
    type: string;
    block: number;
}

interface IUploadRequestInput {
    input: {uploadedFiles?: {path: string; type: string}[]};
}

const PluginMarkupFields = t.intersection([
    t.partial({
        inputplaceholder: nullable(t.string),
        inputstem: t.string,
    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
        autoupdate: withDefault(t.number, 500),
    }),
]);

const PluginFields = t.intersection([
    getTopLevelFields(PluginMarkupFields),
    t.type({
        state: nullable(t.type({})),
    }),
]);

@Component({
    selector: "reviewcanvas-runner",
    template: `
        <tim-plugin-frame [markupError]="markupError">
            <tim-plugin-header *ngIf="header" header>
                <span [innerHTML]="header"></span>
            </tim-plugin-header>
            <p stem *ngIf="stem" [innerHTML]="stem"></p>
            <ng-container body>
                 <file-select-manager class="small"
                        [dragAndDrop]="dragAndDrop"
                        [uploadUrl]="uploadUrl"
                        [stem]="uploadstem"
                        (file)="onFileLoad($event)"
                        (upload)="onUploadResponse($event)"
                        (uploadDone)="onUploadDone($event)">
                </file-select-manager>
                <div class="form-inline small">
                    <span *ngFor="let item of uploadedFiles">
                        <cs-upload-result [src]="item.path" [type]="item.type"></cs-upload-result>
                    </span>
                </div>
                <span *ngIf="connectionErrorMessage" class="error" style="font-size: 12px" [innerHTML]="connectionErrorMessage"></span>
                <tim-loading *ngIf="isRunning"></tim-loading>
                <div *ngIf="error" [innerHTML]="error"></div>
                <pre *ngIf="result">{{result}}</pre>
            </ng-container>
            <p footer *ngIf="footer" [textContent]="footer"></p>
        </tim-plugin-frame>
    `,
    styleUrls: ["./reviewcanvas.scss"],
})
export class ReviewCanvasComponent
    extends AngularPluginBase<
        t.TypeOf<typeof PluginMarkupFields>,
        t.TypeOf<typeof PluginFields>,
        typeof PluginFields
    >
    implements OnInit, OnDestroy {
    result?: string;
    error?: string;
    isRunning = false;
    timeout: number = 0;
    connectionErrorMessage?: string;
    file?: object;
    modelChanged: Subject<object> = new Subject<object>();
    private modelChangeSub!: Subscription;

    uploadUrl?: string;
    dragAndDrop: boolean = true;
    uploadstem?: string;
    uploadedFiles = new Set((o: IUploadedFile) =>
        this.uploadedFileName(o.path)
    );

    uploadedFileName(url: string) {
        return url.split("/").slice(6).join("/");
    }

    getDefaultMarkup() {
        return {};
    }

    buttonText() {
        return super.buttonText() ?? "Save";
    }

    ngOnInit() {
        super.ngOnInit();

        this.modelChangeSub = this.modelChanged
            .pipe(debounceTime(this.autoupdate), distinctUntilChanged())
            .subscribe((newValue) => {
                this.file = newValue;
            });
    }

    ngOnDestroy() {
        this.modelChangeSub.unsubscribe();
    }

    get correct() {
        return true;
    }

    get autoupdate(): number {
        return this.markup.autoupdate;
    }

    get inputplaceholder() {
        return this.markup.inputplaceholder ?? "";
    }

    get inputstem() {
        return this.markup.inputstem ?? null;
    }

    reset(e: MouseEvent) {
        this.error = undefined;
        this.result = undefined;
    }

    onFileLoad(file: IFile) {
        return;
    }

    onUploadResponse(resp: unknown) {
        if (!resp) {
            return;
        }

        const response = resp as IUploadResponse;
        this.uploadedFiles.clear();
        this.uploadedFiles.push({path: response.file, type: response.type});
    }

    onUploadDone(success: boolean) {
        if (!success) {
            return;
        }

        this.postUploadAnswer();
    }

    async postUploadAnswer() {
        this.isRunning = true;

        const params: IUploadRequestInput = {
            input: {
                uploadedFiles: this.uploadedFiles.toArray(),
            },
        };

        const r = await this.postAnswer<IUploadRequestInput>(
            params,
            new HttpHeaders({timeout: `${this.timeout + defaultTimeout}`})
        );

        if (r.ok) {
            this.isRunning = false;
            const data = r.result;
            this.error = data.error;
        } else {
            this.isRunning = false;
            const data = r.result.error;
            if (data?.error) {
                this.error = data.error;
            }
            this.connectionErrorMessage =
                this.error ??
                this.markup.connectionErrorMessage ??
                defaultErrorMessage;
        }

        this.isRunning = false;
    }

    getAttributeType() {
        return PluginFields;
    }
}

@NgModule({
    declarations: [ReviewCanvasComponent],
    imports: [
        BrowserModule,
        HttpClientModule,
        FormsModule,
        TimUtilityModule,
        CsUtilityModule,
    ],
})
export class ReviewCanvasModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

export const moduleDefs = [
    doDowngrade(
        createDowngradedModule((extraProviders) =>
            platformBrowserDynamic(extraProviders).bootstrapModule(
                ReviewCanvasModule
            )
        ),
        "reviewcanvasRunner",
        ReviewCanvasComponent
    ),
];
