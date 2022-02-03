/**
 * Defines the client-side implementation of the review canvas.
 */

import * as t from "io-ts";
import {
    ApplicationRef,
    Component,
    DoBootstrap,
    NgModule,
    OnDestroy,
    OnInit,
    ViewChild,
} from "@angular/core";
import {BrowserModule} from "@angular/platform-browser";
import {HttpClientModule, HttpHeaders} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {Subject, Subscription} from "rxjs";
import {debounceTime, distinctUntilChanged} from "rxjs/operators";
import {defaultErrorMessage, defaultTimeout} from "../util/utils";
import {TimUtilityModule} from "../ui/tim-utility.module";
import {createDowngradedModule, doDowngrade} from "../downgrade";
import {CsUtilityModule} from "../../../../modules/cs/js/util/module";
import {
    FileSelectManagerComponent,
    IFile,
    IFileSpecification,
} from "../../../../modules/cs/js/util/file-select";
// import {Set} from "../../../../modules/cs/js/util/set";
import {AngularPluginBase} from "./angular-plugin-base.directive";
import {
    GenericPluginMarkup,
    getTopLevelFields,
    nullable,
    withDefault,
} from "./attributes";

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

interface IReviewCanvasAnswerInput {
    input: {uploadedFiles?: {path: string; type: string}[]};
}

const PluginMarkupFields = t.intersection([
    t.partial({
        inputplaceholder: nullable(t.string),
        inputstem: t.string,
        filename: t.string,
    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
        autoupdate: withDefault(t.number, 500),
        autosave: withDefault(t.boolean, true),
        maxSize: withDefault(t.number, 500),
    }),
]);

const PluginFields = t.intersection([
    getTopLevelFields(PluginMarkupFields),
    t.partial({
        uploadedFiles: nullable(t.array(UploadedFile)),
    }),
    t.type({
        state: nullable(
            t.type({
                uploadedFiles: nullable(t.array(UploadedFile)),
            })
        ),
    }),
]);

@Component({
    selector: "reviewcanvas-runner",
    template: `
        <tim-plugin-header *ngIf="header" header>
            <span [innerHTML]="header"></span>
        </tim-plugin-header>
        <p stem *ngIf="stem" [innerHTML]="stem"></p>
        <ng-container body>
            <div class="form-inline small">
                    <div style="position: relative;" *ngFor="let item of uploadedFiles; let i = index">
                        <cs-upload-result [src]="item.path" [type]="item.type"></cs-upload-result>
                        <div style="position: absolute; top: 50%; display: flex; flex-flow: column; gap: 1em;
                         -ms-transform: translateY(-50%); transform: translateY(-50%);">
                            <button class="timButton" title="Move up" (click)="moveImageUp(i)">&uarr;</button>
                            <button class="timButton" title="Move down" (click)="moveImageDown(i)">&darr;</button>
                            <!--<button class="timButton" title="Rotate clockwise" (click)="rotateImage(i)">&#8635;</button>-->
                            <button class="timButton" title="Delete picture" (click)="deleteImage(i)">
                                <i class="glyphicon glyphicon-trash"></i>
                            </button>
                        </div>
                    </div>
            </div>
            <file-select-manager class="small"
                                 [dragAndDrop]="dragAndDrop"
                                 [uploadUrl]="uploadUrl"
                                 [stem]="uploadstem"
                                 (file)="onFileLoad($event)"
                                 (upload)="onUploadResponse($event)"
                                 (uploadDone)="onUploadDone($event)">
            </file-select-manager>
            <tim-loading *ngIf="isRunning"></tim-loading>
            <div *ngIf="error" [innerHTML]="error"></div>
            <pre *ngIf="result">{{result}}</pre>
        </ng-container>
            <div *ngIf="connectionErrorMessage || userErrorMessage">
                <span *ngIf="connectionErrorMessage" class="error" [innerHTML]="connectionErrorMessage"></span>
                <span *ngIf="userErrorMessage" class="error" [innerHTML]="userErrorMessage"></span>
            </div>
            <button class="timButton" (click)="saveAnswer()">Save answer</button>
            <p footer *ngIf="footer" [textContent]="footer"></p>
    `,
    styleUrls: ["./reviewcanvas.scss"],
})
export class ReviewCanvasComponent
    extends AngularPluginBase<
        t.TypeOf<typeof PluginMarkupFields>,
        t.TypeOf<typeof PluginFields>,
        typeof PluginFields
    >
    implements OnInit, OnDestroy
{
    result?: string;
    error?: string;
    isRunning = false;
    timeout: number = 0;
    connectionErrorMessage?: string;
    userErrorMessage?: string;
    file?: object;
    modelChanged: Subject<object> = new Subject<object>();
    private modelChangeSub!: Subscription;

    fileSelect?: FileSelectManagerComponent;
    uploadUrl?: string;
    dragAndDrop: boolean = true;
    uploadstem?: string;
    uploadedFiles: IUploadedFile[] = [];

    // uploadedFiles = new Set((o: IUploadedFile) =>
    //     this.uploadedFileName(o.path)
    // );

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

        const taskId = this.pluginMeta.getTaskId();
        if (taskId?.docId) {
            this.uploadUrl = `/pluginUpload/${taskId.docId}/${taskId.name}/`;
        }

        this.modelChangeSub = this.modelChanged
            .pipe(debounceTime(this.autoupdate), distinctUntilChanged())
            .subscribe((newValue) => {
                this.file = newValue;
            });

        if (
            this.attrsall.state?.uploadedFiles &&
            this.attrsall.state?.uploadedFiles.length > 0
        ) {
            this.attrsall.state.uploadedFiles.forEach((uf) =>
                this.uploadedFiles.push(uf)
            );
        }
    }

    ngOnDestroy() {
        this.modelChangeSub.unsubscribe();
    }

    @ViewChild(FileSelectManagerComponent)
    set fileSelectSetter(component: FileSelectManagerComponent | undefined) {
        this.fileSelect = component;
        if (!component) {
            return;
        }

        const path = this.markup.filename ?? "";
        const files: IFileSpecification[] = [];
        files.push({
            paths: [path],
            upload: true,
        });

        component.allowMultiple = false; // this.markup.allowMultipleFiles;
        component.multipleElements = false; // this.markup.multipleUploadElements;
        component.files = files;
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
        // this.uploadedFiles.clear();
        this.uploadedFiles.push({path: response.file, type: response.type});
    }

    onUploadDone(success: boolean) {
        if (!success) {
            return;
        }

        this.doAutoSave();
    }

    async moveImageUp(index: number) {
        if (index === 0) {
            const item = this.uploadedFiles.shift() as IUploadedFile;
            this.uploadedFiles.push(item);

            await this.doAutoSave();
        } else {
            await this.swapUploadedFilePositions(index, index - 1);
        }
    }

    async moveImageDown(index: number) {
        if (index === this.uploadedFiles.length - 1) {
            const item = this.uploadedFiles.pop() as IUploadedFile;
            this.uploadedFiles.unshift(item);

            await this.doAutoSave();
        } else {
            await this.swapUploadedFilePositions(index, index + 1);
        }
    }

    rotateImage(index: number) {
        console.log("not implemented", index);
    }

    async deleteImage(index: number) {
        this.uploadedFiles.splice(index, 1);
        await this.doAutoSave();
    }

    async swapUploadedFilePositions(index1: number, index2: number) {
        const tmp = this.uploadedFiles[index2];

        this.uploadedFiles[index2] = this.uploadedFiles[index1];
        this.uploadedFiles[index1] = tmp;

        await this.doAutoSave();
    }

    async doAutoSave() {
        if (this.markup.autosave) {
            await this.saveAnswer();
        }
    }

    async saveAnswer() {
        if (this.uploadedFiles.length === 0) {
            this.userErrorMessage =
                "Cannot save answer; no files have been uploaded.";
            return;
        }

        this.userErrorMessage = undefined;

        this.isRunning = true;

        const params: IReviewCanvasAnswerInput = {
            input: {
                uploadedFiles: this.uploadedFiles,
            },
        };

        const r = await this.postAnswer<IReviewCanvasAnswerInput>(
            params,
            new HttpHeaders({timeout: `${this.timeout + defaultTimeout}`})
        );

        if (r.ok) {
            const data = r.result;
            this.error = data.error;
        } else {
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
