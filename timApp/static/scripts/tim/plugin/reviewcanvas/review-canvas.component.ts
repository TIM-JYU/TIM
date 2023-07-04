/**
 * Defines the client-side implementation of the review canvas.
 */

import * as t from "io-ts";
import type {
    AfterViewInit,
    ApplicationRef,
    DoBootstrap,
    OnDestroy,
    OnInit,
} from "@angular/core";
import {
    Component,
    ElementRef,
    NgModule,
    QueryList,
    ViewChild,
    ViewChildren,
} from "@angular/core";
import {HttpClientModule, HttpHeaders} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import type {Subscription} from "rxjs";
import {Subject} from "rxjs";
import {debounceTime, distinctUntilChanged} from "rxjs/operators";
import {PurifyModule} from "tim/util/purify.module";
import {defaultErrorMessage, defaultTimeout} from "tim/util/utils";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import type {ITimComponent, IVelpableComponent} from "tim/document/viewctrl";
import type {IUser} from "tim/user/IUser";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import {
    GenericPluginMarkup,
    getTopLevelFields,
    nullable,
    withDefault,
} from "tim/plugin/attributes";
import {registerPlugin} from "tim/plugin/pluginRegistry";
import {CommonModule} from "@angular/common";
import {TooltipDirective, TooltipModule} from "ngx-bootstrap/tooltip";
import type {
    IFile,
    IFileSpecification,
} from "../../../../../modules/cs/js/util/file-select";
import {FileSelectManagerComponent} from "../../../../../modules/cs/js/util/file-select";
import {CsUtilityModule} from "../../../../../modules/cs/js/util/module";

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

const UploadedFile = t.intersection([
    t.type({
        path: t.string,
        type: t.string,
    }),
    t.partial({
        rotation: t.number,
    }),
]);

interface IUploadedFile extends t.TypeOf<typeof UploadedFile> {}

interface IUploadResponse {
    file: string;
    type: string;
    block: number;
}

interface IReviewCanvasAnswerInput {
    input: {
        uploadedFiles?: {
            path: string;
            type: string;
            rotation?: number | null;
        }[];
    };
}

const PluginMarkupFields = t.intersection([
    t.partial({
        inputplaceholder: nullable(t.string),
        inputstem: t.string,
        filename: t.string,
        pdfLinkText: nullable(t.string),
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
            <span [innerHTML]="header | purify"></span>
        </tim-plugin-header>
        <a *ngIf="uploadedFiles.length > 0 && !isUnSaved() && downloadPDFUrl" [href]="downloadPDFUrl" target="_blank">{{downloadPDFText()}}</a>
        <p stem *ngIf="stem" [innerHTML]="stem | purify"></p>
        <div *ngIf="!inReviewView()">
            <ng-container *ngIf="enabled" body>
                <div class="form-inline small">
                    <div style="position: relative;" *ngFor="let item of uploadedFiles; let i = index">
                        <div class="uploadContainer" #wraps>
                            <img alt="Uploaded image" #img [src]="item.path" (load)="onImgLoad($event, i)">
                        </div>
                        <div class="tools">
                            <button class="timButton" title="Move up" i18n-title (click)="moveImageUp(i)">&uarr;
                            </button>
                            <button class="timButton" title="Move down" i18n-title (click)="moveImageDown(i)">&darr;
                            </button>
                            <button class="timButton" title="Rotate clockwise" i18n-title (click)="increaseRotation(i)">
                                &#8635;
                            </button>
                            <button class="timButton" title="Delete picture" i18n-title (click)="deleteImage(i)">
                                <i class="glyphicon glyphicon-trash"></i>
                            </button>
                        </div>
                    </div>
                </div>
                <div class="flex">
                    <file-select-manager class="small"
                                         [dragAndDrop]="dragAndDrop"
                                         [uploadUrl]="uploadUrl"
                                         [stem]="uploadstem"
                                         (file)="onFileLoad($event)"
                                         (upload)="onUploadResponse($event)"
                                         (uploadDone)="onUploadDone($event)"
                                         [accept]="'image/*,.pdf'">
                    </file-select-manager>
                    <ng-template #tolTemplate>{{ tooltipText }}</ng-template>
                    <input #pasteInput (focusout)="onPasteFocusout()" (paste)="onPaste($event)" i18n-placeholder placeholder="Paste image" #tooltip="bs-tooltip" [tooltip]="tolTemplate" size="9" style="font-size: 0.7em;"/>
                </div>
                <tim-loading *ngIf="isRunning"></tim-loading>
                <div *ngIf="error" [innerHTML]="error"></div>
                <pre *ngIf="result">{{result}}</pre>
            </ng-container>
            <div *ngIf="connectionErrorMessage || userErrorMessage">
                <span *ngIf="connectionErrorMessage" class="error" [innerHTML]="connectionErrorMessage"></span>
                <span *ngIf="userErrorMessage" class="error" [innerHTML]="userErrorMessage"></span>
            </div>
            <button *ngIf="enabled" class="timButton" (click)="save()" i18n>Save answer</button>
        </div>
        <p footer *ngIf="footer" [textContent]="footer"></p>
    `,
    styleUrls: ["./review-canvas.component.scss"],
})
export class ReviewCanvasComponent
    extends AngularPluginBase<
        t.TypeOf<typeof PluginMarkupFields>,
        t.TypeOf<typeof PluginFields>,
        typeof PluginFields
    >
    implements
        AfterViewInit,
        OnInit,
        OnDestroy,
        ITimComponent,
        IVelpableComponent
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
    changes = false;
    private loadedImages = 0;

    enabled = true;

    fileSelect?: FileSelectManagerComponent;
    uploadUrl?: string;
    downloadPDFUrl?: string;
    dragAndDrop: boolean = true;
    uploadstem?: string;
    uploadedFiles: IUploadedFile[] = [];
    tooltipText = this.defaultTooltipText();

    @ViewChildren("img") imgElements!: QueryList<ElementRef<HTMLImageElement>>;
    @ViewChildren("wraps") wraps!: QueryList<ElementRef<HTMLDivElement>>;
    @ViewChild("pasteInput") pasteInput?: ElementRef<HTMLInputElement>;
    @ViewChild("tooltip") pasteTooltip?: TooltipDirective;

    // uploadedFiles = new Set((o: IUploadedFile) =>
    //     this.uploadedFileName(o.path)
    // );

    uploadedFileName(url: string) {
        return url.split("/").slice(6).join("/");
    }

    onPaste(event: ClipboardEvent) {
        const items = event.clipboardData?.items;
        if (!items || !this.fileSelect) {
            return;
        }
        const files: File[] = [];
        for (const i of items) {
            if (i.type.startsWith("image") || i.type == "application/pdf") {
                const file = i.getAsFile();
                if (file !== null) {
                    files.push(file);
                }
            }
        }
        if (files.length != 0) {
            this.uploadFiles(files);
        } else {
            this.tooltipText = $localize`No images found in the clipboard`;
            if (this.pasteTooltip) {
                this.pasteTooltip.isOpen = true;
            }
        }
    }

    uploadFiles(files: File[]) {
        if (!this.fileSelect) {
            return;
        }
        const fs = this.fileSelect.fileSelectors;
        const fss = Object.values(fs);
        if (fss.length > 0) {
            fss[0].onFilesGot(files);
        }
    }

    onPasteFocusout() {
        if (this.pasteInput) {
            this.pasteInput.nativeElement.value = "";
        }
        this.tooltipText = this.defaultTooltipText();
    }

    getDefaultMarkup() {
        return {};
    }

    buttonText() {
        return super.buttonText() ?? "Save";
    }

    inReviewView(): boolean {
        return window.location.pathname.startsWith("/review/");
    }

    ngOnInit() {
        super.ngOnInit();

        if (!this.attrsall.preview) {
            this.vctrl.addTimComponent(this);
            this.vctrl.addReviewCanvas(this);
        }
        const taskId = this.pluginMeta.getTaskId();
        if (taskId?.docId) {
            this.uploadUrl = `/pluginUpload/${taskId.docId}/${taskId.name}/`;
        } else {
            this.enabled = false;
            return;
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
        } else {
        }
    }

    ngAfterViewInit() {
        this.updatePDFDownloadUrl();
    }

    downloadPDFText() {
        return this.markup.pdfLinkText ?? $localize`Show images as PDF`;
    }

    updatePDFDownloadUrl(answerId?: number) {
        if (
            this.markup.pdfLinkText === "" ||
            this.markup.pdfLinkText === null
        ) {
            return;
        }
        const taskId = this.pluginMeta.getTaskId();
        this.downloadPDFUrl = undefined;
        let user: IUser | undefined;
        let aid: number | undefined;
        if (taskId?.docId && taskId.docTask()) {
            const ab = this.vctrl.getAnswerBrowser(taskId.docTask().toString());
            if (ab) {
                user = ab?.getUser();
                if (user) {
                    aid =
                        answerId ??
                        ab.selectedAnswer?.id ??
                        ab.getPluginHtmlAnswerId();
                }
            } else {
                const pl = this.vctrl.getPluginLoader(
                    taskId.docTask().toString()
                );
                aid = answerId ?? pl?.getAnswerId();
                user = this.vctrl.selectedUser;
            }
            if (user && aid) {
                this.downloadPDFUrl = `/reviewcanvaspdf/${user.name}_${taskId.docId}_${taskId.name}_${aid}.pdf`;
            }
        }
    }

    ngOnDestroy() {
        if (!this.attrsall.preview) {
            this.vctrl.removeTimComponent(this);
        }
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

        component.allowMultiple = true; // this.markup.allowMultipleFiles;
        component.multipleElements = true; // this.markup.multipleUploadElements;
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

    defaultTooltipText() {
        return $localize`You can paste your image directly from clipboard here`;
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
        const resps = resp as [IUploadResponse];
        for (const response of resps) {
            this.uploadedFiles.push({path: response.file, type: response.type});
        }
    }

    onUploadDone(success: boolean) {
        this.tooltipText = this.defaultTooltipText();
        if (!success) {
            return;
        }
        this.changes = true;
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

    increaseRotation(index: number) {
        this.changes = true;
        const uploadedFile = this.uploadedFiles[index];
        if (uploadedFile.rotation == undefined) {
            uploadedFile.rotation = 0;
        }
        uploadedFile.rotation += 1;
        if (uploadedFile.rotation > 3) {
            uploadedFile.rotation = 0;
        }
        this.rotateImage(index);
        this.doAutoSave();
    }

    /**
     * Rotate image with css according to current rotation (90deg per one rotation)
     * @param index image position in uploadedFiles
     */
    rotateImage(index: number) {
        const uploadedFile = this.uploadedFiles[index];
        if (uploadedFile.rotation == undefined) {
            return;
        }
        const imgElement = this.imgElements.get(index)?.nativeElement;
        const wrapper = this.wraps.get(index)?.nativeElement;
        if (!wrapper || !imgElement) {
            return;
        }
        const oldWidth = imgElement.width;
        const oldHeight = imgElement.height;
        const wrapperWidth = wrapper.clientWidth;
        const rotations = [
            "",
            "rotate(90deg)",
            "rotate(180deg)",
            "rotate(270deg)",
        ];
        imgElement.style.transform = rotations[uploadedFile.rotation];
        if (uploadedFile.rotation % 2 == 0) {
            imgElement.style.removeProperty("height");
            wrapper.style.removeProperty("height");
        } else {
            if (oldHeight < oldWidth) {
                wrapper.style.height = oldWidth + "px";
            } else {
                const newHeight = Math.min(oldHeight, wrapperWidth);
                imgElement.style.height = newHeight + "px";
                const newWidth = imgElement.width;
                wrapper.style.height = newWidth + "px";
            }
        }
    }

    onImgLoad(e: Event, index: number): void {
        this.loadedImages += 1;
        this.rotateImage(index);
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
            await this.save();
        }
    }

    async save() {
        if (this.uploadedFiles.length === 0) {
            this.userErrorMessage =
                "Cannot save answer; no files have been uploaded.";
            return {saved: false, message: this.userErrorMessage};
        }

        this.userErrorMessage = undefined;
        this.connectionErrorMessage = undefined;

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
            this.error = data.errors?.join("\n");
            this.changes = false;
            if (r.result.savedNew) {
                this.updatePDFDownloadUrl(r.result.savedNew);
            }
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
        return {
            saved: r.ok,
            message: r.ok ? undefined : this.connectionErrorMessage,
        };
    }

    isUnSaved() {
        return this.changes;
    }

    getAttributeType() {
        return PluginFields;
    }

    getContent() {
        return JSON.stringify(this.uploadedFiles);
    }

    /**
     * Return promise of images' dataUrl presentation or their original source
     * The returned images are fully rotated to their current rotation value (90deg per one rotation)
     */
    async getVelpImages(): Promise<string[]> {
        const rotatedimgsrcs = this.uploadedFiles.filter(
            (file) => file.rotation != undefined && file.rotation !== 0
        );
        const rotatedimgs = rotatedimgsrcs.map((file) => {
            const newImg = new Image();
            newImg.src = file.path;
            return newImg;
        });
        await Promise.all(rotatedimgs.map((img) => img.decode()));
        const stringRet: string[] = [];
        for (const file of this.uploadedFiles) {
            const uploadedFile = file;
            if (
                uploadedFile.rotation == undefined ||
                uploadedFile.rotation == 0
            ) {
                stringRet.push(uploadedFile.path);
            } else {
                const img = rotatedimgs.shift();
                if (img == undefined) {
                    throw Error("Failed to load velp images");
                }
                const canvas = document.createElement("canvas");
                const ctx = canvas.getContext("2d")!;
                let dx = 0;
                let dy = 0;
                const rotation = uploadedFile.rotation;
                if (rotation == 1) {
                    dy = -img.height;
                }
                if (rotation == 2) {
                    dx = -img.width;
                    dy = -img.height;
                }
                if (rotation == 3) {
                    dx = -img.width;
                }
                canvas.height = rotation % 2 == 0 ? img.height : img.width;
                canvas.width = rotation % 2 == 0 ? img.width : img.height;
                ctx.rotate((Math.PI / 2) * rotation);
                ctx.drawImage(img, dx, dy);
                stringRet.push(canvas.toDataURL());
            }
        }
        return stringRet;
    }
}

@NgModule({
    declarations: [ReviewCanvasComponent],
    imports: [
        CommonModule,
        HttpClientModule,
        FormsModule,
        TimUtilityModule,
        CsUtilityModule,
        PurifyModule,
        TooltipModule.forRoot(),
    ],
})
export class ReviewCanvasModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}
registerPlugin(
    "reviewcanvas-runner",
    ReviewCanvasModule,
    ReviewCanvasComponent
);
