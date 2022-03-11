/**
 * Defines the client-side implementation of the review canvas.
 */

import * as t from "io-ts";
import {
    ApplicationRef,
    Component,
    DoBootstrap,
    ElementRef,
    NgModule,
    OnDestroy,
    OnInit,
    QueryList,
    ViewChild,
    ViewChildren,
} from "@angular/core";
import {BrowserModule} from "@angular/platform-browser";
import {HttpClientModule, HttpHeaders} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {Subject, Subscription} from "rxjs";
import {debounceTime, distinctUntilChanged} from "rxjs/operators";
import {PurifyModule} from "tim/util/purify.module";
import {defaultErrorMessage, defaultTimeout, timeout} from "../util/utils";
import {TimUtilityModule} from "../ui/tim-utility.module";
import {createDowngradedModule, doDowngrade} from "../downgrade";
import {CsUtilityModule} from "../../../../modules/cs/js/util/module";
import {
    FileSelectManagerComponent,
    IFile,
    IFileSpecification,
} from "../../../../modules/cs/js/util/file-select";
import {
    ITimComponent,
    IVelpableComponent,
    ViewCtrl,
} from "../document/viewctrl";
import {vctrlInstance} from "../document/viewctrlinstance";
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
        <p stem *ngIf="stem" [innerHTML]="stem | purify"></p>
        <ng-container body>
            <div class="form-inline small">
                <div style="position: relative;" *ngFor="let item of uploadedFiles; let i = index">
                   <div #wraps>
                        <img alt="Uploaded image" #img [src]="item.path" (load)="onImgLoad($event, i)">
                    </div>
                    <div class="tools">
                        <button class="timButton" title="Move up" i18n-title (click)="moveImageUp(i)">&uarr;</button>
                        <button class="timButton" title="Move down" i18n-title (click)="moveImageDown(i)">&darr;
                        </button>
                        <button class="timButton" title="Rotate clockwise" i18n-title (click)="increaseRotation(i)">&#8635;
                        </button>
                        <button class="timButton" title="Delete picture" i18n-title (click)="deleteImage(i)">
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
        <button class="timButton" (click)="save()" i18n>Save answer</button>
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
    implements OnInit, OnDestroy, ITimComponent, IVelpableComponent
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
    private init = false;
    private loadedImages = 0;
    private vctrl!: ViewCtrl;

    fileSelect?: FileSelectManagerComponent;
    uploadUrl?: string;
    dragAndDrop: boolean = true;
    uploadstem?: string;
    uploadedFiles: IUploadedFile[] = [];

    @ViewChildren("img") imgElements!: QueryList<ElementRef<HTMLImageElement>>;
    @ViewChildren("wraps") wraps!: QueryList<ElementRef<HTMLDivElement>>;

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
        this.vctrl = vctrlInstance!;
        this.vctrl.addTimComponent(this, this.markup.tag);
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
            this.init = false;
            this.attrsall.state.uploadedFiles.forEach((uf) =>
                this.uploadedFiles.push(uf)
            );
        } else {
            this.init = true;
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
            imgElement.style.removeProperty("top");
            imgElement.style.removeProperty("height");
            wrapper.style.removeProperty("height");
        } else {
            imgElement.style.position = "relative";
            if (oldHeight < oldWidth) {
                imgElement.style.top = (oldWidth - oldHeight) / 2 + "px";
                wrapper.style.height = oldWidth + "px";
            } else {
                const newHeight = Math.min(oldHeight, wrapperWidth);
                imgElement.style.height = newHeight + "px";
                const newWidth = imgElement.width;
                wrapper.style.height = newWidth + "px";
                imgElement.style.top = -(newHeight - newWidth) / 2 + "px";
            }
        }
    }

    onImgLoad(e: Event, index: number): void {
        this.loadedImages += 1;
        if (
            this.attrsall.state?.uploadedFiles &&
            this.attrsall.state?.uploadedFiles.length <= this.loadedImages
        ) {
            this.init = true;
        }
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
            this.changes = false;
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
     * Return promise of images' dataUrl presentation
     * The returned images are fully rotated to their current rotation value (90deg per one rotation)
     */
    async getVelpImages(): Promise<string[] | undefined> {
        while (!this.init) {
            await timeout();
        }
        const imgs = this.uploadedFiles.map((file) => {
            const newImg = new Image();
            newImg.src = file.path;
            return newImg;
        });
        await Promise.all(imgs.map((img) => img.decode()));
        return imgs.map((img, index) => {
            const uploadedFile = this.uploadedFiles[index];
            if (uploadedFile.rotation == undefined) {
                return "";
            }
            const canvas = document.createElement("canvas");
            const ctx = canvas.getContext("2d")!;
            let dx = 0;
            let dy = 0;
            if (uploadedFile.rotation == 1) {
                dy = -img.height;
            }
            if (uploadedFile.rotation == 2) {
                dx = -img.width;
                dy = -img.height;
            }
            if (uploadedFile.rotation == 3) {
                dx = -img.width;
            }
            canvas.height =
                uploadedFile.rotation % 2 == 0 ? img.height : img.width;
            canvas.width =
                uploadedFile.rotation % 2 == 0 ? img.width : img.height;
            ctx.rotate((Math.PI / 2) * uploadedFile.rotation);
            ctx.drawImage(img, dx, dy);
            return canvas.toDataURL();
        });
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
        PurifyModule,
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
