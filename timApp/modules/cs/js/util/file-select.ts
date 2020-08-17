/* eslint no-underscore-dangle: ["error", { "allow": ["files_", "multipleElements_"] }] */
/* eslint-disable @typescript-eslint/tslint/config -- decorators cause issues on setters */
import {
        Component,
        ViewChild,
        ViewChildren,
        ElementRef,
        Input,
        Output,
        EventEmitter,
        QueryList,
        ChangeDetectorRef,
    } from "@angular/core";
import {HttpClient, HttpEventType, HttpErrorResponse} from "@angular/common/http";
import {NotificationComponent} from "./notification";
import {timeString, sizeString} from "./util";
import {Set} from "./set";

export interface IFile {
    path: string;
    content: string;
    realName: string;
}

interface IFileSpecificationBase {
    extensions?: string[],
    maxSize?: number,
    upload?: boolean,
}

export interface IFileSpecification extends IFileSpecificationBase {
    paths: string[],
}

export interface IMapping {
    path: string;
    upload?: boolean;
}

// a function to map loaded files to filenames and whether to upload or load
export type MappingFunction = (source: FileSelectComponent, filesArray: File[]) => Promise<IMapping[] | undefined>;

class Path {
    parent?: string;
    name: string;
    extension?: string;

    constructor(path: string) {
        let index = path.lastIndexOf("/");
        if (index != -1) {
            this.parent = path.substring(0, index + 1);
            path = path.substring(index + 1);
        }
        index = path.indexOf(".");
        if (index != -1) {
            this.extension = path.substring(index);
            path = path.substring(0, index);
        }
        this.name = path;
    }

    nameMatches(path: string) {
        return this.name == new Path(path).name;
    }

    toString() {
        return (this.parent ?? "") + this.name + (this.extension ?? "");
    }
}


let nextId = 0;
// TODO: reset input value so same file can be uploaded again
// done on drag-and-drop but default html file input makes it seem
// like the file wasn't loaded at all if input value is reset.
// Maybe only use drag-and-drop + label combo or replace the default
// file input using a label.
@Component({
    selector: "file-select",
    template: `
        <div *ngIf="!dragAndDrop && stem">{{stem}}:</div>
        <input #input [disabled]="!!progress" type="file" [hidden]="dragAndDrop" [id]="inputId" (change)="onFileChange($event)" (attr.multiple)="multiple"/>
        <notification *ngIf="!dragAndDrop" class="error" #error></notification>
        <ng-container *ngIf="dragAndDrop">
            <label title="Drag-and-drop or Click to browse" [for]="inputId" *ngIf="dragAndDrop"
                    class="drag-and-drop"
                    (drop)="onDrop($event)"
                    (dragover)="onDragOver($event)"
                    (click)="clearStatus();">
                <div>
                    <div>
                        <p *ngIf="!progress && loadInfo.length == 0 && error.length == 0">{{stem}}</p>
                        <ng-container *ngIf="progress">
                            <p *ngIf="numFiles > 1">File {{numUploaded+1}} / {{numFiles}}</p>
                            <p>{{progress}}</p>
                        </ng-container>
                        <notification #loadInfo></notification>
                        <notification class="error" #error></notification>
                    </div>
                </div>
            </label>
        </ng-container>`,
})
export class FileSelectComponent { // TODO: translations
    @Input() inputId: string = `file-select-${nextId++}`;
    @Input() id: string = "";
    @Input() path?: string;
    @Input() multiple: boolean = false;
    @Input() dragAndDrop: boolean = true;
    @Input() stem?: string;
    @Input() uploadUrl?: string;
    @Input() maxSize: number = -1;
    @Input() mappingFunction?: MappingFunction;
    @Output("file") fileEmitter: EventEmitter<IFile> = new EventEmitter(true);
    @Output("files") filesEmitter: EventEmitter<IFile[]> = new EventEmitter(true);
    @Output("upload") uploadEmitter: EventEmitter<unknown> = new EventEmitter();
    @Output("uploadDone") uploadDoneEmitter: EventEmitter<boolean> = new EventEmitter(true);

    @ViewChild("input") inputElement?: ElementRef;
    @ViewChild("error") error?: NotificationComponent;
    @ViewChild("loadInfo") loadInfo?: NotificationComponent;

    progress?: string;
    numFiles: number = 0;
    numUploaded: number = 0;

    constructor(private http: HttpClient) {}

    clearStatus() {
        this.error?.clear();
        this.loadInfo?.clear();

        this.numFiles = 0;
        this.numUploaded = 0;
    }

    onFileChange(event: Event) {
        this.clearStatus();
        const target = event.target as HTMLInputElement | null;
        if (!target) { return; }

        this.onFilesGot(target.files);
    }

    onDrop(event: DragEvent) {
        this.clearStatus();
        if (!event.dataTransfer) { return; }

        event.preventDefault();
        this.onFilesGot(event.dataTransfer.files);
    }

    onDragOver(event: Event) {
        event.stopPropagation();
        event.preventDefault();
    }

    addError(error: string, timeout?: number) {
        this.error?.push(error, timeout);
    }

    uploadFiles(files: File[], mappings: IMapping[]) {
        if (!this.uploadUrl) { return; }
        if (this.progress) {
            this.addError("File upload already in progress", 3000);
            return;
        }

        const upload = (i: number) => {
            if (i < files.length) {
                const mapping = mappings[i];
                const file = files[i];
                if (!mapping.upload) {
                    upload(i + 1);
                    return;
                }
                const formdata = new FormData();
                formdata.append("file", file, mapping.path);
                const obs = this.http.post(this.uploadUrl!, formdata, { reportProgress: true, observe: "events" }).subscribe(
                    (event) => {
                        switch (event.type) {
                            case HttpEventType.Sent:
                                this.progress = "Upload starting... ";
                                break;
                            case HttpEventType.UploadProgress:
                                const total = event.total !== undefined ? sizeString(event.total) : "?";
                                const percentage = event.total ? Math.round(event.loaded / event.total * 100) : "?";
                                this.progress = `Uploading... ${sizeString(event.loaded)} / ${total} (${percentage} %)`;
                                break;
                            case HttpEventType.Response:
                                this.numUploaded++;
                                if (file.name == mapping.path) {
                                    this.loadInfo?.push(`Uploaded ${file.name} at ${timeString()}`); // TODO: push to correct file-select
                                } else {
                                    this.loadInfo?.push(`Uploaded ${file.name} as ${mapping.path} at ${timeString()}`); // TODO: push to correct file-select
                                }
                                this.uploadEmitter.emit(event.body);
                                upload(i + 1);
                                break;
                            default:
                                break;
                        }
                    },
                    (error: HttpErrorResponse) => {
                        this.addError(`Http error: ${error.message}`);
                        this.progress = undefined;
                    }
                );
            } else {
                this.uploadDoneEmitter.emit(this.numUploaded == this.numFiles);
                this.progress = undefined;
            }
        };

        this.numFiles = files.length;
        this.numUploaded = 0;

        upload(0);
    }

    async onFilesGot(files: FileList | null) {
        const outFiles: IFile[] = [];
        if (files) {
            if (!this.multiple && files.length > 1) {
                this.addError("You may only upload one file");
                return;
            }

            if (this.maxSize > 0) {
                const maxSize = this.maxSize*1000;
                for (const file of files) {
                    if (file.size > maxSize) {
                        this.addError(`Maximum file size is ${sizeString(maxSize)}`);
                        return;
                    }
                }
            }

            let mappings;
            const filesArray: File[] = Array.from(files);
            if (this.mappingFunction) {
                mappings = await this.mappingFunction?.(this, filesArray);
                if (!mappings) { return; }
            } else {
                const doUpload = !!this.uploadUrl;
                mappings = filesArray.map((f) => ({path: f.name, upload: doUpload}));
            }

            this.uploadFiles(filesArray, mappings);

            if (this.fileEmitter.observers.length != 0 || this.filesEmitter.observers.length != 0) {
                const promises = [];
                for (let i = 0; i < filesArray.length; i++) {
                    const mapping = mappings[i];
                    const file = filesArray[i];
                    if (mapping.upload) {
                        continue;
                    }
                    const promise = new Promise((resolve) => {
                        const reader = new FileReader();
                        reader.onload = (e) => {
                            const f = {
                                content: reader.result as string,
                                path: mapping.path,
                                realName: file.name,
                            };
                            outFiles.push(f);
                            this.fileEmitter.emit(f);
                            if (file.name == mapping.path) {
                                this.loadInfo?.push(`Loaded ${file.name} at ${timeString()}`);
                            } else {
                                this.loadInfo?.push(`Loaded ${file.name} as ${mapping.path} at ${timeString()}`);
                            }
                            resolve();
                        };
                        reader.onerror = (e) => {
                            this.addError(`Failed to read ${file.name}`);
                            resolve();
                        };
                        reader.readAsText(file);
                    });
                    promises.push(promise);
                }
                await Promise.all(promises);
            } else if (!this.uploadUrl) {
                this.addError("No file upload or observers present. This is a programming/configuration error.");
            }
        }
        this.filesEmitter.emit(outFiles);

        if (this.inputElement && this.dragAndDrop) {
            (this.inputElement.nativeElement as HTMLInputElement).value = "";
        }
    }
}

@Component({
    selector: "file-select-manager",
    template: `
        <file-select *ngFor="let info of fileInfo"
            [dragAndDrop]="dragAndDrop"
            [stem]="info.stem"
            [id]="info.id"
            [multiple]="allowMultiple || (fileInfo.length > 1 && (files.length > 1 || files[0].paths.length > 1))"
            [uploadUrl]="uploadUrl"
            (file)="onFileLoad($event)"
            (files)="filesEmitter.emit($event)"
            (upload)="uploadEmitter.emit($event)"
            (uploadDone)="uploadDoneEmitter.emit($event)"
            [mappingFunction]="fileMappings(this)">
        </file-select>`,
})
export class FileSelectManagerComponent { // TODO: translations
    @Input() allowMultiple: boolean = true;
    @Input() dragAndDrop: boolean = true;
    @Input() uploadUrl?: string;
    @Input() stem?: string;
    @Input() maxSize: number = -1;
    @Output("file") fileEmitter: EventEmitter<IFile> = new EventEmitter();
    @Output("files") filesEmitter: EventEmitter<IFile[]> = new EventEmitter();
    @Output("upload") uploadEmitter: EventEmitter<unknown> = new EventEmitter();
    @Output("uploadDone") uploadDoneEmitter: EventEmitter<boolean> = new EventEmitter();

    private fileSelects: {[key: string]: FileSelectComponent} = {};

    loadedFiles = new Set((e: IFile) => e.path);

    multipleElements_: boolean = true;
    files_: IFileSpecification[] = [];

    private idToFile: {[key: string]: [number, string | undefined]} = {}; // contains (index, path)-tuples
    fileInfo: {
        stem: string,
        id: string,
    }[] = [];

    constructor(public cdr: ChangeDetectorRef) {}

    get multipleElements() {
        return this.multipleElements_;
    }
    @Input()
    set multipleElements(val: boolean) {
        this.multipleElements_ = val;
        this.updateFileInfo();
    }

    get files() {
        return this.files_;
    }
    @Input()
    set files(specs: IFileSpecification[]) {
        this.files_ = specs;
        this.updateFileInfo();
    }

    updateFileInfo() {
        const self = this;
        let nfileInfo: typeof self.fileInfo = [];
        const nidToFile: typeof self.idToFile = {};
        if (this.files.length != 0) {
            if (this.multipleElements) {
                for (let i = 0; i < this.files.length; i++) {
                    for (const path of this.files[i].paths) {
                        const id: string = `${i}.${path}`;
                        const stem = (this.files.length == 1 && this.files[0].paths.length == 1 ? this.stem : undefined) ?? `Upload ${path} here`;
                        nfileInfo.push({
                            stem: stem,
                            id: id,
                        });
                        nidToFile[id] = [i, path];
                    }
                }
            } else {
                nfileInfo = [{
                    stem: this.stem ?? "Upload files here",
                    id: "",
                }];
                nidToFile[""] = [0, undefined];
            }
        }
        this.idToFile = nidToFile;
        this.fileInfo = nfileInfo;
        this.cdr.detectChanges();
    }

    @ViewChildren(FileSelectComponent)
    set fileSelectsSetter(list: QueryList<FileSelectComponent> | undefined) {
        this.fileSelects = {};
        if (!list) {
            return;
        }
        for (const item of list) {
            this.fileSelects[item.id] = item;
        }
    }

    onFileLoad(file: IFile) {
        this.loadedFiles.push(file);
        this.fileEmitter.emit(file);
    }

    fileMappings(self: FileSelectManagerComponent) {
        // return a list of (index, path) tuples for mapping names for the given files
        return async (child: FileSelectComponent, filesArray: File[]): Promise<IMapping[] | undefined> => {
            await undefined; // dummy for async

            if (filesArray.length == 1 && self.multipleElements) {
                return [{
                    path: !!self.idToFile[child.id][1] ? self.idToFile[child.id][1] as string : filesArray[0].name,
                    upload: self.files[self.idToFile[child.id][0]].upload,
                }];
            }

            const maxFiles = self.files.flatMap((f) => f.paths).length;
            if (filesArray.length > maxFiles) {
                child.addError(`You may only upload ${maxFiles} file(s)`);
                return;
            }

            const canGo: number[][] = Array(filesArray.length).fill([]); // maps filesArray indices to possible self.files indices
            const fileNames: Path[] = [];
            for (let i = 0; i < filesArray.length; i++) { // first filter by file extensions and names
                const name = filesArray[i].name;
                const path = new Path(name);
                fileNames.push(path);
                for (let j = 0; j < self.files.length; j++) {
                    const paths = self.files[j].paths.map((p) => new Path(p));
                    if (paths.some((e) => e.name == path.name) ||
                            !self.files[j].extensions ||
                            (path.extension && self.files[j].extensions!.some((e) => path.extension!.endsWith(e)))) {
                        canGo[i].push(j);
                    }
                }
            }

            const lengthZeroIndex = (list: unknown[][]) => list.findIndex((l) => l.length == 0);
            const isTooLarge = (file: File, index: number) => {
                const maxSize = (this.files[index].maxSize ?? this.maxSize)*1000;
                if (maxSize > 0 && file.size > maxSize) {
                    child.addError(`${file.name} exceeds the maximum ${sizeString(maxSize)} size limit set for the following files: ${this.files[index].paths.join(", ")}`);
                    return true;
                }
                return false;
            };

            const zeroIndex = lengthZeroIndex(canGo);
            if (zeroIndex != -1) { // check that all files have a possible mapping
                child.addError(`Failed to match the extension or name of file ${filesArray[zeroIndex].name} to any of the wanted files.`);
                return;
            }

            const mappings: [number, string][] = [];
            const notAssigned = (JSON.parse(JSON.stringify(self.files)) as IFileSpecification[]).map(
                (f) => {
                    const k: IFileSpecificationBase & {paths: Path[]} = f as IFileSpecification & {paths: Path[]};
                    k.paths = f.paths.map((p) => new Path(p));
                    return k;
                }
            );
            let numMapped = 0;
            let numMappedLast = -1;
            // while new mappings are found. Former condition is to stop infinite loops in case of bugs
            while (numMapped < filesArray.length && numMapped != numMappedLast) {
                numMappedLast = numMapped;

                for (let i = 0; i < notAssigned.length; i++) {
                    const matches = canGo.map((val, index) => [val, index] as [number[], number]) // zip with index
                        .filter((val) => !mappings[val[1]]) // remove already mapped ones
                        .filter((val) => val[0].includes(i)); // get all that map to self.files index i
                    if (matches.length == notAssigned[i].paths.length) {
                        for (const m of matches) {
                            if (isTooLarge(filesArray[m[1]], i)) {
                                return;
                            }
                            canGo[m[1]] = [i];
                            mappings[m[1]] = [i, notAssigned[i].paths.pop()?.toString() ?? ""];
                            numMapped++;
                        }
                        continue;
                    }

                    for (let j = 0; j < notAssigned[i].paths.length; j++) {
                        const path = notAssigned[i].paths[j];
                        const nameMatches = matches.filter((val) => fileNames[val[1]].name == path.name); // find files that match the path
                        if (nameMatches.length == 1) { // if the only match, assign it
                            if (isTooLarge(filesArray[nameMatches[0][1]], i)) {
                                return;
                            }
                            notAssigned[i].paths.splice(j);
                            canGo[nameMatches[0][1]] = [i];
                            mappings[nameMatches[0][1]] = [i, path.toString()];
                            numMapped++;
                            j--;
                        }
                    }
                }

                const zIndex = lengthZeroIndex(canGo);
                if (zIndex != -1) { // check that all files have a possible mapping
                    child.addError(`Failed to match file ${filesArray[zIndex].name} to any of the wanted files.`);
                    return;
                }
            }

            if (canGo.map((val, index) => [val, index] as [number[], number]).filter((val) => !mappings[val[1]]).length != 0) {
                // TODO: open a dialog to user for manually mapping the unmapped (length of matching array in canGo greater than one) files
                child.addError("Failed to match given files to wanted files. Check the file extensions and try matching the filenames to the wanted ones.");
                return;
            }

            return mappings.map((e, i) => ({
                path: e[1] != "" ? e[1] : filesArray[i].name,
                upload: self.files[e[0]].upload,
            }));
        };
    }
}
