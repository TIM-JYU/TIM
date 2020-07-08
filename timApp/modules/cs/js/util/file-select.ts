/* eslint no-underscore-dangle: ["error", { "allow": ["files_", "multipleElements_"] }] */
/* eslint-disable @typescript-eslint/tslint/config -- decorators cause issues on setters */
import {
        Component,
        ViewChild,
        ElementRef,
        Input,
        Output,
        EventEmitter,
    } from "@angular/core"
import {HttpClient, HttpEventType} from "@angular/common/http";

export interface IFile {
    name: string;
    content: string;
    realName: string;
}

@Component({
    selector: "file-select",
    template: `
        <div *ngIf="!dragAndDrop && uploadStem">{{uploadStem}}:</div>
        <div *ngIf="!dragAndDrop && !uploadStem">{{fileName ? fileName + ":" : ""}}</div>
        <input [disabled]="!!progress" type="file" [hidden]="dragAndDrop" id="fileinput" (change)="onFileChange($event)" (attr.multiple)="multiple"/>
        <label title="Drag-and-drop or Click to browse" for="fileinput" *ngIf="dragAndDrop"
                class="drag-and-drop" 
                (drop)="onDrop($event)" 
                (dragover)="onDragOver($event)"
                (click)="clearStatus()">
            <div>
                <div>
                    <ng-container *ngIf="!error && !progress">
                        <p *ngIf="uploadInfo">{{uploadInfo}}</p>
                        <ng-container *ngIf="!uploadInfo">
                            <p *ngIf="submitInfo">{{submitInfo}}</p>
                            <p *ngIf="!submitInfo">Upload {{fileName}}</p>
                        </ng-container>
                    </ng-container>
                    <ng-container *ngIf="progress">
                        <p *ngIf="numFiles > 1">File {{numUploaded+1}} / {{numFiles}}</p>
                        <p>{{progress}}</p>
                    </ng-container>
                    <p *ngIf="error" class="error" >Error: {{error}}</p>
                </div>
            </div>
        </label>
        <div class="error" *ngIf="!dragAndDrop && error">Error: {{error}}</div>`,
})
export class FileSelectComponent { // TODO: translations
    @Input() multiple: boolean = false;
    @Input() dragAndDrop: boolean = true;
    @Input() fileName?: string;
    @Input() maxSize: number = -1;
    @Input() maxTotalSize: number = -1;
    @Input() url?: string;
    @Input() uploadStem?: string;
    @Output("file") fileEmitter: EventEmitter<IFile> = new EventEmitter(true);
    @Output("files") filesEmitter: EventEmitter<IFile[]> = new EventEmitter(true);
    @Output("upload") uploadEmitter: EventEmitter<unknown> = new EventEmitter();
    @Output("uploadDone") uploadDoneEmitter: EventEmitter<boolean> = new EventEmitter();
    
    error?: string;
    
    submitInfo?: string;
    
    progress?: string;
    numFiles: number = 0;
    numUploaded: number = 0;
    uploadInfo?: string;
    
    constructor(private http: HttpClient) {}
    
    clearStatus() {
        this.error = undefined;
        
        this.progress = undefined;
        this.numFiles = 0;
        this.numUploaded = 0;
        this.uploadInfo = undefined;
        
        this.submitInfo = undefined;
    }
    
    onFileChange(event: Event) {
        this.clearStatus();
        const target = <HTMLInputElement | null>event.target;
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
    
    addError(error: string) {
        this.error = (this.error ? this.error + "\n" : "") + error;
    }
    
    sizeString(inBytes: number): string {
        if (inBytes < 1000) {
            return inBytes + " B";
        } else if (inBytes < 1000000) {
            return Math.round(inBytes/1000) + " kB";
        } else if (inBytes < 1000000000) {
            return Math.round(inBytes/1000000) + " MB";
        } else {
            return Math.round(inBytes/1000000000) + " GB";
        }
    }
    
    async uploadFiles(files: FileList) {
        if (!this.url) { return; }
        if (this.progress) {
            this.addError("File upload already in progress");
            return;
        }
        
        this.numFiles = files.length;
        this.numUploaded = 0;
        this.progress = undefined;
        this.uploadInfo = undefined;
        for (const file of files) {
            const formdata = new FormData();
            formdata.append("file", file, this.fileName ?? file.name);
            const obs = this.http.post(this.url, formdata, { reportProgress: true, observe: "events" });
            obs.subscribe(
                event => {
                    switch(event.type) {
                        case HttpEventType.Sent:
                            this.progress = "Upload starting... ";
                            break;
                        case HttpEventType.UploadProgress:
                            const total = event.total !== undefined ? this.sizeString(event.total) : "?";
                            const percentage = event.total ? Math.round(event.loaded / event.total) : "?";
                            this.progress = `Uploading... ${this.sizeString(event.loaded)} / ${total} (${percentage} %)`;
                            break;
                        case HttpEventType.Response:
                            this.progress = undefined;
                            this.numUploaded++;
                            this.uploadEmitter.emit(event.body);
                            break;
                    }
                },
                error => {
                    this.addError(error.statusText);
                }
            );
            await obs.toPromise();
        }
        
        this.uploadDoneEmitter.emit(this.numUploaded == this.numFiles);
        
        const date = new Date;
        if (this.numFiles > 1) {
            this.uploadInfo = `Uploaded ${this.numUploaded} files at ${date.getHours()}:${date.getMinutes()}`;
        } else if(files.length > 0) {
            this.uploadInfo = `Uploaded ${files.item(0)?.name} at ${date.getHours()}:${date.getMinutes()}`;
        }
        this.progress = undefined;
    }
    
    async onFilesGot(files: FileList | null) {
        let outFiles: IFile[] = [];
        if (files) {
            
            if(!this.multiple && files.length > 1) {
                this.addError("You may only upload one file");
                return;
            }
            
            if (this.maxSize > 0 || this.maxTotalSize > 0) {
                let totalSize = 0;
                const maxSize = this.maxSize*1000;
                const maxTotalSize = this.maxTotalSize*1000;
                for (const f of files) {
                    if (maxSize > 0 && f.size > maxSize) {
                        this.addError(`Maximum file size is ${this.sizeString(maxSize)}`);
                        return;
                    }
                    totalSize += f.size;
                }
                if(maxTotalSize > 0 && totalSize > maxTotalSize) {
                    this.addError(`Maximum total file size is ${this.sizeString(maxTotalSize)}`);
                    return;
                }
            }
            
            this.uploadFiles(files);
            
            if(this.fileEmitter.observers.length != 0 || this.filesEmitter.observers.length != 0) {
                let promises = [];
                for (const file of files) {
                    const promise = new Promise(resolve => {
                        const reader = new FileReader();
                        reader.onload = (e) => {
                            const f = {
                                content: reader.result as string,
                                name: this.fileName ?? file.name, // TODO: multiple files filenames
                                realName: file.name,
                            };
                            outFiles.push(f);
                            this.fileEmitter.emit(f);
                            resolve();
                        };
                        reader.onerror = (e) => {
                            this.error = "Failed to read";
                            resolve();
                        }
                        reader.readAsText(file);
                    });
                    promises.push(promise);
                }
                
                await Promise.all(promises);
            } else if (!this.url) {
                this.error = "No file upload or observers present. This is a programming/configuration error.";
            }
        }
        
        this.filesEmitter.emit(outFiles);
        
        const date = new Date;
        if (this.numFiles > 1) {
            this.submitInfo = `Loaded ${this.numUploaded} files at ${date.getHours()}:${date.getMinutes()}`;
        } else if(outFiles.length > 0) {
            this.submitInfo = `Loaded ${outFiles[0].realName} at ${date.getHours()}:${date.getMinutes()}`;
        }
    }
}