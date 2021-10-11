import {Component, Input, OnInit} from "@angular/core";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import {HttpClient} from "@angular/common/http";
import {IFolder, IItem} from "../item/IItem";
import {to2} from "../util/utils";

type PreviewList = {from: string; to: string}[];

@Component({
    selector: "tim-copy-folder",
    template: `
        <form>
            <p>You can copy all documents and folders in this folder to another folder.</p>
            <p>This will also copy</p>
            <ul>
                <li>Document and printing templates</li>
                <li>Documents' edit history</li>
                <li>Current rights</li>
                <li>Expired rights (this may affect document and folder access for some users)</li>
            </ul>
            <div class="form-group" timErrorState>
                <label for="destination" class="control-label">Destination:</label>
                <input name="copyPath" class="form-control" timLocation id="destination" type="text" autocomplete="off"
                       [(ngModel)]="copyFolderPath" (ngModelChange)="copyParamChanged()" #copyPath="ngModel">
                <tim-error-message></tim-error-message>
            </div>
            <p>You can optionally exclude some documents/folders from being copied.</p>
            <div class="form-group" timErrorState>
                <label for="exclude" class="control-label">Exclude documents/folders that match:</label>
                <input name="exclude" class="form-control" id="exclude" type="text" autocomplete="off"
                       [(ngModel)]="copyFolderExclude" (ngModelChange)="copyParamChanged()">
                <tim-error-message></tim-error-message>
            </div>
            <button (click)="copyFolderPreview(copyFolderPath, copyFolderExclude)" class="timButton"
                    [disabled]="copyFolderPath == item.path || copyPath.invalid"
                    *ngIf="copyingFolder == 'notcopying'">Copy preview...
            </button>
            <ul *ngIf="previewLength > 0">
                <li *ngFor="let p of copyPreviewList">
                    <span [innerText]="p.from"></span>
                    <i class="glyphicon glyphicon-arrow-right"></i>
                    <span [innerText]="p.to"></span>
                </li>
            </ul>
            <p *ngIf="previewLength == 0">Nothing would be copied.</p>
            <tim-alert severity="warning" *ngIf="destExists">
                The destination folder already exists. Make sure this is intended before copying.
            </tim-alert>
            <button (click)="copyFolder(copyFolderPath, copyFolderExclude)" class="timButton"
                    *ngIf="copyFolderPath != item.path &&
                     previewLength > 0 &&
                     copyingFolder == 'notcopying'">Copy
            </button>
            <span *ngIf="copyingFolder == 'copying'">Copying...</span>
            <span *ngIf="copyingFolder == 'finished'">
                Folder {{ item.name }} copied to
                <a href="/manage/{{ newFolder?.path }}" [innerText]="newFolder?.path"></a>.
            </span>
        </form>
    `,
})
export class CopyFolderComponent implements OnInit {
    copyingFolder: "notcopying" | "copying" | "finished";
    @Input() item!: IItem;
    copyPreviewList?: PreviewList;
    destExists?: boolean;
    copyFolderPath!: string;
    copyFolderExclude: string;
    newFolder?: IFolder;

    constructor(private http: HttpClient) {
        this.copyingFolder = "notcopying";
        this.copyFolderExclude = "";
    }

    get previewLength() {
        return this.copyPreviewList?.length ?? -1;
    }

    ngOnInit() {
        this.copyFolderPath = this.item.path;
    }

    async copyFolderPreview(path: string, exclude: string) {
        this.copyingFolder = "notcopying";
        const r = await to2(
            this.http
                .post<{preview: PreviewList; dest_exists: boolean}>(
                    `/copy/${this.item.id}/preview`,
                    {
                        destination: path,
                        exclude: exclude,
                    }
                )
                .toPromise()
        );
        if (r.ok) {
            this.copyPreviewList = r.result.preview;
            this.destExists = r.result.dest_exists;
        } else {
            await showMessageDialog(r.result.error.error);
        }
    }

    async copyFolder(path: string, exclude: string) {
        this.copyingFolder = "copying";
        const r = await to2(
            this.http
                .post<IFolder>(`/copy/${this.item.id}`, {
                    destination: path,
                    exclude: exclude,
                })
                .toPromise()
        );
        if (r.ok) {
            this.copyingFolder = "finished";
            this.copyPreviewList = undefined;
            this.destExists = undefined;
            this.newFolder = r.result;
        } else {
            this.copyingFolder = "notcopying";
            await showMessageDialog(r.result.error.error);
        }
    }

    copyParamChanged() {
        this.copyPreviewList = undefined;
        this.destExists = undefined;
    }
}
