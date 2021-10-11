import {Component, Input, OnInit} from "@angular/core";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import {HttpClient} from "@angular/common/http";
import {IFolder, IItem} from "../item/IItem";
import {to2} from "../util/utils";

type PreviewList = {from: string; to: string}[];

interface CopyOptions {
    copy_active_rights: boolean;
    copy_expired_rights: boolean;
    stop_on_errors: boolean;
}

const DEFAULT_COPY_OPTIONS: CopyOptions = {
    copy_active_rights: true,
    copy_expired_rights: false,
    stop_on_errors: true,
};

@Component({
    selector: "tim-copy-folder",
    template: `
        <form>
            <p>You can copy all documents and folders in this folder to another folder.</p>
            <p>Copy options</p>
            <div class="cb-group">
                <div class="checkbox">
                    <label><input type="checkbox" name="copy-active-rights" [(ngModel)]="copyOptions.copy_active_rights"> Copy active access rights</label>
                </div>
                <div class="checkbox">
                    <label><input type="checkbox" name="copy-expired-rights" [(ngModel)]="copyOptions.copy_expired_rights"> Copy expired access rights</label>
                </div>
                <div class="checkbox">
                    <label><input type="checkbox" name="stop-errors" [(ngModel)]="copyOptions.stop_on_errors"> Stop copying on errors</label>
                </div>
            </div>
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
            <span *ngIf="copyingFolder == 'copying'"><tim-loading></tim-loading> Copying, this might take a while...</span>
            <span *ngIf="copyingFolder == 'finished'">
                Folder {{ item.name }} copied to
                <a href="/manage/{{ newFolder?.path }}" [innerText]="newFolder?.path"></a>.
            </span>
            <div *ngIf="copyErrors">
                <p>The following errors occurred while copying:</p>
                <ul>
                    <li *ngFor="let e of copyErrors">{{e}}</li>
                </ul>
            </div>
        </form>
    `,
    styleUrls: ["copy-folder.component.scss"],
})
export class CopyFolderComponent implements OnInit {
    copyingFolder: "notcopying" | "copying" | "finished";
    @Input() item!: IItem;
    copyPreviewList?: PreviewList;
    destExists?: boolean;
    copyFolderPath!: string;
    copyFolderExclude: string;
    newFolder?: IFolder;
    copyOptions: CopyOptions = {...DEFAULT_COPY_OPTIONS};
    copyErrors?: string[];

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
                .post<{new_folder?: IFolder; errors: string[]}>(
                    `/copy/${this.item.id}`,
                    {
                        destination: path,
                        exclude: exclude,
                        copy_options: this.copyOptions,
                    }
                )
                .toPromise()
        );
        if (r.ok && r.result.errors.length == 0) {
            this.copyingFolder = "finished";
            this.copyPreviewList = undefined;
            this.destExists = undefined;
            this.newFolder = r.result.new_folder;
        } else {
            this.copyingFolder = "notcopying";
            if (!r.ok) {
                await showMessageDialog(r.result.error.error);
            } else {
                if (!this.copyOptions.stop_on_errors) {
                    this.copyingFolder = "finished";
                    this.newFolder = r.result.new_folder;
                }
                this.copyErrors = r.result.errors;
            }
        }
    }

    copyParamChanged() {
        this.copyPreviewList = undefined;
        this.destExists = undefined;
        this.copyErrors = undefined;
    }
}
