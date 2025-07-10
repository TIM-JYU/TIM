import type {OnInit} from "@angular/core";
import {Component, Input} from "@angular/core";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import {HttpClient} from "@angular/common/http";
import type {IFolder} from "tim/item/IItem";
import {IItem} from "tim/item/IItem";
import {toPromise} from "tim/util/utils";

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

const COPY_HELP_ADDRESS: string = "/view/tim/TIM-ohjeet#hakemistonkopionti";

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
            <p>You can optionally enter a regular expression to exclude specific documents or folders from being copied.</p> 
            <p *ngIf="copyHelp">For more information on copying see the <a [href]="copyHelp">help page</a>.</p>
            <div class="form-group" timErrorState>
                <label for="exclude" class="control-label">Exclude documents/folders that match:</label>
                <input name="exclude" class="form-control" id="exclude" type="text" autocomplete="off"
                       [(ngModel)]="copyFolderExclude" (ngModelChange)="copyParamChanged()">
                <tim-error-message></tim-error-message>
            </div>
            <div class="form-group">
                <button (click)="copyFolderPreview(copyFolderPath, copyFolderExclude)" class="timButton"
                        [disabled]="copyFolderPath == item.path || copyPath.invalid"
                        *ngIf="copyingFolder == 'notcopying'">Copy preview...
                </button>
            </div>
            <div class="panel panel-default" *ngIf="previewLength > 0">
                <div class="panel-heading">Exclude folder or document items</div>
                    <div class="panel-body">
                        <p>These are the items that will be copied. To exclude an item, simply check its corresponding checkbox.</p>
                    </div>
                <table class="table-responsive">
                    <table class="table table-hover">
                    <thead>
                        <tr>
                            <th><input type="checkbox" [checked]="allSelected()" (change)="toggleAll($event)"></th>
                            <th>From</th>
                            <th></th>
                            <th>To</th>
                        </tr>
                    </thead>
                    <tbody>
                        <tr *ngFor="let listItem of copyPreviewList" (click)="toggleItem(listItem)" [ngClass]="{'active text-muted': isSelected(listItem)}">
                            <td><input type="checkbox" [checked]="isSelected(listItem)" (click)="toggleItem(listItem); $event.stopPropagation()"></td>
                            <td><span [innerText]="listItem.from"></span></td>
                            <td><i class="glyphicon glyphicon-arrow-right"></i></td>
                            <td><span [innerText]="listItem.to"></span></td>
                        </tr>
                    </tbody>
                    </table>
                </table>
            </div>
            <p *ngIf="previewLength == 0 || allSelected()">Nothing would be copied.</p>
            <tim-alert severity="warning" *ngIf="destExists">
                The destination folder already exists. Make sure this is intended before copying.
            </tim-alert>
            <button (click)="copyFolder(copyFolderPath, copyFolderExclude)" class="timButton"
                    *ngIf="copyFolderPath != item.path &&
                     previewLength > 0 &&
                     copyingFolder == 'notcopying'" [disabled]="allSelected()">Copy
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
    excludedItems: Set<string>;
    copyHelp: string = COPY_HELP_ADDRESS;

    constructor(private http: HttpClient) {
        this.copyingFolder = "notcopying";
        this.copyFolderExclude = "";
        this.excludedItems = new Set<string>();
    }

    get previewLength() {
        return this.copyPreviewList?.length ?? -1;
    }

    ngOnInit() {
        this.copyFolderPath = this.item.path;
    }

    async copyFolderPreview(path: string, exclude: string) {
        this.copyingFolder = "notcopying";
        const r = await toPromise(
            this.http.post<{preview: PreviewList; dest_exists: boolean}>(
                `/copy/${this.item.id}/preview`,
                {
                    destination: path,
                    exclude: exclude,
                }
            )
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
        const excludingRe = this.makeRegularExpressionFromSet(
            this.excludedItems,
            exclude
        );
        const r = await toPromise(
            this.http.post<{new_folder?: IFolder; errors: string[]}>(
                `/copy/${this.item.id}`,
                {
                    destination: path,
                    exclude: excludingRe,
                    copy_options: this.copyOptions,
                }
            )
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
        this.excludedItems.clear();
    }

    toggleItem(item: {from: string; to: string}) {
        const path = item.from;
        if (this.excludedItems.has(path)) {
            this.excludedItems.delete(path);
        } else {
            this.excludedItems.add(path);
            // Any items that are children of this item are also excluded
            this.copyPreviewList?.forEach((prevItem) => {
                if (prevItem.from.startsWith(path)) {
                    this.excludedItems.add(prevItem.from);
                }
            });
        }
    }

    isSelected(item: {from: string; to: string}) {
        return this.excludedItems.has(item.from);
    }

    allSelected() {
        return this.excludedItems.size === this.copyPreviewList?.length;
    }

    toggleAll(event: Event) {
        const checked = (event.target as HTMLInputElement).checked;
        if (checked) {
            this.copyPreviewList?.forEach((item) => {
                this.excludedItems.add(item.from);
            });
        } else {
            this.excludedItems.clear();
        }
    }

    makeRegularExpressionFromSet(names: Set<string>, expression: string) {
        const escapedCharacters = /[\-\[\]\/{}.()*+?\\^$|]/g;
        for (const item of names) {
            const escapedItem = item.replace(escapedCharacters, "\\$&");
            expression = expression + "|\\b" + escapedItem + "\\b";
        }
        return expression.startsWith("|")
            ? expression.substring(1)
            : expression;
    }
}
