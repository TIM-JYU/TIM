import type {OnInit} from "@angular/core";
import {Component, Input} from "@angular/core";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import {HttpClient} from "@angular/common/http";
import type {IFolder} from "tim/item/IItem";
import {toPromise} from "tim/util/utils";
import {itemglobals} from "tim/util/globals";

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
            <div *ngIf="copyFrom && sourcePath; else sourceNotGiven">
                <p i18n>You can copy all documents and folders from folder </p>
                <pre>{{ sourcePath }}</pre>
                <p i18n>to another folder.</p>
            </div>
            <ng-template #sourceNotGiven>
                <p i18n>You can copy all documents and folders in this folder to another folder.</p>
            </ng-template>
            <p i18n>Copy options</p>
            <div class="cb-group">
                <div class="checkbox">
                    <label><input type="checkbox" name="copy-active-rights"
                                  [(ngModel)]="copyOptions.copy_active_rights"><ng-container i18n> Copy active access rights</ng-container></label>
                </div>
                <div class="checkbox">
                    <label><input type="checkbox" name="copy-expired-rights"
                                  [(ngModel)]="copyOptions.copy_expired_rights"><ng-container i18n> Copy expired access rights</ng-container></label>
                </div>
                <div class="checkbox">
                    <label><input type="checkbox" name="stop-errors" [(ngModel)]="copyOptions.stop_on_errors"><ng-container i18n> Stop
                        copying on errors</ng-container></label>
                </div>
            </div>
            <div class="form-group" timErrorState>
                <label for="destination" class="control-label" i18n>Destination:</label>
                <input name="copyPath" class="form-control" timLocation id="destination" type="text" autocomplete="off"
                       [(ngModel)]="copyFolderPath" (ngModelChange)="copyParamChanged()" #copyPath="ngModel">
                <tim-error-message></tim-error-message>
            </div>
            <p i18n>You can optionally enter a regular expression to exclude specific documents or folders from being copied.</p> 
            <p *ngIf="copyHelp"><ng-container i18n>For more information on copying see the </ng-container><a [href]="copyHelp"><ng-container i18n>help page</ng-container></a>.</p>
            <div class="form-group" timErrorState>
                <label for="exclude" class="control-label" i18n>Exclude documents/folders that match:</label>
                <input name="exclude" class="form-control" id="exclude" type="text" autocomplete="off"
                       [(ngModel)]="copyFolderExclude" (ngModelChange)="copyParamChanged()">
                <tim-error-message></tim-error-message>
            </div>
            <div class="form-group">
                <button (click)="copyFolderPreview(copyFolderPath, copyFolderExclude)" class="timButton"
                        [disabled]="copyFolderPath == currentItem.path || copyPath.invalid"
                        *ngIf="copyingFolder == 'notcopying'" i18n>Copy preview...
                </button>
            </div>
            <div class="panel panel-default" *ngIf="previewLength > 0">
                <div class="panel-heading" i18n>Exclude folder or document items</div>
                    <div class="panel-body">
                        <p i18n>These are the items that will be copied. Select the items you want to exclude from being copied.</p>
                    </div>
                <div class="scrollable-table">
                    <table class="table-responsive">
                        <table class="table table-hover">
                        <thead>
                            <tr>
                                <th><input type="checkbox" [checked]="allSelected()" (change)="toggleAll($event)"></th>
                                <th i18n="Column name for source folders|tableHeader">From</th>
                                <th></th>
                                <th i18n="Column name for target folders|tableHeader">To</th>
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
            </div>
            <p *ngIf="previewLength == 0 || allSelected()" i18n>Nothing would be copied.</p>
            <tim-alert severity="warning" *ngIf="destExists" i18n>
                The destination folder already exists. Make sure this is intended before copying.
            </tim-alert>
            <div>
                <button (click)="copyFolder(copyFolderPath, copyFolderExclude)" class="timButton"
                        *ngIf="copyFolderPath != currentItem.path &&
                         previewLength > 0 &&
                         copyingFolder == 'notcopying' " [disabled]="allSelected()" i18n>Copy
                </button>
            </div>
            <span *ngIf="copyingFolder == 'copying'"><tim-loading></tim-loading><ng-container i18n> Copying, this might take a while...</ng-container></span>
            <span *ngIf="copyingFolder == 'finished'">
                <ng-container i18n>Folder </ng-container>{{ currentItem.name }}<ng-container i18n> copied to </ng-container>
                <a href="/manage/{{ newFolder?.path }}" [innerText]="newFolder?.path"></a>.
            </span>
            <div *ngIf="copyErrors">
                <p i18n>The following errors occurred while copying:</p>
                <ul>
                    <li *ngFor="let e of copyErrors">{{ e }}</li>
                </ul>
            </div>
        </form>
    `,
    styleUrls: ["copy-folder.component.scss"],
})
export class CopyFolderComponent implements OnInit {
    copyingFolder: "notcopying" | "copying" | "finished";
    @Input() copyFrom?: string;
    @Input() copyTo?: string;
    currentItem: {name: string; path: string; id: number};
    copyPreviewList?: PreviewList;
    destExists?: boolean;
    copyFolderPath!: string;
    copyFolderExclude: string;
    newFolder?: IFolder;
    copyOptions: CopyOptions = {...DEFAULT_COPY_OPTIONS};
    copyErrors?: string[];
    sourcePath?: string;
    excludedItems: Set<string>;
    copyHelp: string = COPY_HELP_ADDRESS;

    constructor(private http: HttpClient) {
        this.copyingFolder = "notcopying";
        this.copyFolderExclude = "";
        this.currentItem = itemglobals().curr_item;
        this.excludedItems = new Set<string>();
    }

    get previewLength() {
        return this.copyPreviewList?.length ?? -1;
    }

    ngOnInit() {
        if (this.copyTo) {
            this.copyFolderPath = this.copyTo;
        } else {
            this.copyFolderPath = this.currentItem.path;
        }
        if (this.copyFrom) {
            this.fetchItemBasicInfo(this.copyFrom);
        }
    }

    private async fetchItemBasicInfo(itemPath: string) {
        const r = await toPromise(
            this.http.get<{id: number; location: string; short_name: string}>(
                `/itemInfo/${itemPath}`
            )
        );
        if (!r.ok) {
            return;
        } else {
            this.currentItem = {
                id: r.result.id,
                name: r.result.short_name,
                path: r.result.location,
            };
            this.sourcePath =
                this.currentItem?.path + "/" + this.currentItem?.name;
        }
    }

    async copyFolderPreview(
        path: string,
        exclude: string,
        itemId: number = this.currentItem.id
    ) {
        this.copyingFolder = "notcopying";

        const r = await toPromise(
            this.http.post<{preview: PreviewList; dest_exists: boolean}>(
                `/copy/${itemId}/preview`,
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

    async copyFolder(
        path: string,
        exclude: string,
        itemId: number = this.currentItem.id
    ) {
        this.copyingFolder = "copying";
        const excludingRe = this.makeRegularExpressionFromSet(
            this.excludedItems,
            exclude
        );
        const r = await toPromise(
            this.http.post<{new_folder?: IFolder; errors: string[]}>(
                `/copy/${itemId}`,
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

    private getSubFolders(path: string) {
        const splitPath = path.split("/");
        const subFolders = [];
        let current = "";
        for (let i = 0; i < splitPath.length - 1; i++) {
            current = current ? current + "/" + splitPath[i] : splitPath[i];
            subFolders.push(current);
        }
        // remove the root folder
        subFolders.shift();
        return subFolders;
    }

    toggleItem(item: {from: string; to: string}) {
        const path = item.from;
        if (this.excludedItems.has(path)) {
            this.excludedItems.delete(path);
            const subFolders = this.getSubFolders(path);
            subFolders.forEach((subFolderItem) => {
                this.excludedItems.delete(subFolderItem);
            });
        } else {
            this.excludedItems.add(path);
            // Any items that are children of this item are also excluded
            this.copyPreviewList?.forEach((previewItem) => {
                if (previewItem.from.startsWith(path)) {
                    this.excludedItems.add(previewItem.from);
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
            expression = expression + "|^" + escapedItem + "$";
        }
        return expression.startsWith("|")
            ? expression.substring(1)
            : expression;
    }
}
