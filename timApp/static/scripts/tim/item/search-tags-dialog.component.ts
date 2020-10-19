/**
 * Controller and HTML template for tag search dialog.
 */

import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule} from "@angular/core";
import {ngStorage} from "ngstorage";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {FormsModule} from "@angular/forms";
import {HttpClientModule} from "@angular/common/http";
import {BrowserModule} from "@angular/platform-browser";
import {TaggedDocumentListComponent} from "tim/item/tagged-document-list.component";
import {TypeaheadModule} from "ngx-bootstrap/typeahead";
import {$localStorage} from "../util/ngimport";

@Component({
    selector: "tim-search-tags-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container header>
                {{ getTitle() }}
            </ng-container>
            <ng-container body>
                <h4>{{header}}</h4>
                <tim-tagged-document-list [enableSearch]="enableSearch"
                                          tagFilter=""
                                          [exactMatch]="exactMatch"
                                          [listDocTags]="listDocTags"
                                          [caseSensitive]="caseSensitive">
                </tim-tagged-document-list>
                <div>
                    <label class="font-weight-normal"><input type="checkbox"
                                                             [(ngModel)]="advancedOptions"> Advanced options</label>
                </div>
                <div *ngIf="advancedOptions">
                    <label class="font-weight-normal"><input type="checkbox" [(ngModel)]="exactMatch"> Search whole
                        words</label>&ngsp;
                    <label class="font-weight-normal"><input type="checkbox" [(ngModel)]="caseSensitive"> Case sensitive</label>&ngsp;
                    <label class="font-weight-normal"><input type="checkbox" [(ngModel)]="listDocTags"
                                                             title="List all document tags in search results"> List all
                        tags</label>
                </div>
            </ng-container>
            <ng-container footer>
                <button class="timButton" (click)="dismiss()">Close</button>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class SearchTagsDialogComponent extends AngularDialogComponent<
    void,
    void
> {
    protected dialogName = "SearchTags";
    enableSearch = true;
    header = "";
    advancedOptions = false;
    caseSensitive = false;
    listDocTags = true;
    exactMatch = false;
    private storage: ngStorage.StorageService & {
        searchOptionStorage: null | boolean[];
    };

    constructor() {
        super();
        this.storage = $localStorage.$default({
            searchOptionStorage: null,
        });
    }

    ngOnInit() {
        if (this.enableSearch) {
            this.header = "Input a tag name to search documents";
            if (
                this.storage.searchOptionStorage &&
                this.storage.searchOptionStorage.length === 3
            ) {
                [
                    this.caseSensitive,
                    this.exactMatch,
                    this.listDocTags,
                ] = this.storage.searchOptionStorage;
            }
        }
    }

    ngOnDestroy() {
        this.storage.searchOptionStorage = [
            this.caseSensitive,
            this.exactMatch,
            this.listDocTags,
        ];
    }

    public getTitle() {
        return "Tag search";
    }
}

@NgModule({
    declarations: [SearchTagsDialogComponent, TaggedDocumentListComponent],
    imports: [
        BrowserModule,
        HttpClientModule,
        FormsModule,
        TimUtilityModule,
        DialogModule,
        TypeaheadModule.forRoot(),
    ],
})
export class TagSearchDialogModule {}
