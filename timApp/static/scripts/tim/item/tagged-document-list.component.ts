/**
 * A component that shows and filters documents based on their tags.
 *
 * Contains a search field with visibility and exact/partial word search.
 */
import {HttpClient} from "@angular/common/http";
import {Component, Input} from "@angular/core";
import {TimStorage, to2} from "tim/util/utils";
import * as t from "io-ts";
import {ITag, ITaggedItem, tagStyleClass} from "./IItem";

@Component({
    selector: "tim-tagged-document-list",
    template: `
        <div class="input-group" *ngIf="enableSearch">
            <input [(ngModel)]="tagFilter" name="filterField" (keydown.enter)="enterPressed()"
                   type="text"
                   title="Search documents by entering a tag"
                   placeholder="Search documents by entering a tag"
                   class="form-control" id="tagFilterField" autocomplete="off"
                   [typeahead]="allUniqueTags"
                   [typeaheadMinLength]="1">
            <span class="input-group-addon btn">
                <span class="glyphicon glyphicon-search"
                      (click)="searchClicked(tagFilter)"
                      title="Search documents with tag '{{tagFilter}}'"></span>
            </span>
        </div>
        <ul *ngIf="docList.length > 0">
            <li *ngFor="let d of docList">
                <a href="/view/{{d.path}}" title="Open {{d.title}}">{{d.title}}</a>
                <span *ngFor="let tag of d.tags" (click)="searchClicked(tag.name)">
                    <span class="btn-xs" [ngClass]="tagClass(tag)"
                          title="{{tagToolTip}}'{{tag.name}}'">{{tag.name}}</span>
                </span>
            </li>
        </ul>
        <span *ngIf="docList.length == 0">No documents found!</span>
    `,
})
export class TaggedDocumentListComponent {
    @Input() tagFilter!: string;
    @Input() exactMatch!: boolean; // Get only documents with exactly matching tag.
    @Input() caseSensitive!: boolean;
    @Input() enableSearch!: boolean;
    @Input() listDocTags!: boolean;
    docList: ITaggedItem[] = [];
    allUniqueTags: string[] = [];
    tagToolTip: string = "";
    private storage = new TimStorage("tagFilter", t.string);

    constructor(private http: HttpClient) {}

    async ngOnInit() {
        if (this.enableSearch) {
            this.tagToolTip = "Search documents with tag ";
        } else {
            this.tagToolTip = "Document has tag ";
        }

        const stored = this.storage.get();
        if (this.enableSearch && stored) {
            this.tagFilter = stored;
        } else {
            this.tagFilter = "";
        }
        await this.getAllTags();
        await this.getDocumentsByTag(this.tagFilter);
    }

    $onDestroy() {
        this.storage.set(this.tagFilter);
    }

    private async getAllTags() {
        const r = await to2(
            this.http.get<string[]>(`/tags/getAllTags`).toPromise()
        );
        if (r.ok) {
            this.allUniqueTags = r.result;
        }
    }

    /*
     * Filter documents by tag.
     * @param tagName Tag word to search with.
     * @param exactMatch Search only documents with the whole tag.
     * @param list_doc_tags Get also tags in each document.
     * If false will also search for partial matches.
     */
    private async getDocumentsByTag(tagName: string) {
        // Changes tag in input field to this in case the tagName is different.
        this.tagFilter = tagName;

        const response = await to2(
            this.http
                .get<ITaggedItem[]>("/tags/getDocs", {
                    params: {
                        case_sensitive: this.caseSensitive.toString(),
                        exact_search: this.exactMatch.toString(),
                        list_doc_tags: this.listDocTags.toString(),
                        name: tagName,
                    },
                })
                .toPromise()
        );
        if (!response.ok) {
            return;
        }
        this.docList = response.result;
    }

    /*
     * Calls tag search function when Enter is pressed.
     * @param event Keyboard event.
     */
    async enterPressed() {
        await this.getDocumentsByTag(this.tagFilter);
    }

    /*
     * Calls tag search function. Has option to searching for exactly matching
     * tags or all that containg parts of the tag.
     */
    async searchClicked(tagName: string) {
        if (!this.enableSearch) {
            return;
        } else {
            await this.getDocumentsByTag(tagName);
        }
    }

    /**
     * Changes tag css class depending on whether search is enabled and
     * if it's regular or special tag.
     * @param {ITag} tag
     * @returns {string}
     */
    tagClass(tag: ITag) {
        let classes = tagStyleClass(tag, false);
        if (this.enableSearch) {
            classes += " cursor-pointer";
        }
        return classes;
    }
}
