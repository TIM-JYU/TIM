/**
 * Controller and HTML template for search results dialog.
 */

import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule} from "@angular/core";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {BrowserModule} from "@angular/platform-browser";
import {FormsModule} from "@angular/forms";
import {ITag, TagType} from "../item/IItem";
import {
    IDocSearchResult,
    ITagSearchResult,
    SearchBoxComponent,
} from "./search-box.component";

export interface ISearchResultDisplay {
    result: IDocSearchResult;
    closed: boolean; // Whether this is shown collapsed or not.
    tags: ITag[];
    num_tag_results: number; // Same tag may contain the search word more than once.
}

enum SortOption {
    Relevance,
    Title,
    Matches,
    Path,
}

@Component({
    selector: "tim-search-results-dialog",
    template: `
        <tim-dialog-frame class="search-result-dialog">
            <ng-container header>
                {{ getTitle() }}
            </ng-container>
            <ng-container body>
                <div *ngIf="errorMessage" class="alert alert-warning">
                    <span class="glyphicon glyphicon-exclamation-sign"></span> {{errorMessage}}
                </div>
                <div *ngIf="displayResults.length == 0 && !errorMessage">
                    <h5>Your search <i>{{searchWord}}</i> did not match any documents in <i>{{folder}}</i></h5>
                </div>
                <div *ngIf="displayResults.length > 0">
                    <h5>Your search <i>{{searchWord}}</i> was found {{totalResults}}
                        <ng-container
                                [ngPlural]="totalResults">
                            <ng-template ngPluralCase="=1">time</ng-template>
                            <ng-template ngPluralCase="other">times</ng-template>
                        </ng-container>
                        from {{displayResults.length}}
                        <ng-container
                                [ngPlural]="displayResults.length">
                            <ng-template ngPluralCase="=1">document</ng-template>
                            <ng-template ngPluralCase="other">documents</ng-template>
                        </ng-container>
                        in <i *ngIf="folder">{{folder}}</i>
                        <i *ngIf="!folder">root</i>&ngsp;
                        <a *ngIf="collapsibles && !limitedDisplay" title="Toggle results collapse"
                           (click)="toggleCollapseAll()">
                            <i class="glyphicon"
                               [ngClass]="allClosed ? 'glyphicon-plus-sign' : 'glyphicon-minus-sign'"></i>
                        </a>
                    </h5>
                    <ul class="list-unstyled">
                        <li *ngFor="let r of displayResults">
                            <a class="cursor-pointer" (click)="r.closed = !r.closed"
                               *ngIf="collapsibles && !limitedDisplay && (r.result.num_par_results + r.num_tag_results) > 0">
                                <i class="glyphicon" [ngClass]="r.closed ? 'glyphicon-plus' : 'glyphicon-minus'"
                                   title="Toggle preview" style="width:1.3em;"></i></a>
                            <span title="Note: hidden elements can affect the result count">
                <a href="/view/{{r.result.doc.path}}"
                   title="Open {{r.result.doc.title}}"> {{r.result.doc.title}}</a> {{r.result.doc.path}}
                                <i> ({{r.result.num_par_results + r.result.num_title_results + r.num_tag_results}} <span
                                        *ngIf="r.result.incomplete">or more matches</span>
                <ng-container *ngIf="!r.result.incomplete"
                              [ngPlural]="r.result.num_par_results + r.result.num_title_results + r.num_tag_results">
                            <ng-template ngPluralCase="=1">match</ng-template>
                            <ng-template ngPluralCase="other">matches</ng-template>
                </ng-container>)
                                </i>
                </span>
                            <ul *ngIf="!r.closed">
                                <ng-container *ngFor="let p of r.result.par_results">
                                    <li *ngIf="p.preview">
                                        <a href="/view/{{r.result.doc.path}}#{{p.par_id}}"
                                           title="Open paragraph">{{p.preview}}</a>
                                        <ng-container *ngFor="let tag of r.tags">
                                            <span *ngIf="!r.closed"
                                                  class="btn-xs"
                                                  [ngClass]="tagClass(tag)">
                                                {{tag.name}}
                                            </span>
                                        </ng-container>
                                    </li>
                                </ng-container>
                            </ul>
                        </li>
                    </ul>
                </div>
            </ng-container>
            <ng-container footer>
                <div class="pull-left" id="order-selector-box">
                    <select id="order-selector"
                            class="form-control"
                            [(ngModel)]="orderByOption"
                            (ngModelChange)="resortResults()"
                            title="Select the result sorting order"
                            name="order-selector">
                        <option *ngFor="let s of sortingOptions" [ngValue]="s">{{s.name}}</option>
                    </select>
                </div>
                <button class="timButton" (click)="dismiss()">Close</button>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class SearchResultsDialogComponent extends AngularDialogComponent<
    SearchBoxComponent,
    void
> {
    protected dialogName = "SearchResults";
    private results: IDocSearchResult[] = [];
    searchWord: string = "";
    displayResults: ISearchResultDisplay[] = [];
    private tagResults: ITagSearchResult[] = [];
    private titleResults: IDocSearchResult[] = [];
    private pathResults: IDocSearchResult[] = [];
    folder: string = "";
    totalResults: number = 0;
    limitedDisplay: boolean = false; // If there's large number of results, don't show previews.
    limitedDisplayThreshold: number = 100000; // More than this could cause memory overload in browser.
    errorMessage: string | undefined;
    allClosed = true;
    collapsibles = false; // True if there are any collapsible results.
    private searchComponent: undefined | SearchBoxComponent;
    sortingOptions = [
        {value: SortOption.Relevance, name: "Sort by relevance"},
        {value: SortOption.Title, name: "Sort by title"},
        {value: SortOption.Matches, name: "Sort by matches"},
        {value: SortOption.Path, name: "Sort by path"},
    ];

    orderByOption = this.sortingOptions[0];

    ngOnInit() {
        this.updateAttributes(this.data);
        if (this.searchComponent) {
            this.searchComponent.registerResultsDialog(this);
        }
    }

    ngOnDestroy() {
        super.ngOnDestroy();
        // Unregister the dialog when closing.
        if (this.searchComponent) {
            this.searchComponent.registerResultsDialog(undefined);
        }
    }

    /**
     * Get all data from the search controller.
     */
    public updateAttributes(ctrl: SearchBoxComponent) {
        this.collapsibles = false;
        this.limitedDisplay = false;
        this.searchComponent = ctrl;
        this.results = ctrl.results;
        this.tagResults = ctrl.tagResults;
        this.titleResults = ctrl.titleResults;
        this.pathResults = ctrl.pathResults;
        this.totalResults =
            ctrl.titleMatchCount +
            ctrl.tagMatchCount +
            ctrl.wordMatchCount +
            ctrl.pathMatchCount;
        this.folder = ctrl.folder!;
        this.errorMessage = ctrl.resultErrorMessage;
        this.searchWord = ctrl.query;
        // If result count is over the threshold, skip paragraph grouping and previews.
        // Without some limit massive results can crash browser.
        if (this.totalResults > this.limitedDisplayThreshold) {
            this.limitedDisplay = true;
        }
        if (
            !this.limitedDisplay &&
            (ctrl.tagMatchCount > 0 || ctrl.wordMatchCount > 0)
        ) {
            this.collapsibles = true;
        }
        this.filterResults();
    }

    public getTitle() {
        return "Search results";
    }

    /**
     * Merge different types of results as display results with open/close state variable.
     */
    private filterResults() {
        this.displayResults = [];
        // Combine title-path results.
        const titleAndPathResults: IDocSearchResult[] = [];
        for (const p of this.pathResults) {
            let pathResultsFound = false;
            for (const t of this.titleResults) {
                // If path and title result are from same doc, combine.
                if (t.doc.id === p.doc.id) {
                    const temp = t;
                    temp.num_title_results =
                        t.num_title_results + p.num_title_results;
                    titleAndPathResults.push(temp);
                    pathResultsFound = true;
                    break;
                }
            }
            // If path result didn't overlap with any title result, add.
            if (!pathResultsFound) {
                titleAndPathResults.push(p);
            }
        }
        // Add missing title results to combined title-path results.
        for (const t of this.titleResults) {
            let titleResultsFound = false;
            for (const tp of titleAndPathResults) {
                // If title result is already in combined list, go to next one.
                if (t.doc.id === tp.doc.id) {
                    titleResultsFound = true;
                    break;
                }
            }
            // If title result didn't overlap with any combined title and path result, add.
            if (!titleResultsFound) {
                titleAndPathResults.push(t);
            }
        }

        // Combine title and tag results with existing content results.
        for (const r of this.results) {
            const newDisplayResult: ISearchResultDisplay = {
                closed: true,
                num_tag_results: 0,
                result: r,
                tags: [],
            };
            // Add tags to existing word search result objects.
            for (const t of this.tagResults) {
                if (t.doc.id === r.doc.id) {
                    newDisplayResult.tags = t.matching_tags;
                    newDisplayResult.num_tag_results = t.num_results;
                }
            }
            // Add titlematches to existing word search result objects.
            for (const t of titleAndPathResults) {
                if (t.doc.id === r.doc.id) {
                    newDisplayResult.result.title_results.push(
                        ...t.title_results
                    );
                    newDisplayResult.result.num_title_results =
                        t.num_title_results;
                }
            }
            this.displayResults.push(newDisplayResult);
        }

        for (const t of titleAndPathResults) {
            let found = false;
            for (const r of this.displayResults) {
                if (t.doc.id === r.result.doc.id) {
                    found = true;
                }
            }
            // Add documents found only with the title and/or path to results list.
            if (!found) {
                const newDocResult = {
                    closed: true,
                    num_path_results: 0,
                    num_tag_results: 0,
                    path_results: [],
                    result: {
                        doc: t.doc,
                        incomplete: false,
                        num_par_results: 0,
                        num_title_results: t.num_title_results,
                        par_results: [],
                        title_results: t.title_results,
                    },
                    tags: [],
                };
                this.displayResults.push(newDocResult);
            }
        }
        for (const t of this.tagResults) {
            let found = false;
            for (const r of this.displayResults) {
                if (t.doc.path === r.result.doc.path) {
                    // Add tag results to existing content-title-path results.
                    r.tags = t.matching_tags;
                    r.num_tag_results = t.num_results;
                    found = true;
                }
            }
            // Add documents found only with the tag to results list.
            if (!found) {
                const newDocResult = {
                    closed: true,
                    num_path_results: 0,
                    num_tag_results: t.num_results,
                    path_results: [],
                    result: {
                        doc: t.doc,
                        incomplete: false,
                        num_par_results: 0,
                        num_title_results: 0,
                        par_results: [],
                        title_results: [],
                    },
                    tags: t.matching_tags,
                };
                this.displayResults.push(newDocResult);
            }
        }
        this.resortResults();
    }

    resortResults() {
        this.displayResults.sort((a, b) => this.resultSortFn(a, b));
    }

    /**
     * Changes tag css class depending on whether it's regular or special tag.
     * @param {ITag} tag The tag to display.
     * @returns {string} The class as a string.
     */
    tagClass(tag: ITag) {
        let style = "";
        if (tag.type === TagType.Regular) {
            style += "btn-primary";
        } else {
            style += "btn-success";
        }
        return style;
    }

    /**
     * Set all document content views to either collapsed or closed state.
     */
    toggleCollapseAll() {
        this.allClosed = !this.allClosed;
        for (const r of this.displayResults) {
            r.closed = this.allClosed;
        }
    }

    /**
     * Determines how to order the results.
     */
    resultSortFn(a: ISearchResultDisplay, b: ISearchResultDisplay) {
        const orderByOption = this.orderByOption;
        switch (orderByOption.value) {
            case SortOption.Title:
                return a.result.doc.title.localeCompare(b.result.doc.title);
            case SortOption.Matches:
                const getMatches = (r: ISearchResultDisplay) => {
                    // Negative values to get ascending order.
                    let matches = -(
                        r.result.num_par_results +
                        r.num_tag_results +
                        r.result.num_title_results
                    );
                    // Show "x or more matches" before "x matches".
                    if (r.result.incomplete) {
                        matches -= 1;
                    }
                    return matches;
                };
                return getMatches(a) - getMatches(b);
            case SortOption.Relevance:
                const getRelevance = (r: ISearchResultDisplay) => {
                    let matches = -(
                        r.result.num_par_results +
                        r.num_tag_results +
                        r.result.num_title_results
                    );
                    // Show "x or more matches" before "x matches".
                    if (r.result.incomplete) {
                        matches -= 1;
                    }
                    const relevance = r.result.doc.relevance;
                    let relevanceValue = 10; // TODO: Set a global constant for default relevance.
                    if (relevance) {
                        relevanceValue = relevance.relevance;
                    }
                    return matches - relevanceValue;
                };
                return getRelevance(a) - getRelevance(b);
            case SortOption.Path:
                return a.result.doc.path.localeCompare(b.result.doc.path);
        }
    }
}

@NgModule({
    declarations: [SearchResultsDialogComponent],
    imports: [BrowserModule, FormsModule, TimUtilityModule, DialogModule],
})
export class SearchResultsDialogModule {}
