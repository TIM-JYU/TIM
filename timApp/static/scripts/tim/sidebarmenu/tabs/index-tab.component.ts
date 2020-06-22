import {Component, Input, OnInit} from "@angular/core";
import {TabEntry} from "tim/sidebarmenu/menu-tab.directive";
import {HeaderIndexerService, IHeaderDisplayIndexItem} from "tim/sidebarmenu/services/header-indexer.service";
import {getViewRangeWithHeaderId, partitionDocument} from "tim/document/viewRangeInfo";
import {vctrlInstance} from "tim/document/viewctrlinstance";

@Component({
    selector: "index-tab",
    template: `
        <ng-template i18n="@@indexTabTitle">Document index</ng-template>
        <h5 i18n>Index <a href="#" i18n-title title="Go to top" class="pull-right">Go to top</a></h5>
        <ul class="subexp">
            <li *ngFor="let header of displayIndex" [class.no-sub-headings]="!hasSubHeadings(header)">
                <a class="exptoggle" *ngIf="hasSubHeadings(header)">
                    <i (click)="toggleClosed(header)"
                       class="glyphicon"
                       [class.glyphicon-plus]="header.closed"
                       [class.glyphicon-minus]="!header.closed"></i>
                </a>
                <a class="a{{header.h1.level}}"
                   href="{{pageUrl}}#{{header.h1.id}}"
                   target="_self"
                   (click)="headerClicked($event, header.h1.id)">
                    {{header.h1.text}}
                </a>
                <ul class="list-unstyled" *ngIf="!header.closed" (click)="$event.stopPropagation()">
                    <li *ngFor="let header2 of header.h2List">
                        <a class="a{{header2.level}}"
                           href="{{pageUrl}}#{{header2.id}}"
                           target="_self"
                           (click)="headerClicked($event, header2.id)">
                            {{header2.text}}
                        </a>
                    </li>
                </ul>
            </li>
        </ul>
    `,
    styleUrls: ["./index-tab.component.scss"],
})
export class IndexTabComponent implements OnInit {
    @Input() entry!: TabEntry;
    pageUrl = `${document.location.origin}${document.location.pathname}${document.location.search}`;
    isFullRange = true;

    constructor(private headerIndexer: HeaderIndexerService) {
    }

    ngOnInit(): void {
    }

    get displayIndex() {
        return this.headerIndexer.headers;
    }

    toggleClosed(header: IHeaderDisplayIndexItem) {
        header.closed = !header.closed;
    }

    hasSubHeadings(header: IHeaderDisplayIndexItem) {
        return header.h2List.length > 0;
    }

    /**
     * Handles clicking index header links. If view range is set, load corresponding part.
     * If partitioning is not in use or the header is in the current part, jump to its
     * location normally.
     * @param evt Event.
     * @param headerId Header id (HTML) from the link.
     */
    async headerClicked(evt: Event, headerId: string) {
        const isInCurrentPart = document.getElementById(headerId);
        if (!isInCurrentPart && !this.isFullRange && vctrlInstance) {
            const headerRange = await getViewRangeWithHeaderId(vctrlInstance.docId, headerId);
            if (headerRange) {
                partitionDocument(headerRange.b, headerRange.e, true);
            }
        }
        evt.stopPropagation();
    }

}
