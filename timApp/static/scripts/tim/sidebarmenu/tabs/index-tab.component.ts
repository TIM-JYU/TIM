import type {OnInit} from "@angular/core";
import {Component, Input} from "@angular/core";
import {TabEntry} from "tim/sidebarmenu/menu-tab.directive";
import type {
    IHeader,
    IHeaderDisplayIndexItem,
} from "tim/sidebarmenu/services/header-indexer.service";
import {HeaderIndexerService} from "tim/sidebarmenu/services/header-indexer.service";
import {
    getViewRangeWithHeaderId,
    partitionDocument,
} from "tim/document/viewRangeInfo";
import {vctrlInstance} from "tim/document/viewctrlinstance";

@Component({
    selector: "index-tab",
    template: `
        <ng-template i18n>Document index</ng-template>
        <h5 i18n>Index <a (click)="goToTop()" i18n-title title="Go to top" class="pull-right">Go to top</a></h5>
        <ul (window:scrollend)="updateActive()" class="subexp">
            <li *ngFor="let header of displayIndex" [class.no-sub-headings]="!hasSubHeadings(header)">
                <a class="exptoggle" *ngIf="hasSubHeadings(header)">
                    <i (click)="toggleClosed(header)"
                       class="glyphicon"
                       [class.glyphicon-plus]="header.closed"
                       [class.glyphicon-minus]="!header.closed"></i>
                </a>
                <a [ngClass]="getHeaderClassList(header.h1)"
                   [href]="getHeaderHref(header.h1)"
                   target="_self"
                   (click)="headerClicked($event, header.h1.id)">
                    {{header.h1.text}}
                </a>
                <ul class="list-unstyled" *ngIf="!header.closed" (click)="$event.stopPropagation()">
                    <li *ngFor="let header2 of header.h2List">
                        <a [ngClass]="getHeaderClassList(header2)"
                           [href]="getHeaderHref(header2)"
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
    activeHeaderId = "";

    constructor(private headerIndexer: HeaderIndexerService) {}

    ngOnInit(): void {}

    get displayIndex() {
        return this.headerIndexer.headers;
    }

    goToTop() {
        // window.scrollTo(window.scrollX, 1);
        window.scrollTo(0, 1); // Must be y:1, otherwise does no react???
    }

    toggleClosed(header: IHeaderDisplayIndexItem) {
        header.closed = !header.closed;
    }

    hasSubHeadings(header: IHeaderDisplayIndexItem) {
        return header.h2List.length > 0;
    }

    /**
     * Set active header.
     */
    updateActive() {
        let urlHash = document.location.hash;
        if (urlHash.length === 0) {
            this.activeHeaderId = "";
            return;
        }
        urlHash = decodeURI(urlHash.substring(1));

        const knownHeaders = new Set();
        for (const header of this.displayIndex) {
            knownHeaders.add(header.h1.id);
            for (const header2 of header.h2List) {
                knownHeaders.add(header2.id);
            }
        }

        if (knownHeaders.has(urlHash)) {
            this.activeHeaderId = urlHash;
            return;
        }
        // go backwards from current paragraph to find paragraph that is in navbar
        const pars = document.getElementsByClassName("par");
        for (let i = 0; i < pars.length; i++) {
            if (pars[i].id === urlHash) {
                for (let j = i; j >= 0; j--) {
                    for (const h of pars[j].querySelectorAll("h1, h2, h3")) {
                        if (knownHeaders.has(h.id)) {
                            this.activeHeaderId = h.id;
                            return;
                        }
                    }
                }
            }
        }

        this.activeHeaderId = "";
    }

    /**
     * Handles clicking index header links. If view range is set, load corresponding part.
     * If partitioning is not in use or the header is in the current part, jump to its
     * location normally.
     * @param evt Event.
     * @param headerId Header id (HTML) from the link.
     */
    async headerClicked(evt: Event, headerId: string) {
        this.activeHeaderId = headerId;

        const isInCurrentPart = document.getElementById(headerId);
        if (!isInCurrentPart && !this.isFullRange && vctrlInstance) {
            const headerRange = await getViewRangeWithHeaderId(
                vctrlInstance.docId,
                headerId
            );
            if (headerRange) {
                partitionDocument(headerRange, true);
            }
        }
        evt.stopPropagation();
    }

    protected readonly getViewRangeWithHeaderId = getViewRangeWithHeaderId;

    getHeaderClassList(h: IHeader) {
        let res = [`a${h.level}`];
        if (h.classList) {
            res = res.concat(h.classList);
        }
        if (h.id === this.activeHeaderId) {
            res.push("chosen-heading");
        }
        return res;
    }

    getHeaderHref(h: IHeader) {
        if (h.href) {
            return h.href;
        } else {
            return `${this.pageUrl}#${h.id ?? ""}`;
        }
    }
}
