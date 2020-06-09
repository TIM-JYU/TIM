import {Injectable} from "@angular/core";
import {documentglobals, isDocumentGlobals, someglobals} from "tim/util/globals";

export interface IHeader {
    id: string;
    level: number;
    text: string;
}

export interface IHeaderDisplayIndexItem {
    h1: IHeader;
    h2List: IHeader[];
    closed: boolean;
}

export type HeaderIndexItem = [IHeader, IHeader[]];

@Injectable({
    providedIn: "root",
})
export class HeaderIndexerService {
    private headerItems?: IHeaderDisplayIndexItem[];

    constructor() {
    }

    get headers(): IHeaderDisplayIndexItem[] {
        if (!this.headerItems) {
            const globals = someglobals();
            if (isDocumentGlobals(globals)) {
                this.headerItems = this.formDisplayIndex(globals.index);
            } else {
                this.headerItems = [];
            }
        }
        return this.headerItems;
    }

    /**
     * Add closed states to header index.
     * @param index Index containing headers.
     * @returns {HeaderIndexItem[]} Index with added closed states.
     */
    private formDisplayIndex(index: HeaderIndexItem[]) {
        if (!index || !index[0]) {
            return [];
        }

        let closedState = true;
        const headerCount = this.getHeaderCount(index);
        if (index.length == 1 || headerCount < 40) {
            closedState = false;
        }

        const displayIndex: IHeaderDisplayIndexItem[] = [];
        for (const h of index) {
            if (!h[0]) {
                continue;
            }
            const h1: IHeader = h[0];
            let h2List: IHeader[] = [];
            if (h[1]) {
                h2List = h[1];
            }
            displayIndex.push({closed: closedState, h1: h1, h2List: h2List});
        }
        return displayIndex;
    }

    /**
     * Count the total number of headers and subheaders.
     * @param index Index containing the headers.
     * @returns {number} Total count of all headers.
     */
    private getHeaderCount(index: HeaderIndexItem[]) {
        let temp = 0;
        for (const h of index) {
            if (!h[0]) {
                continue;
            }
            temp += 1;
            if (!h[1]) {
                continue;
            }
            temp += h[1].length;
        }
        return temp;
    }
}
