import {Injectable} from "@angular/core";
import {IDocument} from "tim/item/IItem";
import {isDocumentGlobals, someglobals} from "tim/util/globals";

export interface ITaskScoreInfo {
    taskName: string;
    fragId: string;
    points: number;
    maxPoints: number;
}

export interface IDocScoreInfo {
    doc: IDocument;
    total: number;
    maxTotal: number;
    tasks: ITaskScoreInfo[];
}

@Injectable({
    providedIn: "root",
})
export class ScoreboardService {
    hasScore: boolean = false;
    total: number = 0;
    maxTotal: number = 0;
    infos: IDocScoreInfo[] = [];
    currentDocScoreInfo: IDocScoreInfo | null = null;

    constructor() {
        const g = someglobals();
        if (!isDocumentGlobals(g) || !g.score_infos) {
            return;
        }
        this.hasScore = true;

        this.currentDocScoreInfo = g.score_infos.find((s) => s.doc.id == g.curr_item.id) ?? null;
        this.infos = g.score_infos;
        this.total = this.infos.reduce((prev, cur) => prev + cur.total, 0);
        this.maxTotal = this.infos.reduce((prev, cur) => prev + cur.maxTotal, 0);
    }

    get valid(): boolean {
        return this.hasScore && (this.currentDocScoreInfo != null || this.infos?.length > 0);
    }
}
