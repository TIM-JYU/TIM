import {Component, Input, OnInit} from "@angular/core";
import {Changes} from "../../util/angularchanges";
import {to2} from "../../util/utils";

@Component({
    selector: "tim-graph-viz",
    template: `
<span *ngIf="error" class="vizerror" [innerHTML]="error"></span>
<div *ngIf="svg" [innerHTML]="svg|purify"></div>
    `,
    styleUrls: ["./graph-viz.component.scss"],
})
export class GraphVizComponent implements OnInit {
    error?: string;
    svg?: string;
    @Input() vizcmd!: string;
    @Input() jsparams?: Record<string, unknown>;
    viz?: Viz;

    constructor() {}

    async ngOnInit() {
        // await this.loadViz();
    }

    async loadViz() {
        if (this.viz) {
            return this.viz;
        }
        const Viz = (await import("viz.js")).default;
        const renderjs = await import("viz.js/full.render.js");
        const {Module, render} = renderjs;
        this.viz = new Viz({Module, render});
        return this.viz;
    }

    async ngOnChanges(changedObject: Changes<this, "vizcmd">) {
        const viz = await this.loadViz();
        let params = {format: "svg"};
        if (this.jsparams) {
            params = {...params, ...this.jsparams};
        }

        const result = await to2<string, {message: string}>(
            viz.renderString(changedObject.vizcmd!.currentValue, params)
        );
        if (result.ok) {
            this.svg = result.result;
            this.error = undefined;
        } else {
            this.error = result.result.message;
            this.viz = undefined;
            this.loadViz();
        }
    }
}
