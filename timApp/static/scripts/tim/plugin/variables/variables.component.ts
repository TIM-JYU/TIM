import type {AfterViewInit, OnInit} from "@angular/core";
import {Component, ElementRef, Input, ViewChild} from "@angular/core";
import DOMPurify from "dompurify";
import type {Changes} from "tim/util/angularchanges";
import {TimDefer} from "tim/util/timdefer";
import type setData from "../../../../../modules/cs/static/dfa/vars.js";

@Component({
    selector: "tim-variables",
    template: `
<div class="variablesDiv" #variablesDiv>

</div>
    `,
    styleUrls: ["./variables.component.scss"],
})
export class VariablesComponent implements OnInit, AfterViewInit {
    error?: string;
    svg?: string;
    viewInitReady = new TimDefer<void>();
    @Input() code!: string;
    @Input() height?: string | number;
    @Input() jsparams?: Record<string, unknown>;
    varfunctions?: typeof setData;
    @ViewChild("variablesDiv") private variablesDiv!: ElementRef<HTMLElement>;

    constructor() {
        //  private cdr: ChangeDetectorRef // private elementRef: ElementRef,
        // this.cdr.detach(); //
    }

    async ngOnInit() {
        await this.loadVariables();
    }

    ngAfterViewInit(): void {
        this.viewInitReady.resolve();
    }

    async loadVariables() {
        if (this.varfunctions) {
            return this.varfunctions;
        }
        // https://coryrylan.com/blog/javascript-module-pattern-basics
        this.varfunctions = (
            await import("../../../../../modules/cs/static/dfa/vars.js")
        ).default;
    }

    setSVG(svg: string, svgdiv: HTMLElement, _height: number) {
        svgdiv.innerHTML = DOMPurify.sanitize(svg, {ADD_TAGS: ["use"]});
        // svgdiv.innerHTML = svg;
    }

    async ngOnChanges(changedObject: Changes<this, "code">) {
        // if (!this.variablesDiv) return;
        await this.loadVariables();
        await this.viewInitReady.promise;
        // noinspection JSUnusedGlobalSymbols
        let params = {
            variablesDiv: this.variablesDiv.nativeElement,
            setSVGCallback: (
                svg: string,
                svgDiv: HTMLElement,
                height: number
            ) => this.setSVG(svg, svgDiv, height),
            params: this.jsparams,
        };

        if (this.jsparams) {
            params = {...params, ...this.jsparams};
        }

        if (this.height) {
            this.variablesDiv.nativeElement.style.height = this.height + "px";
        }
        const setData = this.varfunctions!;
        const data = {
            code: changedObject.code!.currentValue,
            params: params,
            args: "",
        };
        setData(data);
    }
}
