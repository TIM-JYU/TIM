import {Component, Input, OnInit, ElementRef} from "@angular/core";
import {Changes} from "../../util/angularchanges";

@Component({
    selector: "tim-variables",
    template: `
<div class="variablesDiv" id="variablesDiv">
    
</div>        
    `,
    styleUrls: ["./variables.component.scss"],
})
export class VariablesComponent implements OnInit {
    error?: string;
    svg?: string;
    @Input() usercode!: string;
    @Input() height?: string | number;
    @Input() jsparams?: unknown;
    varfunctions?: unknown;

    constructor(private elementRef: ElementRef) {}

    async ngOnInit() {
        await this.loadVariables();
    }

    async loadVariables() {
        if (this.varfunctions) {
            return this.varfunctions;
        }
        // https://coryrylan.com/blog/javascript-module-pattern-basics
        this.varfunctions = await import(
            "../../../../../modules/cs/static/dfa/vars.js"
        );
    }

    async ngOnChanges(changedObject: Changes<this, "usercode">) {
        await this.loadVariables();
        const root = this.elementRef.nativeElement;
        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
        const variablesDiv = root.querySelector("#variablesDiv");
        let params: unknown = {variablesDiv: variablesDiv};
        if (this.jsparams) {
            params = this.jsparams;
            // eslint-disable-next-line @typescript-eslint/ban-ts-comment
            // @ts-ignore
            params.variablesDiv = variablesDiv;
        }
        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
        if (this.height) variablesDiv.style.height = this.height + "px";
        // eslint-disable-next-line @typescript-eslint/ban-ts-comment
        // @ts-ignore
        const setData = this.varfunctions.setData;
        const data = {
            code: changedObject.usercode!.currentValue,
            params: params,
            args: "",
        };
        setData(data);
    }
}
