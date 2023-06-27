import type {OnInit} from "@angular/core";
import {Component} from "@angular/core";

@Component({
    selector: "tim-quantum-toolbox",
    template: `
    <div class="toolbox">
        <a href="https://tim.jyu.fi/view/tim/TIMin-kehitys/pluginien-suunnittelu/kvantti"
           target="_blank">
            <span class="glyphicon glyphicon-question-sign help-icon" title="Instructions"></span>
        </a>
        
    </div>
  `,
    styleUrls: ["./quantum-toolbox.component.scss"],
})
export class QuantumToolboxComponent implements OnInit {
    constructor() {}

    ngOnInit(): void {}
}
