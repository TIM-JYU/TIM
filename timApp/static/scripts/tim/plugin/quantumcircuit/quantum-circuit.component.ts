import * as t from "io-ts";
import type {ApplicationRef, DoBootstrap, OnInit} from "@angular/core";
import {Component, NgModule} from "@angular/core";
import {HttpClientModule} from "@angular/common/http";
import {CommonModule} from "@angular/common";
import {registerPlugin} from "tim/plugin/pluginRegistry";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import {GenericPluginMarkup, getTopLevelFields} from "tim/plugin/attributes";

// All settings that are defined in the plugin markup YAML
const QuantumCircuitMarkup = t.intersection([
    t.partial({}),
    GenericPluginMarkup,
]);

// All data that plugin receives from the server (Markup + any extra state data)
const QuantumCircuitFields = t.intersection([
    getTopLevelFields(QuantumCircuitMarkup),
    t.type({}),
]);

@Component({
    selector: "tim-quantum-circuit",
    template: `
        <p>
            quantum-circuit works!
        </p>
    `,
    styleUrls: ["./quantum-circuit.component.scss"],
})
export class QuantumCircuitComponent
    extends AngularPluginBase<
        t.TypeOf<typeof QuantumCircuitMarkup>,
        t.TypeOf<typeof QuantumCircuitFields>,
        typeof QuantumCircuitFields
    >
    implements OnInit
{
    ngOnInit(): void {
        super.ngOnInit();
    }

    getAttributeType() {
        return QuantumCircuitFields;
    }

    getDefaultMarkup() {
        return {};
    }
}

@NgModule({
    declarations: [QuantumCircuitComponent],
    exports: [QuantumCircuitComponent],
    imports: [CommonModule, HttpClientModule],
})
export class QuantumCircuitModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

registerPlugin(
    "tim-quantum-circuit",
    QuantumCircuitModule,
    QuantumCircuitComponent
);
