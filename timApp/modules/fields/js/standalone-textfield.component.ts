/**
 * Defines the client-side implementation of textfield/label plugin.
 */
import * as t from "io-ts";
import type {
    ApplicationRef,
    DoBootstrap,
    OnInit,
    SimpleChanges,
    OnChanges,
} from "@angular/core";
import {
    Component,
    Output,
    Input,
    NgModule,
    NgZone,
    EventEmitter,
} from "@angular/core";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {TooltipModule} from "ngx-bootstrap/tooltip";
import type {PluginJson} from "tim/plugin/angular-plugin-base.directive";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {PurifyModule} from "tim/util/purify.module";
import {registerPlugin} from "tim/plugin/pluginRegistry";
import {CommonModule} from "@angular/common";
import {nullable} from "tim/plugin/attributes";

export const FieldContent = t.union([t.string, t.number, t.null]);
export const FieldBasicData = t.type({
    c: FieldContent,
});
export const FieldDataWithStyles = t.intersection([
    FieldBasicData,
    t.partial({
        styles: nullable(t.record(t.string, t.string)),
    }),
]);

export type InputType = "TEXTAREA" | "TEXT";

@Component({
    selector: "tim-standalone-textfield",
    standalone: true,
    styleUrls: ["standalone-textfield.component.scss"],
    imports: [FormsModule, TooltipModule, CommonModule],
    template: `
        <div>
    <span class="textfield">
        <input type="text"
               *ngIf="inputType == 'TEXT'"
               class="form-control textarea"
               [name]="name"
               [(ngModel)]="initialValue"
               (ngModelChange)="updateInput()"
               [ngModelOptions]="{standalone: true}"
               [tooltip]="errormessage"
               [placeholder]="placeholder"
               [class.warnFrame]="inputWarn">
       <textarea
               *ngIf="inputType == 'TEXTAREA'"
               class="form-control textarea"
               [name]="name"
               [(ngModel)]="initialValue"
               [ngModelOptions]="{standalone: true}"
               (ngModelChange)="updateInput()"
               [tooltip]="errormessage"
               [placeholder]="placeholder"
               [class.warnFrame]="inputWarn">
               </textarea>
         </span>
        </div>
    `,
})
export class StandaloneTextfieldComponent
    implements OnInit, OnChanges, PluginJson
{
    @Input() inputType: InputType = "TEXT";
    @Input() initialValue: string = "Your description.";
    @Input() placeholder: string = "Words are powerful.";
    @Input() name: string = "";
    @Input() inputWarn: boolean | null = false;
    @Output() valueChange = new EventEmitter<string>();
    isRunning = false;
    userword = "";
    errormessage?: string;
    styles: Record<string, string> = {};
    saveFailed = false;

    constructor(private http: HttpClient, private zone: NgZone) {
        this.json = "{}";
    }

    ngOnInit() {}

    ngOnChanges(change: SimpleChanges) {
        console.log(change);
    }

    /**
     * Returns (user) content in string form.
     */
    getContent(): string {
        return this.userword;
    }

    updateInput() {
        this.valueChange.emit(this.initialValue);
        console.log(this.inputWarn);
        if (!this.inputWarn) {
            this.inputWarn = true;
            // this.hideSavedText = true;
        }
    }

    json: string;
}

@NgModule({
    imports: [
        CommonModule,
        HttpClientModule,
        TimUtilityModule,
        FormsModule,
        PurifyModule,
        TooltipModule.forRoot(),
        StandaloneTextfieldComponent,
    ],
    exports: [StandaloneTextfieldComponent],
})
export class StandaloneTextfieldModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

registerPlugin(
    "standalone-textfield",
    StandaloneTextfieldModule,
    StandaloneTextfieldComponent
);
