/**
 * Defines the client-side implementation of textfield/label plugin.
 */
import * as t from "io-ts";
import {
    ApplicationRef,
    Component,
    Output,
    DoBootstrap,
    Input,
    NgModule,
    NgZone,
    OnInit,
    EventEmitter,
    SimpleChange,
    SimpleChanges,
    OnChanges,
} from "@angular/core";
import {
    GenericPluginMarkup,
    Info,
    nullable,
    withDefault,
} from "../../../static/scripts/tim/plugin/attributes";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {TooltipModule} from "ngx-bootstrap/tooltip";
import {PluginJson} from "../../../static/scripts/tim/plugin/angular-plugin-base.directive";
import {TimUtilityModule} from "../../../static/scripts/tim/ui/tim-utility.module";
import {PurifyModule} from "../../../static/scripts/tim/util/purify.module";
import {registerPlugin} from "../../../static/scripts/tim/plugin/pluginRegistry";
import {CommonModule} from "@angular/common";
import {
    ChangeType,
    FormModeOption,
} from "../../../static/scripts/tim/document/viewctrl";
import {boolean, string} from "fp-ts";
import {re} from "mathjs";

const TextfieldMarkup = t.intersection([
    t.partial({
        tag: nullable(t.string),
        inputplaceholder: nullable(t.string),
        inputstem: nullable(t.string),
        initword: nullable(t.string),
        validinput: nullable(t.string),
        errormessage: nullable(t.string),
        readOnlyStyle: nullable(t.string),
        showname: nullable(t.number),
        autosave: t.boolean,
        nosave: t.boolean,
        ignorestyles: t.boolean,
        clearstyles: t.boolean,
        textarea: t.boolean,
        autogrow: t.boolean,
        downloadButton: t.string,
        downloadButtonFile: t.string,
    }),
    GenericPluginMarkup,
    t.type({
        autoupdate: withDefault(t.number, 500),
        autoUpdateTables: withDefault(t.boolean, true),
        cols: withDefault(t.number, 6),
        rows: withDefault(t.number, 1),
    }),
]);
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
const TextfieldAll = t.intersection([
    t.partial({}),
    t.type({
        info: Info,
        markup: TextfieldMarkup,
        preview: t.boolean,
        state: nullable(FieldDataWithStyles),
    }),
]);
export type TFieldContent = t.TypeOf<typeof FieldContent>;
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

    /**
     * Method to check grading input type for textfield.
     * Used as e.g. grading checker for hyv | hyl | 1 | 2 | 3 | 4 | 5.
     * @param re validinput defined by given attribute.
     */
    validityCheck(re: string) {
        if (this.userword === "") {
            return new RegExp("").test(this.userword);
        }
        const regExpChecker = new RegExp(re);
        return regExpChecker.test(this.userword);
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
