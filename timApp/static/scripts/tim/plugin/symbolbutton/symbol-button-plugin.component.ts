import * as t from "io-ts";
import {
    GenericPluginMarkup,
    getTopLevelFields,
    nullable,
    withDefault,
} from "tim/plugin/attributes";
import type {
    AfterViewInit,
    ApplicationRef,
    DoBootstrap,
    OnInit,
} from "@angular/core";
import {Component, NgModule, ViewChild, ViewContainerRef} from "@angular/core";
import {HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import {PurifyModule} from "tim/util/purify.module";
import {CommonModule} from "@angular/common";
import {registerPlugin} from "tim/plugin/pluginRegistry";
import {ace} from "tim/editor/ace";
import type {Ace} from "ace-builds/src-noconflict/ace";
import {EditorModule} from "../../../../../modules/cs/js/editor/module";
import type {ITemplateButton} from "../../../../../modules/cs/js/csPlugin";
import {
    createTemplateButtons,
    TemplateButton,
} from "../../../../../modules/cs/js/csPlugin";
import type {SymbolButtonMenuComponent} from "../../../../../modules/cs/js/editor/math-editor/symbol-button-menu.component";
import type {FormulaEvent} from "../../../../../modules/cs/js/editor/math-editor/symbol-button-menu.component";
import {CURSOR} from "../../../../../modules/cs/js/editor/editor";

type IAceEditor = Ace.Editor;

const SymbolButtonMarkupFields = t.intersection([
    t.partial({
        buttons: t.string,
        mdButtons: nullable(t.array(TemplateButton)),
        mini: t.boolean,
        float: t.boolean,
    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
        autoupdate: withDefault(t.number, 500),
        cols: withDefault(t.number, 20),
    }),
]);
const SymbolButtonFields = t.intersection([
    getTopLevelFields(SymbolButtonMarkupFields),
    t.type({}),
]);

@Component({
    selector: "symbolbutton-runner",
    template: `
        <div class="symbol-button-plugin-filler" *ngIf="isFloat()"></div>
        <div [class.symbol-button-plugin-float]="isFloat()">
            <ng-container #symbolButtonMenu></ng-container>
        </div>
    `,
    styleUrls: ["./symbol-button-plugin.component.scss"],
})
export class SymbolButtonPluginComponent
    extends AngularPluginBase<
        t.TypeOf<typeof SymbolButtonMarkupFields>,
        t.TypeOf<typeof SymbolButtonFields>,
        typeof SymbolButtonFields
    >
    implements OnInit, AfterViewInit
{
    templateButtons: ITemplateButton[] = [];
    requiresTaskId = false;
    float = false;
    @ViewChild("symbolButtonMenu", {read: ViewContainerRef, static: true})
    symbolButtonMenuContainer!: ViewContainerRef;

    symbolButtonMenuComponent!: SymbolButtonMenuComponent;

    ngOnInit() {
        super.ngOnInit();
        const b = this.markup.buttons;
        const mdButtons = this.markup.mdButtons;
        this.templateButtons = createTemplateButtons(b, mdButtons);
        this.float = this.markup.float ?? false;
    }

    /**
     * Loads component.
     */
    ngAfterViewInit() {
        void this.loadComponent();
    }

    async loadComponent() {
        const component = (
            await import(
                "../../../../../modules/cs/js/editor/math-editor/symbol-button-menu.component"
            )
        ).SymbolButtonMenuComponent;
        const comp = this.symbolButtonMenuContainer.createComponent(component);
        this.symbolButtonMenuComponent = comp.instance;
        comp.instance.mini = true;
        comp.instance.templateButtons = this.templateButtons;
        comp.instance.setFormula.subscribe((e) => this.addFormula(e));
    }

    addFormula(formulaInput: FormulaEvent) {
        const txtarea = this.vctrl.lastActiveInput;
        if (!txtarea) {
            return;
        }
        const isAce = txtarea.classList.contains("ace_text-input");
        if (isAce) {
            const parent = txtarea.parentElement;
            if (parent) {
                const acee = ace.edit(parent);
                this.aceInsert(formulaInput.text, acee);
                return;
            }
        }
        const endPos = txtarea.selectionEnd ?? 0;
        const strPos = txtarea.selectionStart ?? 0;
        let input = formulaInput.text;
        const currentString = txtarea.value;

        let back = 0;
        const ci = input.indexOf(CURSOR);
        if (ci >= 0) {
            input = input.replace(CURSOR, "");
            back = input.length - ci;
        }

        const cont = currentString;
        const ret = cont.slice(0, strPos) + input + cont.slice(endPos);

        const newPos = strPos + input.length - back;
        if (
            (txtarea.selectionStart && txtarea.selectionStart >= 0) ||
            back >= 0
        ) {
            function setpos() {
                if (!txtarea) {
                    return;
                }
                txtarea.selectionStart = newPos;
                txtarea.selectionEnd = newPos;
            }

            setTimeout(() => setpos());
        }
        txtarea.value = ret;
        txtarea.dispatchEvent(new Event("input", {bubbles: true}));
    }

    aceInsert(str: string, aceEditor: IAceEditor, strPos?: number): void {
        if (!aceEditor) {
            return;
        }
        const sess = aceEditor.getSession();
        let cursor;
        let back = -1;
        const ci = str.indexOf(CURSOR); // check if there is a cursor marker
        if (ci >= 0) {
            str = str.replace(CURSOR, "");
            back = str.length - ci;
        }
        if (strPos) {
            cursor = sess.getDocument().indexToPosition(strPos, 0);
            sess.insert(cursor, str);
        } else {
            sess.replace(sess.selection.getRange(), str);
            if (back > 0) {
                aceEditor.navigateLeft(back);
            }
        }
    }

    isFloat() {
        return this.float && !this.isPreview();
    }

    getAttributeType() {
        return SymbolButtonFields;
    }

    getDefaultMarkup() {
        return {};
    }
}

@NgModule({
    declarations: [SymbolButtonPluginComponent],
    imports: [
        CommonModule,
        HttpClientModule,
        FormsModule,
        TimUtilityModule,
        PurifyModule,
        EditorModule,
    ],
})
export class SymbolButtonPluginModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

registerPlugin(
    "symbolbutton-runner",
    SymbolButtonPluginModule,
    SymbolButtonPluginComponent
);
