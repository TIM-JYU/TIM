import {NgModule} from "@angular/core";
import {FormsModule} from "@angular/forms";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {CommonModule} from "@angular/common";
import {CsUtilityModule} from "../util/module";
import {CountBoardComponent} from "./countboard";
import {EditorComponent, JSParsonsEditorComponent} from "./editor";
import {NormalEditorComponent} from "./normal";
import {AceEditorComponent} from "./ace";
import {ParsonsEditorComponent} from "./parsons";
import {FormulaEditorComponent} from "./math-editor/formula-editor.component";
import {FormulaFieldComponent} from "./math-editor/formula-field.component";
import {
    SymbolButtonMenuComponent,
    SymbolsPipe,
} from "./math-editor/symbol-button-menu.component";
import {FormulaEditorLoaderComponent} from "./math-editor/formula-editor-loader.component";

@NgModule({
    declarations: [
        EditorComponent,
        NormalEditorComponent,
        AceEditorComponent,
        ParsonsEditorComponent,
        CountBoardComponent,
        JSParsonsEditorComponent,
        FormulaFieldComponent,
        FormulaEditorComponent,
        FormulaEditorLoaderComponent,
        SymbolButtonMenuComponent,
        SymbolsPipe,
    ],
    imports: [CommonModule, FormsModule, TimUtilityModule, CsUtilityModule],
    exports: [
        CountBoardComponent,
        EditorComponent,
        AceEditorComponent,
        FormulaEditorComponent,
        SymbolsPipe,
    ],
})
export class EditorModule {}
