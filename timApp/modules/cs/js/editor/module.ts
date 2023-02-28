import {NgModule} from "@angular/core";
import {FormsModule, ReactiveFormsModule} from "@angular/forms";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {CommonModule} from "@angular/common";
import {CsUtilityModule} from "../util/module";
import {CountBoardComponent} from "./countboard";
import {EditorComponent, JSParsonsEditorComponent} from "./editor";
import {NormalEditorComponent} from "./normal";
import {AceEditorComponent} from "./ace";
import {ParsonsEditorComponent} from "./parsons";
import {MathEditorComponent} from "./math-editor/math-editor.component";
import {FormulaEditorComponent} from "./math-editor/formula-editor.component";

@NgModule({
    declarations: [
        EditorComponent,
        NormalEditorComponent,
        AceEditorComponent,
        ParsonsEditorComponent,
        CountBoardComponent,
        JSParsonsEditorComponent,
        MathEditorComponent,
        FormulaEditorComponent,
    ],
    imports: [
        CommonModule,
        FormsModule,
        TimUtilityModule,
        CsUtilityModule,
        ReactiveFormsModule,
    ],
    exports: [CountBoardComponent, EditorComponent, AceEditorComponent],
})
export class EditorModule {}
