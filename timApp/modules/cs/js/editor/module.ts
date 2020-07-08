import {NgModule} from "@angular/core";
import {FormsModule} from "@angular/forms";
import {BrowserModule} from "@angular/platform-browser";
import {MatTabsModule} from "@angular/material/tabs";
import {BrowserAnimationsModule} from "@angular/platform-browser/animations";
import {TimUtilityModule} from "tim/ui/tim-utility.module";

import {CountBoardComponent} from "./countboard";
import {EditorComponent, JSParsonsEditorComponent} from "./editor";
import {NormalEditorComponent} from "./normal";
import {AceEditorComponent} from "./ace";
import {ParsonsEditorComponent} from "./parsons";

@NgModule({
    declarations: [
        EditorComponent,
        NormalEditorComponent,
        AceEditorComponent,
        ParsonsEditorComponent,
        CountBoardComponent,
        JSParsonsEditorComponent,
    ],
    imports: [
        BrowserModule,
        FormsModule,
        MatTabsModule,
        BrowserAnimationsModule,
    ],
    exports: [
        CountBoardComponent,
        EditorComponent,
    ],
})
export class EditorModule {}
