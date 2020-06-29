import {NgModule} from "@angular/core";
import { FormsModule } from '@angular/forms';
import {BrowserModule} from "@angular/platform-browser";

import {EditorComponent, JSParsonsEditorComponent} from "./editor";
import {NormalEditorComponent} from "./normal";
import {AceEditorComponent} from "./ace";
import {ParsonsEditorComponent} from "./parsons";

// noinspection AngularInvalidImportedOrDeclaredSymbol
@NgModule({
    declarations: [
        EditorComponent,
        NormalEditorComponent,
        AceEditorComponent,
        ParsonsEditorComponent,
        JSParsonsEditorComponent,
    ],
    imports: [
        BrowserModule,
        FormsModule,
    ],
    exports: [
        EditorComponent,
    ]
})
export class EditorModule {}