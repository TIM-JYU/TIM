import {NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {ScriptedInnerHTMLDirective} from "./scripted-inner-html.directive";

@NgModule({
    declarations: [ScriptedInnerHTMLDirective],
    imports: [CommonModule],
    exports: [ScriptedInnerHTMLDirective],
})
export class ScriptedInnerHTMLModule {}
