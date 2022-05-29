import {NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {TimUtilityModule} from "../ui/tim-utility.module";
import {ScriptedInnerHTMLDirective} from "./scripted-inner-html.directive";

@NgModule({
    declarations: [ScriptedInnerHTMLDirective],
    imports: [CommonModule, TimUtilityModule],
    exports: [ScriptedInnerHTMLDirective],
})
export class ScriptedInnerHTMLModule {}
