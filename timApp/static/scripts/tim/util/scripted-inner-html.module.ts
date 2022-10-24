import {NgModule} from "@angular/core";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {ScriptedInnerHTMLDirective} from "tim/util/scripted-inner-html.directive";
import {CommonModule} from "@angular/common";

@NgModule({
    declarations: [ScriptedInnerHTMLDirective],
    imports: [CommonModule, TimUtilityModule],
    exports: [ScriptedInnerHTMLDirective],
})
export class ScriptedInnerHTMLModule {}
