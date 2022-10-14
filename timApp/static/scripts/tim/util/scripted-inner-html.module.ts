import {NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {ScriptedInnerHTMLDirective} from "tim/util/scripted-inner-html.directive";

@NgModule({
    declarations: [ScriptedInnerHTMLDirective],
    imports: [CommonModule, TimUtilityModule],
    exports: [ScriptedInnerHTMLDirective],
})
export class ScriptedInnerHTMLModule {}
