import {NgModule} from "@angular/core";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {ScriptedInnerHTMLDirective} from "tim/util/scripted-inner-html.directive";
import {BrowserModule} from "@angular/platform-browser";

@NgModule({
    declarations: [ScriptedInnerHTMLDirective],
    imports: [BrowserModule, TimUtilityModule],
    exports: [ScriptedInnerHTMLDirective],
})
export class ScriptedInnerHTMLModule {}
