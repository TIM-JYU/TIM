/**
 * Module for HTML sanitizer based on html-sanitize library.
 *
 * NOTE: Prefer using PurifyModule instead! It allows faster and better sanitization.
 *       Use this module only for cases where PurifyModule does not yield desired sanitization.
 */

import {NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {HTMLSanitizePipe} from "tim/util/htmlsanitize.pipe";

@NgModule({
    declarations: [HTMLSanitizePipe],
    exports: [HTMLSanitizePipe],
    imports: [CommonModule],
})
export class HTMLSanitizeModule {}
