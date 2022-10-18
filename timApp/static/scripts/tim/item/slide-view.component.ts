import type {ApplicationRef, DoBootstrap, OnInit} from "@angular/core";
import {Component, NgModule} from "@angular/core";
import type {SafeResourceUrl} from "@angular/platform-browser";
import {BrowserModule, DomSanitizer} from "@angular/platform-browser";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {createDowngradedModule, doDowngrade} from "tim/downgrade";
import {documentglobals} from "tim/util/globals";
import {TimUtilityModule} from "tim/ui/tim-utility.module";

@Component({
    selector: "tim-slide-view",
    template: `
        <div class="slideFrameContainer">
            <iframe id="slideFrame" [src]="itemUrl" allowfullscreen></iframe>
        </div>
        <a id="showSlidesLink" [href]="itemUrl">Show slides only</a>
    `,
    styleUrls: ["./slide-view.component.scss"],
})
export class SlideViewComponent implements OnInit {
    item = documentglobals().curr_item;
    itemUrl: SafeResourceUrl;

    constructor(domSanitizer: DomSanitizer) {
        this.itemUrl = domSanitizer.bypassSecurityTrustResourceUrl(
            `/show_slide/${this.item.path}`
        );
    }

    ngOnInit(): void {}
}

@NgModule({
    declarations: [SlideViewComponent],
    imports: [BrowserModule, TimUtilityModule],
})
export class SlideViewModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef): void {}
}

export const moduleDefs = [
    doDowngrade(
        createDowngradedModule((extraProviders) =>
            platformBrowserDynamic(extraProviders).bootstrapModule(
                SlideViewModule
            )
        ),
        "timSlideView",
        SlideViewComponent
    ),
];
