import {
    ApplicationRef,
    Component,
    DoBootstrap,
    NgModule,
    OnInit,
} from "@angular/core";
import {
    BrowserModule,
    DomSanitizer,
    SafeResourceUrl,
} from "@angular/platform-browser";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {createDowngradedModule, doDowngrade} from "../downgrade";
import {documentglobals} from "../util/globals";

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
    imports: [BrowserModule],
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
