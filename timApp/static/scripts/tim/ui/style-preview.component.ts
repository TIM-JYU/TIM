import type {ApplicationRef, DoBootstrap} from "@angular/core";
import {Component, NgModule} from "@angular/core";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {toPromise} from "tim/util/utils";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {createDowngradedModule, doDowngrade} from "tim/downgrade";
import {documentglobals} from "tim/util/globals";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {BrowserModule} from "@angular/platform-browser";

@Component({
    selector: "tim-style-preview",
    template: `
    <tim-alert *ngIf="error">
        <strong *ngIf="error.title">{{error.title}}</strong>
        <p *ngIf="error.message">{{error.message}}</p>
        <div class="error-code" *ngIf="error.code">
            <pre>{{error.code}}</pre>
        </div>
    </tim-alert>
    <div class="preview-items">
        <tim-loading *ngIf="loading"></tim-loading>
        <button [disabled]="!docId || loading" class="timButton" (click)="previewStyle()">
            <ng-container *ngIf="currentStyle == originalStyle" i18n>Preview style</ng-container>
            <ng-container *ngIf="currentStyle != originalStyle" i18n>Refresh preview</ng-container>
        </button>
        <button [disabled]="!docId || loading || currentStyle == originalStyle" class="timButton" (click)="stopPreview()" i18n>Stop preview</button>
    </div>
  `,
    styleUrls: ["./style-preview.component.scss"],
})
export class StylePreviewComponent {
    error?: {title?: string; message?: string; code?: string} = undefined;
    userStyle!: HTMLLinkElement;
    loading: boolean = false;
    docId!: number;
    originalStyle!: string;
    currentStyle!: string;

    constructor(private http: HttpClient) {
        const doc = documentglobals().curr_item;

        if (!doc) {
            this.error = {
                title: $localize`This component only works for documents`,
            };
            return;
        }

        const docSettings = documentglobals().docSettings;

        if (!docSettings.description) {
            this.error = {
                title: $localize`Only styles in styles folder can be previewed`,
                message: $localize`Move the document under styles folder to see the preview`,
            };
            return;
        }

        this.docId = doc.id;

        this.userStyle =
            document.querySelector(
                'link[rel="stylesheet"][data-style-origin="user-prefs-style"]'
            ) ??
            document.querySelector(
                'link[rel="stylesheet"][data-style-origin="document-style"]'
            )!;
        this.originalStyle = this.userStyle.getAttribute("href")!;
        this.currentStyle = this.originalStyle;
    }

    async previewStyle() {
        this.error = undefined;
        this.loading = true;

        const r = await toPromise<string, {error: string}>(
            this.http.get("/styles/path", {
                params: {
                    docs: this.docId,
                },
                responseType: "text",
            })
        );

        if (r.ok) {
            this.currentStyle = `/${r.result}?${Date.now()}`;
            this.userStyle.setAttribute("href", this.currentStyle);
        } else {
            this.error = {
                title: $localize`The style could not be compiled`,
                message: $localize`The following error occurred:`,
                code: r.result.error,
            };
        }

        this.loading = false;
    }

    stopPreview() {
        this.error = undefined;
        this.currentStyle = this.originalStyle;
        this.userStyle.setAttribute("href", this.originalStyle);
    }
}

@NgModule({
    declarations: [StylePreviewComponent],
    exports: [StylePreviewComponent],
    imports: [BrowserModule, TimUtilityModule, HttpClientModule],
})
export class StylePreviewModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef): void {}
}

export const angularJsModule = createDowngradedModule((extraProviders) => {
    const platformRef = platformBrowserDynamic(extraProviders);
    return platformRef.bootstrapModule(StylePreviewModule);
});
doDowngrade(angularJsModule, "timStylePreview", StylePreviewComponent);
export const moduleDefs = [angularJsModule];
