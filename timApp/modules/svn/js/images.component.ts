import * as t from "io-ts";
import {
    ApplicationRef,
    Component,
    DoBootstrap,
    ElementRef,
    NgModule,
    ViewChild,
} from "@angular/core";
import {ViewCtrl} from "tim/document/viewctrl";
import {
    GenericPluginMarkup,
    Info,
    nullable,
    withDefault,
} from "tim/plugin/attributes";

import {valueDefu} from "tim/util/utils";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import {BrowserModule} from "@angular/platform-browser";
import {FormsModule} from "@angular/forms";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {createDowngradedModule, doDowngrade} from "tim/downgrade";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import {HttpClientModule} from "@angular/common/http";

const ShowFileMarkup = t.intersection([
    t.partial({
        followid: t.string,
        width: t.number,
        height: t.number,
        hidetext: nullable(t.string),
    }),
    GenericPluginMarkup,
    t.type({
        autoplay: withDefault(t.number, 0),
        repeat: withDefault(t.boolean, true),
        fade: withDefault(t.boolean, true),
        open: withDefault(t.boolean, true),
        random: withDefault(t.boolean, false),
        autostart: withDefault(t.boolean, true),
        files: withDefault(
            t.array(
                t.union([
                    t.type({
                        name: t.string,
                        caption: withDefault(t.string, ""),
                        alt: withDefault(t.string, ""),
                    }),
                    t.string,
                ])
            ),
            []
        ),
    }),
]);

const ShowFileAll = t.type({
    info: Info,
    markup: ShowFileMarkup,
    preview: t.boolean,
});

// noinspection TypeScriptUnresolvedVariable
@Component({
    selector: "tim-images",
    template: `
        <div [class]="imagesClass" 
             tabindex="0"
             (keydown.-)="speed(1.0/1.2, $event)"
             (keydown.1)="speed(0, $event)"
             (keydown.+)="speed(1.2, $event)"
             (keydown.arrowLeft)="jump(-1, $event)"
             (keydown.arrowRight)="jump(1, $event)"
             (keydown.space)="jump(1, $event)"
             (keydown.control.arrowLeft)="jumpTo(1, $event)"
             (keydown.control.arrowRight)="jumpTo(-1, $event)"
        >
            <tim-markup-error *ngIf="markupError" [data]="markupError"></tim-markup-error>
            <p *ngIf="header" [innerHtml]="header"></p>
            <p *ngIf="stem" class="stem" [innerHtml]="stem"></p>


            <div class="images-container"
                        [style.width.px]="width"
                        [style.height.px]="height"
            >
                <div *ngFor="let file of files; let i = index">
                    <div [class]="{fade: markup.fade}" *ngIf="fileIndex===(i+1)"  (click)="jump(1)">
                        <div class="numbertext">{{(i+1)}} / {{files.length}}</div>
                        <img src="{{file.name}}" alt="{{file.alt}}" style="width:100%">
                        <div class="text">{{file.caption}}</div>
                    </div>    
                </div>
                <a class="prev" (click)="plusFile(-1)">&#10094;</a>
                <a class="next" (click)="plusFile(1)">&#10095;</a>
            </div>
            <br>
            
            <div style="text-align:center" class="images-control">
              <span *ngFor="let file of files; let i = index" [ngClass]="{'active': (i+1) === fileIndex}" class="dot" (click)="currentFile(i+1)"></span>
            </div>                

            <div *ngIf="markup.autoplay" class="flex"  style="justify-content: flex-end">
                <div class="margin-5-right">
                    <span *ngIf="intervalId">
                        <span class="text-smaller">
                            Interval:
                            {{this.duration/1000 | number: '1.1-1'}}
                            s
                        </span>
                        <a (click)="speed(1.0/1.2)" title="Shorter interval (-)"><i class="glyphicon glyphicon-minus"></i></a>&ngsp;
                        <a (click)="speed(0)" title="Normal interval (1)">1x</a>&ngsp;
                        <a (click)="speed(1.2)" title="Longer interval (+)"><i class="glyphicon glyphicon-plus"></i></a>&ngsp;
                    </span>
                    <span *ngIf="intervalId===0">
                        <a (click)="speed(1)" title="Run">Run</a>&ngsp;
                    </span>
                </div>
            </div>
            <p class="plgfooter" *ngIf="footer" [innerHtml]="footer"></p>
        </div>
    `,
    styleUrls: ["./images.component.scss"],
})
export class ImagesComponent extends AngularPluginBase<
    t.TypeOf<typeof ShowFileMarkup>,
    t.TypeOf<typeof ShowFileAll>,
    typeof ShowFileAll
> {
    get imagesClass() {
        return "imagesRunDiv";
    }

    get hidetext() {
        return valueDefu(this.markup.hidetext, "Hide file");
    }

    span?: string | null;
    @ViewChild("images") images?: ElementRef<HTMLImageElement>;
    width?: number;
    height?: number;
    private vctrl?: ViewCtrl;
    imagesOn: boolean = true;
    fileIndex = 1;
    files: {name: string; caption: string; alt: string}[] = [];
    duration: number = 0;
    intervalId: number = 0;

    ngOnInit() {
        super.ngOnInit();
        this.vctrl = vctrlInstance;
        this.width = this.markup.width;
        this.height = this.markup.height;
        for (const file of this.markup.files) {
            if (typeof file === "string") {
                this.files.push({name: file, caption: "", alt: ""});
            } else {
                this.files.push(file);
            }
        }
        // this.files = this.markup.files;
        if (this.markup.random) {
            this.fileIndex = Math.floor(Math.random() * this.files.length) + 1;
        }
        this.duration = this.markup.autoplay;
        if (this.markup.autostart) {
            this.speed(0);
        }
    }

    currentFile(n: number) {
        this.stop();
        this.showFile(n);
    }

    plusFile(n: number = 1) {
        this.showFile(this.fileIndex + n);
    }

    private showFile(n: number) {
        if (n > this.files.length) {
            this.fileIndex = 1;
        } else if (n < 1) {
            this.fileIndex = this.files.length;
        } else this.fileIndex = n;
    }

    stop() {
        if (this.intervalId === 0) {
            return;
        }
        window.clearInterval(this.intervalId);
        this.intervalId = 0;
    }

    speed(mult: number, $event: Event | undefined = undefined) {
        if ($event) {
            $event.preventDefault();
        }
        this.stop();
        this.duration *= mult;
        if (mult === 0) {
            this.duration = this.markup.autoplay;
        }
        const images = this;
        if (this.duration) {
            this.intervalId = window.setInterval(function () {
                if (
                    !images.markup.repeat &&
                    images.fileIndex >= images.files.length
                ) {
                    images.stop();
                } else {
                    images.plusFile(1);
                }
            }, this.duration);
        }
    }

    jumpTo(value: number, $event: Event | undefined = undefined) {
        if ($event) {
            $event.preventDefault();
        }
        this.currentFile(value);
    }

    jump(value: number = 1, $event: Event | undefined = undefined) {
        if ($event) {
            $event.preventDefault();
        }
        this.stop();
        this.plusFile(value);
    }

    getDefaultMarkup() {
        return {};
    }

    getAttributeType() {
        return ShowFileAll;
    }
}

@NgModule({
    declarations: [ImagesComponent],
    imports: [BrowserModule, TimUtilityModule, HttpClientModule, FormsModule],
})
export class ImagesModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

export const moduleDefs = [
    doDowngrade(
        createDowngradedModule((extraProviders) =>
            platformBrowserDynamic(extraProviders).bootstrapModule(ImagesModule)
        ),
        "timImages",
        ImagesComponent
    ),
];
