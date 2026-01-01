import * as t from "io-ts";
import type {ApplicationRef, DoBootstrap} from "@angular/core";
import {
    Component,
    ElementRef,
    NgModule,
    ViewChild,
    ViewEncapsulation,
} from "@angular/core";
import {
    GenericPluginMarkup,
    Info,
    nullable,
    withDefault,
} from "tim/plugin/attributes";

import {valueDefu} from "tim/util/utils";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import {FormsModule} from "@angular/forms";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {HttpClientModule} from "@angular/common/http";
import {PurifyModule} from "tim/util/purify.module";
import {registerPlugin} from "tim/plugin/pluginRegistry";
import {CommonModule} from "@angular/common";

const ShowFileMarkup = t.intersection([
    t.partial({
        followid: t.string,
        width: t.number,
        height: t.number,
        hidetext: nullable(t.string),
    }),
    GenericPluginMarkup,
    t.type({
        first: withDefault(t.number, 1),
        autoplay: withDefault(t.number, 0),
        repeat: withDefault(t.boolean, true),
        fade: withDefault(t.boolean, true),
        open: withDefault(t.boolean, true),
        random: withDefault(t.boolean, false),
        autostart: withDefault(t.boolean, true),
        dots: withDefault(t.boolean, true),
        arrows: withDefault(t.boolean, true),
        counter: withDefault(t.boolean, true),
        change: withDefault(t.boolean, true),
        noFlicker: withDefault(t.boolean, false),
        nearFiles: withDefault(t.number, 1),
        hover: withDefault(t.boolean, true),
        hoverNext: withDefault(t.boolean, false),
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

type ImageFile = {name: string; caption: string; alt: string};
type FileEntry = ImageFile | string;

const ShowFileAll = t.type({
    info: Info,
    markup: ShowFileMarkup,
    preview: t.boolean,
});

// noinspection TypeScriptUnresolvedVariable
@Component({
    selector: "tim-images",
    styleUrls: ["./images.component.scss"],
    encapsulation: ViewEncapsulation.None, // Prevents style isolation
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
            <tim-markup-error *ngIf="markupError" [data]="markupError!"></tim-markup-error>
            <p *ngIf="header" [innerHtml]="header | purify"></p>
            <p *ngIf="stem" class="stem" [innerHtml]="stem | purify"></p>


            <div #images class="images-container"
                        [style.width.px]="width"
                        [style.height.px]="height"
            >
                <ng-container *ngFor="let file of files; let i = index">
                    <div [class]="{fade: markup.fade}" *ngIf="noFlicker || isNear(i+1)" [hidden]="fileIndex!==(i+1)"  (click)="jump(1)">
                        <div *ngIf="markup.counter" class="numbertext">{{(i+1)}} / {{files.length}}</div>
                        <img src="{{file.name}}" alt="{{file.alt}}" style="width:100%">
                        <div class="text"><span [innerHTML]="file.caption | purify"></span></div>
                    </div>
                </ng-container>
                <a *ngIf="markup.change && markup.arrows" class="prev" (click)="jump(-1)">&#10094;</a>
                <a *ngIf="markup.change && markup.arrows" class="next" (click)="jump(1)">&#10095;</a>
            </div>
            <br>

            <div *ngIf="markup.dots" style="text-align:center" class="images-control" >
              <span *ngFor="let file of files;
                let i = index" [ngClass]="{'active': (i+1) === fileIndex}"
                    class="dot"
                    (click)="currentFile(i+1)"
                    (mouseover)="hoverCurrentFile(i+1)"
                    (touchstart)="$event.preventDefault(); hoverCurrentFile(i+1)"
              ></span>
            </div>
<!--                    (touchmove)="$event.preventDefault(); hoverCurrentFile(i+1)"-->

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
            <p class="plgfooter" *ngIf="footer" [innerHtml]="footer | purify"></p>
        </div>
    `,
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
    @ViewChild("images", {static: true}) images?: ElementRef<HTMLDivElement>;

    width?: number;
    height?: number;

    imagesOn: boolean = true;
    fileIndex = 1;
    files: {name: string; caption: string; alt: string}[] = [];
    duration: number = 0;
    intervalId: number = 0;
    noFlicker: boolean = false;
    nearFiles: number = 1;
    requiresTaskId = false;

    private hoverStart?: EventListener;
    private hoverEnd?: EventListener;

    ngOnInit() {
        super.ngOnInit();
        this.width = this.markup.width;
        this.height = this.markup.height;
        this.noFlicker = this.markup.noFlicker || this.markup.autoplay != 0;
        for (const file of this.markup.files as FileEntry[]) {
            if (typeof file === "string") {
                this.files.push({name: file, caption: "", alt: ""});
            } else {
                this.files.push(file);
            }
        }
        // this.files = this.markup.files;
        const n = this.files.length;
        if (!this.markup.change) {
            this.nearFiles = 0;
        } else {
            this.nearFiles = this.markup.nearFiles;
        }
        // take positive modulo so always in [1,n]
        this.fileIndex = ((((this.markup.first - 1) % n) + n) % n) + 1;
        if (this.markup.random) {
            this.fileIndex = Math.floor(Math.random() * n) + 1;
        }
        this.duration = this.markup.autoplay;
        if (this.markup.autostart) {
            this.speed(0);
        }

        const el = this.images?.nativeElement as HTMLDivElement | null;
        if (!el || !this.markup.hoverNext) {
            return;
        }

        // set up hover listeners for changing images on hover
        this.hoverStart = (e: Event) => {
            // prevent scroll on touchstart if needed
            if (e.type === "touchstart") {
                e.preventDefault();
            }
            this.onHover(true);
        };
        this.hoverEnd = () => this.onHover(false);

        el.addEventListener("mouseenter", this.hoverStart);
        el.addEventListener("mouseleave", this.hoverEnd);
        el.addEventListener("touchstart", this.hoverStart, {
            passive: false,
        } as AddEventListenerOptions);
        el.addEventListener("touchend", this.hoverEnd);
        el.addEventListener("touchcancel", this.hoverEnd);
    }

    onHover(_active: boolean) {
        this.plusFile();
    }

    isNear(n: number) {
        const diff = Math.abs(this.fileIndex - n);
        if (diff <= this.nearFiles) {
            return true;
        }
        return diff >= this.files.length - this.nearFiles;
    }

    currentFile(n: number) {
        if (!this.markup.change) {
            return;
        }
        this.stop();
        this.showFile(n);
    }

    hoverCurrentFile(n: number) {
        if (!this.markup.hover) {
            return;
        }
        this.currentFile(n);
    }

    plusFile(n: number = 1) {
        this.showFile(this.fileIndex + n);
    }

    private showFile(n: number) {
        if (n > this.files.length) {
            this.fileIndex = 1;
        } else if (n < 1) {
            this.fileIndex = this.files.length;
        } else {
            this.fileIndex = n;
        }
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
        if (!this.markup.change) {
            return;
        }
        this.stop();
        this.currentFile(value);
    }

    jump(value: number = 1, $event: Event | undefined = undefined) {
        if ($event) {
            $event.preventDefault();
        }
        if (!this.markup.change) {
            return;
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
    imports: [
        CommonModule,
        TimUtilityModule,
        HttpClientModule,
        FormsModule,
        PurifyModule,
    ],
})
export class ImagesModule implements DoBootstrap {
    ngDoBootstrap(_appRef: ApplicationRef) {}
}

registerPlugin("tim-images", ImagesModule, ImagesComponent);
