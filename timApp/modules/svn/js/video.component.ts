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
import {parseIframeopts, seconds2Time, valueDefu} from "tim/util/utils";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import {BrowserModule} from "@angular/platform-browser";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {createDowngradedModule, doDowngrade} from "tim/downgrade";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import {HttpClientModule} from "@angular/common/http";
import {Iframesettings} from "../../cs/js/jsframe";
import {VideoLinkComponent} from "./video-link.component";

function toSeconds(value: string | number | undefined): number | undefined {
    if (value === null || value === undefined) {
        return undefined;
    }
    if (typeof value == "number") {
        return value;
    }

    let s = "0 0 0 " + value.replace(/s/g, "").replace(/[,\/;:.hm]/g, " "); // forget the final 's' from 1h3m2s
    s = s.trim();
    const sc = s.split(" ");
    const n = sc.length;
    const h = parseFloat(sc[n - 3]);
    const m = parseFloat(sc[n - 2]);
    const ss = parseFloat(sc[n - 1]);
    const result = h * 3600.0 + m * 60.0 + ss;
    if (isNaN(result)) {
        return undefined;
    }
    return result;
}

function time2String(time: number) {
    const {hours, minutes, seconds} = seconds2Time(time);
    let hs;
    let ms;
    if (!hours) {
        hs = "";
    } else {
        hs = hours + "h";
    }
    if (!hours && !minutes) {
        ms = "";
    } else {
        ms = minutes + "m";
    }
    const ss = seconds + "s";
    return hs + ms + ss;
}

const youtubeDomains = new Set(["www.youtube.com", "youtube.com", "youtu.be"]);

function isYoutube(file: string) {
    try {
        const u = new URL(file, location.origin);
        return youtubeDomains.has(u.hostname);
    } catch {
        return false;
    }
}

const ShowFileMarkup = t.intersection([
    t.partial({
        doclink: nullable(t.string),
        doctext: nullable(t.string),
        end: t.union([t.number, t.string]),
        followid: t.string,
        height: t.number,
        hidetext: nullable(t.string),
        iframe: t.boolean,
        iframeopts: nullable(t.string),
        start: t.union([t.number, t.string]),
        videoname: nullable(t.string),
        width: t.number,
    }),
    GenericPluginMarkup,
    t.type({
        autoplay: withDefault(t.boolean, true),
        file: withDefault(t.string, ""),
        target: withDefault(t.string, "timdoc"),
        open: withDefault(t.boolean, false),
        videoicon: withDefault(t.boolean, true),
        type: withDefault(
            t.keyof({normal: null, small: null, list: null}),
            "normal"
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
    selector: "tim-video",
    template: `
        <div [class]="videoClass">
            <tim-markup-error *ngIf="markupError" [data]="markupError"></tim-markup-error>
            <p *ngIf="header" [innerHtml]="header"></p>
            <p *ngIf="stem && isNormalSize" class="stem" [innerHtml]="stem"></p>

            <div *ngIf="!isNormalSize" class="videoInfo">
                <span [innerHtml]="stem"></span>&ngsp;
                <a *ngIf="videoname" class="videoname"
                   (click)="toggleVideo()">
                    <i *ngIf="markup.videoicon" class="glyphicon glyphicon-facetime-video"></i>
                    {{videoname}}
                    <ng-container *ngIf="markup.type === 'list'">
                        &ndash;
                        {{startt}}
                    </ng-container>
                    {{duration}} {{span}}</a>
                &ngsp;
                <tim-video-link
                        *ngIf="markup.doclink"
                        [doclink]="markup.doclink"
                        [doctext]="doctext"
                        [target]="markup.target"
                ></tim-video-link>
            </div>

            <ng-container *ngIf="videoOn">
                <iframe *ngIf="iframesettings"
                        class="showVideo"
                        frameborder="0"
                        allowfullscreen
                        [src]="iframesettings.src"
                        [sandbox]="iframesettings.sandbox"
                        [style.width.px]="width"
                        [style.height.px]="height"
                        [attr.allow]="iframesettings.allow"
                >
                </iframe>
                <video *ngIf="videosettings"
                       #video
                       class="showVideo"
                       controls
                       (metadataloaded)="metadataloaded()"
                       (timeupdate)="timeupdate()"
                       [style.width.px]="width"
                       [style.height.px]="height"
                       [src]="videosettings.src"
                       [autoplay]="markup.autoplay"
                >
                </video>
            </ng-container>
            <ng-container *ngIf="isNormalSize">
                <div *ngIf="!videoOn" class="no-popup-menu play">
                    <a (click)="toggleVideo()">
                        <i class="glyphicon glyphicon-play-circle"
                           title="Click here to show the video"></i>
                    </a>
                </div>
                <tim-video-link
                        *ngIf="markup.doclink"
                        [doclink]="markup.doclink"
                        [doctext]="doctext"
                        [target]="markup.target"
                ></tim-video-link>
            </ng-container>
            <div class="flex" *ngIf="videoOn" style="justify-content: flex-end">
                <div *ngIf="videosettings" class="margin-5-right">
                    Speed:
                    <span class="text-smaller">
                        {{playbackRateString}}
                    </span>
                    <a (click)="speed(1.0/1.2)" title="Slower speed"><i class="glyphicon glyphicon-minus"></i></a>&ngsp;
                    <a (click)="speed(0)" title="Normal speed">1x</a>&ngsp;
                    <a (click)="speed(1.2)" title="Faster speed"><i class="glyphicon glyphicon-plus"></i></a>&ngsp;
                </div>
                <div class="margin-5-right">
                    Zoom:
                    <a (click)="zoom(1.0/1.4)" title="Zoom out"><i class="glyphicon glyphicon-minus"></i></a>&ngsp;
                    <a (click)="zoom(0)" title="Normal zoom">R</a>&ngsp;
                    <a (click)="zoom(1.4)" title="Zoom in"><i class="glyphicon glyphicon-plus"></i></a>
                </div>
                <a (click)="hideVideo()">{{hidetext}}</a>
            </div>
            <p class="plgfooter" *ngIf="footer" [innerHtml]="footer"></p>
        </div>
    `,
    styleUrls: ["./video.component.scss"],
})
export class VideoComponent extends AngularPluginBase<
    t.TypeOf<typeof ShowFileMarkup>,
    t.TypeOf<typeof ShowFileAll>,
    typeof ShowFileAll
> {
    get videoClass() {
        switch (this.markup.type) {
            case "normal": {
                return "videoRunDiv";
            }
            case "small": {
                return "smallVideoRunDiv";
            }
            case "list": {
                return "listVideoRunDiv";
            }
        }
    }

    get isNormalSize() {
        return this.markup.type === "normal";
    }

    get iframe() {
        return this.markup.iframe ?? isYoutube(this.markup.file);
    }

    get videoname() {
        return this.markup.videoname;
    }

    get hidetext() {
        return valueDefu(this.markup.hidetext, "Hide file");
    }

    get doctext() {
        return this.markup.doctext;
    }

    private watchEnd?: number;
    private start?: number;
    private end?: number;
    videoOn: boolean = false;
    span?: string | null;
    private origSize!: string;
    private origWidth?: number;
    private origHeight?: number;
    @ViewChild("video") video?: ElementRef<HTMLVideoElement>;
    private limits!: string | null;
    duration!: string | null;
    startt!: string | null;
    width?: number;
    height?: number;
    private vctrl?: ViewCtrl;
    iframesettings?: Iframesettings;
    videosettings?: {src: string};
    playbackRateString = "";

    ngOnInit() {
        super.ngOnInit();
        this.vctrl = vctrlInstance;
        this.start = toSeconds(this.markup.start);
        this.end = toSeconds(this.markup.end);
        this.width = this.markup.width;
        this.height = this.markup.height;
        if (this.start != null && this.end != null) {
            this.duration = `(${time2String(this.end - this.start)})`;
            this.limits = `(${time2String(this.start)}-${time2String(
                this.end
            )})`;
            this.startt = time2String(this.start);
        } else {
            this.duration = null;
            this.limits = null;
            this.startt = null;
        }
        this.getPrevZoom();
        if (this.markup.open) {
            this.toggleVideo();
        }
    }

    hideVideo() {
        this.videoOn = false;
        this.span = "";
    }

    getCurrentZoom() {
        const origw = localStorage[this.origSize + ".width"];
        if (t.number.is(origw)) {
            this.width = origw;
        }
        const origh = localStorage[this.origSize + ".height"];
        if (t.number.is(origh)) {
            this.height = origh;
        }
    }

    getPrevZoom() {
        let name = "z";

        if (this.width) {
            name += this.width;
        }
        this.origWidth = this.width;

        name += "x";

        if (this.height) {
            name += this.height;
        }
        this.origHeight = this.height;

        this.origSize = name;
    }

    speed(mult: number) {
        if (!this.video) {
            return;
        }
        const v = this.video.nativeElement;
        if (mult === 0) {
            v.playbackRate = 1.0;
        } else {
            v.playbackRate *= mult;
        }
        this.playbackRateString = v.playbackRate.toFixed(1);
    }

    zoom(mult: number) {
        if (mult === 0) {
            this.width = this.origWidth;
            this.height = this.origHeight;
            localStorage.removeItem(this.origSize + ".width");
            localStorage.removeItem(this.origSize + ".height");
        } else {
            if (this.width) {
                this.width *= mult;
                localStorage[this.origSize + ".width"] = "" + this.width;
            }
            if (this.height) {
                this.height *= mult;
                localStorage[this.origSize + ".height"] = "" + this.height;
            }
        }
    }

    toggleVideo() {
        if (this.videoOn) {
            this.hideVideo();
            return;
        }
        this.getCurrentZoom();

        this.span = this.limits;
        const moniviestin = this.markup.file.includes("m3.jyu.fi");
        let params = "?";
        if (this.start) {
            if (moniviestin) {
                params = "#position=" + this.start;
            } else {
                params = "?start=" + this.start + "&end=" + this.end;
            }
        }
        if (this.iframe) {
            let file = this.markup.file;
            if (isYoutube(file) && !file.includes("embed")) {
                const yname = "youtu.be/"; // could be also https://youtu.be/1OygRiwlAok
                const yembed = "//www.youtube.com/embed/";
                const iy = file.indexOf(yname);
                const parts = file.split("=");
                if (parts.length > 1) {
                    file = yembed + parts[1];
                } else if (iy >= 0) {
                    file = yembed + file.substring(iy + yname.length);
                }
            }
            this.iframesettings = {
                src: this.domSanitizer.bypassSecurityTrustResourceUrl(
                    `${file}${params}`
                ),
                width: this.width ?? null,
                height: this.height ?? null,
                sandbox: parseIframeopts(
                    this.markup.iframeopts ?? 'sandbox="allow-scripts"'
                ).sandbox,
                allow: null,
            };
        } else {
            params = "";
            let tbe = "";
            if (this.start) {
                tbe += this.start; // loadedmetadata event doesn't work on iPad
                if (this.end) {
                    tbe += "," + this.end;
                }
            }
            if (tbe) {
                params = "#t=" + tbe;
            }
            this.videosettings = {
                src: `${this.markup.file}${params}`,
            };
        }
        this.videoOn = true;
        this.watchEnd = this.end;
    }

    metadataloaded() {
        this.video!.nativeElement.currentTime = this.start ?? 0;
        if (this.markup.followid && this.vctrl) {
            this.vctrl.registerVideo(
                this.markup.followid,
                this.video!.nativeElement
            );
        }
    }

    timeupdate() {
        const v = this.video!.nativeElement;
        if (this.watchEnd && v.currentTime > this.watchEnd) {
            v.pause();
            this.watchEnd = 1000000;
        }
    }

    getDefaultMarkup() {
        return {
            file: "https://example.com",
            iframe: true,
        };
    }

    getAttributeType() {
        return ShowFileAll;
    }
}

@NgModule({
    declarations: [VideoComponent, VideoLinkComponent],
    imports: [BrowserModule, TimUtilityModule, HttpClientModule],
})
export class VideoModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

export const moduleDefs = [
    doDowngrade(
        createDowngradedModule((extraProviders) =>
            platformBrowserDynamic(extraProviders).bootstrapModule(VideoModule)
        ),
        "timVideo",
        VideoComponent
    ),
];
