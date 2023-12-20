import * as t from "io-ts";
import type {ApplicationRef, DoBootstrap} from "@angular/core";
import {Component, ElementRef, NgModule, ViewChild} from "@angular/core";
import {
    GenericPluginMarkup,
    Info,
    nullable,
    withDefault,
} from "tim/plugin/attributes";
import {
    copyToClipboard,
    isSafari,
    parseIframeopts,
    seconds2Time,
    TimStorage,
    valueDefu,
} from "tim/util/utils";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import {FormsModule} from "@angular/forms";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {HttpClientModule} from "@angular/common/http";
import {PurifyModule} from "tim/util/purify.module";
import {getKeyCode, KEY_LEFT, KEY_RIGHT} from "tim/util/keycodes";
import {registerPlugin} from "tim/plugin/pluginRegistry";
import {CommonModule} from "@angular/common";
import {Users} from "tim/user/userService";
import type {IUser} from "tim/user/IUser";
import type {Iframesettings} from "../../cs/js/jsframe";
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

function time02String(time: number) {
    const {hours, minutes, seconds} = seconds2Time(time);
    let hs = "";
    let ms = "";
    if (hours) {
        hs = hours + ":";
    }
    if (hours || minutes) {
        ms = minutes + ":";
        if (minutes < 10) {
            ms = "0" + ms;
        }
    }
    let ss = "" + seconds;
    if (seconds < 10) {
        ss = "0" + ss;
    }
    return hs + ms + ss;
}

const youtubeDomains = new Set(["www.youtube.com", "youtube.com", "youtu.be"]);
const moniviestinDomains = new Set(["m3.jyu.fi", "moniviestin.jyu.fi"]);
const moniviestinIdConverters = new Map<string, RegExp>([
    // TODO: Moniviestin does not support video end, figure out how to get it
    // ["m3static.cc.jyu.fi", /m3videos\/3\/jyumv\/([^\/]+)/],
]);

const SubtitlesMarkup = t.type({
    name: withDefault(t.string, ""),
    file: withDefault(t.string, ""),
});
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
        crossOrigin: nullable(t.string),
        start: t.union([t.number, t.string]),
        videoname: nullable(t.string),
        thumbnailFile: nullable(t.string),
        width: t.number,
        defaultSubtitles: t.string,
        autoplay: t.boolean,
        endJSRunner: t.string,
    }),
    GenericPluginMarkup,
    t.type({
        file: withDefault(t.string, ""),
        subtitles: withDefault(t.array(SubtitlesMarkup), []),
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
        <div [class]="videoClass"
             [class.videoOn]="videoOn"
             tabindex="0"
             (keydown.-)="speed(1.0/1.2, $event)"
             (keydown.1)="speed(0, $event)"
             (keydown.+)="speed(1.2/1.0, $event)"
             (keydown.x)="zoom(1.0/1.4, $event)"
             (keydown.r)="zoom(0, $event)"
             (keydown.z)="zoom(1.4/1.0, $event)"
             (keydown.s)="markStart($event)"
             (keydown.e)="markEnd($event)"
             (keydown.c)="copyStartEnd($event)"
             (keydown.arrowLeft)="jump(-1, $event)"
             (keydown.arrowRight)="jump(1, $event)"
             (keydown.control.arrowLeft)="jump(-10, $event)"
             (keydown.control.arrowRight)="jump(10, $event)"
             (keydown.shift.control.arrowLeft)="jump(-60, $event)"
             (keydown.shift.control.arrowRight)="jump(60, $event)"
             (keydown.f)="jump(-10, $event)"
             (keydown.g)="jump(-1, $event)"
             (keydown.h)="jump(1, $event)"
             (keydown.j)="jump(10, $event)"
        >
            <tim-markup-error *ngIf="markupError" [data]="markupError"></tim-markup-error>
            <p *ngIf="header" [innerHtml]="header | purify"></p>
            <p *ngIf="stem && isNormalSize" class="stem" [innerHtml]="stem | purify"></p>

            <div *ngIf="!isNormalSize" class="videoInfo">
                <span [innerHtml]="stem | purify"></span>&ngsp;
                <a *ngIf="videoName" class="videoname"
                   (click)="toggleVideo()">
                    <i *ngIf="markup.videoicon" class="glyphicon glyphicon-facetime-video"></i>
                    {{videoName}}
                    <ng-container *ngIf="markup.type === 'list'">
                        &ndash;
                        {{startt}}
                    </ng-container>
                    {{duration}} {{span}}</a>
                &ngsp;
                <tim-video-link
                        *ngIf="markup.doctext"
                        [doclink]="markup.doclink ?? markup.file"
                        [doctext]="doctext"
                        [target]="markup.target"
                ></tim-video-link>
            </div>

            <ng-container *ngIf="videoOn">
                <iframe *ngIf="iframesettings && isPdf"
                        class="showVideo"
                        frameborder="0"
                        allowfullscreen
                        [src]="iframesettings.src"
                        [style.width.px]="width"
                        [style.height.px]="height"
                        [attr.allow]="iframesettings.allow"
                >
                </iframe>
                <iframe *ngIf="iframesettings && !isPdf"
                        class="showVideo"
                        frameborder="0"
                        allowfullscreen
                        [src]="iframesettings.src"
                        [style.width.px]="width"
                        [style.height.px]="height"
                        [sandbox]="iframesettings.sandbox"
                        [attr.allow]="iframesettings.allow"
                >
                </iframe>
                <video *ngIf="videosettings"
                       #video
                       class="showVideo"
                       controls
                       (loadedmetadata)="metadataloaded()"
                       (timeupdate)="timeupdate()"
                       (pause)="handlePause()"
                       (ended)="handleEnded()"
                       [style.width.px]="width"
                       [style.height.px]="height"
                       [src]="videosettings.src"
                       [crossOrigin]="videosettings.crossOrigin"
                       [autoplay]="videoAutoPlay"
                >
                        <track *ngFor="let subtitle of markup.subtitles" [src]="subtitle.file" [label]="subtitle.name" />    
                </video>
            </ng-container>
            <ng-container *ngIf="isNormalSize">
                <div *ngIf="!videoOn" class="no-popup-menu play">
                    <a (click)="toggleVideo()" [class.video-thumbnail]="markup.thumbnailFile">
                        <img *ngIf="markup.thumbnailFile" class="video-thumbnail-image" [src]="markup.thumbnailFile">
                        <i *ngIf="markup.videoicon" class="glyphicon glyphicon-play-circle video-icon"
                           title="Click here to show the video"></i>
                    </a>
                </div>
                <tim-video-link
                        *ngIf="markup.doctext"
                        [doclink]="markup.doclink ?? markup.file"
                        [doctext]="doctext"
                        [target]="markup.target"
                ></tim-video-link>
            </ng-container>
            <div class="flex" *ngIf="videoOn" style="justify-content: flex-end">
                <div *ngIf="videosettings" class="margin-5-right">
                    <label class="normalLabel" title="Advanced video controls">Adv <input type="checkbox" [(ngModel)]="advVideo" (ngModelChange)="onAdvVideoStateChange($event)" /></label>
                    Speed:
                    <span class="text-smaller">
                        {{playbackRateString}}
                    </span>
                    <a (click)="speed(1.0/1.2)" title="Slower speed (-)"><i class="glyphicon glyphicon-minus"></i></a>&ngsp;
                    <a (click)="speed(0)" title="Normal speed (1)">1x</a>&ngsp;
                    <a (click)="speed(1.2)" title="Faster speed (+)"><i class="glyphicon glyphicon-plus"></i></a>&ngsp;
                </div>
                <div class="margin-5-right">
                    Zoom:
                    <a (click)="zoom(1.0/1.4)" title="Zoom out (x)"><i class="glyphicon glyphicon-minus"></i></a>&ngsp;
                    <a (click)="zoom(0)" title="Normal zoom (r)">R</a>&ngsp;
                    <a (click)="zoom(1.4)" title="Zoom in (z)"><i class="glyphicon glyphicon-plus"></i></a>
                </div>
                <a (click)="hideVideo()">{{hidetext}}</a>
            </div>
            <div *ngIf="advVideo && videoOn">
                <span>Jump sec: </span>
                <a (click)="jump(-10)" title="Jump -10s (ctrl <- or f)">-10</a>
                <a (click)="jump(-2)" title="Jump -2s">-2</a>
                <a (click)="jump(-1)" title="Jump -1s (<- or g)">-1</a>
                <a (click)="jump(1)" title="Jump  +1s (-> or h)">+1</a>
                <a (click)="jump(2)" title="Jump  +2s">+2</a>
                <a (click)="jump(10)" title="Jump +10s (ctrl -> or j)">+10</a>
                <span>&nbsp;&nbsp;</span>
                <a (click)="markStart()" title="Mark start (s)">Start: </a>
                <a (click)="jumpTo(0)" [innerHtml]="startTime" title="Jump to start time"> </a>
                <span>&nbsp;&nbsp;</span>
                <a (click)="markEnd()" title="Mark end (e)">End: </a>
                <a (click)="jumpTo(1)" [innerHtml]="endTime" title="Jump to end time"> </a>
                <span>&nbsp;&nbsp;</span>
                <a (click)="copyStartEnd()" title="Copy start/end to clipboard (c)">Copy</a>
            </div>
            <p class="plgfooter" *ngIf="footer" [innerHtml]="footer | purify"></p>
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

    get iframe(): boolean {
        try {
            const url = new URL(this.markup.file, location.origin);
            const conditions: boolean[] = [
                this.markup.iframe !== undefined,
                youtubeDomains.has(url.hostname),
                moniviestinDomains.has(url.hostname),
                moniviestinIdConverters.has(url.hostname),
            ];
            return conditions.includes(true);
        } catch {
            return false;
        }
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

    // Removing the ElementRef caused the video's advanced controls not to work properly.
    @ViewChild("video") video?: ElementRef<HTMLVideoElement>;

    private limits!: string | null;
    duration!: string | null;
    startt!: string | null;
    width?: number;
    height?: number;
    videoName?: string;

    iframesettings?: Iframesettings;
    isPdf = false;
    videosettings?: {src: string; crossOrigin: string | null};
    playbackRateString = "";
    advVideo: boolean = true;
    requiresTaskId = false;
    advVideoState = new TimStorage("advVideoState", t.boolean);

    onAdvVideoStateChange(newValue: boolean) {
        this.advVideoState.set(newValue);
    }

    ngOnInit() {
        super.ngOnInit();
        this.advVideo = this.advVideoState.get() ?? false;
        this.start = toSeconds(this.markup.start);
        this.end = toSeconds(this.markup.end);
        this.bookmarks[0] = this.start ?? 0;
        this.bookmarks[1] = this.end ?? 10000;
        this.startTime = time02String(this.bookmarks[0]);
        this.endTime = time02String(this.bookmarks[1]);
        if (!this.end) {
            this.endTime = "to end";
        }
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
        this.videoName = this.markup.videoname ?? undefined;
        if (this.markup.open) {
            this.toggleVideo();
        } else if (!this.videoName && !this.doctext) {
            this.videoName = $localize`Open embedded content`;
        }
    }

    hideVideo() {
        this.removeEventListeners();
        this.videoOn = false;
        this.span = "";
    }

    eventListenersActive: boolean = false;

    private removeEventListeners() {
        if (!this.eventListenersActive) {
            return;
        }
        this.eventListenersActive = false;
        document.removeEventListener("keydown", this.keyDownVideo);
        // document.removeEventListener("click", this.onClick);
    }

    private addEventListeners() {
        return;
        if (this.eventListenersActive) {
            return;
        }
        this.eventListenersActive = true;
        document.addEventListener("keydown", this.keyDownVideo);
        // document.addEventListener("click", this.onClick);
    }

    private keyDownVideo = (ev: KeyboardEvent) => {
        const keyCode = getKeyCode(ev);
        if (keyCode === KEY_LEFT) {
            ev.preventDefault();
            this.jump(-10);
        } else if (keyCode === KEY_RIGHT) {
            ev.preventDefault();
            this.jump(10);
        }
    };

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

    speed(mult: number, $event: Event | undefined = undefined) {
        if (!this.video) {
            return;
        }
        if ($event) {
            $event.preventDefault();
        }
        const v = this.video.nativeElement;
        if (mult === 0) {
            v.playbackRate = 1.0;
        } else {
            v.playbackRate *= mult;
        }
        this.playbackRateString = v.playbackRate.toFixed(1);
    }

    bookmarks: number[] = [0, 0];
    startTime: string = "";
    endTime: string = "";

    markTime(
        nr: number,
        $event: Event | undefined = undefined
    ): string | undefined {
        if (!this.video) {
            return undefined;
        }
        if ($event) {
            $event.preventDefault();
        }
        const v = this.video.nativeElement;
        this.bookmarks[nr] = v.currentTime;
        return time02String(v.currentTime);
    }

    markStart($event: Event | undefined = undefined) {
        const mt = this.markTime(0, $event);
        if (mt == undefined) {
            return;
        }
        this.startTime = mt;
    }

    markEnd($event: Event | undefined = undefined) {
        const mt = this.markTime(1, $event);
        if (mt == undefined) {
            return;
        }
        this.endTime = mt;
    }

    copyStartEnd($event: Event | undefined = undefined) {
        if ($event) {
            $event.preventDefault();
        }
        const s =
            "start: " + this.startTime + "\n" + "end: " + this.endTime + "\n";
        copyToClipboard(s);
    }

    jumpTo(value: number, $event: Event | undefined = undefined) {
        if (!this.video) {
            return;
        }
        if ($event) {
            $event.preventDefault();
        }
        const v = this.video.nativeElement;
        v.currentTime = this.bookmarks[value];
    }

    jump(value: number, $event: Event | undefined = undefined) {
        if (!this.video) {
            return;
        }
        if ($event) {
            $event.preventDefault();
        }
        const v = this.video.nativeElement;
        v.currentTime += value;
    }

    zoom(mult: number, $event: Event | undefined = undefined) {
        if ($event) {
            $event.preventDefault();
        }
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

    get videoAutoPlay() {
        return this.markup.autoplay ?? true;
    }

    toggleVideo() {
        if (this.videoOn) {
            this.hideVideo();
            return;
        }
        this.getCurrentZoom();
        this.addEventListeners();

        this.span = this.limits;
        let corsOptions: string | null = null;
        const srcUrl = new URL(this.markup.file, location.origin);
        if (moniviestinDomains.has(srcUrl.hostname)) {
            if (this.start) {
                srcUrl.hash = "#position=" + this.start;
            }
        } else {
            if (this.start) {
                srcUrl.searchParams.set("start", this.start.toString());
            }
            if (this.end) {
                srcUrl.searchParams.set("end", this.end.toString());
            }
        }
        if (srcUrl.hostname.includes("courses.it.jyu.fi")) {
            corsOptions = "anonymous";
        }
        if (this.iframe) {
            if (
                youtubeDomains.has(srcUrl.hostname) &&
                !srcUrl.pathname.includes("embed")
            ) {
                let id;
                if (srcUrl.hostname.includes("youtu.be")) {
                    // Shortened form: https://youtu.be/1OygRiwlAok
                    id = srcUrl.pathname.substring(1);
                } else {
                    // Normal form: https://www.youtube.com/watch?v=1OygRiwlAok
                    id = srcUrl.searchParams.get("v") ?? "";
                    srcUrl.searchParams.delete("v");
                }
                srcUrl.hostname = "www.youtube.com";
                srcUrl.pathname = `/embed/${id}`;
            }
            if (youtubeDomains.has(srcUrl.hostname) && this.markup.autoplay) {
                srcUrl.searchParams.set("autoplay", "1");
            }
            const moniviestinConvertPattern = moniviestinIdConverters.get(
                srcUrl.hostname
            );
            if (moniviestinConvertPattern) {
                const data = moniviestinConvertPattern.exec(srcUrl.pathname);
                if (data) {
                    srcUrl.hostname = "m3.jyu.fi";
                    srcUrl.pathname = `/jyumv/embed`;
                    srcUrl.search = "";
                    srcUrl.searchParams.append("uid", data[1]);
                }
                console.log(data);
            }
            const src = srcUrl.toString();
            this.isPdf =
                src.includes(".pdf") && // TODO: hack for Mac Safari see https://github.com/TIM-JYU/TIM/issues/2114
                isSafari();
            let defaultOpts = 'sandbox="allow-scripts allow-same-origin"';
            if (this.markup.autoplay) {
                defaultOpts += ' allow="autoplay"';
            }
            const iframeopts = parseIframeopts(
                this.markup.iframeopts ??
                    // Some GeoGebra instances use showVideo plugin.
                    // The allow-same-origin is needed for GeoGebra on iPad.
                    defaultOpts,
                src
            );
            this.iframesettings = {
                src: this.domSanitizer.bypassSecurityTrustResourceUrl(src),
                width: this.width ?? null,
                height: this.height ?? null,
                sandbox: iframeopts.sandbox,
                allow: iframeopts.allow,
            };
        } else {
            let range = "";
            if (this.start) {
                range += this.start; // loadedmetadata event doesn't work on iPad
                if (this.end) {
                    range += "," + this.end;
                }
            }
            if (range) {
                srcUrl.hash = "#t=" + range;
            }
            this.videosettings = {
                src: srcUrl.toString(),
                crossOrigin: this.markup.crossOrigin ?? corsOptions ?? null,
            };
        }
        this.videoOn = true;
        this.watchEnd = this.end;
    }

    metadataloaded() {
        if (!this.video) {
            return;
        }
        const v = this.video.nativeElement;
        if (this.markup.defaultSubtitles) {
            const track = [...v.textTracks].find(
                (vt) => vt.label === this.markup.defaultSubtitles
            );
            if (track) {
                track.mode = "showing";
            }
        }
        this.video.nativeElement.currentTime = this.start ?? 0;
        if (this.markup.followid && this.vctrl) {
            this.vctrl.registerVideo(
                this.markup.followid,
                this.video.nativeElement
            );
        }
        if (this.end && this.end < 0) {
            this.end = v.duration + this.end;
            this.watchEnd = this.end;
        }
    }

    timeupdate() {
        const v = this.video!.nativeElement;
        if (this.watchEnd && v.currentTime > this.watchEnd) {
            v.pause();
            this.watchEnd = 1000000;
        }
    }

    handlePause() {
        const videoEl = this.video?.nativeElement;
        if (!videoEl) {
            return;
        }

        const curTime = Math.floor(videoEl.currentTime);
        const end = this.end ?? Math.floor(videoEl.duration);
        // Account for the case where the video is continued beyond the end
        if (curTime >= end) {
            this.handleEnded();
        }
    }

    handleEnded() {
        this.markVideoCompleted();
    }

    markVideoCompleted() {
        if (this.markup.endJSRunner) {
            this.vctrl.runJsRunner(
                this.markup.endJSRunner,
                Users.getSessionUsers().map((u: IUser) => u.name)
            );
        }
        // this.vctrl.runJsRunner();
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
    imports: [
        CommonModule,
        TimUtilityModule,
        HttpClientModule,
        FormsModule,
        PurifyModule,
    ],
})
export class VideoModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

registerPlugin("tim-video", VideoModule, VideoComponent);
