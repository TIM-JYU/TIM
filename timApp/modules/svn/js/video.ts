import angular from "angular";
import * as t from "io-ts";
import {ViewCtrl} from "tim/document/viewctrl";
import {GenericPluginMarkup, Info, nullable, withDefault} from "tim/plugin/attributes";
import {PluginBase, pluginBindings} from "tim/plugin/util";
import {seconds2Time, valueDefu, valueOr} from "tim/util/utils";

const videoApp = angular.module("videoApp", ["ngSanitize"]);
export const moduleDefs = [videoApp];

function muunna(value: string | number | undefined): number | undefined {
    if (!value) {
        return undefined;
    }
    if (typeof value == "number") {
        return value;
    }

    let s = "0 0 0 " + value.replace(/s/g, "").replace(/[,\/;:.hm]/g, " "); // loppu s unohdetaan muodosta 1h3m2s
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

videoApp.component("videoZoom", {
    bindings: {
        c: "<",
    },
    template: `
<p ng-if="$ctrl.c.videoOn" class="pluginShow"><span ng-if="::$ctrl.c.video.playbackRate"> Speed:
    <span class="videoSpeed">{{$ctrl.c.playbackRateString}}</span>
    <a ng-click="$ctrl.c.speed(1.0/1.2)" title="Slow speed"> - </a>
    <a ng-click="$ctrl.c.speed(0)" title="Speed to 1x"> 1x </a>
    <a ng-click="$ctrl.c.speed(1.2)" title="Faster speed"> + </a> </span><span class="zoom">Zoom:
    <a ng-click="$ctrl.c.zoom(1.0/1.4)" title="Zoom out"> - </a>
    <a ng-click="$ctrl.c.zoom(0)" title="Reset to original size"> r </a>
    <a ng-click="$ctrl.c.zoom(1.4)" title="Zoom in"> + </a></span>
    <a ng-click="$ctrl.c.hideVideo()">{{::$ctrl.c.hidetext}}</a>
</p>
`,
});

function time2String(time: number) {
    if (!time) {
        return "";
    }
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

function isYoutube(file: string) {
    if (!file) {
        return false;
    }
    if (file.includes("youtube")) {
        return true;
    }
    return file.includes("youtu.be");
}

function ifIs(value: number | undefined, name: string) {
    if (!value) {
        return "";
    }
    return name + '="' + value + '" ';
}

const ShowFileMarkup = t.intersection([
    t.partial({
        docicon: nullable(t.string),
        doclink: nullable(t.string),
        doctext: nullable(t.string),
        end: t.union([t.number, t.string]),
        followid: t.string,
        height: t.number,
        hidetext: nullable(t.string),
        iframe: t.boolean,
        iframeopts: nullable(t.string),
        start: t.union([t.number, t.string]),
        videoicon: t.union([t.literal(false), t.null, t.string]),
        videoname: nullable(t.string),
        width: t.number,
    }),
    GenericPluginMarkup,
    t.type({
        autoplay: withDefault(t.boolean, true),
        file: withDefault(t.string, ""),
        target: withDefault(t.string, "timdoc"),
        open: withDefault(t.boolean, false),
    }),
]);
const ShowFileAll = t.type({
    info: Info,
    markup: ShowFileMarkup,
    preview: t.boolean,
});

class ShowFileController extends PluginBase<t.TypeOf<typeof ShowFileMarkup>,
    t.TypeOf<typeof ShowFileAll>,
    typeof ShowFileAll> {

    get videoicon() {
        return valueOr(this.attrs.videoicon, "/csstatic/video_small.png");
    }

    get docicon() {
        return this.attrs.docicon ?? "/csstatic/book.png";
    }

    get iframe() {
        return this.attrs.iframe ?? isYoutube(this.attrs.file);
    }

    get videoname() {
        return this.attrs.videoname ?? null;
    }

    get doclink() {
        return valueDefu(this.attrs.doclink, "");
    }

    get hidetext() {
        return valueDefu(this.attrs.hidetext, "hide video");
    }

    get doctext() {
        return this.attrs.doctext ?? null;
    }

    private watchEnd?: number;
    private videoHtml!: HTMLElement;
    private start?: number;
    private end?: number;
    private videoOn: boolean = false;
    private span?: string | null;
    private origSize!: string;
    private origWidth?: number;
    private origHeight?: number;
    private video?: HTMLVideoElement;
    private limits!: string | null;
    private duration!: string | null;
    private startt!: string | null;
    private width?: number;
    private height?: number;
    private vctrl?: ViewCtrl;
    private iframeopts?: string;
    private playbackRateString: string = "";

    $onInit() {
        super.$onInit();
        const n = this.attrs.file || "";
        this.iframeopts =  this.attrs.iframeopts ?? 'sandbox="allow-scripts allow-same-origin"';
        if (n.endsWith(".pdf")) {
            this.iframeopts = ""; // for Chrome :-( Sandboxed viewer does not work!
        }

        this.start = muunna(this.attrs.start);
        this.end = muunna(this.attrs.end);
        this.width = this.attrs.width;
        this.height = this.attrs.height;
        if (this.start != null && this.end != null) {
            this.duration = `(${time2String(this.end - this.start)})`;
            this.limits = `(${time2String(this.start)}-${time2String(this.end)})`;
            this.startt = `, ${time2String(this.start)}`;
        } else {
            this.duration = null;
            this.limits = null;
            this.startt = null;
        }
        this.getPrevZoom();
    }

    $postLink() {
        super.$postLink();
        this.videoHtml = this.element.find(".videoContainer")[0];
        if (this.attrs.open) {
            this.showVideo();
        }
    }

    hideVideo() {
        this.videoOn = false;
        this.videoHtml.innerHTML = "<p></p>";
        this.span = "";
        return true;
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
        if (mult === 0) {
            this.video.playbackRate = 1.0;
        } else {
            this.video.playbackRate *= mult;
        }
        this.playbackRateString = this.video.playbackRate.toFixed(1);
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
        this.hideVideo();
        this.showVideo();
    }

    showVideo() {
        if (this.videoOn) {
            return this.hideVideo();
        }
        this.getCurrentZoom();

        this.span = this.limits;
        const w = ifIs(this.width, "width");
        const h = ifIs(this.height, "height");
        const moniviestin = this.attrs.file.includes("m3.jyu.fi");
        let params = "?";
        if (this.start) {
            if (moniviestin) {
                params = "#position=" + this.start;
            } else {
                params = "?start=" + this.start + "&end=" + this.end;
            }
        }
        if (this.iframe) {
            let file = this.attrs.file;
            if (isYoutube(file) && !file.includes("embed")) {
                const yname = "youtu.be/";  // could be also https://youtu.be/1OygRiwlAok
                const yembed = "//www.youtube.com/embed/";
                const iy = file.indexOf(yname);
                const parts = file.split("=");
                if (parts.length > 1) {
                    file = yembed + parts[1];
                } else if (iy >= 0) {
                    file = yembed + file.substring(iy + yname.length);
                }
            }
            this.videoHtml.innerHTML = `
<iframe class="showVideo"
        src="${file}${params}"
        ${w}${h}
        frameborder="0"
        allowfullscreen
        ${this.iframeopts ?? ""}>
</iframe>`;
        } else {
            params = "";
            let tbe = "";
            if (this.start) {
                tbe += this.start; // iPad ei tottele 'loadedmetadata'
                if (this.end) {
                    tbe += "," + this.end;
                }
            }
            if (tbe) {
                params = "#t=" + tbe;
            }
            let autoplay = "";
            if (this.attrs.autoplay) {
                autoplay = "autoplay";
            }
            this.videoHtml.innerHTML = `
<video class="showVideo"
       src="${this.attrs.file}${params}"
       controls
       ${autoplay}
       ${w}${h}/>`;
            this.video = this.videoHtml.firstElementChild as HTMLVideoElement;
        }
        this.videoOn = true;
        if (!this.video) {
            return;
        }
        if (this.attrs.followid && this.vctrl) {
            this.vctrl.registerVideo(this.attrs.followid, this.video);
        }
        this.video.addEventListener("loadedmetadata", () => {
            this.video!.currentTime = this.start ?? 0;
        }, false);

        this.watchEnd = this.end;
        this.video.addEventListener("timeupdate", () => {
            if (this.watchEnd && this.video!.currentTime > this.watchEnd) {
                this.video!.pause();
                this.watchEnd = 1000000;
            }
        }, false);
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

const common = {
    bindings: pluginBindings,
    controller: ShowFileController,
    require: {
        vctrl: "?^timView",
    },
};

videoApp.component("videoRunner", {
    ...common,
    template: `
<div class="videoRunDiv">
    <tim-markup-error ng-if="::$ctrl.markupError" [data]="::$ctrl.markupError"></tim-markup-error>
    <p ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></p>
    <p ng-if="::$ctrl.stem" class="stem" ng-bind-html="::$ctrl.stem"></p>
    <div class="videoContainer"></div>
    <div class="no-popup-menu">
        <img src="/csstatic/video.png"
             ng-if="!$ctrl.videoOn"
             ng-click="$ctrl.showVideo()"
             width="200"
             alt="Click here to show the video"/></div>
    <a href="{{::$ctrl.doclink}}" ng-if="::$ctrl.doclink" rel=”noopener” target="{{::$ctrl.attrs.target}}">
        <span ng-if="::$ctrl.docicon"><img ng-src="{{::$ctrl.docicon}}"
                                    alt="Go to doc"/> </span>{{::$ctrl.doctext}}</a>
    <video-zoom c="::$ctrl"></video-zoom>
    <p class="plgfooter" ng-if="::$ctrl.footer" ng-bind-html="::$ctrl.footer"></p>
</div>
`,
});

videoApp.component("smallVideoRunner", {
    ...common,
    template: `
<div class="smallVideoRunDiv">
    <tim-markup-error ng-if="::$ctrl.markupError" [data]="::$ctrl.markupError"></tim-markup-error>
    <p ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></p>
    <p><span class="stem" ng-bind-html="::$ctrl.stem"></span>
        <a ng-if="::$ctrl.videoname" class="videoname"
           ng-click="$ctrl.showVideo()"><span ng-if="::$ctrl.videoicon">
            <img ng-src="{{::$ctrl.videoicon}}" alt="Click here to show"/> </span>
            {{::$ctrl.videoname}} {{::$ctrl.duration}} {{::$ctrl.span}}</a>
        <a href="{{::$ctrl.doclink}}" ng-if="::$ctrl.doclink" rel=”noopener” target="{{::$ctrl.attrs.target}}">
            <span ng-if="::$ctrl.docicon"><img ng-src="{{::$ctrl.docicon}}"
                                             alt="Go to doc"/> </span>{{::$ctrl.doctext}}</a>
    </p>
    <div class="videoContainer"></div>
    <video-zoom c="::$ctrl"></video-zoom>
    <p class="plgfooter" ng-if="::$ctrl.footer" ng-bind-html="::$ctrl.footer"></p>
</div>
`,
});

videoApp.component("listVideoRunner", {
    ...common,
    template: `
<div class="listVideoRunDiv">
    <tim-markup-error ng-if="::$ctrl.markupError" [data]="::$ctrl.markupError"></tim-markup-error>
    <p ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></p>
    <ul>
        <li><span class="stem" ng-bind-html="::$ctrl.stem"></span>
            <a ng-if="::$ctrl.videoname" class="videoname"
               ng-click="$ctrl.showVideo()">
                <span ng-if="::$ctrl.videoicon">
                    <img ng-src="{{::$ctrl.videoicon}}" alt="Click here to show"/>
                </span>{{::$ctrl.videoname}}{{::$ctrl.startt}}
                {{::$ctrl.duration}}
                {{::$ctrl.span}}</a>
            <a href="{{::$ctrl.doclink}}" ng-if="::$ctrl.doclink" rel=”noopener” target="{{::$ctrl.attrs.target}}"><span
                    ng-if="::$ctrl.docicon"><img
                    ng-src="{{::$ctrl.docicon}}" alt="Go to doc"/> </span>{{::$ctrl.doctext}}</a></li>
    </ul>
    <div class="videoContainer"></div>
    <video-zoom c="::$ctrl"></video-zoom>
    <p class="plgfooter" ng-if="::$ctrl.footer" ng-bind-html="::$ctrl.footer"></p>
</div>
`,
});
