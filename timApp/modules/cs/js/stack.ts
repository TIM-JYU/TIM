import angular from "angular";
import * as t from "io-ts";
import {GenericPluginMarkup, PluginBase, withDefault} from "tim/plugin/util";

const stackApp = angular.module("stackApp", ["ngSanitize"]);


function ifIs(value: number | undefined, name: string) {
    if (!value) {
        return "";
    }
    return name + '="' + value + '" ';
}

const StackMarkup = t.intersection([
    t.partial({
        docicon: t.string,
        doclink: t.string,
        doctext: t.string,
        end: t.string,
        followid: t.string,
        height: t.number,
        hidetext: t.string,
        iframe: t.boolean,
        iframeopts: t.string,
        start: t.string,
        videoicon: t.string,
        videoname: t.string,
        width: t.number,
    }),
    GenericPluginMarkup,
    t.type({
        autoplay: withDefault(t.boolean, true),
        file: t.string,
        open: withDefault(t.boolean, false),
    }),
]);
const ShowFileAll = t.type({markup: StackMarkup});

// TODO: register video to ViewCtrl so that ImageX can access it
class StackController extends PluginBase<t.TypeOf<typeof StackMarkup>,
    t.TypeOf<typeof ShowFileAll>,
    typeof ShowFileAll> {
    private static $inject = ["$scope", "$element"];

    get videoicon() {
        return this.attrs.videoicon || "/csstatic/video_small.png";
    }

    get docicon() {
        return this.attrs.docicon || "/csstatic/book.png";
    }

    get iframe() {
        return this.attrs.iframe;
    }

    get videoname() {
        return this.attrs.videoname;
    }

    get doclink() {
        return this.attrs.doclink;
    }

    get hidetext() {
        return this.attrs.hidetext || "hide video";
    }

    get doctext() {
        return this.attrs.doctext;
    }

    private watchEnd?: number;
    private videoHtml!: HTMLElement;
    private start?: number;
    private end?: number;
    private videoOn: boolean = false;
    private span: string = "";
    private origSize!: string;
    private origWidth: any;
    private origHeight: any;
    private video?: HTMLVideoElement;
    private limits: string = "";
    private duration: string = "";
    private startt: string = "";
    private width?: number;
    private height?: number;

    $onInit() {
        super.$onInit();
        this.width = this.attrs.width;
        this.height = this.attrs.height;
    }

    $postLink() {
        super.$postLink();
        this.videoHtml = this.element.find(".videoContainer")[0];
        if (this.attrs.open) {
            this.showStack();
        }
    }

    hideVideo() {
        this.videoOn = false;
        this.videoHtml.innerHTML = "<p></p>";
        this.span = "";
        return true;
    }



    showStack() {
        if (this.videoOn) {
            return this.hideVideo();
        }

        this.span = this.limits;
        const w = ifIs(this.width, "width");
        const h = ifIs(this.height, "height");
        const moniviestin = this.attrs.file.indexOf("m3.jyu.fi") >= 0;
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
            this.videoHtml.innerHTML = `
<iframe class="showVideo"
        src="${file}${params}"
        ${w}${h}
        frameborder="0"
        allowfullscreen
        ${this.attrs.iframeopts || ""}>
</iframe>`;
        } else {
            params = "";
            if (this.start) {
                params = "#params=" + this.start; // iPad ei tottele 'loadedmetadata'
                if (this.end) {
                    params += "," + this.end;
                }
            }
            let autoplay = "";
            if (this.attrs.autoplay) {
                autoplay = "autoplay";
            }
            this.videoHtml.innerHTML = `
<stack class="showVideo"
       src="${this.attrs.file}${params}"
       ${w}${h}/>`;
            this.video = this.videoHtml.firstElementChild as HTMLVideoElement;
        }
        this.videoOn = true;
        if (!this.video) {
            return;
        }
        this.video.addEventListener("loadedmetadata", () => {
            this.video!.currentTime = this.start || 0;
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

    protected getAttributeType() {
        return ShowFileAll;
    }
}

const common = {
    bindings: {
        json: "@",
    },
    controller: StackController,
};

stackApp.component("stackRunner", {
    ...common,
    template: `
<div class="videoRunDiv">
    <p ng-if="$ctrl.header" ng-bind-html="$ctrl.header"></p>
    <p ng-if="$ctrl.stem" class="stem" ng-bind-html="$ctrl.stem"></p>
    <div class="videoContainer"></div>
    <div class="no-popup-menu">
        <img src="/csstatic/video.png"
             ng-if="!$ctrl.videoOn"
             ng-click="$ctrl.showVideo()"
             width="200"
             alt="Click here to show the video"/></div>
    <a href="{{$ctrl.doclink}}" ng-if="$ctrl.doclink" target="timdoc">
        <span ng-if="$ctrl.docicon"><img ng-src="{{$ctrl.docicon}}"
                                    alt="Go to doc"/> </span>{{$ctrl.doctext}}</a>
    <video-zoom c="$ctrl"></video-zoom>
    <p class="plgfooter" ng-if="$ctrl.footer" ng-bind-html="$ctrl.footer"></p>
</div>
`,
});

