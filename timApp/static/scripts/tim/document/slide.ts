import $ from "jquery";
import {getURLParameter, IOkResponse, to} from "tim/util/utils";
import {IDocument, IItem} from "../item/IItem";
import {$http, $log, $timeout, $window} from "../util/ngimport";

const pollInterval = 500;
let receiving = true;

interface ISlideStatus {
    indexh?: number;
    indexv?: number;
    indexf?: number;
}

async function refresh(rv: IFixedReveal) {
    if (1 === 1) { // suppress "unreachable code" warning
        return; // TODO: think this so that things are paired
    }
    const w = window as any;
    const item: IItem = w.item;
    while (true) {
        const r = await to($http.get<null | ISlideStatus>("/getslidestatus", {
            cache: false,
            params: {doc_id: item.id},
        }));
        if (r.ok) {
            const oldstate = rv.getState();
            let oldh = 0;
            let oldv = 0;
            let newh = 0;
            let newv = 0;
            if (oldstate.indexh != null) {
                oldh = oldstate.indexh;
            }
            if (oldstate.indexv != null) {
                oldv = oldstate.indexv;
            }
            const data = r.result.data;
            $log.info(data);
            if (data != null) {
                if (data.indexh != null) {
                    newh = data.indexh;
                }
                if (data.indexv != null) {
                    newv = data.indexv;
                }
                if ((newh != oldh || newv != oldv
                    || data.indexf != oldstate.indexf) && receiving) {
                    $log.info("Change slide");
                    rv.slide(newh, newv, data.indexf, "remote");
                }
            }
        } else {
            console.error("Failed to get slide status");
        }
        await $timeout(pollInterval);
    }
}

async function updateSlideStatus(h: number, v: number, f: number) {
    if (getURLParameter("controls") != null) {
        return;
    }
    receiving = false;
    const w = window as any;
    const item: IItem = w.item;
    const r = await to($http.post<IOkResponse>("/setslidestatus", {
        doc_id: item.id,
        indexf: f,
        indexh: h,
        indexv: v,
    }));
    if (r.ok) {
    } else {
        console.error("Failed to set slide status");
    }
    receiving = true;
}

async function initReveal(rv: IFixedReveal) {
    // Full list of configuration options available here:
    // https://github.com/hakimel/reveal.js#configuration
    const w = window as any;
    const item: IItem = w.item;
    const hasManage = item.rights.manage;
    const pluginPath = "/static/scripts/jspm_packages/npm/reveal.js@3.8.0/plugin";

    $window.Reveal = rv; // required for Reveal dependencies

    rv.initialize({
        fragments: true,
        width: 1150,
        controls: true,
        progress: true,
        history: true,
        center: true,
        showNotes: hasManage,
        viewDistance: 10,
        theme: rv.getQueryHash().theme, // available themes are in /css/theme
        transition: rv.getQueryHash().transition || "linear", // default/cube/page/concave/zoom/linear/fade/none
        dependencies: [
            // {
            //     async: true,
            //     src: `${pluginPath}/zoom-js/zoom.js`,
            // },
            {
                async: true,
                src: `${pluginPath}/notes/notes.js`,
            },
        ],
    });
}

export async function initSlideView(d: IDocument) {
    const w = window as any;
    const bgUrl = w.background_url;
    const bgColor = w.background_color;
    const hasManage = d.rights.manage;
    const rv = await import("reveal");
    if (getURLParameter("controls") == null && hasManage) {
        refresh(rv);
    }
    document.onkeyup = (evt) => {
        if (evt.keyCode === 82) {
            refresh(rv);
        }
    };
    await import("reveal/css/reveal.css" as any);
    await import("./jyu.css" as any);

    initReveal(rv);
    rv.addEventListener("slidechanged", () => {
        fitCurrentSlide();
    });
    fitCurrentSlide();
    if (bgUrl) {
        $(".backgrounds").css("background-image", `url('${bgUrl}')`);
    }

    if (bgColor) {
        $(".backgrounds").css("background-color", bgColor);
    }
}

function fitCurrentSlide() {
    const slide = $("section.present").first();
    const show = $("div.slides").first();
    const inner = slide.innerHeight()!;
    const base = show.height()!;
    const scale = (base * 1.0) / (inner * 1.0);
    const oldscale = $(slide).css("transform");
    if (scale < 1 && (true || oldscale === "none" || oldscale === "matrix(1, 0, 0, 1, 0, 0)")) {
        $(slide).css("transform", "scale(" + scale + ")");
        $(slide).css("transform-origin", "50% 0");
    }
}
