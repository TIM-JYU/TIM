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

function initReveal(rv: IFixedReveal) {
    // Full list of configuration options available here:
    // https://github.com/hakimel/reveal.js#configuration
    const w = window as any;
    const item: IItem = w.item;
    const hasManage = item.rights.manage;
    const pluginPath = "/static/scripts/jspm_packages/npm/reveal.js@3.8.0/plugin";

    $window.Reveal = rv; // required for Reveal dependencies

    rv.initialize({
        fragments: true,
        controls: true,
        progress: true,
        history: true,
        center: true,
        showNotes: hasManage,
        viewDistance: 10,
        theme: rv.getQueryHash().theme, // available themes are in /css/theme
        transition: rv.getQueryHash().transition || "linear", // default/cube/page/concave/zoom/linear/fade/none
        dependencies: [
            {
                async: true,
                src: `${pluginPath}/zoom-js/zoom.js`,
            },
            {
                async: true,
                src: `${pluginPath}/notes/notes.js`,
            },
        ],
        maxScale: 1, // csplugins become too wide in fullscreen view without this
    });
}

export async function initSlideView(d: IDocument) {
    const w = window as any;
    const bgUrl = w.background_url;
    const bgColor = w.background_color;
    const hasManage = d.rights.manage;
    const revealCss = import("reveal/css/reveal.css" as any);
    const jyuCss = import("./jyu.css" as any);
    const rv = await import("reveal");
    if (getURLParameter("controls") == null && hasManage) {
        refresh(rv);
    }
    document.onkeyup = (evt) => {
        if (evt.keyCode === 82) {
            refresh(rv);
        }
    };
    await revealCss;
    await jyuCss;

    initReveal(rv);
    const ctrls = document.querySelector("aside.controls");
    if (!ctrls) {
        return;
    }
    const btn = document.createElement("button");
    btn.classList.add("enabled");
    btn.style.right = ".7em";
    btn.style.bottom = "6em";
    btn.innerHTML = `<i title="Toggle fullscreen mode" class="glyphicon glyphicon-fullscreen" style="font-size: x-large"></i>`;
    ctrls.appendChild(btn);
    btn.addEventListener("click", () => {
        if (document.fullscreenElement) {
            document.exitFullscreen();
        } else {
            document.documentElement.requestFullscreen();
        }
    });
    rv.addEventListener("slidechanged", () => {
        fitCurrentSlide();
    });
    fitCurrentSlide();
    const bgElem = document.querySelector(".backgrounds");
    if (bgElem instanceof HTMLElement) {
        if (bgUrl) {
            bgElem.style.backgroundImage = `url('${bgUrl}')`;
            bgElem.style.backgroundRepeat = "no-repeat";
            bgElem.style.backgroundSize = "contain";
        }
        if (bgColor) {
            bgElem.style.backgroundColor = bgColor;
        }
    } else {
        console.error("Did not find backgrounds element");
    }
    const rElem = document.querySelector(".reveal");
    if (rElem instanceof HTMLElement) {
        // The slides are initially hidden to avoid showing style changes.
        rElem.style.visibility = null;
    } else {
        console.error("Did not find reveal element");
    }
}

function fitCurrentSlide() {
    const slide = $("section.present").first();
    const show = $("div.slides").first();
    const innerH = slide.innerHeight()!;
    const baseH = show.height()!;
    const innerW = slide.innerWidth()!;
    const baseW = show.width()!;
    const scaleH = (baseH * 1.0) / (innerH * 1.0);
    const scaleW = (baseW * 1.0) / (innerW * 1.0); // this seems to be always 1
    const scale = Math.min(scaleH, scaleW);
    const oldscale = $(slide).css("transform");
    if (scale < 1 && (true || oldscale === "none" || oldscale === "matrix(1, 0, 0, 1, 0, 0)")) {
        $(slide).css("transform", "scale(" + scale + ")");
        $(slide).css("transform-origin", "50% 0");
    }
}
