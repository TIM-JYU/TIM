import $ from "jquery";
import {getURLParameter, injectStyle, IOkResponse, to} from "tim/util/utils";
import {environment} from "tim/environments/environment";
import {IDocument, IItem} from "tim/item/IItem";
import {documentglobals, ISlideGlobals, slideglobals} from "tim/util/globals";
import {$http, $log, $timeout} from "tim/util/ngimport";

const pollInterval = 500;
let receiving = true;

const REVEAL_BASE = environment.production ? "en-US/reveal" : "reveal";

interface ISlideStatus {
    indexh?: number;
    indexv?: number;
    indexf?: number;
}

async function refresh(rv: IFixedReveal) {
    if (1 === 1) {
        // suppress "unreachable code" warning
        return; // TODO: think this so that things are paired
    }
    const item: IItem = documentglobals().curr_item;
    while (true) {
        const r = await to(
            $http.get<null | ISlideStatus>("/getslidestatus", {
                cache: false,
                params: {doc_id: item.id},
            })
        );
        if (r.ok) {
            const oldstate = rv.getState() as ISlideStatus;
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
                if (
                    (newh != oldh ||
                        newv != oldv ||
                        data.indexf != oldstate.indexf) &&
                    receiving
                ) {
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

// eslint-disable-next-line @typescript-eslint/no-unused-vars
async function updateSlideStatus(h: number, v: number, f: number) {
    if (getURLParameter("controls") != null) {
        return;
    }
    receiving = false;
    const item: IItem = documentglobals().curr_item;
    const r = await to(
        $http.post<IOkResponse>("/setslidestatus", {
            doc_id: item.id,
            indexf: f,
            indexh: h,
            indexv: v,
        })
    );
    if (r.ok) {
    } else {
        console.error("Failed to set slide status");
    }
    receiving = true;
}

function getTransition(transitionName?: string): RevealOptions["transition"] {
    if (!transitionName) {
        return "slide";
    }
    const ts = ["none", "fade", "slide", "convex", "concave", "zoom"];
    if (ts.includes(transitionName)) {
        return transitionName as RevealOptions["transition"];
    }
    return "slide";
}

async function initReveal(rv: IFixedReveal, s: ISlideGlobals) {
    // Full list of configuration options available here:
    // https://revealjs.com/config/#reconfiguring

    const zoom = (await import("reveal.js/plugin/zoom/zoom.esm")).default;
    const notes = (await import("reveal.js/plugin/notes/notes.esm")).default;
    const [width, height] = s.slide_size;
    await rv.initialize({
        width: width,
        height: height,
        fragments: true,
        controls: true,
        progress: true,
        history: true,
        center: true,
        showNotes: false,
        viewDistance: 10,
        theme: getURLParameter("theme"), // available themes are in /css/theme
        transition: getTransition(getURLParameter("transition")), // default/cube/page/concave/zoom/linear/fade/none
        plugins: [zoom, notes],
        maxScale: 1, // csplugins become too wide in fullscreen view without this
        ...(s.docSettings.slide_revealjs_options ?? {}),
    });
}

function addControl(
    controlContainer: Element,
    iconName: string,
    right: string,
    bottom: string,
    title: string,
    clickHandler: () => void
) {
    const btn = document.createElement("button");
    btn.classList.add("enabled");
    btn.style.right = right;
    btn.style.bottom = bottom;
    const i = document.createElement("i");
    i.title = title;
    i.classList.add("glyphicon");
    i.classList.add(`glyphicon-${iconName}`);
    i.style.fontSize = "x-large";
    btn.appendChild(i);
    controlContainer.appendChild(btn);
    btn.addEventListener("click", clickHandler);
}

export async function initSlideView(d: IDocument) {
    const w = slideglobals();
    const bgUrl = w.background_url;
    const bgColor = w.background_color;
    const hasManage = d.rights.manage;

    injectStyle(`/static/scripts/build/${REVEAL_BASE}/reveal.css`); // base style

    for (const theme of w.themes) {
        if (theme == "jyu") {
            injectStyle("/static/scripts/tim/document/jyu.css");
        } else {
            injectStyle(
                `/static/scripts/build/${REVEAL_BASE}/theme/${theme}.css`
            );
        }
    }

    const rv = (await import("reveal.js")).default;
    if (getURLParameter("controls") == null && hasManage) {
        refresh(rv);
    }
    document.onkeyup = (evt) => {
        if (evt.keyCode === 82) {
            refresh(rv);
        }
    };

    await initReveal(rv, w);
    const ctrls = document.querySelector("aside.controls");
    if (!ctrls) {
        console.error("aside.controls not found");
        return;
    }

    const isSpeakerWindow =
        window.parent.document.location.pathname.endsWith("/notes.html");
    if (!isSpeakerWindow) {
        addControl(
            ctrls,
            "fullscreen",
            ".7em",
            "6em",
            "Toggle fullscreen mode (F / ESC)",
            () => {
                if (document.fullscreenElement) {
                    document.exitFullscreen();
                } else {
                    document.documentElement.requestFullscreen();
                }
            }
        );
        addControl(
            ctrls,
            "list-alt",
            ".7em",
            "9.4em",
            "Open speaker notes (S)",
            () => {
                rv.getPlugin("notes").open();
            }
        );
    }
    document.addEventListener("fullscreenchange", () => {
        fitCurrentSlide();
    });
    rv.addEventListener("slidechanged", () => {
        fitCurrentSlide();
    });
    rv.addEventListener("fragmentshown", (event) => {
        fitCurrentSlide();
    });
    rv.addEventListener("fragmenthidden", (event) => {
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
        rElem.style.visibility = "";
    } else {
        console.error("Did not find reveal element");
    }
}

function fitCurrentSlide() {
    // Pick last to always pick the child
    const slide = $("section.present").last();
    const show = $("div.slides").first();
    const innerH = slide.innerHeight()!;
    const baseH = show.height()!;
    const innerW = slide.innerWidth()!;
    const baseW = show.width()!;
    const scaleH = (baseH * 1.0) / (innerH * 1.0);
    const scaleW = (baseW * 1.0) / (innerW * 1.0); // this seems to be always 1
    const scale = Math.min(scaleH, scaleW);
    const oldscale = $(slide).css("transform");
    if (
        scale < 1 &&
        (true || oldscale === "none" || oldscale === "matrix(1, 0, 0, 1, 0, 0)")
    ) {
        $(slide).css("transform", "scale(" + scale + ")");
        $(slide).css("transform-origin", "50% 0");
    }
}
