import $ from "jquery";
import Reveal from "reveal";
import {background_color, background_url, is_owner, item} from "tim/show_slide_vars";
import {GetURLParameter} from "tim/utils";

// Full list of configuration options available here:
// https://github.com/hakimel/reveal.js#configuration
Reveal.initialize({
    fragments: true,
    width: 1150,
    controls: true,
    progress: true,
    history: true,
    center: true,
    // Flags if speaker notes should be visible to all viewers
    showNotes: false,
    viewDistance: 10,
    theme: Reveal.getQueryHash().theme, // available themes are in /css/theme
    transition: Reveal.getQueryHash().transition || "fade", // default/cube/page/concave/zoom/linear/fade/none
    updateSlideStatus,
    isOwner: is_owner,
    // Optional libraries used to extend on reveal.js
    dependencies: [
        {
            src: "/static/scripts/reveal/lib/js/classList.js",
            condition() {
                return !document.body.classList;
            },
        },
        {

            src: "/static/scripts/reveal/plugin/zoom-js/zoom.js",
            async: true,
            condition() {
                return !!document.body.classList;
            },
        },
        {
            src: "/static/scripts/reveal/plugin/notes/notes.js",
            async: true,
            condition() {
                return !!document.body.classList;
            },
        },
    ],
});
const pollInterval = 500;
let pollTimeout;
let receiving = true;

function refresh() {
    return; // TODO: think this so that things are paired
    clearTimeout(pollTimeout);
    $.ajax({
        cache: false,
        url: "/getslidestatus/",
        data: {doc_id: item.id},
        dataType: "json",
        error(xhr, status, err) {
            console.log("error");
            pollTimeout = setTimeout(refresh, pollInterval);
        },
        success(data) {
            const oldstate = Reveal.getState();
            let oldh = 0;
            let oldv = 0;
            let newh = 0;
            let newv = 0;
            if (oldstate.indexh !== undefined) {
                oldh = oldstate.indexh;
            }
            if (oldstate.indexv !== undefined) {
                oldv = oldstate.indexv;
            }
            data = JSON.parse(data);
            console.log(data);
            if (data !== null) {
                if (data.indexh !== undefined) {
                    newh = data.indexh;
                }
                if (data.indexv !== undefined) {
                    newv = data.indexv;
                }
                if ((newh != oldh || newv != oldv
                    || data.indexf != oldstate.indexf) && receiving) {
                    console.log("Change slide");
                    Reveal.slide(newh, newv, data.indexf, "remote");
                }
            }
            pollTimeout = setTimeout(refresh, pollInterval);
        },
    });
}

function updateSlideStatus(h, v, f) {
    if (GetURLParameter("controls") !== undefined) {
        return;
    }
    receiving = false;
    clearTimeout(pollTimeout);
    $.ajax({
        dataType: "json",
        url: "/setslidestatus",
        data: {doc_id: item.id, status: JSON.stringify({indexh: h, indexv: v, indexf: f})},
        success() {
            pollTimeout = setTimeout(refresh, pollInterval);
            receiving = true;
        },
        error() {
            console.log("error");
            pollTimeout = setTimeout(refresh, pollInterval);
            receiving = true;
        },
    });
}

if (GetURLParameter("controls") === undefined && is_owner) {
    pollTimeout = setTimeout(refresh, pollInterval);
}

window.onload = function() {
    document.onkeyup = function(evt) {
        if (evt.keyCode == 82) {
            pollTimeout = setTimeout(refresh, pollInterval);
        }
    };
    setTimeout(function() {
        Reveal.slide();
    }, 2000);
};

if (background_url) {
    if (background_url == "none") {
        $(".backgrounds").css("background-image", "None");
    } else {
        $(".backgrounds").css("background-image", "url('" + background_url + "')");
    }
}

if (background_color) {
    if (background_color != "none") {
        $(".backgrounds").css("background-color", background_color);
    }
}
