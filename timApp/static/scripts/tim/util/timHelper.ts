import {$sanitize} from "./ngimport";

// DO NOT USE THESE FUNCTIONS IN NEW CODE!

export function toHeading(h: string, defElem: string) {
    if (!h) {
        return "";
    }
    const st = h.split("!!"); // h4 class="h3" width="23"!!Tehtava 1
    let elem = defElem;
    let val = st[0];
    let attributes = "";
    if (st.length >= 2) {
        elem = st[0];
        val = st[1];
    }
    const i = elem.indexOf(" ");
    let ea = [elem];
    if (i >= 0) {
        ea = [elem.substring(0, i), elem.substring(i)];
    }
    if (ea.length > 1) {
        elem = ea[0];
        attributes = " " + ea[1] + " ";
    }
    try {
        val = decodeURIComponent(encodeURI(val));
    } catch (err) {
    }
    let html = "<" + elem + attributes + ">" + val + "</" + elem + ">";
    html = $sanitize(html);
    return html;
}
