/*
  To use timTiming, call

    timLogInit(document.location.search.slice(1));

  at the start of main module. When you want log timing, call:

    timLogTime("description of the event", "location of this call")

  First parameter is printed as it is and the second is for filtering the output;
  see timLogTime comments.

*/
let timJavaScriptStartTime = new Date();
let timJavaScriptLastTime = timJavaScriptStartTime;
let timDivLogger;
let timDivPreLogger = document.createElement("pre");

export function insertLogDivIfEnabled() {
    if ( timTimingDiv && !timDivLogger ) {
        timDivLogger = document.createElement("div");
        timDivLogger.appendChild(timDivPreLogger);
        //document.body.appendChild(timDivLogger);
        document.body.insertBefore(timDivLogger, document.body.firstChild);
    }
}

let timTimingDiv = false;
let timTimingLog = false;
let timTimingFilter;
let timTiming = false;
let timTimingSummary = false;
let timTimingLevel = 0;

export function timLogInit(params: string) {
    /*
    .../view/1?timing=div    - logittaa vain diviin (vain taso 0 tai ei tasoa)
    .../view/1?timing=log    - logittaa vain console.log
    .../view/1?timing=log&timingLevel=1 - logittaa vain console.log, tasot 0-1
    .../view/1?timing=log,div,summary - logit div ja konsolit ja vain summat
    .../view/1?timing=log,div&timingFilter=cs - logit div,konsoli ja vain cs id:llä olevat.  filter on regexp
    */
    let p = params.replace("b&#39;", "");
    p = p.replace("&#39;", "");
    p = p.replace("&amp;", "&");
    p = p.replace("b'", "");
    p = p.replace("'", "");
    let ps = p.split("&");
    for (let i = 0; i < ps.length; i++) {
        let k = (ps[i] + "=").split("=");
        let key = k[0];
        let val = k[1];
        if ( key === "timing" ) {
           if ( val.indexOf("div") >= 0 ) timTimingDiv = true;
           if ( val.indexOf("log") >= 0 ) timTimingLog = true;
           if ( val.indexOf("summary") >= 0 ) timTimingSummary = true;
        }
        if ( key === "timingFilter" ) timTimingFilter = val;
        if ( key === "timingLevel" ) timTimingLevel = parseInt(val, 10);
    }
    timTiming = timTimingDiv || timTimingLog;
    timLogTime("Tim log initialized", "main");
}

let timLogText = "";

export function timLogTime(msg, id, level?) {
    if ( !timTiming ) return;
    if ( timTimingFilter && !id.match(timTimingFilter) ) return;
    let tlevel = level | 0;
    if ( tlevel > timTimingLevel ) return;
    let sid = "    ";
    if ( id ) sid = (id + "   ").substring(0, 4);
    let d = new Date();
    let diff = d.getTime() - timJavaScriptStartTime.getTime();
    let diffLast = d.getTime() - timJavaScriptLastTime.getTime();
    let str;
    if ( timTimingSummary ) {
        timLogText = "";
        str = diff.valueOf();
    }
    else
        str = sid + ": " + d.toLocaleTimeString() + " " + diff.valueOf() + " " + diffLast.valueOf() + " - " + msg;
    if ( timTimingLog ) console.log(str);
    if ( timTimingDiv ) {
        timLogText += str + "\r\n";
        timDivPreLogger.innerHTML = "<code>" + timLogText + "</code>"; // IE:n takia pitää kikkailla näin, innerText ei toimi.
    }
    timJavaScriptLastTime = d;
}
