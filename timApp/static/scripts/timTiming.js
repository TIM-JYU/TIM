/*
  To use timTiming, add to meta part of the document:
  
    <script src="{{ url_for('static', filename='scripts/timTiming.js') }}"></script>
    <script>timLogInit("{{ request.query_string }}");</script>

  before any othe script loadings.  Then when you want log timing, add:

    <script>timLogTime("Document end","base");</script>
  
  to html (f.ex end of the whole document before </body>-tag
  To JavaScript codes add the bare call to the timLogTime-function.
  First parameter is printed as it is and secod is for filtering the output,
  see timLogTime comments.  
  
*/  
var timJavaScriptStartTime = new Date();
var timJavaScriptLastTime = timJavaScriptStartTime;
var timDivLogger;
var timDivPreLogger = document.createElement("pre");

document.addEventListener('DOMContentLoaded', function () {
    timLogTime("Document loaded","main");
});



document.onreadystatechange = function () {
    timLogTime("ready changed " + document.readyState,"main");
    if ( timTimingDiv & !timDivLogger ) {
        timDivLogger = document.createElement("div")
        timDivLogger.appendChild(timDivPreLogger);
        //document.body.appendChild(timDivLogger);
        document.body.insertBefore(timDivLogger,document.body.firstChild);
    }    
};


var timTimingDiv = false;
var timTimingLog = false;
var timTimingFilter;
var timTiming = false;
var timTimingSummary = false;
var timTimingLevel = 0;

function timLogInit(params) {
    /*
    .../view/1?timing=div    - logittaa vain diviin (vain taso 0 tai ei tasoa)
    .../view/1?timing=log    - logittaa vain console.log
    .../view/1?timing=log&timingLevel=1 - logittaa vain console.log, tasot 0-1
    .../view/1?timing=log,div,summary - logit div ja konsolit ja vain summat
    .../view/1?timing=log,div&timingFilter=cs - logit div,konsoli ja vain cs id:llä olevat.  filter on regexp
    */
    var p = params.replace("b&#39;","");
    p = p.replace("&#39;","");
    p = p.replace("&amp;","&");
    p = p.replace("b'",""); 
    p = p.replace("'","");
    var ps = p.split("&");
    for (var i=0; i<ps.length; i++) {
        var k = (ps[i]+"=").split("=");
        var key = k[0];
        var val = k[1];
        if ( key === "timing" ) { 
           if ( val.indexOf("div") >= 0 ) timTimingDiv = true;
           if ( val.indexOf("log") >= 0 ) timTimingLog = true;
           if ( val.indexOf("summary") >= 0 ) timTimingSummary = true;
        }   
        if ( key === "timingFilter" ) timTimingFilter = val;
        if ( key === "timingLevel" ) timTimingLevel = val;
    }
    timTiming = timTimingDiv || timTimingLog;
    timLogTime("Tim log initialized","main");
}    

var timLogText = "";

function timLogTime(msg,id,level) {
    if ( !timTiming ) return;
    if ( timTimingFilter && !id.match(timTimingFilter) ) return;
    var tlevel = level | 0;
    if ( tlevel > timTimingLevel ) return;
    var sid = "    ";
    if ( id ) sid = (id+"   ").substring(0, 4);
    var d = new Date();       
    var diff = d - timJavaScriptStartTime;
    var diffLast = d - timJavaScriptLastTime;
    if ( timTimingSummary ) {
        timLogText = "";
        str = diff.valueOf();
    }    
    else 
        str = sid+": " + d.toLocaleTimeString()+ " " + diff.valueOf() + " " + diffLast.valueOf() + " - " + msg;
    if ( timTimingLog ) console.log(str);  
    if ( timTimingDiv ) {
        timLogText += str +"\r\n";     
        timDivPreLogger.innerHTML = "<code>"+timLogText+"</code>"; // IE:n takia pitää kikkailla näin, innerText ei toimi.
    }    
    timJavaScriptLastTime = d;
}   


