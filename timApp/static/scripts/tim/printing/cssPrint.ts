import $ from "jquery";

export function initCssPrint() {
    // Removes the empty first page.
    $(".par:eq(0) > .parContent > h1").attr("class", "firstH1");

    if ($(".par:eq(0)").has(".parContent").has(".docsettings").length) {
        $(".par:eq(1) > .parContent > h1").attr("class", "firstH1");
    }

    // OPERA, CHROME
    if (window.matchMedia) {
        const mediaQueryList = window.matchMedia("print");
        mediaQueryList.addListener((mql) => {
            if (mql.matches) {
                BeforePrint();
            } else {
                AfterPrint();
            }
        });
    }

    // MOZILLA FIREFOX, MICROSOFT EDGE
    window.addEventListener("beforeprint", BeforePrint, false);
    window.addEventListener("afterprint", AfterPrint, false);

    // INTERNET EXPLORER
    const ua = window.navigator.userAgent;
    if (ua.indexOf("MSIE ") > 0) {
        window.onbeforeprint = BeforePrint;
        window.onafterprint = AfterPrint;
    }
}

// Automatic page break
function AutoPageBreak() {
    let selectedPars: HTMLElement[] = [];
    let headersFound = 0;
    const pars = $("#pars").children();
    pars.each((index, par) => {
        if (headersFound) {
            selectedPars.push(par);
            if (!$(par).has("h2:only-child").length &&
                !$(par).has("h3:only-child").length &&
                !$(par).has("h4:only-child").length &&
                !$(par).has("h5:only-child").length) {
                headersFound = 0;
                $(selectedPars).wrapAll("<div id='noBreak'>");
            }
        } else {
            if ($(par).has("h2:only-child").length ||
                $(par).has("h3:only-child").length ||
                $(par).has("h4:only-child").length ||
                $(par).has("h5:only-child").length) {
                headersFound = 1;
                selectedPars = [];
                selectedPars.push(par);
            }
        }
    });
}

// Undoing autoprint settings to minimize or prevent possible errors
// when editing the document after the printing event is done.
function UndoAutoPageBreak() {
    $("#noBreak > .par").unwrap();
}

// For troubleshooting.
function GenerateColor() {
    const letters = "0123456789ABCDEF";
    let color = "#";
    for (let i = 0; i < 6; i++) {
        color += letters[Math.floor(Math.random() * 16)];
    }
    return color;
}

// Printing events
function AfterPrint() {
    console.log("Undoing the print settings");
    UndoAutoPageBreak();
}

function BeforePrint() {
    console.log("Setting up for print");
    AutoPageBreak();
}
