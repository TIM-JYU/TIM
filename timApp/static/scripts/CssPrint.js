//Removes the empty first page.
$(".par:eq(0) > .parContent > h1").attr("class", "firstH1");

if($(".par:eq(0)").has(".parContent").has(".docsettings").length)
    $(".par:eq(1) > .parContent > h1").attr("class", "firstH1");

//Automatic page break
function AutoPageBreak() {
    var selectedPars = [];
    var headersFound = 0;
    pars = $("#pars").children();
    for(var i = 0; i <  pars.length; i++){
        if(headersFound){
            selectedPars.push(pars[i]);
            if(!$(pars[i]).has("h2:only-child").length && !$(pars[i]).has("h3:only-child").length && !$(pars[i]).has("h4:only-child").length && !$(pars[i]).has("h5:only-child").length){
                headersFound = 0;
                $(selectedPars).wrapAll("<div id='noBreak'>")//.css("background-color", GenerateColor());
            }
        }else{
            if($(pars[i]).has("h2:only-child").length || $(pars[i]).has("h3:only-child").length || $(pars[i]).has("h4:only-child").length || $(pars[i]).has("h5:only-child").length){
                headersFound = 1;
                selectedPars = [];
                selectedPars.push(pars[i]);
            }
        }
    }
}

//Undoing autoprint settings to minimize or prevent possible errors when editing the document after the printing event is done.
function  UndoAutoPageBreak() {
    $(".par").unwrap();
}


//For troubleshooting.
function GenerateColor() {
    var letters = '0123456789ABCDEF';
    var color = '#';
    for (var i = 0; i < 6; i++){
        color += letters[Math.floor(Math.random() * 16)];
    }
    return color;
}

//Printing events
function AfterPrint(){
    console.log('Undoing the print settings');
    UndoAutoPageBreak();
}

function  BeforePrint() {
    console.log('Setting up for print');
    AutoPageBreak();
}

if(window.matchMedia){
    var mediaQueryList = window.matchMedia('print');
    mediaQueryList.addListener(function (mql) {
        (mql.matches) ? BeforePrint() : AfterPrint();
    });
}else{
    window.addEventListener('beforeprint', BeforePrint, false);
    window.addEventListener('afterprint', AfterPrint, false);
}

//window.onbeforeprint = AutoPageBreak();
//window.onafterprint = UndoAutoPageBreak();