//Poistaa tyhjän ensimmäisen etusivun.
$(".par:eq(0) > .parContent > h1").attr("class", "firstH1");

if($(".par:eq(0)").has(".parContent").has(".docsettings").length)
    $(".par:eq(1) > .parContent > h1").attr("class", "firstH1");

//Automaatinen sivunvaihto
$(".par").has(".parContent").has("h5:only-child").each(function (i, e) {
    $(e).next(".par").andSelf().wrapAll("<div id='noBreak'>");
    //$(e).next(".par").andSelf().css("background-color", generateColor());
});

$(".par").has(".parContent").has("h4:only-child").each(function (i, e) {
    $(e).next(".par").andSelf().wrapAll("<div id='noBreak'>");
    //$(e).next(".par").andSelf().css("background-color", generateColor());
});

$(".par").has(".parContent").has("h3:only-child").each(function (i, e) {
    $(e).next(".par").andSelf().wrapAll("<div id='noBreak'>");
    //$(e).next(".par").andSelf().css("background-color", generateColor());
});

$(".par").has(".parContent").has("h2:only-child").each(function (i, e) {
    $(e).next(".par").andSelf().wrapAll("<div id='noBreak'>");
    //$(e).next(".par").andSelf().css("background-color", generateColor());
});

function generateColor() {
    var letters = '0123456789ABCDEF';
    var color = '#';
    for (var i = 0; i < 6; i++){
        color += letters[Math.floor(Math.random() * 16)];
    }
    return color;
}