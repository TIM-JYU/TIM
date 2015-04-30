$( document ).ready(function() {

    $( ".menu" ).click(function() {
        if (sidebarOpen) {
            $( ".sidebar" ).animate({left: "-=220"}, 50);
            $( ".menu" ).animate({left: "-=220"}, 50);
        }
        else {
            $( ".sidebar" ).animate({left: "+=220"}, 50);
            $( ".menu" ).animate({left: "+=220"}, 50);
        }
        sidebarOpen = !sidebarOpen;
    });

    $( ".par" ).click(function() {
        if (sidebarOpen) {
            $( ".sidebar" ).animate({left: "-=220"}, 50);
            $( ".menu" ).animate({left: "-=220"}, 50);
            sidebarOpen = false;
        }
    });

});

function autoHide() {
    if (sidebarOpen && $( window ).width() < 800) {
        $( ".sidebar" ).animate({left: "-=220"}, 50);
        $( ".menu" ).animate({left: "-=220"}, 50);
        sidebarOpen = false;
    }
}

sidebarOpen = false;
