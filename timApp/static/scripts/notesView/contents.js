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
  		sidebarOpen = !sidebarOpen
	});
});

sidebarOpen = false