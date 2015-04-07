var angular;
var timApp = angular.module('timApp');

timApp.directive('timDraggableFixed', ['$document', '$window', function ($document, $window) {
    return function (scope, element, attr) {
        var startX = 0, startY = 0, x = 0, y = 0;

        var iTop = Number(element.css('top').slice(0, -2));
        var iRight = Number(element.css('right').slice(0, -2));
        var iPad = false;
        x = -iRight;
        y = iTop;
        if ( isNaN(x) ) x = 0;
        if ( isNaN(y) ) y = 0;

        // var ua = $window.navigator.userAgent, startEvent, moveEvent, endEvent;
        tstartEvent = "touchstart";
        tmoveEvent = "touchmove";
        tendEvent = "touchend";
        startEvent = "mousedown";
        moveEvent = "mousemove";
        endEvent = "mouseup";
        /*
        if ( window.navigator.msPointerEnabled ) { // ei t‰t‰ saa toimimaan erkkik‰‰n 
            pstartEvent = "MSPointerDown";
            pmoveEvent = "MSPointerMove";
            pendEvent = "MSPointerUp";
            // document.getElementById("releaseButton").innerText="MS";  
        }
        */
        
        function getXY(e,ix,iy,iPad) {
            if ( iPad ) {
               var touch = e.originalEvent.touches[0] || e.originalEvent.changedTouches[0];
               // document.getElementById("releaseButton").innerText="dy="+touch.pageY + " iy="+iy + " ix="+ix;  
               return { x: touch.pageX-ix, y: touch.pageY-iy };
            }   
            return { x: e.pageX-ix, y: e.pageY-iy };
        }
        
        
        element.on(startEvent, function (event) {
            // document.getElementById("releaseButton").innerText="Tarttu"; 
            var $target = angular.element(event.target);
            var tag = $target.prop("tagName");
            if ((tag !== 'DIV' && tag !== 'SPAN') || $target.hasClass('ui-resizable-handle')) {
                return;
            }
            event.preventDefault();
            //document.getElementById("releaseButton").innerText="dy="+event.touche + " iy="+y + " ix="+x; 
            //console.log("mouse dy="+event + " iy="+y + " ix="+x);
            var p = getXY(event,x,y,false);
            startX = p.x;
            startY = p.y;
            $document.on(moveEvent, mousemove);
            $document.on(endEvent, mouseup);
        });

        function mousemove(event) {
            var p = getXY(event,startX,startY,false);
            x = p.x;
            y = p.y;
            // document.getElementById("releaseButton").innerText="y="+y; 
            element.css({
                top: (y) + 'px',
                right: (-x) + 'px'
            });
        }

        function mouseup() {
            $document.off(moveEvent, mousemove);
            $document.off(endEvent, mouseup);
        }
        
        /*
        if ( window.navigator.msPointerEnabled ) 
          // element.addEventListener("MSPointerDown", function (event) {
          element.on("MSPointerDown", function (event) {
            document.getElementById("releaseButton").innerText="Tarttu"; 
            var $target = angular.element(event.target);
            var tag = $target.prop("tagName");
            if ((tag !== 'DIV' && tag !== 'SPAN') || $target.hasClass('ui-resizable-handle')) {
                return;
            }
            event.preventDefault();
            document.getElementById("releaseButton").innerText="dy="+event + " iy="+y + " ix="+x; 
            var p = getXY(event,x,y,true);
            startX = p.x;
            startY = p.y;
            $document.on(tmoveEvent, tmousemove);
            $document.on(tendEvent, tmouseup);
        }, false);
        */
        element.context.style.msTouchAction = 'none';
        
        element.on(tstartEvent, function (event) {
            // document.getElementById("releaseButton").innerText="Tarttu"; 
            var $target = angular.element(event.target);
            var tag = $target.prop("tagName");
            if ((tag !== 'DIV' && tag !== 'SPAN') || $target.hasClass('ui-resizable-handle')) {
                return;
            }
            event.preventDefault();
            // document.getElementById("releaseButton").innerText="dy="+event + " iy="+y + " ix="+x; 
            // console.log("dy="+event + " iy="+y + " ix="+x);
            var p = getXY(event,x,y,true);
            startX = p.x;
            startY = p.y;
            $document.on(tmoveEvent, tmousemove);
            $document.on(tendEvent, tmouseup);
        });

        function tmousemove(event) {
            var p = getXY(event,startX,startY,true);
            x = p.x;
            y = p.y;
            // document.getElementById("releaseButton").innerText="y="+y; 
            element.css({
                top: (y) + 'px',
                right: (-x) + 'px'
            });
        }

        function tmouseup() {
            $document.off(tmoveEvent, tmousemove);
            $document.off(tendEvent, tmouseup);
        }
    };
}]);