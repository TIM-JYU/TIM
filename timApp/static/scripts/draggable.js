var angular;
var timApp = angular.module('timApp');

timApp.directive('timDraggableFixed', ['$document', '$window', function ($document, $window) {
    return function (scope, element, attr) {
        var startX = 0, startY = 0, x = 0, y = 0;

        var iTop = Number(element.css('top').slice(0, -2));
        var iRight = Number(element.css('right').slice(0, -2));
        x = -iRight;
        y = iTop;

        var ua = $window.navigator.userAgent, startEvent, moveEvent, endEvent;
        if (ua.match(/iPad/i)) {
            startEvent = "touchstart";
            moveEvent = "touchmove";
            endEvent = "touchend";
        } else {
            startEvent = "mousedown";
            moveEvent = "mousemove";
            endEvent = "mouseup";
        }

        element.on(startEvent, function (event) {
            event.preventDefault();
            startX = event.pageX - x;
            startY = event.pageY - y;
            $document.on(moveEvent, mousemove);
            $document.on(endEvent, mouseup);
        });

        function mousemove(event) {
            y = event.pageY - startY;
            x = event.pageX - startX;
            element.css({
                top: (y) + 'px',
                right: (-x) + 'px'
            });
        }

        function mouseup() {
            $document.off(moveEvent, mousemove);
            $document.off(endEvent, mouseup);
        }
    };
}]);