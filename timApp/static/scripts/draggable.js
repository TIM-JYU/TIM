var angular;
var timApp = angular.module('timApp');

timApp.directive('timDraggableFixed', ['$document', function ($document) {
    return function (scope, element, attr) {
        var startX = 0, startY = 0, x = 0, y = 0;

        var iTop = Number(element.css('top').slice(0, -2));
        var iRight = Number(element.css('right').slice(0, -2));
        x = -iRight;
        y = iTop;
        element.on('mousedown', function (event) {
            event.preventDefault();
            startX = event.pageX - x;
            startY = event.pageY - y;
            $document.on('mousemove', mousemove);
            $document.on('mouseup', mouseup);
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
            $document.off('mousemove', mousemove);
            $document.off('mouseup', mouseup);
        }
    };
}]);