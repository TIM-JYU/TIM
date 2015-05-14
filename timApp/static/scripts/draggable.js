var angular;
var timApp = angular.module('timApp');

timApp.directive('timDraggableFixed', ['$document', '$window', function ($document, $window) {
    return function (scope, element, attr) {
        
        var handle = $("<div>", {class: "draghandle"});
        handle.height(13);
        element.prepend(handle);

        function getPageXY(e) {
            if ( !('pageX' in e) || (e.pageX == 0 && e.pageY == 0) ) {
                return {
                    X: e.originalEvent.touches[0].pageX,
                    Y: e.originalEvent.touches[0].pageY
                };
            }

            return {X: e.pageX, Y: e.pageY};
        }
        
        handle.on('mousedown touchstart', function(e) {
            console.log("klik");
            lastPos = getPageXY(e);

            // Rules for what we should set in CSS
            // to keep the element dimensions (X).
            // Prefer left over right.
            var leftSet  = element.css('left') != 'auto';
            var rightSet = element.css('right') != 'auto';
            setLeft      = (!leftSet & !rightSet) | leftSet;
            setRight     = rightSet;

            // Rules for what we should set in CSS
            // to keep the element dimensions (Y).
            // Prefer top over bottom.
            var topSet = element.css('top') != 'auto';
            var botSet = element.css('bottom') != 'auto';
            setTop     = (!topSet & !botSet) | topSet;
            setBottom  = botSet;

            prevTop    = Number(element.css('top'))    || 0;
            prevLeft   = Number(element.css('left'))   || 0;
            prevBottom = Number(element.css('bottom')) || 0;
            prevRight  = Number(element.css('right'))  || 0;

            $document.on('mouseup touchend', release);
            $document.on('mousemove touchmove', move);
        });

        function release(e) {
            console.log("klak");
            $document.off('mouseup touchend', release);
            $document.off('mousemove touchmove', move);
        }

        function move(e) {
            pos = getPageXY(e);
            delta = {X: pos.X - lastPos.X, Y: pos.Y - lastPos.Y};

            if ( setTop )
                element.css( 'top', prevTop + delta.Y );
            if ( setLeft )
                element.css( 'left', prevLeft + delta.X );
            if ( setBottom )
                element.css( 'bottom', prevBottom - delta.Y );
            if ( setRight )
                element.css( 'right', prevRight - delta.X );            

            e.preventDefault();
            e.stopPropagation();
        }

        element.context.style.msTouchAction = 'none';
    };
}]);
