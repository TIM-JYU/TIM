var angular;
var timApp = angular.module('timApp');

timApp.directive('timDraggableFixed', ['$document', '$window', '$parse', function ($document, $window, $parse) {

    var resizableConfig = {};

    return {
        restrict: 'A',
        replace: false,

        link: function (scope, element, attr) {

            var clickFn = null;
            if (attr.click) {
                var clickFn = $parse(attr.click);
            }

            function resizeElement(e, up, right, down, left) {
                upResize = up;
                rightResize = right;
                downResize = down;
                leftResize = left;
                $document.off('mouseup pointerup touchend', release);
                $document.off('mousemove pointermove touchmove', moveResize);
                lastPos = getPageXY(e);

                var leftSet = element.css('left') != 'auto';
                var rightSet = element.css('right') != 'auto';
                setLeft = (!leftSet & !rightSet) | leftSet;
                setRight = rightSet;

                // Rules for what we should set in CSS
                // to keep the element dimensions (Y).
                // Prefer top over bottom.

                var topSet = element.css('top') != 'auto';
                var botSet = element.css('bottom') != 'auto';
                setTop = (!topSet & !botSet) | topSet;
                setBottom = botSet;
                prevHeight = element.height();
                prevWidth = element.width();

                prevTop = getPixels(element.css('top'));
                prevLeft = getPixels(element.css('left'));
                prevBottom = getPixels(element.css('bottom'));
                prevRight = getPixels(element.css('right'));

                //prevTop = element.position().top;
                //prevLeft = element.position().left;

                $document.on('mouseup pointerup touchend', release);
                $document.on('mousemove pointermove touchmove', moveResize);
            }

            /*
             element.css('top', element.position().top);
             element.css('left', element.position().left); */

            if (attr.resize) {
                var handleRight = $("<div>", {class: 'resizehandle-r resizehandle'});
                handleRight.on('mousedown pointerdown touchstart', function (e) {
                    resizeElement(e, false, true, false, false)
                });
                element.append(handleRight);
                var handleDown = $("<div>", {class: 'resizehandle-d resizehandle'});
                handleDown.on('mousedown pointerdown touchstart', function (e) {
                    resizeElement(e, false, false, true, false)
                });
                element.append(handleDown);
                var handleRightDown = $("<div>", {class: 'resizehandle-rd resizehandle'});
                handleRightDown.on('mousedown pointerdown touchstart', function (e) {
                    resizeElement(e, false, true, true, false)
                });
                element.append(handleRightDown);
            }


            var handle = $("<div>", {class: "draghandle"});
            if (attr.caption) handle.append('<p>' + attr.caption + '</p>');
            element.prepend(handle);

            attr.$observe('caption', function () {
                var handle = $(element).find('.draghandle');
                handle.empty();
                handle.append('<p>' + attr.caption + '</p>');
            });

            updateHandle(element, handle);

            function updateHandle(e, h) {
                // TODO: find an efficient way to call this whenever
                // position is changed between static and absolute
                position = e.css("position");
                movable = position != 'static';
                h.css("visibility", movable ? "visible" : "hidden");
            }


            function getPageXY(e) {
                if (!('pageX' in e) || (e.pageX == 0 && e.pageY == 0)) {
                    return {
                        X: e.originalEvent.touches[0].pageX,
                        Y: e.originalEvent.touches[0].pageY
                    };
                }

                return {X: e.pageX, Y: e.pageY};
            }

            function getPixels(s) {
                s2 = s.replace(/px$/, '');
                return Number(s2) || 0;
            }

            handle.on('mousedown pointerdown touchstart', function (e) {
                upResize = false;
                rightResize = false;
                downResize = false;
                leftResize = false;
                $document.off('mouseup pointerup touchend', release);
                $document.off('mousemove pointermove touchmove', move);
                lastPos = getPageXY(e);
                // Rules for what we should set in CSS
                // to keep the element dimensions (X).
                // Prefer left over right.

                var leftSet = element.css('left') != 'auto';
                var rightSet = element.css('right') != 'auto';
                setLeft = (!leftSet & !rightSet) | leftSet;
                setRight = rightSet;

                // Rules for what we should set in CSS
                // to keep the element dimensions (Y).
                // Prefer top over bottom.

                var topSet = element.css('top') != 'auto';
                var botSet = element.css('bottom') != 'auto';
                setTop = (!topSet & !botSet) | topSet;
                setBottom = botSet;

                prevTop = getPixels(element.css('top'));
                prevLeft = getPixels(element.css('left'));
                prevBottom = getPixels(element.css('bottom'));
                prevRight = getPixels(element.css('right'));

                //prevTop = element.position().top;
                //prevLeft = element.position().left;

                $document.on('mouseup pointerup touchend', release);
                $document.on('mousemove pointermove touchmove', move);
            });

            function release(e) {
                $document.off('mouseup pointerup touchend', release);
                $document.off('mousemove pointermove touchmove', move);
                $document.off('mousemove pointermove touchmove', moveResize);
                pos = getPageXY(e);

                if (!(upResize || rightResize || downResize || leftResize) && clickFn && e.which === 1) {
                    delta = {X: pos.X - lastPos.X, Y: pos.Y - lastPos.Y};
                    if (Math.abs(delta.Y) < 3 && Math.abs(delta.X) < 3) {
                        clickFn(scope);
                    }
                }
            }

            function move(e) {
                pos = getPageXY(e);
                delta = {X: pos.X - lastPos.X, Y: pos.Y - lastPos.Y};

                if (setTop)
                    element.css('top', prevTop + delta.Y);
                if (setLeft)
                    element.css('left', prevLeft + delta.X);
                if (setBottom)
                    element.css('bottom', prevBottom - delta.Y);
                if (setRight)
                    element.css('right', prevRight - delta.X);

                e.preventDefault();
                e.stopPropagation();
            }

            function moveResize(e) {
                pos = getPageXY(e);
                delta = {X: pos.X - lastPos.X, Y: pos.Y - lastPos.Y};

                if (upResize) {
                    element.css('height', prevHeight - delta.Y);
                    if (setTop)
                        element.css('top', prevTop + delta.Y);
                }
                if (leftResize) {
                    element.css('width', prevWidth - delta.X);
                    if (setLeft)
                        element.css('left', prevLeft + delta.X);
                }
                if (downResize) {
                    element.css('height', prevHeight + delta.Y);
                    if (setBottom)
                        element.css('bottom', prevBottom - delta.Y);
                }
                if (rightResize) {
                    element.css('width', prevWidth + delta.X);
                    if (setRight && delta.X >= 0)
                        element.css('right', prevRight - delta.X);
                }

                e.preventDefault();
                e.stopPropagation();
            }

            element.context.style.msTouchAction = 'none';
        }
    }
}]);