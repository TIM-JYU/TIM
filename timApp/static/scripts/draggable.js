var angular;
var timApp = angular.module('timApp');

timApp.directive('timDraggableFixed', ['$document', '$window', '$parse', function ($document, $window, $parse) {

    var resizableConfig = {};

    return {
        restrict: 'A',
        replace: false,

        link: function (scope, element, attr) {

            var clickFn = null;
            var areaMinimized = false;
            var areaHeight = 0;

            if (attr.click) {
                if ( attr.click === "true" )
                    clickFn = minimize;
                else
                    clickFn = $parse(attr.click);
            }

            var closeFn = null;
            if (attr.close) {
                closeFn = $parse(attr.close);
            }

            function minimize() {
                areaMinimized = !areaMinimized;
                var base = element.find('.draggable-content');
                if (areaMinimized) {
                    areaHeight = element.height();
                    element.height(15);
                    base.css('display', 'none');
                    element.css('min-height', '0');
                } else {
                    base.css('display', '');
                    element.css('min-height', '');
                    element.height(areaHeight);
                }
                scope.$apply();
            };


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

            function createResizeHandles() {
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

            function removeResizeHandles() {
                element.find('.resizehandle').remove();
            }


            var handle = $("<div>", {class: "draghandle"});
            if (attr.click) {
                var minimize = $("<img>", {
                    src: "/static/images/minimize-window-16.png",
                    class: 'titlebutton minimize'
                });
                minimize.on('click', function () {
                    clickFn(scope)
                });
                handle.append(minimize);
            }
            if (attr.close) {
                var close = $("<img>", {src: "/static/images/close-window-16.png", class: 'titlebutton close'});
                close.on('click', function () {
                    closeFn(scope)
                });
                handle.append(close);
            }
            element.prepend(handle); // TODO.  Laita elementin ympärille se mitä raahataan.

            attr.$observe('resize', function () {
                if (attr.resize === "true") {
                    createResizeHandles();
                } else {
                    removeResizeHandles();
                }
            });

            attr.$observe('caption', function () {
                var handle = $(element).find('.draghandle');
                handle.find('p').remove();
                handle.append('<p>' + attr.caption + '</p>');
            });

            /* Prevent document scrolling, when element inside draggable is scrolled. Currently doesn't work on touch
             $(element).find('.scrollable').on('scroll wheel DOMMouseScroll mousewheel', function (e) {
             var e0 = e.originalEvent,
             delta = e0.wheelDelta || -e0.detail;
             console.log(delta);
             e.preventDefault();
             if (isNaN(delta)) {
             return;
             }
             if ($(e.target).hasClass('scrollable')) {
             console.log('target ', e.target);
             e.target.scrollTop += ( delta < 0 ? 1 : -1 ) * 30;
             } else {
             e.target.closest('.scrollable').scrollTop += ( delta < 0 ? 1 : -1 ) * 30;
             console.log('closest ', e.target.closest('.scrollable'));
             }
             });*/

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

                /*
                 if (!(upResize || rightResize || downResize || leftResize) && clickFn && e.which === 1) {
                 delta = {X: pos.X - lastPos.X, Y: pos.Y - lastPos.Y};
                 if (Math.abs(delta.Y) < 3 && Math.abs(delta.X) < 3) {
                 clickFn(scope);
                 }
                 }*/
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