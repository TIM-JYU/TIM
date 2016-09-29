var angular;
var timApp = angular.module('timApp');

function setStorage(key, value) {
    value = JSON.stringify(value);
    window.localStorage.setItem(key, value);
}


function getStorage(key) {
    var s = window.localStorage.getItem(key);
    if  ( !s ) return s;
    s = JSON.parse(s);
    return s;
}

function getPixels(s) {
    s2 = s.replace(/px$/, '');
    return Number(s2) || 0;
}


function wmax(value, min, max) {
    if ( !value ) return min+"px";
    if ( value == "auto" ) return value;
    var v = getPixels(value);

    if ( v <=  min ) return min+"px";
    if ( v >=  max ) return max + "px"
    return value;
}

timApp.directive('timDraggableFixed', ['$document', '$window', '$parse', function ($document, $window, $parse) {

    var resizableConfig = {};

    return {
        restrict: 'A',
        replace: false,

        link: function (scope, element, attr) {
            if ( attr.save ) {
                scope.pageId = window.location.pathname.split('/')[1];  // /velp/???
                scope.posKey = attr.save.replace('%%PAGEID%%', scope.pageId);
            }

            var clickFn = null;
            var areaMinimized = false;
            var areaHeight = 0;

            var setLeft = 1;
            var setRight = 0;
            var setBottom = 0;
            var setTop = 1;
            var prevTop;
            var prevLeft;
            var prevBottom;
            var prevRight;



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
                var handles;
                var base = element.find('.draggable-content');
                if (areaMinimized) {
                    areaHeight = element.height();
                    element.height(15);
                    base.css('display', 'none');
                    element.css('min-height', '0');
                    setStorage(scope.posKey+"min", true);
                    handles = element.find('.resizehandle');
                    if ( handles.length ) handles.css('display', 'none');

                } else {
                    base.css('display', '');
                    element.css('min-height', '');
                    element.height(areaHeight);
                    setStorage(scope.posKey+"min", false)
                    element.find('.resizehandle').css('display', '');
                }
                if ( handles && handles.length ) scope.$apply();
            };


            scope.minimize = minimize;

            function getSetDirs() {
                var leftSet = element.css('left') != 'auto';
                var rightSet = element.css('right') != 'auto';
                setLeft = (!leftSet & !rightSet) | leftSet;
                setRight = rightSet;
                // setLeft = true; // because for some reason in iPad it was right???

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
                timLogTime("set:" + setLeft + "," + setTop, "drag")
            }


            function resizeElement(e, up, right, down, left) {
                upResize = up;
                rightResize = right;
                downResize = down;
                leftResize = left;
                $document.off('mouseup pointerup touchend', release);
                $document.off('mousemove pointermove touchmove', moveResize);
                lastPos = getPageXY(e);

                getSetDirs();

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

                if ( areaMinimized ) {
                    var handles = element.find('.resizehandle');
                    if ( handles.length ) handles.css('display', 'none');
                }
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


            function getPageXYnull(e) {
                if (!('pageX' in e) || (e.pageX == 0 && e.pageY == 0)) {
                    if ( e.originalEvent.touches.length ) return {
                        X: e.originalEvent.touches[0].pageX,
                        Y: e.originalEvent.touches[0].pageY
                    };
                    if ( e.originalEvent.changedTouches.length ) return {
                        X: e.originalEvent.changedTouches[0].pageX,
                        Y: e.originalEvent.changedTouches[0].pageY
                    };
                    return null;
                }

                return {X: e.pageX, Y: e.pageY};
            }


            var lastPageXYPos = {X:0, Y: 0};

            function getPageXY(e) {
                var pos = getPageXYnull(e);
                if ( pos ) lastPageXYPos = pos;
                return lastPageXYPos;
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
                getSetDirs();



                //prevTop = element.position().top;
                //prevLeft = element.position().left;

                $document.on('mouseup pointerup touchend', release);
                $document.on('mousemove pointermove touchmove', move);
            });

            function release(e) {
                $document.off('mouseup pointerup touchend', release);
                $document.off('mousemove pointermove touchmove', move);
                $document.off('mousemove pointermove touchmove', moveResize);
                // element.css("background", "red");
                pos = getPageXY(e);
                // element.css("background", "blue");
                if ( scope.posKey ) {
                    // element.css("background", "yellow");
                    var css = element.css(['top', 'bottom', 'left', 'right']);
                    setStorage(scope.posKey, css);
                    // element.css("background", "green");
                    timLogTime("pos:" + css.left + "," + css.top, "drag")
                }

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

                if ( scope.posKey ) {
                    var size = element.css(["width", "height"]);
                    setStorage(scope.posKey + "Size", size );
                }
            }

            element.context.style.msTouchAction = 'none';

            if ( attr.save ) {
                getSetDirs();
                var oldSize = getStorage(scope.posKey + "Size");
                if ( oldSize ) {
                    if (oldSize.width) element.css('width', oldSize.width);
                    if (oldSize.height) element.css('height', oldSize.height);
                }
                var oldPos = getStorage(scope.posKey);
                var w = window.innerWidth;
                var h = window.innerHeight;
                var ew = element.width();
                var eh = element.height();
                if ( oldPos ) {
                    if (oldPos.top && setTop) element.css('top', wmax(oldPos.top,0,h-20));
                    if (oldPos.left && setLeft) element.css('left', wmax(oldPos.left,0-ew+20,w-20));
                    if (oldPos.right && setRight) element.css('right', wmax(oldPos.right,20,w));
                    if (oldPos.bottom && setBottom) element.css('bottom', wmax(oldPos.bottom,20,h));
                    // element.css("background", "red");
                    timLogTime("oldpos:" + oldPos.left +", " + oldPos.top, "drag")
                }
                if ( getStorage(scope.posKey+"min") ) scope.minimize();
            }


        }

    }
}]);