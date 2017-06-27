
import $ from "jquery";
import {timApp} from "tim/app";
import {timLogTime} from "tim/timTiming";
import {$document, $parse} from "../ngimport";
function setStorage(key, value) {
    value = JSON.stringify(value);
    window.localStorage.setItem(key, value);
}

function getStorage(key: string) {
    let s = window.localStorage.getItem(key);
    if (!s) {
        return s;
    }
    s = JSON.parse(s);
    return s;
}

function getPixels(s) {
    const s2 = s.replace(/px$/, "");
    return Number(s2) || 0;
}

function wmax(value, min, max) {
    if (!value) {
        return min + "px";
    }
    if (value == "auto") {
        return value;
    }
    const v = getPixels(value);

    if (v <= min) {
        return min + "px";
    }
    if (v >= max) {
        return max + "px";
    }
    return value;
}

timApp.directive("timDraggableFixed", [function() {

    const resizableConfig = {};

    return {
        restrict: "A",
        replace: false,

        link(scope, element: JQuery, attr) {
            let posKey;
            if (attr.save) {
                const pageId = window.location.pathname.split("/")[1];  // /velp/???
                posKey = attr.save.replace("%%PAGEID%%", pageId);
            }

            const broadcastMsg = attr.broadcastmsg;

            let clickFn = null;
            let areaMinimized = false;
            let areaHeight = 0;

            let setLeft = true;
            let setRight = false;
            let setBottom = false;
            let setTop = true;
            let prevTop;
            let prevLeft;
            let prevBottom;
            let prevRight;
            let prevHeight;
            let prevWidth;
            let upResize;
            let rightResize;
            let downResize;
            let leftResize;
            let lastPos;
            let pos;
            let delta;

            if (attr.click) {
                if (attr.click === "true") {
                    clickFn = minimize;
                } else {
                    clickFn = $parse(attr.click);
                }
            }

            let closeFn = null;
            if (attr.close) {
                closeFn = $parse(attr.close);
            }

            function minimize() {
                areaMinimized = !areaMinimized;
                let handles;
                const base = element.find(".draggable-content");
                if (areaMinimized) {
                    areaHeight = element.height();
                    element.height(15);
                    base.css("display", "none");
                    element.css("min-height", "0");
                    setStorage(posKey + "min", true);
                    handles = element.find(".resizehandle");
                    if (handles.length) {
                        handles.css("display", "none");
                    }

                } else {
                    base.css("display", "");
                    element.css("min-height", "");
                    element.height(areaHeight);
                    setStorage(posKey + "min", false);
                    element.find(".resizehandle").css("display", "");
                }
                if (handles && handles.length) {
                    scope.$apply();
                }
            }

            scope.minimize = minimize;

            function getSetDirs() {
                const leftSet = element.css("left") != "auto";
                const rightSet = element.css("right") != "auto";
                setLeft = (!leftSet && !rightSet) || leftSet;
                setRight = rightSet;
                // setLeft = true; // because for some reason in iPad it was right???

                // Rules for what we should set in CSS
                // to keep the element dimensions (Y).
                // Prefer top over bottom.

                const topSet = element.css("top") != "auto";
                const botSet = element.css("bottom") != "auto";
                setTop = (!topSet && !botSet) || topSet;
                setBottom = botSet;
                prevHeight = element.height();
                prevWidth = element.width();

                prevTop = getPixels(element.css("top"));
                prevLeft = getPixels(element.css("left"));
                prevBottom = getPixels(element.css("bottom"));
                prevRight = getPixels(element.css("right"));
                timLogTime("set:" + setLeft + "," + setTop, "drag");
            }

            function resizeElement(e, up, right, down, left) {
                upResize = up;
                rightResize = right;
                downResize = down;
                leftResize = left;
                $document.off("mouseup pointerup touchend", release);
                $document.off("mousemove pointermove touchmove", moveResize);
                lastPos = getPageXY(e);

                getSetDirs();

                //prevTop = element.position().top;
                //prevLeft = element.position().left;

                $document.on("mouseup pointerup touchend", release);
                $document.on("mousemove pointermove touchmove", moveResize);
            }

            /*
             element.css('top', element.position().top);
             element.css('left', element.position().left); */

            function createResizeHandles() {
                const handleRight = $("<div>", {class: "resizehandle-r resizehandle"});
                handleRight.on("mousedown pointerdown touchstart", function(e) {
                    resizeElement(e, false, true, false, false);
                });
                element.append(handleRight);
                const handleDown = $("<div>", {class: "resizehandle-d resizehandle"});
                handleDown.on("mousedown pointerdown touchstart", function(e) {
                    resizeElement(e, false, false, true, false);
                });
                element.append(handleDown);
                const handleRightDown = $("<div>", {class: "resizehandle-rd resizehandle"});
                handleRightDown.on("mousedown pointerdown touchstart", function(e) {
                    resizeElement(e, false, true, true, false);
                });
                element.append(handleRightDown);

                if (areaMinimized) {
                    const handles = element.find(".resizehandle");
                    if (handles.length) {
                        handles.css("display", "none");
                    }
                }
            }

            function removeResizeHandles() {
                element.find(".resizehandle").remove();
            }

            const handle = $("<div>", {class: "draghandle"});
            if (attr.click) {
                const minimizeElem = $("<img>", {
                    src: "/static/images/minimize-window-16.png",
                    class: "titlebutton minimize",
                });
                minimizeElem.on("click", function() {
                    clickFn(scope);
                });
                handle.append(minimizeElem);
            }
            if (attr.close) {
                const close = $("<img>", {src: "/static/images/close-window-16.png", class: "titlebutton close"});
                close.on("click", function() {
                    closeFn(scope);
                });
                handle.append(close);
            }
            element.prepend(handle); // TODO.  Laita elementin ympärille se mitä raahataan.

            attr.$observe("resize", function() {
                if (attr.resize === "true") {
                    createResizeHandles();
                } else {
                    removeResizeHandles();
                }
            });

            attr.$observe("caption", function() {
                const handle = $(element).find(".draghandle");
                handle.find("p").remove();
                handle.append("<p>" + attr.caption + "</p>");
            });

            /* Prevent document scrolling, when element inside draggable is scrolled. Currently doesn't work on touch
             $(element).find('.scrollable').on('scroll wheel DOMMouseScroll mousewheel', function (e) {
             var e0 = e.originalEvent,
             delta = e0.wheelDelta || -e0.detail;
             $log.info(delta);
             e.preventDefault();
             if (isNaN(delta)) {
             return;
             }
             if ($(e.target).hasClass('scrollable')) {
             $log.info('target ', e.target);
             e.target.scrollTop += ( delta < 0 ? 1 : -1 ) * 30;
             } else {
             e.target.closest('.scrollable').scrollTop += ( delta < 0 ? 1 : -1 ) * 30;
             $log.info('closest ', e.target.closest('.scrollable'));
             }
             });*/

            updateHandle(element, handle);

            function updateHandle(e, h) {
                // TODO: find an efficient way to call this whenever
                // position is changed between static and absolute
                const position = e.css("position");
                const movable = position != "static";
                h.css("visibility", movable ? "visible" : "hidden");
            }

            function getPageXYnull(e) {
                if (!("pageX" in e) || (e.pageX == 0 && e.pageY == 0)) {
                    if (e.originalEvent.touches.length) {
                        return {
                            X: e.originalEvent.touches[0].pageX,
                            Y: e.originalEvent.touches[0].pageY,
                        };
                    }
                    if (e.originalEvent.changedTouches.length) {
                        return {
                            X: e.originalEvent.changedTouches[0].pageX,
                            Y: e.originalEvent.changedTouches[0].pageY,
                        };
                    }
                    return null;
                }

                return {X: e.pageX, Y: e.pageY};
            }

            let lastPageXYPos = {X: 0, Y: 0};

            function getPageXY(e) {
                const pos = getPageXYnull(e);
                if (pos) {
                    lastPageXYPos = pos;
                }
                return lastPageXYPos;
            }

            handle.on("mousedown pointerdown touchstart", function(e) {
                upResize = false;
                rightResize = false;
                downResize = false;
                leftResize = false;
                $document.off("mouseup pointerup touchend", release);
                $document.off("mousemove pointermove touchmove", move);
                lastPos = getPageXY(e);
                // Rules for what we should set in CSS
                // to keep the element dimensions (X).
                // Prefer left over right.
                getSetDirs();

                //prevTop = element.position().top;
                //prevLeft = element.position().left;

                $document.on("mouseup pointerup touchend", release);
                $document.on("mousemove pointermove touchmove", move);
            });

            function release(e) {
                $document.off("mouseup pointerup touchend", release);
                $document.off("mousemove pointermove touchmove", move);
                $document.off("mousemove pointermove touchmove", moveResize);
                // element.css("background", "red");
                pos = getPageXY(e);
                // element.css("background", "blue");
                if (posKey) {
                    // element.css("background", "yellow");
                    const css = element.css(["top", "bottom", "left", "right"]);
                    setStorage(posKey, css);
                    // element.css("background", "green");
                    timLogTime("pos:" + css.left + "," + css.top, "drag");
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

                if (setTop) {
                    element.css("top", prevTop + delta.Y);
                }
                if (setLeft) {
                    element.css("left", prevLeft + delta.X);
                }
                if (setBottom) {
                    element.css("bottom", prevBottom - delta.Y);
                }
                if (setRight) {
                    element.css("right", prevRight - delta.X);
                }

                e.preventDefault();
                e.stopPropagation();
            }

            function moveResize(e) {
                pos = getPageXY(e);
                delta = {X: pos.X - lastPos.X, Y: pos.Y - lastPos.Y};

                if (upResize) {
                    element.css("height", prevHeight - delta.Y);
                    if (setTop) {
                        element.css("top", prevTop + delta.Y);
                    }
                }
                if (leftResize) {
                    element.css("width", prevWidth - delta.X);
                    if (setLeft) {
                        element.css("left", prevLeft + delta.X);
                    }
                }
                if (downResize) {
                    element.css("height", prevHeight + delta.Y);
                    if (setBottom) {
                        element.css("bottom", prevBottom - delta.Y);
                    }
                }
                if (rightResize) {
                    element.css("width", prevWidth + delta.X);
                    if (setRight && delta.X >= 0) {
                        element.css("right", prevRight - delta.X);
                    }
                }

                e.preventDefault();
                e.stopPropagation();

                const size = element.css(["width", "height"]);
                if (posKey) {
                    setStorage(posKey + "Size", size);
                }
                if (broadcastMsg) {
                    scope.$broadcast(broadcastMsg, {size});
                    // $rootScope.$broadcast(broadcastMsg, {size: size});
                }
            }

            // TODO context is deprecated
            (element.context as HTMLElement).style.msTouchAction = "none";

            if (attr.save) {
                getSetDirs();
                const oldSize: any = getStorage(posKey + "Size");
                if (oldSize) {
                    if (oldSize.width) {
                        element.css("width", oldSize.width);
                    }
                    if (oldSize.height) {
                        element.css("height", oldSize.height);
                    }
                }
                const oldPos: any = getStorage(posKey);
                const w = window.innerWidth;
                const h = window.innerHeight;
                const ew = element.width();
                const eh = element.height();
                if (oldPos) {
                    if (oldPos.top && setTop) {
                        element.css("top", wmax(oldPos.top, 0, h - 20));
                    }
                    if (oldPos.left && setLeft) {
                        element.css("left", wmax(oldPos.left, 0 - ew + 20, w - 20));
                    }
                    if (oldPos.right && setRight) {
                        element.css("right", wmax(oldPos.right, 20, w));
                    }
                    if (oldPos.bottom && setBottom) {
                        element.css("bottom", wmax(oldPos.bottom, 20, h));
                    }
                    // element.css("background", "red");
                    timLogTime("oldpos:" + oldPos.left + ", " + oldPos.top, "drag");
                }
                if (getStorage(posKey + "min")) {
                    scope.minimize();
                }
            }

        },

    };
}]);
