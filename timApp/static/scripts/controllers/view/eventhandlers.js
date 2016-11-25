/* globals angular, $ */

var timApp = angular.module('timApp');

timApp.defineEventHandlers = function (sc, http, q, $injector, $compile, $window, $document, $rootScope, $localStorage, $filter, $timeout, $log, Users) {
    "use strict";

    sc.onClick = function (className, func, overrideModalCheck) {
        var downEvent = null;
        var downCoords = null;
        var lastDownEvent = null;
        var lastclicktime = -1;

        $document.on('mousedown touchstart', className, function (e) {
            if (!overrideModalCheck && ($(".actionButtons").length > 0 || $(sc.EDITOR_CLASS_DOT).length > 0)) {
                // Disable while there are modal gui elements
                return;
            }
            if (lastDownEvent && lastDownEvent.type !== e.type && new Date().getTime() - lastclicktime < 500) {
                // This is to prevent chaotic behavior from both mouseDown and touchStart
                // events happening at the same coordinates
                $log.info("Ignoring event:");
                $log.info(e);
                return;
            }

            downEvent = sc.fixPageCoords(e);
            lastDownEvent = downEvent;
            downCoords = {left: downEvent.pageX, top: downEvent.pageY};
            lastclicktime = new Date().getTime();
        });
        $document.on('mousemove touchmove', className, function (e) {
            if (downEvent === null) {
                return;
            }

            var e2 = sc.fixPageCoords(e);
            if (sc.dist(downCoords, {left: e2.pageX, top: e2.pageY}) > 10) {
                // Moved too far away, cancel the event
                downEvent = null;
            }
        });
        $document.on('touchcancel', className, function (e) {
            downEvent = null;
        });
        $document.on('mouseup touchend', className, function (e) {
            if (downEvent !== null) {
                if (func($(this), downEvent)) {
                    //e.preventDefault();
                    //e.stopPropagation();
                }
                downEvent = null;
            }
        });
    };

    sc.onMouseOver = function (className, func) {
        $document.on('mouseover', className, function (e) {
            if (func($(this), sc.fixPageCoords(e))) {
                e.preventDefault();
                e.stopPropagation();
            }
        });
    };

    sc.onMouseOut = function (className, func) {
        $document.on('mouseout', className, function (e) {
            if (func($(this), sc.fixPageCoords(e))) {
                e.preventDefault();
                e.stopPropagation();
            }
        });
    };

    sc.onMouseOverOut = function (className, func) {
        // A combination function with a third parameter
        // true when over, false when out

        $document.on('mouseover', className, function (e) {
            if (func($(this), sc.fixPageCoords(e), true)) {
                e.preventDefault();
            }
        });

        $document.on('mouseout', className, function (e) {
            if (func($(this), sc.fixPageCoords(e), false)) {
                e.preventDefault();
            }
        });
    };

    sc.fixPageCoords = function (e) {
        if (!('pageX' in e) || (e.pageX === 0 && e.pageY === 0)) {
            e.pageX = e.originalEvent.touches[0].pageX;
            e.pageY = e.originalEvent.touches[0].pageY;
        }
        return e;
    };

};
