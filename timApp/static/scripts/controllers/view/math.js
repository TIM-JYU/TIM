/* globals angular, $, timLogTime, MathJax */

var timApp = angular.module('timApp');

timApp.defineMath = function (sc, http, q, $injector, $compile, $window, $document, $rootScope, $localStorage, $filter, $timeout, $log, Users) {
    "use strict";

    sc.mathJaxLoaded = false;
    sc.mathJaxLoadDefer = null;

    sc.processAllMathDelayed = function ($elem, delay) {
        $timeout(function () {
            sc.processAllMath($elem);
        }, delay || 300);
    };

    sc.processAllMath = function ($elem) {
        timLogTime("processAllMath start", "view");
        $elem.find('.math').each(function () {
            sc.processMath(this);
        });
        timLogTime("processAllMath end", "view");
    };

    sc.processMath = function (elem) {
        try {
            $window.renderMathInElement(elem);
        }
        catch (e) {
            if (sc.mathJaxLoaded) {
                MathJax.Hub.Queue(["Typeset", MathJax.Hub, elem]);
            } else {
                if (sc.mathJaxLoadDefer === null) {
                    sc.mathJaxLoadDefer = $.ajax({
                        dataType: "script",
                        cache: true,
                        url: "//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
                    });
                }
                sc.mathJaxLoadDefer.done(function () {
                    sc.mathJaxLoaded = true;
                    MathJax.Hub.Queue(["Typeset", MathJax.Hub, elem]);
                });
            }
        }
    };

    sc.processAllMathDelayed($('body'), 1500);
};
