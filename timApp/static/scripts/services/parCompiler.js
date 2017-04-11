/* globals angular, $, timLogTime, MathJax */

var timApp = angular.module('timApp'); // ,['ParCompiler']);

timApp.factory('ParCompiler', ['$http', '$window', '$q', '$httpParamSerializer', '$compile', '$ocLazyLoad', '$timeout', '$log',
    function ($http, $window, $q, $httpParamSerializer, $compile, $ocLazyLoad, $timeout, $log) {
        "use strict";
        var parCompiler = {};
        GlobalParCompiler = parCompiler;

        parCompiler.compile = function (data, scope, callback) {
            var simpleDirectiveUrl = '/mmcq/SimpleDirective.js';
            var loadingFn = function () {
                $ocLazyLoad.load(data.js.concat(data.css)).then(function () {
                    var compiled = $compile(data.texts)(scope);
                    parCompiler.processAllMathDelayed(compiled);
                    callback(compiled);
                });
            };
            // Workaround: load SimpleDirective.js before other scripts; otherwise there
            // will be a ReferenceError.
            if (angular.isUndefined($window.standardDirective) &&
                data.js.indexOf(simpleDirectiveUrl) >= 0) {
                $.ajax({
                    dataType: "script",
                    cache: true,
                    url: simpleDirectiveUrl
                }).done(loadingFn);
            } else {
                loadingFn();
            }
        };

        parCompiler.mathJaxLoaded = false;
        parCompiler.mathJaxLoadDefer = null;

        parCompiler.processAllMathDelayed = function ($elem, delay) {
            $timeout(function () {
                parCompiler.processAllMath($elem);
            }, delay || 300);
        };

        parCompiler.processAllMath = function ($elem) {
            timLogTime("processAllMath start", "view");
            var katexFailures = [];
            $elem.find('.math').each(function () {
                var result = parCompiler.processMath(this, false);
                if (result !== null) {
                    katexFailures.push(result);
                }
            });
            if (katexFailures.length > 0) {
                parCompiler.processMathJax(katexFailures);
            }
            timLogTime("processAllMath end", "view");
        };

        parCompiler.processMathJax = function (elements) {
            if (parCompiler.mathJaxLoaded) {
                MathJax.Hub.Queue(["Typeset", MathJax.Hub, elements]);
            } else {
                if (parCompiler.mathJaxLoadDefer === null) {
                    // HTML-CSS output does not work for MathJax in some mobile devices (e.g. Android Chrome, iPad),
                    // so we use SVG. Other output formats have not been tested so far.
                    parCompiler.mathJaxLoadDefer = $.ajax({
                        dataType: "script",
                        cache: true,
                        url: "//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_SVG"
                    });
                }
                parCompiler.mathJaxLoadDefer.done(function () {
                    parCompiler.mathJaxLoaded = true;
                    MathJax.Hub.Queue(["Typeset", MathJax.Hub, elements]);
                });
            }
        };

        /**
         * Processes the math for a single element.
         *
         * @param elem The HTML element to process.
         * @param tryMathJax true to attempt to process using MathJax if KaTeX fails.
         * @returns null if KaTeX processed the element successfully. Otherwise, the failed element.
         */
        parCompiler.processMath = function (elem, tryMathJax) {
            try {
                $window.renderMathInElement(elem);
                return null;
            }
            catch (e) {
                $log.warn(e.message);
                if (tryMathJax) {
                    parCompiler.processMathJax(elem);
                }
                return elem;
            }
        };

        return parCompiler;
    }]);
