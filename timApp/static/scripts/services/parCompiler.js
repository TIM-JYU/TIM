/* globals angular, $, timLogTime, MathJax */

var timApp = angular.module('timApp');

timApp.factory('ParCompiler', ['$http', '$window', '$q', '$httpParamSerializer', '$compile', '$ocLazyLoad', '$timeout',
    function ($http, $window, $q, $httpParamSerializer, $compile, $ocLazyLoad, $timeout) {
        "use strict";
        var parCompiler = {};

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
            $elem.find('.math').each(function () {
                parCompiler.processMath(this);
            });
            timLogTime("processAllMath end", "view");
        };

        parCompiler.processMath = function (elem) {
            try {
                $window.renderMathInElement(elem);
            }
            catch (e) {
                if (parCompiler.mathJaxLoaded) {
                    MathJax.Hub.Queue(["Typeset", MathJax.Hub, elem]);
                } else {
                    if (parCompiler.mathJaxLoadDefer === null) {
                        parCompiler.mathJaxLoadDefer = $.ajax({
                            dataType: "script",
                            cache: true,
                            url: "//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
                        });
                    }
                    parCompiler.mathJaxLoadDefer.done(function () {
                        parCompiler.mathJaxLoaded = true;
                        MathJax.Hub.Queue(["Typeset", MathJax.Hub, elem]);
                    });
                }
            }
        };

        return parCompiler;
    }]);
