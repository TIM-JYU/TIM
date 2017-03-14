import * as angular from 'angular';
import {timApp} from 'tim/app';
import * as ocLazyLoad from 'oclazyload'
import {markAsAngular1Module} from "tim/angular-utils";

markAsAngular1Module(ocLazyLoad);

declare const timLogTime: (message: string, id: string, level?: number) => void;

timApp.factory('ParCompiler', ['$http', '$window', '$q', '$httpParamSerializer', '$compile', '$ocLazyLoad', '$timeout', '$log',
    ($http, $window, $q, $httpParamSerializer, $compile, $ocLazyLoad, $timeout, $log) => {
        class ParCompiler {
            mathJaxLoaded: boolean = false;
            mathJaxLoadDefer: JQueryXHR;

            compile(data, scope, callback) {
                const simpleDirectiveUrl = '/mmcq/SimpleDirective.js';
                const loadingFn = () => {
                    $ocLazyLoad.load(data.js.concat(data.css)).then(() => {
                        const compiled = $compile(data.texts)(scope);
                        this.processAllMathDelayed(compiled);
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
            }

            processAllMathDelayed($elem, delay?: number) {
                $timeout(() => {
                    this.processAllMath($elem);
                }, delay || 300);
            }

            processAllMath($elem) {
                timLogTime("processAllMath start", "view");
                let katexFailures = [];
                $elem.find('.math').each(() => {
                    const result = this.processMath(this, false);
                    if (result !== null) {
                        katexFailures.push(result);
                    }
                });
                if (katexFailures.length > 0) {
                    this.processMathJax(katexFailures);
                }
                timLogTime("processAllMath end", "view");
            }

            processMathJax(elements) {
                if (this.mathJaxLoaded) {
                    MathJax.Hub.Queue(["Typeset", MathJax.Hub, elements]);
                } else {
                    if (this.mathJaxLoadDefer === null) {
                        // HTML-CSS output does not work for MathJax in some mobile devices (e.g. Android Chrome, iPad),
                        // so we use SVG. Other output formats have not been tested so far.
                        this.mathJaxLoadDefer = $.ajax({
                            dataType: "script",
                            cache: true,
                            url: "//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_SVG"
                        });
                    }
                    this.mathJaxLoadDefer.done(() => {
                        this.mathJaxLoaded = true;
                        MathJax.Hub.Queue(["Typeset", MathJax.Hub, elements]);
                    });
                }
            }

            /**
             * Processes the math for a single element.
             *
             * @param elem The HTML element to process.
             * @param tryMathJax true to attempt to process using MathJax if KaTeX fails.
             * @returns null if KaTeX processed the element successfully. Otherwise, the failed element.
             */
            processMath(elem, tryMathJax: boolean) {
                try {
                    $window.renderMathInElement(elem);
                    return null;
                }
                catch (e) {
                    $log.warn(e.message);
                    if (tryMathJax) {
                        this.processMathJax(elem);
                    }
                    return elem;
                }
            }
        }

        return new ParCompiler();
    }]);
