import angular = require("angular");
import {timApp} from "tim/app";
import * as ocLazyLoad from "oclazyload";
import * as renderMathInElement from "katex-auto-render";
import * as katex from "katex";
import {markAsUsed} from "tim/angular-utils";
import {timLogTime} from "tim/timTiming";
import $ = require("jquery");
import {ILazyLoad} from "oclazyload";
import {ILogService} from "angular";

markAsUsed(ocLazyLoad);

timApp.factory('ParCompiler', ['$http', '$window', '$q', '$compile', '$ocLazyLoad', '$timeout', '$log',
    ($http, $window, $q, $compile, $ocLazyLoad: ILazyLoad, $timeout, $log: ILogService) => {
        $window.katex = katex; // otherwise auto-render extension cannot find KaTeX
        class ParCompiler {
            private mathJaxLoaded: boolean = false;
            private mathJaxLoadDefer: JQueryXHR;

            public compile(data, scope, callback): void {
                const loadingFn = () => {
                    require(data.js, () => {
                        $ocLazyLoad.inject(data.angularModule).then(() => {
                            $ocLazyLoad.load(data.css).then(() => {
                                const compiled = $compile(data.texts)(scope);
                                this.processAllMathDelayed(compiled);
                                callback(compiled);
                            });
                        }, (err) => $log.error(err));
                    }, (err) => $log.error(err));
                };
                loadingFn();
            }

            public processAllMathDelayed($elem: JQuery, delay?: number): void {
                $timeout(() => {
                    this.processAllMath($elem);
                }, delay || 300);
            }

            public processAllMath($elem: JQuery) {
                timLogTime("processAllMath start", "view");
                let katexFailures = [];
                $elem.find('.math').each((index, elem) => {
                    const result = this.processMath(elem, false);
                    if (result !== null) {
                        katexFailures.push(result);
                    }
                });
                if (katexFailures.length > 0) {
                    this.processMathJax(katexFailures);
                }
                timLogTime("processAllMath end", "view");
            }

            public processMathJax(elements: Element[] | Element): void {
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
            public processMath(elem: Element, tryMathJax: boolean): Element {
                try {
                    renderMathInElement(elem);
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
