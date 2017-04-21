import angular = require("angular");
import * as renderMathInElement from "katex-auto-render";
import {timLogTime} from "tim/timTiming";
import $ = require("jquery");
import {services} from "tim/ngimport";

class ParagraphCompiler {
    private mathJaxLoaded: boolean = false;
    private mathJaxLoadDefer: JQueryXHR;

    public compile(data, scope, callback): void {
        const requireComplete = () => {
            services.$ocLazyLoad.inject(data.angularModule).then(() => {
                services.$ocLazyLoad.load(data.css).then(() => {
                    const compiled = services.$compile(data.texts)(scope);
                    this.processAllMathDelayed(compiled);
                    callback(compiled);
                });
            }, (err) => services.$log.error(err));
        };

        if (data.js.length > 0) {
            SystemJS.amdRequire(data.js, requireComplete);
        } else {
            requireComplete();
        }
    }

    public processAllMathDelayed($elem: JQuery, delay?: number): void {
        services.$timeout(() => {
            this.processAllMath($elem);
        }, delay || 300);
    }

    public processAllMath($elem: JQuery) {
        timLogTime("processAllMath start", "view");
        const katexFailures = [];
        $elem.find(".math").each((index, elem) => {
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
                    url: "//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_SVG",
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
        } catch (e) {
            services.$log.warn(e.message);
            if (tryMathJax) {
                this.processMathJax(elem);
            }
            return elem;
        }
    }
}

export const ParCompiler = new ParagraphCompiler();
