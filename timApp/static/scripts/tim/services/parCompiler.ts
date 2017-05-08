import katex from "katex-auto-render";
import {$compile, $log, $ocLazyLoad, $timeout} from "tim/ngimport";
import {timLogTime} from "tim/timTiming";
import {lazyLoad, lazyLoadMany} from "../lazyLoad";

export class ParagraphCompiler {
    public async compile(data, scope, callback) {
        await lazyLoadMany(data.js);
        await $ocLazyLoad.inject(data.angularModule);
        await $ocLazyLoad.load(data.css);
        const compiled = $compile(data.texts)(scope);
        this.processAllMathDelayed(compiled);
        callback(compiled);
    }

    public processAllMathDelayed($elem: JQuery, delay?: number): void {
        $timeout(() => {
            this.processAllMath($elem);
        }, delay || 300);
    }

    public async processAllMath($elem: JQuery) {
        timLogTime("processAllMath start", "view");
        const katexFailures = [];
        const mathelems = $elem.find(".math");
        if (mathelems.length === 0) {
            return;
        }
        const renderMathInElement = await lazyLoad<typeof katex>("katex-auto-render");
        mathelems.each((index, elem) => {
            const result = this.processMath(renderMathInElement, elem, false);
            if (result !== null) {
                katexFailures.push(result);
            }
        });
        if (katexFailures.length > 0) {
            this.processMathJax(katexFailures);
        }
        timLogTime("processAllMath end", "view");
    }

    public async processMathJax(elements: Element[] | Element) {
        const MathJax = await lazyLoad<jax.IMathJax>("mathjax");
        MathJax.Hub.Queue(["Typeset", MathJax.Hub, elements]);
    }

    /**
     * Processes the math for a single element.
     *
     * @param katexFunction The KaTeX function that processes the elements.
     * @param elem The HTML element to process.
     * @param tryMathJax true to attempt to process using MathJax if KaTeX fails.
     * @returns null if KaTeX processed the element successfully. Otherwise, the failed element.
     */
    public processMath(katexFunction: (e: Element) => void,
                       elem: Element,
                       tryMathJax: boolean): Element {
        try {
            katexFunction(elem);
            return null;
        } catch (e) {
            $log.warn(e.message);
            if (tryMathJax) {
                this.processMathJax(elem);
            }
            return elem;
        }
    }
}

export const ParCompiler = new ParagraphCompiler();
