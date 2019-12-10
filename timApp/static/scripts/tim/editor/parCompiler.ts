import {IScope} from "angular";
import {staticDynamicImport} from "tim/staticDynamicImport";
import {$compile, $injector, $log, $timeout} from "tim/util/ngimport";
import {timLogTime} from "tim/util/timTiming";
import {ViewCtrl} from "../document/viewctrl";
import {injectStyle, ModuleArray} from "../util/utils";

export interface IPluginInfoResponse {
    js: string[];
    texts: string;
    css: string[];
    trdiff?: {old: string, new: string};
}

export async function compileWithViewctrl(html: string | Element | JQuery<HTMLElement>,
                                    scope: IScope,
                                    view: ViewCtrl | undefined,
                                    extraCtrls: {[name: string]: {instance: unknown}} = {}) {
    const result = $compile(html)(scope,
        undefined,
        view ? {
            transcludeControllers: {
                timView: {instance: view},
                ...extraCtrls,
            },
        } : {});
    await $timeout();
    return result;
}

export class ParagraphCompiler {
    /**
     * Private function. Use one of: compileAnd{AppendTo,Replace,After,Before}.
     * @param data
     * @param scope
     * @param view
     */
    private async compile(data: IPluginInfoResponse, scope: IScope, view?: ViewCtrl) {
        for (const m of data.js) {
            const modLoad = staticDynamicImport(m);
            if (!modLoad) {
                continue;
            }
            const mod = await modLoad as {moduleDefs?: unknown};
            const defs = mod.moduleDefs;
            if (ModuleArray.is(defs)) {
                $injector.loadNewModules(defs.map((d) => d.name));
            }
        }
        data.css.forEach((s) => injectStyle(s));
        return compileWithViewctrl(data.texts, scope, view);
    }

    public async compileAndAppendTo(element: JQuery, data: IPluginInfoResponse, scope: IScope, view?: ViewCtrl) {
        return this.compileAndDOMAction((e, c) => e.empty().append(c), element, data, scope, view);
    }

    public async compileAndReplace(element: JQuery, data: IPluginInfoResponse, scope: IScope, view?: ViewCtrl) {
        return this.compileAndDOMAction((e, c) => e.replaceWith(c), element, data, scope, view);
    }

    public async compileAndAfter(element: JQuery, data: IPluginInfoResponse, scope: IScope, view?: ViewCtrl) {
        return this.compileAndDOMAction((e, c) => e.after(c), element, data, scope, view);
    }

    public async compileAndBefore(element: JQuery, data: IPluginInfoResponse, scope: IScope, view?: ViewCtrl) {
        return this.compileAndDOMAction((e, c) => e.before(c), element, data, scope, view);
    }

    public async compileAndDOMAction(
        action: (target: JQuery, compiled: JQuery) => void,
        element: JQuery,
        data: IPluginInfoResponse,
        scope: IScope,
        view?: ViewCtrl,
    ) {
        const compiled = await this.compile(data, scope, view);
        action(element, compiled);
        await this.processAllMath(compiled);
        return compiled;
    }

    public async processAllMathDelayed($elem: JQuery, delay?: number) {
        await $timeout(() => {
            this.processAllMath($elem);
        }, delay || 300);
    }

    public async processAllMath($elem: JQuery) {
        const katexFailures: Element[] = [];
        const mathelems = $elem.find(".math");
        if (mathelems.length === 0) {
            return;
        }
        timLogTime("processAllMath start", "view");
        const renderMathInElement = await import("katex/contrib/auto-render/auto-render");
        mathelems.each((index, elem) => {
            const result = this.processMath(renderMathInElement.default, elem, false);
            if (result != null) {
                katexFailures.push(result);
            }
        });
        if (katexFailures.length > 0) {
            this.processMathJax(katexFailures);
        }
        timLogTime("processAllMath end", "view");
    }

    /**
     * Processes MathJax in the given elements. The elements must be in the DOM already.
     * @param elements The element(s) to process.
     */
    public async processMathJax(elements: Element[] | Element) {
        const es = elements instanceof Array ? elements : [elements];
        const mathjaxprocessor = (await import("./mathjaxentry")).mathjaxprocessor();
        mathjaxprocessor.findMath({elements: es}).compile().getMetrics().typeset().updateDocument();
    }

    /**
     * Processes the math for a single element.
     *
     * @param katexFunction The KaTeX function that processes the elements.
     * @param elem The HTML element to process.
     * @param tryMathJax true to attempt to process using MathJax if KaTeX fails.
     * @returns null if KaTeX processed the element successfully. Otherwise, the failed element.
     */
    public processMath(katexFunction: typeof import("katex/contrib/auto-render/auto-render"),
                       elem: Element,
                       tryMathJax: boolean): Element | null {
        let lastError: string | undefined;
        katexFunction(elem, {
            errorCallback: (s) => {
                lastError = s;
            },
        });
        if (!lastError) {
            return null;
        }
        $log.warn(lastError);
        if (tryMathJax) {
            this.processMathJax(elem);
        }
        return elem;
    }
}

export const ParCompiler = new ParagraphCompiler();
