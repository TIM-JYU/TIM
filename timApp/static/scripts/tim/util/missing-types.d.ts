// All 3rd party libraries without type definitions should be listed here:
declare module "humanize-duration";
declare module "angular-messages";
declare module "angular-timer";
declare module "angular-eonasdan-datetimepicker";
declare module "angularjs-color-picker";
declare module "bootstrap-sass";
declare module "angular-touch";
declare module "angular-bootstrap-colorpicker";
declare module "angular-diff-match-patch" {
    const moduleName: string;
    export = moduleName;
}
declare module "katex-auto-render" {

    interface Delimiter {
        left: string;
        right: string;
        display: boolean;
    }

    interface KatexRenderOptions {
        delimiters?: Delimiter[];
        ignoredTags?: string[];
    }

    const renderMathInElement: (e: Element, options?: KatexRenderOptions) => void;
    export = renderMathInElement;
}
declare module "reveal";
declare module "ace" {
    import Ace = AceAjax.Ace;
    let ace: Ace;
    export default ace;
}
declare module "ace/snippets";
declare module "rangyinputs";

// All inline module definitions that exist in HTML templates should be listed here:
declare module "tim/plugins";
declare module "tim/angularmodules";
declare module "tim/extramodules";
declare module "tim/session" {
    let clock_offset: string | undefined;
    let editortab: string | undefined;
    let timelimit: string | undefined;
}
declare module "tim/show_slide_vars";

declare module "mathjax" {
    const MathJax: jax.IMathJax;
    export = MathJax;
}
