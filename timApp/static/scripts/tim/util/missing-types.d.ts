
// All 3rd party libraries without type definitions should be listed here:
declare module "humanize-duration";
declare module "angular-messages";
declare module "angular-timer";
declare module "angular-eonasdan-datetimepicker";
declare module "bootstrap-sass";
declare module "angular-touch";
declare module "angular-bootstrap-colorpicker";
declare module "katex-auto-render" {
    const renderMathInElement: (e: Element) => void;
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
declare let require: typeof SystemJS.amdRequire;

declare module "mathjax" {
    const MathJax: jax.IMathJax;
    export = MathJax;
}
