// All 3rd party libraries without type definitions should be listed here:
declare module "angular-messages";
declare module "angular-timer";
declare module "angular-eonasdan-datetimepicker";
declare module "angularjs-color-picker";
declare module "bootstrap-sass";
declare module "angular-touch";
declare module "angular-drag-and-drop-lists";
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

interface INotesPlugin {
    open(filePath?: string): void;
}

interface IFixedReveal extends RevealStatic {
    slide(indexh: number, indexv?: number, f?: number, o?: unknown): void;
    getPlugin(id: "notes"): INotesPlugin;
}

declare module "reveal" {
    const x: IFixedReveal;
    export = x;
}
declare module "ace/snippets";
declare module "rangyinputs";

declare module "mathjax" {
    const MathJax: jax.IMathJax;
    export = MathJax;
}
