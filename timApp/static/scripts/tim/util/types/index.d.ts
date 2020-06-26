// All 3rd party libraries without type definitions should be listed here:
declare module "angular-messages";
declare module "angular-ui-grid";
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
declare module "katex/contrib/auto-render/auto-render" {

    interface Delimiter {
        left: string;
        right: string;
        display: boolean;
    }

    interface KatexRenderOptions {
        delimiters?: Delimiter[];
        ignoredTags?: string[];
        errorCallback: (s: string) => void;
    }

    const renderMathInElement: (e: Element, options?: KatexRenderOptions) => void;
    export = renderMathInElement;
}

interface INotesPlugin {
    open(filePath?: string): void;
}

interface IFixedRevealOptions extends RevealOptions {
    plugins: unknown[];
}

interface IFixedReveal extends RevealStatic {
    slide(indexh: number, indexv?: number, f?: number, o?: unknown): void;
    getPlugin(id: "notes"): INotesPlugin;
    initialize: (config: IFixedRevealOptions) => Promise<void>;
}

declare module "reveal.js" {
    const r: IFixedReveal;
    export default r;
}

declare module "reveal.js/plugin/zoom/zoom.esm" {
    const p: unknown;
    export default p;
}

declare module "reveal.js/plugin/notes/notes.esm" {
    const p: unknown;
    export default p;
}

declare module "ace/snippets";
declare module "rangyinputs";
declare module "url-search-params-polyfill";
declare module "eonasdan-bootstrap-datetimepicker";
