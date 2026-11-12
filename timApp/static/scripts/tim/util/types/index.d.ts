// All 3rd party libraries without type definitions should be listed here:
declare module "diff-match-patch" {
    type Diff = [number, string];
    export default class DiffMatchPatch {
        diff_main(text1: string, text2: string, checklines?: boolean): Diff[];
        diff_cleanupSemantic(diffs: Diff[]): void;
        diff_linesToChars_(
            text1: string,
            text2: string
        ): {chars1: string; chars2: string; lineArray: string[]};
        diff_charsToLines_(diffs: Diff[], lineArray: string[]): void;
    }
}
declare module "bootstrap-sass";
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

declare module "ace/snippets";
declare module "rangyinputs";
declare module "url-search-params-polyfill";
