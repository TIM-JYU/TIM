export interface CellInfo {
    array: CellInfo[];
    hide: string[];
    defaultLanguage: string;
    linked: boolean;
    outputLocation: string;
    inputLocation: string;
    mode: string;
    code: string;
    evalButtonText: string;
    editor: unknown;
    collapse: unknown;
    session: unknown;
    interacts: unknown[];

    submit(event: unknown): void;
}

interface Sage {
    allLanguages: string[];
    templates: Record<string, {editor: string; hide: string[]}>;
    mathRender: (el: HTMLElement, cb?: () => void) => void;

    makeSagecell(args: unknown): CellInfo;

    deleteSagecell(cellInfo: CellInfo): void;

    moveInputForm(cellInfo: CellInfo): void;

    restoreInputForm(cellInfo: CellInfo): void;
}

declare const sage: Sage;
export default sage;
