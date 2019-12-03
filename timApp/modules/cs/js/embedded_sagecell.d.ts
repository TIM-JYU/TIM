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
    editor: any;
    collapse: any;
    session: any;
    interacts: any[];

    submit(event: any): void;
}

interface Sage {
    allLanguages: string[];
    templates: {[name: string]: {editor: string, hide: string[]}};

    makeSagecell(args: any): CellInfo;

    deleteSagecell(cellInfo: CellInfo): void;

    moveInputForm(cellInfo: CellInfo): void;

    restoreInputForm(cellInfo: CellInfo): void;
}

const sage: Sage;
export default sage;
