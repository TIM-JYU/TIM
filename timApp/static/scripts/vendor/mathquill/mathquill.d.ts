export interface IMathQuill {
    StaticMath: (htmlElement: HTMLElement) => BaseMethods;
    MathField: (
        htmlElement: HTMLElement,
        config?: MathQuillConfig
    ) => MathFieldMethods;
    getInterface: (version: 1 | 2) => IMathQuill;
    noConflict: () => IMathQuill;
    config: (MathQuillConfig: any) => void;
    getLatexMathParser: () => LatexMathParser;
}

export interface LatexMathParser extends Parser {
    commandToBlock: (cmd: any) => any;
    joinBlocks: (blocks: any[]) => any;
}

export interface Parser {
    parse: (stream: string) => any;
}

export interface MathQuillConfig {
    spaceBehavesLikeTab?: boolean;
    leftRightIntoCmdGoes?: "up" | "down";
    restrictMismatchedBrackets?: boolean;
    sumStartsWithNEquals?: boolean;
    supSubsRequireOperand?: boolean;
    charsThatBreakOutOfSupSub?: string;
    autoSubscriptNumerals?: boolean;
    autoCommands?: string;
    autoOperatorNames?: string;
    substituteTextArea?: () => HTMLElement;
    handlers?: {
        edit?: (mathField: MathFieldMethods) => any;
        enter?: (mathField: MathFieldMethods) => any;
        upOutOf?: (mathField: MathFieldMethods) => any;
        downOutOf?: (mathField: MathFieldMethods) => any;
        moveOutOf?: (dir: number, mathField: MathFieldMethods) => any;
        deleteOutOf?: (dir: number, mathField: MathFieldMethods) => any;
        selectOutOf?: (dir: number, mathField: MathFieldMethods) => any;
    };
}

export interface BaseMethods {
    revert: () => HTMLElement;
    reflow: () => void;
    el: () => HTMLElement;

    latex(latex: string): MathFieldMethods;

    latex(): string;
}

export interface MathFieldMethods extends BaseMethods {
    focus: () => this;
    blur: () => this;
    write: (latex: string) => this;
    cmd: (latex: string) => this;
    select: () => this;
    clearSelection: () => this;
    moveToLeftEnd: () => this;
    moveToRightEnd: () => this;
    movetoDirEnd: (direction: this) => this;
    keystroke: (keys: string) => this;
    typedText: (text: string) => this;
    config: (config: MathQuillConfig) => this;
    dropEmbedded: (pageX: any, pageY: any, options: any) => this;
    html: () => any;
}

export const MathQuill: IMathQuill;
export default MathQuill;
