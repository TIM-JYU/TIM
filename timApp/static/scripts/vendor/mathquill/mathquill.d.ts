export interface IMathQuill {
    StaticMath: (htmlElement: HTMLElement) => BaseMethods;
    MathField: (
        htmlElement: HTMLElement,
        config?: MathQuillConfig
    ) => MathFieldMethods;
    getInterface: (version: 1 | 2) => IMathQuill;
    noConflict: () => IMathQuill;
    config: (MathQuillConfig: MathQuillConfig) => void;
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
        edit?: (mathField: MathFieldMethods) => void;
        enter?: (mathField: MathFieldMethods) => void;
        upOutOf?: (mathField: MathFieldMethods) => void;
        downOutOf?: (mathField: MathFieldMethods) => void;
        moveOutOf?: (dir: number, mathField: MathFieldMethods) => void;
        deleteOutOf?: (dir: number, mathField: MathFieldMethods) => void;
        selectOutOf?: (dir: number, mathField: MathFieldMethods) => void;
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
    dropEmbedded: (
        pageX: number,
        pageY: number,
        options: {
            htmlString: string;
            text: () => string;
            latex: () => string;
        }
    ) => this;
    html: () => string;
}

export const MathQuill: IMathQuill;
export default MathQuill;
