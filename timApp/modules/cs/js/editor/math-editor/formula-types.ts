/**
 * List of supported types formulas, and their features.
 *
 * @author Janne Lahti
 * @license MIT
 * @date 5.5.2023
 */

export type ReplacePair = [RegExp, string] | undefined;
export type ReplaceFunction =
    | [(what: string, to: string) => string, string]
    | undefined;

export function isReplacePair(
    pair: ReplacePair | ReplaceFunction
): pair is ReplacePair {
    return pair !== undefined && typeof pair[0] === "object";
}

export function matchAndReplace(
    s: string,
    replace: ReplacePair | ReplaceFunction,
    replaceWith?: string
): string {
    if (replace === undefined) {
        return s;
    }
    const replacement = replaceWith ?? replace[1];
    if (isReplacePair(replace)) {
        const regex = replace[0];
        return s.replace(regex, replacement);
    } else {
        return replace[0](s, replacement);
    }
}

type MaybeStringPair = [string, string] | undefined;

/**
 * Describes the type of formula, and the name to show to user.
 * Type is the same for the whole formula.
 */
export enum FormulaType {
    Inline = "Inline",
    Multiline = "Multiline",
    Align = "Align",
    AlignAt = "AlignAt",
    Gather = "Gather",
    Equation = "Equation",
    NotDefined = "undefined",
}

export const FORMULA_TYPES: Partial<Record<string, string>> = {
    [FormulaType.Inline.toString()]: $localize`Inline`,
    [FormulaType.Multiline.toString()]: $localize`Multiline`,
    [FormulaType.Align.toString()]: $localize`Align`,
    [FormulaType.AlignAt.toString()]: $localize`AlignAt`,
    [FormulaType.Gather.toString()]: $localize`Gather`,
    [FormulaType.Equation.toString()]: $localize`Equation`,
    [FormulaType.NotDefined.toString()]: $localize`undefined`,
};

/**
 * Defines which properties formula types should have.
 */
type FormulaProperties = {
    // type of the formula
    type: FormulaType;
    // if it is a begin-end-style formula, define which
    beginEndKeyword: string;
    // If first string is typed into visual field,
    // it will be replaced by second string.
    // No replacing if undefined.
    typeReplace: MaybeStringPair;
    // writeReplace is used when copying text from visual field to latex field.
    // editReplace is used when copying text from latex field to visual field.
    // First value is search RegExp and second is replace string.
    // No replacing if undefined.
    writeReplace: ReplacePair;
    editReplace: ReplacePair | ReplaceFunction;
    // starting mark of the formula
    start: string;
    // ending mark of the formula
    end: string;
    // String between formula lines, when it is a multiple line formula.
    // Single line formula types MUST HAVE empty string and multiple line
    // formula types MUST NOT HAVE empty string.
    join: string;
    // List of the types of formulas that can be inside this formula.
    inner: FormulaType[];
    // Multiple line formulas require a constant to set
    // active field correctly when starting to edit.
    activeFieldConstant: number;
};

// This is needed because Safari <16.4 does not support lookbehind
// See https://caniuse.com/js-regexp-lookbehind
function lookBehindReplace(what: string, to: string, regex: RegExp) {
    const reversed = what.split("").reverse().join("");
    const reversedReplaced = reversed.replace(regex, to);
    return reversedReplaced.split("").reverse().join("");
}

const alignMarkEditReplace: ReplaceFunction = [
    (what, to) => lookBehindReplace(what, to, /&(?!\\)/gm),
    "＆",
];

/**
 * Properties determine how the different types of formulas behave.
 */
export const FormulaPropertyList: FormulaProperties[] = [
    {
        type: FormulaType.Inline,
        beginEndKeyword: "",
        typeReplace: undefined,
        writeReplace: undefined,
        editReplace: undefined,
        start: "$",
        end: "$",
        join: "",
        inner: [],
        activeFieldConstant: 0,
    },
    {
        type: FormulaType.Multiline,
        beginEndKeyword: "",
        typeReplace: undefined,
        writeReplace: undefined,
        editReplace: undefined,
        start: "$$\n",
        end: "\n$$",
        join: "\\\\\n",
        inner: [
            FormulaType.Align,
            FormulaType.AlignAt,
            FormulaType.Gather,
            FormulaType.Equation,
        ],
        activeFieldConstant: 2,
    },
    {
        type: FormulaType.Align,
        beginEndKeyword: "align*",
        typeReplace: ["\\&", "＆"],
        writeReplace: [/＆/gm, "&"],
        editReplace: alignMarkEditReplace,
        start: "\\begin{align*}\n",
        end: "\n\\end{align*}",
        join: "\\\\\n",
        inner: [],
        activeFieldConstant: 2,
    },
    {
        type: FormulaType.AlignAt,
        beginEndKeyword: "alignat",
        typeReplace: ["\\&", "＆"],
        writeReplace: [/＆/gm, "&"],
        editReplace: alignMarkEditReplace,
        start: "\\begin{alignat*}{}\n",
        end: "\n\\end{alignat*}",
        join: "\\\\\n",
        inner: [],
        activeFieldConstant: 2,
    },
    {
        type: FormulaType.Gather,
        beginEndKeyword: "gather",
        typeReplace: ["\\&", "＆"],
        writeReplace: [/＆/gm, "&"],
        editReplace: alignMarkEditReplace,
        start: "\\begin{gather*}\n",
        end: "\n\\end{gather*}",
        join: "\\\\\n",
        inner: [],
        activeFieldConstant: 2,
    },
    {
        type: FormulaType.Equation,
        beginEndKeyword: "equation",
        typeReplace: undefined,
        writeReplace: undefined,
        editReplace: undefined,
        start: "\\begin{equation*}\n",
        end: "\n\\end{equation*}",
        join: "",
        inner: [],
        activeFieldConstant: 0,
    },
    {
        type: FormulaType.NotDefined,
        beginEndKeyword: "",
        typeReplace: undefined,
        writeReplace: undefined,
        editReplace: undefined,
        start: "",
        end: "",
        join: "",
        inner: [],
        activeFieldConstant: 0,
    },
];
