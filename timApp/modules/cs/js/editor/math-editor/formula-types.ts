/**
 * List of supported types formulas, and their features.
 *
 * @author Janne Lahti
 * @license MIT
 * @date 5.5.2023
 */

export type ReplacePair = [RegExp, string] | undefined;

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
    // First value is search RegExp and second is replace string.
    // editReplace is used when receiving latex code from latex field.
    // writeReplace is used when receiving latex code from visual field.
    editReplace: ReplacePair;
    writeReplace: ReplacePair;
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

/**
 * Properties determine how the different types of formulas behave.
 */
export const FormulaPropertyList: FormulaProperties[] = [
    {
        type: FormulaType.Inline,
        beginEndKeyword: "",
        editReplace: undefined,
        writeReplace: undefined,
        start: "$",
        end: "$",
        join: "",
        inner: [],
        activeFieldConstant: 0,
    },
    {
        type: FormulaType.Multiline,
        beginEndKeyword: "",
        editReplace: undefined,
        writeReplace: undefined,
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
        editReplace: [/(?<!\\)&/gm, "¤"],
        writeReplace: [/¤/gm, "&"],
        start: "\\begin{align*}\n",
        end: "\n\\end{align*}",
        join: "\\\\\n",
        inner: [],
        activeFieldConstant: 2,
    },
    {
        type: FormulaType.AlignAt,
        beginEndKeyword: "alignat",
        editReplace: [/(?<!\\)&/gm, "¤"],
        writeReplace: [/¤/gm, "&"],
        start: "\\begin{alignat*}{}\n",
        end: "\n\\end{alignat*}",
        join: "\\\\\n",
        inner: [],
        activeFieldConstant: 2,
    },
    {
        type: FormulaType.Gather,
        beginEndKeyword: "gather",
        editReplace: [/(?<!\\)&/gm, "¤"],
        writeReplace: [/¤/gm, "&"],
        start: "\\begin{gather*}\n",
        end: "\n\\end{gather*}",
        join: "\\\\\n",
        inner: [],
        activeFieldConstant: 2,
    },
    {
        type: FormulaType.Equation,
        beginEndKeyword: "equation",
        editReplace: undefined,
        writeReplace: undefined,
        start: "\\begin{equation*}\n",
        end: "\n\\end{equation*}",
        join: "",
        inner: [],
        activeFieldConstant: 0,
    },
    {
        type: FormulaType.NotDefined,
        beginEndKeyword: "",
        editReplace: undefined,
        writeReplace: undefined,
        start: "",
        end: "",
        join: "",
        inner: [],
        activeFieldConstant: 0,
    },
];
