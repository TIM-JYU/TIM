/**
 * List of supported types formulas, and their features.
 *
 * @author Janne Lahti
 * @license MIT
 * @date 5.5.2023
 */

/**
 * Describes the type of formula, and the name to show to user.
 * Type is the same for the whole formula.
 */
export enum FormulaType {
    Inline = "Inline",
    Multiline = "Multiline",
    Align = "Align",
    Equation = "Equation",
    NotDefined = "undefined",
}

/**
 * Defines which properties formula types should have.
 */
type FormulaProperties = {
    // type of the formula
    type: FormulaType;
    // if it is a begin-end-style formula, define which
    beginEndKeyword: string;
    // starting mark of the formula
    start: string;
    // ending mark of the formula
    end: string;
    // String between formula lines, when it is a multiple line formula.
    // Single line formulas MUST HAVE empty string and multiple line
    // formulas MUST NOT HAVE empty string.
    join: string;
    // String between the first two formula lines. With empty string
    // every line is linked with the regular join.
    firstJoin: string;
    // Type of the formula that can be inside this formula.
    // NotDefined means that inner formula is not possible.
    inner: FormulaType;
    // Multiple line formulas require two constants
    // to set active field correctly when starting to edit.
    // One is for the first row and second is for other rows.
    activeFieldFirstConstant: number;
    activeFieldLineConstant: number;
};

/**
 * Properties determine how the different types of formulas behave.
 */
export const FormulaPropertyList: FormulaProperties[] = [
    {
        type: FormulaType.Inline,
        beginEndKeyword: "",
        start: "$",
        end: "$",
        join: "",
        firstJoin: "",
        inner: FormulaType.NotDefined,
        activeFieldFirstConstant: 0,
        activeFieldLineConstant: 0,
    },
    {
        type: FormulaType.Multiline,
        beginEndKeyword: "",
        start: "$$\n",
        end: "\n$$",
        join: "\\\\\n",
        firstJoin: "",
        inner: FormulaType.Align,
        activeFieldFirstConstant: 2,
        activeFieldLineConstant: 3,
    },
    {
        type: FormulaType.Align,
        beginEndKeyword: "align",
        start: "\\begin{align*}\n",
        end: "\n\\end{align*}",
        join: "\\\\\n&",
        firstJoin: "\n&",
        inner: FormulaType.NotDefined,
        activeFieldFirstConstant: 0,
        activeFieldLineConstant: 4,
    },
    {
        type: FormulaType.Equation,
        beginEndKeyword: "equation",
        start: "\\begin{equation*}\n",
        end: "\n\\end{equation*}",
        join: "",
        firstJoin: "",
        inner: FormulaType.NotDefined,
        activeFieldFirstConstant: 0,
        activeFieldLineConstant: 0,
    },
    {
        type: FormulaType.NotDefined,
        beginEndKeyword: "",
        start: "",
        end: "",
        join: "",
        firstJoin: "",
        inner: FormulaType.NotDefined,
        activeFieldFirstConstant: 0,
        activeFieldLineConstant: 0,
    },
];
