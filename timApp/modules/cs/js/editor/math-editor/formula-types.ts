/**
 * List of supported types formulas, and their features.
 *
 * @author Janne Lahti
 * @license MIT
 * @date 5.5.2023
 */

/**
 * Describes the type of formula. Type is the same for the whole formula.
 */
export enum FormulaType {
    Inline = "inline",
    Multi = "multi",
    Align = "align",
    Equation = "equation",
    NotDefined = "undefined",
}

/**
 * Defines which properties the formulas should have.
 */
type FormulaProperty = {
    // type of the formula
    type: FormulaType;
    // name of the formula showed to user
    name: string;
    // if it is a begin-end-style formula, define which
    beginEndKeyword: string;
    // starting mark of the formula
    start: string;
    // ending mark of the formula
    end: string;
    // string between formula lines, if it is a multiple line formula
    // single line formulas MUST HAVE empty string
    join: string;
    // multiple line formulas require two constants to set active field
    // one is for the first row and second is for other rows
    activeFieldFirstConstant: number;
    activeFieldLineConstant: number;
};

/**
 * Properties determine how the different types of formulas behave.
 */
export const FormulaProperties: FormulaProperty[] = [
    {
        type: FormulaType.Inline,
        name: "Inline",
        beginEndKeyword: "",
        start: "$",
        end: "$",
        join: "",
        activeFieldFirstConstant: 0,
        activeFieldLineConstant: 0,
    },
    {
        type: FormulaType.Multi,
        name: "Multiline",
        beginEndKeyword: "",
        start: "$$\n",
        end: "\n$$",
        join: "\\\\\n",
        activeFieldFirstConstant: 2,
        activeFieldLineConstant: 3,
    },
    {
        type: FormulaType.Align,
        name: "Align",
        beginEndKeyword: "align",
        start: "\\begin{align*}\n",
        end: "\n\\end{align*}",
        join: "\\\\\n&",
        activeFieldFirstConstant: 0,
        activeFieldLineConstant: 4,
    },
    {
        type: FormulaType.Equation,
        name: "Equation",
        beginEndKeyword: "equation",
        start: "\\begin{equation*}\n",
        end: "\n\\end{equation*}",
        join: "",
        activeFieldFirstConstant: 0,
        activeFieldLineConstant: 0,
    },
    {
        type: FormulaType.NotDefined,
        name: "",
        beginEndKeyword: "",
        start: "",
        end: "",
        join: "",
        activeFieldFirstConstant: 0,
        activeFieldLineConstant: 0,
    },
];
