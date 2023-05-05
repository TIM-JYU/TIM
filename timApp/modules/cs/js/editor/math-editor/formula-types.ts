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
    Multi = "multi",
    Inline = "inline",
    Align = "align",
    Equation = "equation",
    NotDefined = "undefined",
}

/**
 * Defines which properties the formulas should have.
 */
type FormulaProperty = {
    type: FormulaType;
    beginEndKeyword: string;
    start: string;
    end: string;
    join: string;
    activeFieldFirstConstant: number;
    activeFieldLineConstant: number;
};

/**
 * Properties determine how the different types of formulas behave.
 */
export const FormulaProperties: FormulaProperty[] = [
    {
        type: FormulaType.Multi,
        beginEndKeyword: "",
        start: "$$\n",
        end: "\n$$",
        join: "\\\\\n",
        activeFieldFirstConstant: 2,
        activeFieldLineConstant: 3,
    },
    {
        type: FormulaType.Inline,
        beginEndKeyword: "",
        start: "$",
        end: "$",
        join: "",
        activeFieldFirstConstant: 0,
        activeFieldLineConstant: 0,
    },
    {
        type: FormulaType.Align,
        beginEndKeyword: "align",
        start: "\\begin{align*}\n",
        end: "\n\\end{align*}",
        join: "\\\\\n&",
        activeFieldFirstConstant: 0,
        activeFieldLineConstant: 4,
    },
    {
        type: FormulaType.Equation,
        beginEndKeyword: "equation",
        start: "\\begin{equation*}\n",
        end: "\n\\end{equation*}",
        join: "",
        activeFieldFirstConstant: 0,
        activeFieldLineConstant: 0,
    },
    {
        type: FormulaType.NotDefined,
        beginEndKeyword: "",
        start: "",
        end: "",
        join: "",
        activeFieldFirstConstant: 0,
        activeFieldLineConstant: 0,
    },
];
