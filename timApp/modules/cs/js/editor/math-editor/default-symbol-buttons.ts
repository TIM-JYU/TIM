/**
 * List of latex symbol buttons for formulaeditor
 */

import type {ITemplateButton} from "../../csPlugin";

export const DEFAULT_SYMBOL_BUTTONS: ITemplateButton[] = [
    {
        text: "\\[ \\pi \\]",
        data: "\\pi",
        expl: "\\pi",
        type: "q",
    },
    {
        text: "\\[ \\sin \\]",
        data: "\\sin",
        expl: "\\sin",
        type: "q",
    },
    {
        text: "\\[ \\cos \\]",
        data: "\\cos",
        expl: "\\cos",
        type: "q",
    },
    {
        text: "\\[ \\tan \\]",
        data: "\\tan",
        expl: "\\tan",
        type: "q",
    },
    {
        text: "\\[ \\overline{\\text{i}} \\]",
        data: "\\overline{\\text{i}}",
        expl: "\\overline{\\text{i}}",
        type: "s",
    },
    {
        text: "\\[ \\overline{\\text{j}} \\]",
        data: "\\overline{\\text{j}}",
        expl: "\\overline{\\text{j}}",
        type: "s",
    },
    {
        text: "\\[ \\overline{\\text{k}} \\]",
        data: "\\overline{\\text{k}}",
        expl: "\\overline{\\text{k}}",
        type: "s",
    },
    {
        text: "\\[ \\frac{\\square}{\\square} \\]",
        data: "\\frac{⁞}{}",
        expl: "\\frac{}{}",
        type: "s",
    },
    {
        text: "\\[ f(\\square) \\]",
        data: "f(⁞)",
        expl: "f()",
        type: "s",
    },
    {
        text: "\\[ \\sqrt{\\square} \\]",
        data: "\\sqrt{⁞}",
        expl: "\\sqrt{}",
        type: "s",
    },
    {
        text: "\\[ \\square^n \\]",
        data: "⁞^n",
        expl: "^n",
        type: "s",
    },
    {
        text: "\\[ \\lim_{\\square} \\]",
        data: "\\lim_{⁞}",
        expl: "\\lim_{}",
        type: "s",
    },
    {
        text: "\\[ \\sum_{\\square}^{\\square} \\]",
        data: "\\sum_{⁞}^{}",
        expl: "\\sum_{}^{}",
        type: "s",
    },
    {
        text: "\\[ \\int_\\square^\\square \\]",
        data: "\\int_{⁞}^{}",
        expl: "\\int_{}^{}",
        type: "s",
    },
    {
        text: "\\[ \\bigg/_{\\!\\!\\!\\!\\!{ \\square }}^{ \\square } \\]",
        data: "\\bigg/_{\\!\\!\\!\\!\\!{}}^{}",
        expl: "\\bigg/_{\\!\\!\\!\\!\\!{}}^{}",
        type: "s",
    },
    {
        text: "\\[ \\overrightarrow{\\square} \\]",
        data: "\\overrightarrow{⁞}",
        expl: "\\overrightarrow{ }",
        type: "s",
    },
    {
        text: "\\[ \\overleftarrow{\\square} \\]",
        data: "\\overleftarrow{⁞}",
        expl: "\\overleftarrow{ }",
        type: "s",
    },
    {
        text: "\\[ \\binom{\\square}{\\square} \\]",
        data: "\\binom{⁞}{}",
        expl: "\\binom{}{}",
        type: "s",
    },
    {
        text: "\\[ \\begin{cases}\\square&\\square\\\\\\square&\\square\\end{cases} \\]",
        data: "\\begin{cases}⁞&\\\\&\\end{cases}",
        expl: "\\begin{cases} &\\\\&\\end{cases}",
        type: "s",
    },
    {
        text: "\\[ \\begin{matrix}\\square&\\square\\\\\\square&\\square\\end{matrix} \\]",
        data: "\\begin{matrix}⁞&\\\\&\\end{matrix}",
        expl: "\\begin{matrix} &\\\\&\\end{matrix}",
        type: "s",
    },
    {
        text: "\\[ \\begin{array}{l|l}\\square&\\square\\\\\\hline\\square&\\square\\end{array} \\]",
        data: "\\begin{array}{l|l}⁞&\\\\\\hline&\\end{array}",
        expl: "\\begin{array}{l|l}&\\\\\\hline&\\end{array}",
        type: "s",
    },
];
