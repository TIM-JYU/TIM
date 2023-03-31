/**
 *
 */

export type FormulaType = {
    text: string;
    command: string;
    display: string;
    useWrite?: boolean;
};

export const FORMULAS: FormulaType[] = [
    {
        text: "\\pi",
        command: "\\pi",
        display: "\\[\\pi \\]",
    },
    {
        text: "\\sqrt{ }",
        command: "\\sqrt",
        display: "\\[\\sqrt{\\square} \\]",
    },
    {
        text: "\\sqrt[ ]{ }",
        command: "\\nthroot",
        display: "\\[\\sqrt[\\square]{\\square}\\]",
    },
    {
        text: "x^{ }",
        command: "x^",
        display: "\\[x^{\\square}\\]",
    },
    {
        text: "\\frac{ }{ }",
        command: "\\frac",
        display: "\\[\\frac{\\square}{\\square}\\]",
    },
    {
        text: "\\int_{ }^{ }",
        command: "\\int",
        display: "\\[\\int_{\\square}^{\\square}\\]",
    },
    {
        text: "\\overline{\\text{i}}",
        command: "\\bar{\\command{i}}",
        display: "\\[\\overline{\\text{i}}\\]",
        useWrite: true,
    },
    {
        text: "\\overline{\\text{j}}",
        command: "\\bar{\\command{j}}",
        display: "\\[\\overline{\\text{j}}\\]",
        useWrite: true,
    },
    {
        text: "\\overline{\\text{k}}",
        command: "\\bar{\\command{k}}",
        display: "\\[\\overline{\\text{k}}\\]",
        useWrite: true,
    },
    {
        text: "\\lim_{ }",
        command: "\\lim_{ }",
        display: "\\[\\lim_{\\square}\\]",
    },
    {
        text: "\\overrightarrow{ }",
        command: "\\overrightarrow",
        display: "\\[\\overrightarrow{\\square}\\]",
    },
    {
        text: "\\overleftarrow{ }",
        command: "\\overleftarrow",
        display: "\\[\\overleftarrow{\\square}\\]",
    },
    {
        text: "\\underrightarrow{ }",
        command: "\\underrightarrow",
        display: "\\[\\underrightarrow{\\square}\\]",
    },
    {
        text: "\\sin",
        command: "\\sin",
        display: "\\[\\sin \\]",
    },
    {
        text: "\\cos",
        command: "\\cos",
        display: "\\[\\cos \\]",
    },
    {
        text: "\\tan",
        command: "\\tan",
        display: "\\[\\tan \\]",
    },
    {
        text: "\\begin{cases}&\\\\&\\end{cases}",
        command: "\\cases",
        display:
            "\\[\\begin{cases}\n" +
            "\\square&\\square\\\\\n" +
            "\\square&\\square\n" +
            "\\end{cases}\\]",
    },
    {
        text: "\\begin{matrix}&\\\\&\\end{matrix}",
        command: "\\matrix",
        display:
            "\\[\\begin{matrix}\n" +
            "\\square&\\square\\\\\n" +
            "\\square&\\square\n" +
            "\\end{matrix}\\]",
    },
    {
        text: "\\binom{ }{ }",
        command: "\\binom",
        display: "\\[\\binom{\\square}{\\square}\\]",
    },
    {
        text: "\\_{ }^{ }",
        command: "\\_{ }^{ }",
        display: "\\[\\ \\square_{\\square}^{\\square}\\]",
    },
    {
        text: "\\mathrm{T}",
        command: "\\mathrm{T}",
        display: "\\[\\mathrm{T}\\]",
        useWrite: true,
    },
    {
        text:
            "\\begin{array}{l|l}\n" +
            "&\\\\\n" +
            "\\hline\n" +
            "&\n" +
            "\\end{array}",
        command: "\\array",
        display:
            "\\[\\begin{array}{l|l}\n" +
            "\\square&\\square\\\\\n" +
            "\\hline\n" +
            "\\square&\\square\n" +
            "\\end{array}\\]",
    },
];
