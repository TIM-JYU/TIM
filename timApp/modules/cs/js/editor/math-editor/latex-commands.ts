/**
 *
 */

const formulas = [
    {
        text: "\\pi",
        display: "\\[\\pi \\]",
    },
    {
        text: "\\sqrt{ }",
        display: "\\[\\sqrt{\\square} \\]",
    },
    {
        text: "\\sqrt[ ]{ }",
        display: "\\[\\sqrt[\\square]{\\square}\\]",
    },
    {
        text: "x^{ }",
        display: "\\[x^{\\square}\\]",
    },
    {
        text: "\\frac{ }{ }",
        display: "\\[\\frac{\\square}{\\square}\\]",
    },
    {
        text: "\\int_{ }^{ }",
        display: "\\[\\int_{\\square}^{\\square}\\]",
    },
    {
        text: "\\overline{\\text{i}}",
        display: "\\[\\overline{\\text{i}}\\]",
    },
    {
        text: "\\overline{\\text{j}}",
        display: "\\[\\overline{\\text{j}}\\]",
    },
    {
        text: "\\overline{\\text{k}}",
        display: "\\[\\overline{\\text{k}}\\]",
    },
    {
        text: "\\lim_{ }",
        display: "\\[\\lim_{\\square}\\]",
    },
    {
        text: "\\overrightarrow{ }",
        display: "\\[\\overrightarrow{\\square}\\]",
    },
    {
        text: "\\overleftarrow{ }",
        display: "\\[\\overleftarrow{\\square}\\]",
    },
    {
        text: "\\underrightarrow{ }",
        display: "\\[\\underrightarrow{\\square}\\]",
    },
    {
        text: "\\sin",
        display: "\\[\\sin \\]",
    },
    {
        text: "\\cos",
        display: "\\[\\cos \\]",
    },
    {
        text: "\\tan",
        display: "\\[\\tan \\]",
    },
    {
        text: "\\begin{cases}&\\\\&\\end{cases}",
        display:
            "\\[\\begin{cases}\n" +
            "\\square&\\square\\\\\n" +
            "\\square&\\square\n" +
            "\\end{cases}\\]",
    },
    {
        text: "\\begin{matrix}&\\\\&\\end{matrix}",
        display:
            "\\[\\begin{matrix}\n" +
            "\\square&\\square\\\\\n" +
            "\\square&\\square\n" +
            "\\end{matrix}\\]",
    },
    {
        text: "\\binom{ }{ }",
        display: "\\[\\binom{\\square}{\\square}\\]",
    },
    {
        text: "\\_{ }^{ }",
        display: "\\[\\ \\square_{\\square}^{\\square}\\]",
    },
    {
        text: "\\mathrm{T}",
        display: "\\[\\mathrm{T}\\]",
    },
    {
        text:
            "\\begin{array}{l|l}\n" +
            "&\\\\\n" +
            "\\hline\n" +
            "&\n" +
            "\\end{array}",
        display:
            "\\[\\begin{array}{l|l}\n" +
            "\\square&\\square\\\\\n" +
            "\\hline\n" +
            "\\square&\\square\n" +
            "\\end{array}\\]",
    },
];

export default formulas;
