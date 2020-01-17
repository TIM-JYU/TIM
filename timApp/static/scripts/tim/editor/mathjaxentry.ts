import {browserAdaptor} from "mathjax-full/js/adaptors/browserAdaptor";
import {RegisterHTMLHandler} from "mathjax-full/js/handlers/html";
import {TeX} from "mathjax-full/js/input/tex";
import {AsciiMath} from "mathjax-full/js/input/asciimath";
import {AllPackages} from "mathjax-full/js/input/tex/AllPackages";
import {mathjax} from "mathjax-full/js/mathjax";
import {SVG} from "mathjax-full/js/output/svg";

RegisterHTMLHandler(browserAdaptor());
const tex = new TeX({packages: AllPackages});
const asciimath = new AsciiMath({});
const svg = new SVG();
export const texprocessor = () => mathjax.document(document, {InputJax: tex, OutputJax: svg});
export const asciimathprocessor = () => mathjax.document(document, {InputJax: asciimath, OutputJax: svg});
