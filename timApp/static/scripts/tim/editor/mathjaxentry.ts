import {browserAdaptor} from "mathjax-full/js/adaptors/browserAdaptor";
import {RegisterHTMLHandler} from "mathjax-full/js/handlers/html";
import {TeX} from "mathjax-full/js/input/tex";
import {AllPackages} from "mathjax-full/js/input/tex/AllPackages";
import {mathjax} from "mathjax-full/js/mathjax";
import {SVG} from "mathjax-full/js/output/svg";

RegisterHTMLHandler(browserAdaptor());
const tex = new TeX({packages: AllPackages});
const svg = new SVG();
export const mathjaxprocessor = mathjax.document(document, {InputJax: tex, OutputJax: svg});
