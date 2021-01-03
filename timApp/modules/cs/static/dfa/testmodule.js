import { varsStringToJson } from './vars.js';

let s = "x 630, y: 40, w: 3, h:2, s";
let res = varsStringToJson(s);
console.log(JSON.stringify(res));

