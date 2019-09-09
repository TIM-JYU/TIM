import aceimp from "ace";
import "ace/ext-language_tools";

let base = (SystemJS.normalizeSync as (x: string) => unknown)("ace") as string;
base = base.substr(0, base.length - "ace.js".length);
// basePath will be like "http://domain.com/static/scripts/jspm_packages/github/ajaxorg/ace-builds@1.4.5/"
aceimp.config.set("basePath", base);

export const ace = aceimp;
export type IAce = typeof ace;
