import * as ace from "ace";
import "ace/ext-language_tools";

let base = SystemJS.normalizeSync("ace");
base = base.substr(0, base.length - "ace.js".length);
// basePath will be like "http://domain.com/static/scripts/jspm_packages/github/ajaxorg/ace-builds@1.2.6/"
(ace as any).config.set("basePath", base);

export = ace;
