import {$sanitize} from "./ngimport";

export function initAttributes(clone, $scope) {
"use strict";
if (!clone[0]) return;
let markJSON = "xxxJSONxxx";
let markHex = "xxxHEXJSONxxx";
let s = clone[0].textContent;
let chex = s.indexOf(markHex) === 0;
let cjson = s.indexOf(markJSON) === 0;
if (!chex && !cjson) {
        return;
    }
if (cjson) s = s.substring(markJSON.length);
if (chex) s = Hex2Str(s.substring(markHex.length));
$scope.attrs = JSON.parse(s);
$scope.scope = $scope;
}

export function getHeading($scope, attrs, key, defElem) {
"use strict";
let h = set($scope, attrs, key, "");
if ( !h ) return "";
let st = h.split("!!"); // h4 class="h3" width="23"!!Tehtava 1
let elem = defElem;
let val = st[0];
let attributes = "";
if ( st.length >= 2 ) { elem = st[0]; val = st[1]; }
let i = elem.indexOf(" ");
let ea = [elem];
if ( i >= 0 ) ea = [elem.substring(0, i), elem.substring(i)];
if ( ea.length > 1 ) { elem = ea[0]; attributes = " " + ea[1] + " "; }
try {
	  val = decodeURIComponent(encodeURI(val));
	} catch (err) {}
let html = "<" + elem + attributes + ">" + val + "</" + elem + ">";
html = $sanitize(html);
return html;
}

export function get(jso, keys) {
"use strict";
if ( !jso ) return undefined;
let val = jso;
for (let k in keys ) {
        if ( val === null ) return undefined;
        if ( !keys[k] ) continue;
        val = val[keys[k]];
        if ( val === undefined ) return undefined;
    }
return val;
}

export function setk(scope, sname, attrs, keys, def) {
"use strict";
scope[sname] = def;
let val = get(attrs, keys);
if ( val != undefined ) scope[sname] = val;
val = get(scope.attrs, keys);
if ( val != undefined ) scope[sname] = val;
if ( scope[sname] == "None" ) scope[sname] = "";
return scope[sname];
}

export function setn(scope, sname, attrs, name, def?) {
"use strict";
if ( name.indexOf(".") < 0 ) name = "markup." + name;
let keys = name.split(".");
return setk(scope, sname, attrs, keys, def);
}

export function set(scope, attrs, name, def?) {
"use strict";
if ( name.indexOf(".") < 0 ) name = "markup." + name;
let keys = name.split(".");
let sname = keys[keys.length - 1];
return setk(scope, sname, attrs, keys, def);
}

export function Hex2Str(s) {
"use strict";
let result = "";
for (let i = 0; i < s.length; i += 2) {
    let c = String.fromCharCode(parseInt(s[i] + s[i + 1], 16));
    result += c;
  }
return result;
}
