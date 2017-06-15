import {$sanitize} from "./ngimport";

export function initAttributes(clone, $scope) {
    if (!clone[0]) {
        return;
    }
    const markJSON = "xxxJSONxxx";
    const markHex = "xxxHEXJSONxxx";
    let s = clone[0].textContent;
    const chex = s.indexOf(markHex) === 0;
    const cjson = s.indexOf(markJSON) === 0;
    if (!chex && !cjson) {
        return;
    }
    if (cjson) {
        s = s.substring(markJSON.length);
    }
    if (chex) {
        s = Hex2Str(s.substring(markHex.length));
    }
    $scope.attrs = JSON.parse(s);
    $scope.scope = $scope;
}

export function getHeading($scope, attrs, key, defElem) {
    const h = set($scope, attrs, key, "");
    if (!h) {
        return "";
    }
    const st = h.split("!!"); // h4 class="h3" width="23"!!Tehtava 1
    let elem = defElem;
    let val = st[0];
    let attributes = "";
    if (st.length >= 2) {
        elem = st[0];
        val = st[1];
    }
    const i = elem.indexOf(" ");
    let ea = [elem];
    if (i >= 0) {
        ea = [elem.substring(0, i), elem.substring(i)];
    }
    if (ea.length > 1) {
        elem = ea[0];
        attributes = " " + ea[1] + " ";
    }
    try {
        val = decodeURIComponent(encodeURI(val));
    } catch (err) {
    }
    let html = "<" + elem + attributes + ">" + val + "</" + elem + ">";
    html = $sanitize(html);
    return html;
}

export function get(jso, keys) {
    if (!jso) {
        return undefined;
    }
    let val = jso;
    for (const k in keys) {
        if (val === null) {
            return undefined;
        }
        if (!keys[k]) {
            continue;
        }
        val = val[keys[k]];
        if (val === undefined) {
            return undefined;
        }
    }
    return val;
}

export function setk(scope, sname, attrs, keys, def) {
    scope[sname] = def;
    let val = get(attrs, keys);
    if (val != undefined) {
        scope[sname] = val;
    }
    val = get(scope.attrs, keys);
    if (val != undefined) {
        scope[sname] = val;
    }
    if (scope[sname] == "None") {
        scope[sname] = "";
    }
    return scope[sname];
}

export function setn(scope, sname, attrs, name, def?) {
    if (name.indexOf(".") < 0) {
        name = "markup." + name;
    }
    const keys = name.split(".");
    return setk(scope, sname, attrs, keys, def);
}

export function set(scope, attrs, name, def?) {
    if (name.indexOf(".") < 0) {
        name = "markup." + name;
    }
    const keys = name.split(".");
    const sname = keys[keys.length - 1];
    return setk(scope, sname, attrs, keys, def);
}

export function Hex2Str(s) {
    let result = "";
    for (let i = 0; i < s.length; i += 2) {
        const c = String.fromCharCode(parseInt(s[i] + s[i + 1], 16));
        result += c;
    }
    return result;
}
