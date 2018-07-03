import {$sanitize} from "./ngimport";

export function initAttributes(clone: JQuery, $scope: any) {
    if (!clone[0]) {
        return;
    }
    const markJSON = "xxxJSONxxx";
    const markHex = "xxxHEXJSONxxx";
    let s = clone[0].textContent;
    if (s == null) {
        return;
    }
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

export function getHeading($scope: any, attrs: IAttributes, key: string, defElem: string) {
    const h = set($scope, attrs, key, "");
    return toHeading(h, defElem);
}

export function toHeading(h: string, defElem: string) {
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

export function get(jso: IAttributes, keys: string[]) {
    if (!jso) {
        return undefined;
    }
    let val = jso;
    for (const k in keys) {
        if (val == null) {
            return undefined;
        }
        if (!keys[k]) {
            continue;
        }
        val = val[keys[k]];
        if (val == null) {
            return undefined;
        }
    }
    return val;
}

export interface IAttributes {
    [index: string]: any;
}

export interface ISettable {
    attrs: IAttributes;
    [index: string]: {} | undefined;
}

export function setk<T extends ISettable, K extends keyof T>(scope: T, sname: K, attrs: IAttributes, keys: string[], def: T[K]): T[K] {
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
    if (scope[sname] === "False") scope[sname] = false;
    return scope[sname];
}

export function setn<T extends ISettable, K extends keyof T>(scope: T, sname: K, attrs: IAttributes, name: string, def?: T[K]): T[K] {
    if (name.indexOf(".") < 0) {
        name = "markup." + name;
    }
    const keys = name.split(".");
    return setk<T, K>(scope, sname, attrs, keys, def);
}

export function set<T extends ISettable>(scope: T, attrs: IAttributes, name: string, def?: any) {
    if (name.indexOf(".") < 0) {
        name = "markup." + name;
    }
    const keys = name.split(".");
    const sname = keys[keys.length - 1] as keyof T;
    return setk(scope, sname, attrs, keys, def);
}

export function Hex2Str(s: string) {
    let result = "";
    for (let i = 0; i < s.length; i += 2) {
        const c = String.fromCharCode(parseInt(s[i] + s[i + 1], 16));
        result += c;
    }
    return result;
}
