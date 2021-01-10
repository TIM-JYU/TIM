// ------------------ Variables BEGIN -----------------------------------
let nullRef = undefined;

function removePairs(s, start, end) {
    if (s === undefined || s === "") return "";
    s = s.replace(/\\0/g, "␀").replace(/\0/g, "␀");
    let i = start.indexOf(s[0]);
    if (end === undefined) end = start;
    if (i >= 0) { // eat parenthes away
        // s[0] = ' ';
        let first = 1;
        let paren = end[i];
        let last = s.length - 1;
        if (s[last] === paren) last--;
        s = s.substring(first, last + 1);
    }
    return s;
}

function removeQuotes(s) {
    return removePairs(s, "\"'`´");
}

function varsLineToArray(vals, separs) {
    // converts a line with multiple inputs to array
    // split is done by , and ; and multiple ,, creates
    // empty inputs.  Space is also a separator, but
    // multiple spaces does not generate many inputs.
    // This is if separs is undefined.  If separs is defined,
    // then only those are used
    // Examples:
    //    kissa istuu => ['kissa', 'istuu']
    //    kissa,, istuu => ['kissa', '', 'istuu']
    //    "kissa,, istuu" => ['kissa,, istuu']
    //    12 "Aku Ankka" Mummo => ['12', 'Aku Ankka', 'Mummo']
    if (!vals) return "";
    let s = vals.trim();
    s = removePairs(s, "[{(<", "]})>");
    let separators = ",;"
    let spaces = " ";
    if (separs !== undefined) {
        separators = separs;
        if (!separs.includes(" ")) spaces = "";
    }
    let quotes = "'\"\`";
    let piece = undefined;
    let insideQuotes = undefined;
    let result = [];
    let afterSpace = false;
    for (let c of s) {
        let wasAfterSpace = afterSpace;
        afterSpace = false;
        if (insideQuotes) {
            if (c === insideQuotes) {
                insideQuotes = undefined;
                continue;
            }
            piece += c;
            continue;
        }
        if (quotes.includes(c)) {
            if (piece === undefined) piece = "";
            insideQuotes = c;
            continue;
        }
        if (spaces.includes(c)) {
            if (piece === undefined) continue;
            result.push(piece);
            piece = undefined;
            afterSpace = true;
            continue;
        }
        if (separators.includes(c)) {
            if (piece === undefined) {
                if (wasAfterSpace) continue;
                piece = "";
            }
            result.push(piece);
            piece = undefined;
            continue;
        }
        if (piece === undefined) piece = "";
        piece += c;
    }
    if (piece !== undefined) result.push(piece);
    return result;
}

function splitOne(s, sep) {
    let i = s.indexOf(sep);
    if (i < 0) return [s, ""];
    return [s.substring(0, i).trim(), s.substring(i + 1).trim()];
}

/*
// see: https://stackoverflow.com/questions/44562635/regular-expression-add-double-quotes-around-values-and-keys-in-javascript
// breaks working JSON
// like: let s = '{four:4, "coord":{"x":5, "y":7}}';
function normalizeJson(str){

    return str.replace(/[\s\n\r\t]/gs, '').replace(/,([}\]])/gs, '$1')
    .replace(/([,{\[]|)(?:("|'|)([\w_\- ]+)\2:|)("|'|)(.*?)\4([,}\]])/gs, (str, start, q1, index, q2, item, end) => {
        item = item.replace(/"/gsi, '').trim();
        if(index){index = '"'+index.replace(/"/gsi, '').trim()+'"';}
        if(!item.match(/^[0-9]+(\.[0-9]+|)$/) && !['true','false'].includes(item)){item = '"'+item+'"';}
        if(index){return start+index+':'+item+end;}
        return start+item+end;
    });
}
*/



function varsStringToJson(s) {
    // tries fix missing quotes and parenthes
    // does not work if object inside object
    // {x 4, y 2} => {"x": 4, "y": 2}
    // x: 4, y: 2 => {"x": 4, "y": 2}
    // x=4, y=2 => {"x": 4, "y": 2}
    // x: 3, s: kissa => {"x": 3, y: "kissa"}
    // x: 3, array: [1,2]  => problems
    function checkValue(v) {
        if (v === "null") return v;
        let vf = parseFloat(v);
        if (v === ""+vf) return vf;
        if (v === "") return '""';
        if (v.startsWith("{")) return v; // TODO should be better
        return '"' + v + '"';
    }

    if (s === undefined) return {};
    s = s.trim();
    if (s === "") return {};

    try {
        return JSON.parse(s);
    } catch {
        // start to study problems
    }
    if (s[0] !== "{") s = "{" + s + "}";
    try {
        return JSON.parse(s);
    } catch {
        // start to study problems
    }

    // let res = normalizeJson(s);
    // return JSON.parse(res);

    s = removePairs(s, "{", "}");
    let res = "";
    let pairs = varsLineToArray(s, ",");
    let sep = "";
    for (let pair of pairs) {
        pair = pair.trim();
        let [key, value] = splitOne(pair, ":");
        if (key.includes("=")) [key, value] = splitOne(pair, "=");
        else if (key.includes(" ")) [key, value] = splitOne(pair, " ");
        key = key.trim();
        value = checkValue(value.trim());
        res += sep + `"${key}": ${value}`;
        sep = ", ";
    }
    res = "{" + res + "}";
    return JSON.parse(res);
}


// Start non visual part of variables
/*!
 * base Class for any program object, so variable or command
 */
class PrgObject {
    constructor() {
        this.createError = undefined;
    }

    run() {
        return "Not possible to run baseclass";
    }

    checkCount() {
        return "";
    }

    isObject() {
        return false;
    }

    isRef() {
        return false;
    }

    solveLazy(variables) {
        return "";
    }
}


/**
 * Baseclass for commands
 *
 * Commands are like CreateValueVariable, AssignTo and so on.
 *
 * cmd has isMy static method that checks if string to parse belongs
 * to this cmd.
 * Check is mosty done by regexp and there should be
 * a regex101 link for every cmd to see examples.
 *
 * If string is for this cmd, new command is created and
 * it holds parameters to do his job when run is called.
 * isMy returns a list of commands
 * representing that string.  Mostly the list has only one command,
 * but there might be syntaxes like
 *    Ref list -> List $1 R 4
 * when isMy returns three commnds representing strings
 *    Ref list
 *    List $1 R 4
 *    list -> $1
 */
class Command extends PrgObject {

    static nameAndIndex(s) {
        // get name and index from s
        // see: https://regex101.com/r/2LojyQ/latest
        const re = /^([^[\]]+)(\[(\d)+])?$/gm
        let r = re.exec(s);
        if (!r) return [s, undefined];
        return [r[1], r[3]];
    }

    constructor() {
        super();
    }

    /*!
     * Runs the command and may use PhaseVariable object (variables).
     * Mostly variables is needed to find other variables
     * and for checking options like step or static mode.
     * variables might change during running commands, so
     * the value in comands creation moment can not be used.
     * Returns error message
     */
    run(variables) {
        return "Run is missing";
    }

    prerun() { // this is run already in parse-round
        return ""; // this has nothing to do normally
    }

}

class TextObject extends PrgObject {
    constructor(name, s, options) {
        super();
        this.name = name;
        this.text = s;
        this.textOptions = options
    }
}

class CreateVariable extends Command {
    /*!
     * Initialize variable creation
     * \param create is function to create the variable, mostly lanbda
     */
    constructor(create, name) {
        super();
        this.create = create;
        this.createdVar = undefined;
        this.name = name;
    }

    add(variables, variable) {
        let error = "";
        if (variable.name !== "null") {
            let v = variables.findVar(variable.name);
            if (v) error = `Nimi ${v.name} on jo käytössä`;
        }
        variables.add(variable);
        return error;
    }

    run(variables) {  // TODO: catch creation errors
        try {
            let v = this.create(variables);
            this.createdVar = v;
            if (v.vars) { // add child vars to flat map
                for (let cv of v.vars) {
                    variables.addFlat(cv);
                }
            }
            return v.handleError(v.createError, variables) +
                this.add(variables, v);
        } catch (e) {
            return e;
        }
    }
}


/*!
 * base Class for one varible
 * Even in normal C# or Java variable can not be reference
 * and value same time, but to allow user errors here it
 * is possible.  But then it hase error if used for both
 * Also real reference can reference only on place, but
 * to allow common user errors, refs is a list.  But
 * if there is more than one ref, it is error.
 */
class Variable extends PrgObject {
    constructor(name, value, ref, rank) {
        super();
        this.name = name;
        this.error = "";  // errors for this variable
        this.init(value);
        this.rank = rank;
        this.refs = [];
        if (ref) this.refs.push(ref);
        this.allowInit = true;
        this.count = undefined; // no error only for lists
        this.countError = false;
        if (value && value !== "0") this.allowInit = false;
    }

    handleError(error, variables) {
        // depending errorlevel marks error for variable and/or
        // to return value
        if (error) {
            if (variables.isMarkErrors()) this.error += error;
            if (variables.isShowErrors()) return error;
        }
        return "";
    }

    handleCountError(error, variables) {
        // depending errorlevel marks error for variable and/or
        // to return value
        if (error) {
            if (variables.isMarkErrors()) this.countError = true;
            return this.handleError(error, variables);
        }
        return "";
    }

    addRef(objTo, variables, force) {
        // adds ref to any variable, but it is error
        // if more than one ref or if not ref varible.
        // In static mode it is not allowed to change the
        // ref but new ref is added.  In step mode ref is changed.
        // index2 is for syntax: $2 = $1[3]
        let error = "";
        if (this.isArray()) {
            error += `${this.name} on taulukko! Sillä ei voi viitata`;
            let arref = new ArrayReferenceTo(this.name, 0, objTo.name, index2);
            return this.vars[0].handleError(error + arref.run(variables), variables);
        }
        let isRef = this.isRef() || force;  // Was originally ref?

        if (variables.isStepMode() ||
            this.refs.length > 0 && this.refs[0] === nullRef) {
            // step modessa viite korvaa
            if (this.refs.length === 0) this.refs.push(objTo);
            else this.refs[0] = objTo;
        } else {
            this.refs.push(objTo);
            if (this.refs.length > 1) error += `Muuttujasta ${this.name} ei voi lähteä montaa viitettä! `;
        }
        if (!isRef) error += `Muuttuja ${this.name} ei ole viitemuuttuja!  `;
        if (!objTo.isObject()) error += `Muuttuja ${objTo.name} ei ole olio!  `;
        return this.handleError(error, variables);
    }

    addRefByName(refname, variables, force) {
        let objTo = variables.findVar(refname);
        if (!objTo) {
            if (variables.isAllowLazy()) {
                this.lazy = refname;
                this.lazyForce = force;
                return "";
            }
            return `Oliota ${refname} ei löydy!`;
        }
        return this.addRef(objTo, variables, force);
    }

    solveLazy(variables) {
        if (!this.lazy) return "";
        let objTo = variables.findVar(this.lazy);
        if (!objTo) {
            return `Oliota ${this.lazy} ei löydy!`;
        }
        this.lazy = undefined;
        return this.addRef(objTo, variables, this.lazyForce);
    }


    assign(value, variables) {
        // assigns value to any variable, but if not value variable,
        // gives an error. In static mode only one initialization is allowed.
        value = removeQuotes(value);
        let error = "";
        if (this.value !== undefined &&
            variables.isStaticMode() && !this.allowInit) {
            error += `${this.name} on jo alustettu. Ei saa alustaa uudelleen! `;
        }
        let notnull = true;
        if (this.isRef()) {
            if (value === "null") {
                error += this.addRef(nullRef, variables);
                notnull = false;
            } else {
                error += `${this.name} on viitemuuttuja. Ei saa sijoittaa arvoa! `;
            }
        }
        if (notnull) {
            this.value = value;
            this.allowInit = false;
        }
        error += this.checkValueAssign();
        return this.handleError(error, variables);
    }

    checkValueAssign() {
        if (!this.isValue()) return "";
        if (this.value === undefined) return "";
        let n = parseFloat(this.value);
        if (!isNaN(n)) return "";  // OK numbers
        if (this.value === "true") return "";  // mimic booleans
        if (this.value === "false") return "";
        if (this.value.length <= 1) return ""; // OK one char
        if (this.isString()) return "";  // ok to string
        return `Arvomuuttujaan ${this.name} ei saa sijoittaa jonoa ${this.value}! `;
    }


    init(value) {
        if (value) value = removeQuotes(value);
        this.value = value;
        this.allowInit = false;
        this.createError = this.checkValueAssign();
    }

    isValue() {
        return false;
    }

    isObject() {
        return false;
    }

    isArray() {
        return false;
    }

    isList() {
        return false;
    }

    isRef() {
        return this.refs && this.refs.length > 0;
    }

    isStruct() {
        return false;
    }

    setCount(n, variables) {
        // Count is a special property to mimic C# list count
        // allowed only for lists
        let error = "";
        this.count = n;
        if (isNaN(parseInt("" + n))) error = `Kokonaislukuun ei saa sijoittaa ${n}! `;

        if (this.isList()) return error;
        error += `${this.name} ei ole lista, joten ei ole count-ominaisuutta`;
        return this.handleCountError(error, variables);
    }

    checkCount(variables) {
        return ""; // error allready given when set
    }

    isString() {
        return false;
    }

    run() {
        return "Variable is not runnable";
    }
}

class NullVariable extends Variable {
    // Null varible for un assigned refs or null ref
    constructor(vals) {
        super("null", undefined, undefined);
        this.rank = undefined;
        this.vals = vals;
    }

    addRef(objTo, variables) {
        return this.handleError("null arvolla ei voi viitata", variables);
    }

    assign(value, variables) {
        return this.handleError("null arvoon ei voi sijoittaa", variables);
    }

    init(value) {
        this.allowInit = false;
    }

    isObject() {
        return true;
    }

    isRef() {
        return false;
    }

    setCount(n, variables) {
        return this.handleError("null ei sisällä counttia", variables);
    }

    handleError(error, variables) {
        return error || "";
    }
}

nullRef = new NullVariable();


class ValueVariable extends Variable {
    constructor(name, value, ref, rank) {
        super(name, value, ref, rank);
    }

    isValue() {
        return true;
    }
}


class StringVariable extends ValueVariable {
    constructor(name, value, ref, rank) {
        super(name, value, ref, rank);
    }

    isString() {
        return true;
    }
}


class CharVariable extends ValueVariable {
    constructor(name, value, ref, rank) {
        super(name, value, ref, rank);
    }
}


class CreateValueVariable extends CreateVariable {
    // Creates a value variable
    // see: https://regex101.com/r/uaLk9H/latest
    // syntax: V summa <- -3.5
    static isMy(s) {
        let re = /^[Vv](al|alue)? +([.:+\-$@\w\d]+) *(<-|=|:=|) *([^\n]+|)$/;
        let r = re.exec(s);
        if (!r) return undefined;
        let name = r[2];
        let value = r[4];
        if (!r[3] && value === "") value = undefined;
        return [new CreateVariable(
            () => new ValueVariable(name, value, undefined, 0))];
    }
}


class RefecenceVariable extends ValueVariable {
    // Creates a reference variable
    constructor(name, value, ref, rank) {
        super(name, value, ref, rank);
    }

    isRef() {
        return true;
    }
}

class SimpleRefecenceVariable extends RefecenceVariable {
    // Creates a reference variable
    constructor(name, value, ref, rank) {
        super(name, value, ref, rank);
    }

    isRef() {
        return true;
    }
}


class CreateRefecenceVariable extends CreateVariable {
    // Creates a reference variable
    // see: https://regex101.com/r/8rghQV/latest
    // syntax: Ref luvut
    static isMy(s) {
        let re = /^[Rr](ef|eference)? +([!:@.+\-\w\d]+)$/;
        let r = re.exec(s);
        if (!r) return undefined;
        return [new CreateVariable(
            () => new RefecenceVariable(r[2], undefined, nullRef, 0))];
    }
}


class CreateNullVariable extends CreateVariable {
    // Creates a reference variable
    // see: https://regex101.com/r/8rghQV/latest
    // syntax: Ref luvut
    static isMy(s) {
        let re = /^null ?(.*)$/;
        let r = re.exec(s);
        if (!r) return undefined;
        return [new CreateVariable(
            () => new NullVariable(r[1]))];
    }
}


class ObjectVariable extends Variable {
    // Creates an object variable
    constructor(name, value, ref, rank) {
        super(name, value, ref, rank);
    }

    isObject() {
        return true;
    }
}


class CreateObjectVariable extends Variable {
    // Creates an object variable
    // see: https://regex101.com/r/uaQrJu/latest
    // syntax: New $2 Aku
    static isMy(s) {
        let re = /^[Nn][ew]{0,2} +([!+\-*$]*[@+\-\S]+) +([^\n]*)$/;
        let r = re.exec(s);
        if (!r) return undefined;
        let name = r[1];
        return [new CreateVariable(
            () => new ObjectVariable(name, r[2], undefined, 1), name)];
    }
}


class ArrayVariable extends ObjectVariable {
    // Creates array or list for refs or values
    // value ei not used in this case
    // kind is [ for Array and L for List (ArrayList in Java)
    // type is R for referneces and V for values.
    constructor(name, value, kind, type, dir, len, rank) {
        super(name, value, undefined, rank);
        this.kind = kind;
        this.type = type;
        this.dir = dir;
        if (this.isList()) this.count = 0;
        this.vars = [];
        this.over = [];  // all under index and over index things goes here
        let nref = undefined;
        let init = undefined;
        let cls;
        switch (type) {
            case "R":
                cls = RefecenceVariable;
                break;
            case "C":
                cls = CharVariable;
                break;
            case "A":
                cls = Variable;
                break;
            case "S":
                cls = StringVariable;
                break;
            default:
                cls = ValueVariable;
        }
        if (name.endsWith(".")) name = name.slice(0, -1);
        // if (name.endsWith(",")) name = name.slice(0, -1);
        if (this.kind === "[" && type === "R") nref = nullRef;
        if (this.kind === "[" && type === "V") init = "0";
        for (let i = 0; i < len; i++) {
            let vn = `${name}[${i}]`;
            vn = vn.replace("],[", ","); // for jagged arrays
            let v = new cls(vn, init, nref);
            this.vars.push(v);
            v.parent = this;
        }
        if (len > 0) this.vars[len - 1].isLast = true; // TODO change if items added later
        this.over[0] = new cls(`${name}.under`, undefined);
        this.over[1] = new cls(`${name}.over`, undefined);
        this.over[0].denynames = true;
        this.over[1].denynames = true;
    }

    isRef() { // TODO pitääkö (???) olla false int taulukoille
        return true;
    }

    isArray() {
        return true;
    }

    isList() {
        return this.kind === "L";
    }

    checkCount(variables) {
        if (!this.isList()) return "";
        let error = "";
        let deferr = `viitteitä lopun jälkeen (count väärin)`;
        let alustamattomia = " alustamattomia viitteitä "
        if (this.count < 0)
            return this.handleCountError(`${this.name}.count ei saa olla negatiivinen`);
        if (this.count > this.vars.length)
            return this.handleCountError(`${this.name}.count on liian iso`, variables);
        // Is all refs in used part initialized?
        for (let i = 0; i < this.count; i++) {
            let v = this.vars[i];
            if (!v.isRef()) continue;  // non refs does not matter
            if (!v.refs || v.refs.length === 0) {
                error += alustamattomia;
                break;
            }
        }
        // Is all refs in not used part without reference? nullRef is OK
        for (let i = this.count; i < this.vars.length; i++) {
            let v = this.vars[i];
            if (!v.isRef()) continue;  // non refs does not matter
            if (v.refs && v.refs.length > 0) {
                if (v.refs.length > 1) {
                    error += deferr;
                    break;
                } else if (v.refs[0] !== nullRef) {
                    error += deferr;
                    break;
                }
            }
        }
        if (error !== "") error = `${this.name}: ` + error;
        return this.handleCountError(error, variables);
    }
}


class StructVariable extends ObjectVariable {
    // Creates array or list for refs or values
    // value ei not used in this case
    // kind is [ for Array and L for List (ArrayList in Java)
    // type is R for referneces and V for values.
    constructor(name, value, kind, sclass, type, dir, len, rank) {
        super(name, value, undefined, rank);
        this.kind = kind;
        this.sclass = sclass;
        this.type = type;
        this.dir = dir;
        if (this.isList()) this.count = 0;
        this.vars = [];
        this.over = [];  // all under index and over index things goes here
        let nref = undefined;
        let init = undefined;
        let cls;

        function getCls(type) {
            nref = undefined;
            init = undefined;
            switch (type.toUpperCase()) {
                case "R":
                    cls = RefecenceVariable;
                    nref = nullRef;
                    break;
                case "SR":
                    cls = SimpleRefecenceVariable;
                    nref = nullRef;
                    break;
                case "C":
                    cls = CharVariable;
                    init = "0";
                    break;
                case "S":
                    cls = StringVariable;
                    init = "";
                    break;
                case "A":
                    cls = Variable;
                    init = "";
                    break;
                default:
                    cls = ValueVariable;
            }
            return cls;
        }

        let tnames = [];
        let types = [];
        if (this.sclass) {
            tnames = Object.keys(sclass.defList);
            types = Object.values(sclass.defList);
            len = tnames.length;
        }
        cls = getCls(this.type);
        for (let i = 0; i < len; i++) {
            let name1 = `${name}[${i}]`;
            let name2 = "";
            if (tnames[i] !== undefined) {
                name2 = name1;
                name1 = `${this.name}.${tnames[i]}`
            }
            if (this.sclass) cls = getCls(types[i]);
            let v = new cls(name1, init, nref);
            if (name2)
                v.name2 = `${name}[${i}]`;
            this.vars.push(v);
            v.parent = this;
        }
        if (len) this.vars[len - 1].isLast = true; // TODO: change if more items added later
    }

    isRef() { // TODO pitääkö (???) olla false int taulukoille
        return true;
    }

    isArray() {
        return false;
    }

    isStruct() {
        return true;
    }

    isList() {
        return false;
    }

}


class InitializedStructVariable extends StructVariable {
    // Creates initilized array or list

    constructor(name, value, kind, sclass, type, dir, vals, rank) {
        // let s = vals.replace(/  */g, " ").replace(/ *[,;] */g, ",");
        let valsarr = varsLineToArray(vals);
        super(name, value, kind, sclass, type, dir, valsarr.length, rank);
        this.valsarr = valsarr;
    }

    initByVals(variables) {
        let error = "";
        let len = this.vars.length;
        let valsarr = this.valsarr;
        let kindstr = "Tietueelle";
        if (!valsarr && !this.sclass) {
            error += `${kindstr} ${this.name} ei ole alustustietoja! `
            return this.handleError(error, variables);
        }
        for (let i = 0; i < len; i++) {
            let v = this.vars[i];
            if (valsarr[i] === undefined) break;
            let to = valsarr[i].trim();
            if (v.isRef() && to==="") { continue; }
            let name = to;
            let objTo = undefined;
            if ("AR".includes(this.type))
                objTo = variables.findVar(name);
            if (!objTo) {
                /*
                if (this.type === "R")
                    error += v.handleError(`${this.name} ei saa alustaa arvolla ${to}! `, variables);
                if (v.isRef())
                    error += v.handleError(`${v.name} ei saa alustaa arvolla ${to}! `, variables);
                if (to) v.init(to);
                 */
                if (this.type === "R" || v.isRef() )
                    error += v.addRefByName(name, variables, true);
                else
                    if (to) {
                        v.init(to);
                        error += v.createError;
                    }
                continue;
            }
            v.init(undefined);  // remove old value
            error += v.addRef(objTo, variables, true);
        }
        return this.handleError(error, variables);
    }
}


class CreateInitializedStructVariable extends CreateVariable {
    // Creates initilized struct
    // see: https://regex101.com/r/nfot3D/latest
    // syntax:
    //    Struct $2 v a,b,d,d
    //    s.People $2 a a,b,d,d
    static isMy(s, variables) {
        let re = /^([Ss](truct)?(\.([\S]+))?) +([^ ]+) +([@\S]+) *(.*)?$/;
        let r = re.exec(s);
        if (!r) return undefined;
        let kind = "[";
        let stclass = r[4];
        let sclass = undefined;
        if (stclass) {
            sclass = variables.findClass(stclass);
            if (!sclass) throw `Luokkaa ${stclass} ei löydy! `;
        }
        let name = r[5];
        let vals = r[7];
        let td = (r[6] + "  ").toUpperCase();
        let type = td[0]
        let dir = td[1];
        if (!"ARVCS".includes(type)) throw `${name} väärä tyyppi tietueelle.  Pitää olla A, C, R tai V! `

        return [new CreateInitializedStructVariable(() =>
            new InitializedStructVariable(name, undefined, kind, sclass, type, dir, vals, 1), name, sclass)];
    }


    run(variables) {
        let error = super.run(variables);
        let array = this.createdVar;
        if (!array) return `Tietuetta ${this.name} ${error} ei voitu luoda! `;
        return array.initByVals(variables);
    }
}


class InitializedArrayVariable extends ArrayVariable {
    // Creates initilized array or list

    constructor(name, value, kind, type, dir, len, vals, rank) {
        // let s = vals.replace(/  */g, " ").replace(/ *[,;] */g, ",");
        let valsarr;
        if (type === "C") { // char array
            valsarr = removeQuotes(vals);
        } else {
            valsarr = varsLineToArray(vals);
        }
        if (len === undefined || len < valsarr.length)
            len = valsarr.length;
        super(name, value, kind, type, dir, len, rank);
        this.valsarr = valsarr;
    }

    initByVals(variables) {
        let error = "";
        let valsarr = this.valsarr;
        let len = valsarr.length;
        let kindstr = "Taulukolle";
        if (this.isList()) kindstr = "Listalle";
        if (!valsarr && len <= 0 && this.vars.length <= 0) {
            error += `${kindstr} ${this.name} ei ole alustustietoja! `
            return this.handleError(error, variables);
        }
        if (this.isList()) this.count = len;
        if ("VCS".includes(this.type)) {
            for (let i = 0; i < len; i++) {
                let v = this.vars[i];
                v.init(valsarr[i].trim());
                error += v.createError;
            }
            return this.handleError(error, variables);
        }
        if (this.type === "R") {
            for (let i = 0; i < len; i++) {
                let v = this.vars[i];
                let name = valsarr[i].trim();
                /*
                let objTo = variables.findVar(name);
                if (!objTo) {
                    error += v.handleError(`Oliota ${name} ei löydy! `, variables)
                    continue;
                }
                error += v.addRef(objTo, variables);
                 */
                error += v.addRefByName(name, variables);
            }
        }
        return this.handleError(error, variables);
    }
}


class CreateInitializedArrayVariable extends CreateVariable {
    // Creates initilized array or list
    // see: https://regex101.com/r/Hre5iQ/latest
    //      https://regex101.com/r/ZepJ1V/latest
    // syntax:
    //    Array $1 V [3]
    //    l $2 v a,b,d,d
    //    []$3 r $1,$2
    //    L $3 r [$1,$2,$[1]]
    static isMy(s) {
        let re = /^([Aa](rray)?|[Ll](ist)?|\[]) *([^ ]+) +([@\S]+) *(.*)?$/;
        let r = re.exec(s);
        if (!r) { // try if format L $3 r [$1,$2,$[1]]
            re = /^([Aa](rray)?|[Ll](ist)?|\[]) +([^ ]+) *([@\S]+) *[{]?([$[\]\d., \w]+)[}]?$/;
            r = re.exec(s);
            if (!r) return undefined;
        }
        let kind = "[";
        if (r[1].toUpperCase().startsWith("L")) kind = "L";
        let name = r[4];
        let vals = r[6];
        // Check type, dir and len, see https://regex101.com/r/QePOJe/latest
        re = /^(.)([VH])?([\d]+)?/gm;
        let td = r[5].toUpperCase();
        r = re.exec(td);
        if (!r) throw `${name} tyypin kohdalla vikaa. Oikeita esim V VH V10 VV9`;
        let type = r[1];
        let dir = r[2] || 0;
        let len = parseInt(r[3] || "-1");
        let valsl = parseInt(vals); // is only on number
        if ("" + valsl !== vals) valsl = NaN;
        if (len < 0 && !isNaN(valsl)) {
            vals = "";
            len = valsl;
        }
        if (!"RVCS".includes(type)) throw `${name} väärä tyyppi taulukolle.  Pitää olla C, R, S tai V! `

        return [new CreateInitializedArrayVariable(() =>
            new InitializedArrayVariable(name, undefined, kind, type, dir, len, vals, 1), name)];
    }

    run(variables) {
        let error = super.run(variables);
        let array = this.createdVar;
        if (!array) return `${this.name} ${error} ei voitu luoda! `;
        return array.initByVals(variables);
    }
}

function addVarsCommandLines(cmds, cls, line) {
    const cms = cls.isMy(line);
    for (let c of cms)
        if (c.line === undefined) c.line = line;
    return cmds.concat(cms);
}


class CreateInitialized2DArrayVariable extends CreateVariable {
    // Creates initilized array or list
    // see: https://regex101.com/r/u2asVs/1
    // syntax:
    //    A $1 V2x3 {3,4,5}
    //    [,] $2 r2x4 a
    //    [][] $2 r2x4 a
    static isMy(s) {
        let re = /^([Jj]?[Aa](rray)? |\[]\[]|\[,]) *([^ ]+) +([@\S]+[\d]+[xX][\d]+) *(.*)?$/;
        let r = re.exec(s);
        if (!r) return undefined;
        let jagged = false;
        let akind = r[1].toUpperCase();
        if (akind==="[][]") jagged = true;
        if (akind.startsWith("J")) jagged = true;
        let name = r[3];
        let vals = r[5];
        // Check type, dir and len, see https://regex101.com/r/ZHKeDJ/2
        re = /^(.)?([\d]+)[xX]([\d]+)$/gm;
        let td = r[4].toUpperCase();
        r = re.exec(td);
        if (!r) throw `${name} tyypin ja koon kohdalla vikaa. Oikeita esim V R V3x5`;
        let type = r[1];
        let leny = parseInt(r[2] || "-1");
        let lenx = parseInt(r[3] || "-1");
        if (!"SRVCL".includes(type)) throw `${name} väärä tyyppi taulukolle.  Pitää olla C, R, S tai V! `
        let valsa = [];
        if (vals) valsa = vals.split("|");
        let firstcmd = `g {"push": 1, "dy":0, "rdy":22, "rd":0}`;
        let cls = CreateInitializedArrayVariable;
        let sep = ",";
        if (jagged) {
            firstcmd = `g {"push": 1, "dy":0, "dx":75, "rdy":30, "rd":0, "xsnap": 0 }`;
            cls = ReferenceTo;
            sep = "."
        }
        let cmds = [];
        cmds = addVarsCommandLines(cmds, CreateNullVariable,`null 0`);
        cmds[0].name = name;
        cmds = addVarsCommandLines(cmds, SetGraphAttributes,firstcmd);
        cmds = addVarsCommandLines(cmds, CreateInitializedArrayVariable,`Array ${name} RV${leny}`);
        cmds = addVarsCommandLines(cmds, SetGraphAttributes,`g {"rev":1}`);

        let printcmd = ""

        for (let iy = 0; iy < leny; iy++) {
            let val = valsa[iy] || "";
            let ref = "";
            if (jagged) ref = `${name}[${iy}] -> `;
            let cmd = `${ref}Array ${printcmd}${name}[${iy}]${sep} ${type}${lenx} ${val}`;
            cmds = addVarsCommandLines(cmds, cls, cmd);
            printcmd = "-";
        }
        cmds = addVarsCommandLines(cmds, SetGraphAttributes,`g {"pop":1}`);
        cmds = addVarsCommandLines(cmds, CreateNullVariable,`null 0`);
        return cmds;
    }

    run(variables) {
        let error = super.run(variables);
        let array = this.createdVar;
        if (!array) return `${this.name} ${error} ei voitu luoda! `;
        return array.initByVals(variables);
    }
}


class ReferenceTo extends Command {
    // Set ref variable to refecence object
    // see: https://regex101.com/r/KTT0RB/latest
    // syntax: lista -> $1
    static isMy(s) {
        let re = /^([\S*]+) *-> *(.+)$/;
        let r = re.exec(s);
        if (!r) return undefined;
        // return [new ReferenceTo(r[1], r[2])];

        let cmds = undefined;
        for (let cmdCls of combinedRefCommands) {
            cmds = cmdCls.isMy(r[2]);
            if (cmds) break;
        }
        if (!cmds) return undefined;
        let name = r[1];
        let cmd = cmds[0];
        let rname = cmd.name;
        cmd.line = r[2];

        let refToCmd = new ReferenceTo(name, rname);
        refToCmd.line = name + " -> " + rname;

        return [cmd, refToCmd];
    }

    constructor(from, to) {
        super();
        this.from = from;
        this.to = to;
    }

    run(variables) {
        let refFrom = variables.findVar(this.from)
        if (!refFrom) return `Viitemuuttujaa ${this.from} ei löydy!`;
        return refFrom.addRefByName(this.to, variables);
    }
}

class CreateFindVariable extends Command {
    // Dummy command to get r a -> $2 to work
    // see: https://regex101.com/r/j0W6U6/latest
    // syntax: $1
    static isMy(s) {
        let re = /^([$\S]+)$/;
        let r = re.exec(s);
        if (!r) return undefined;
        return [new CreateFindVariable(r[1])];
    }

    constructor(name) {
        super();
        this.name = name;
        this.noAnimate = true;
    }

    run(variables) {
        return "";
    }

}


const combinedRefCommands = [
    CreateObjectVariable,
    // CreateArrayVariable,
    CreateInitialized2DArrayVariable,
    CreateInitializedArrayVariable,
    CreateFindVariable,
];

class CreateReferenceAndObject extends CreateRefecenceVariable {
    // Set Crete object and set ref variable to refecence object
    // see: https://regex101.com/r/CmSGH9/latest
    // syntax: ref aku -> new $1 Aku
    // syntax: ref aku -> a $1 v 3
    // syntax: ref aku -> a $1 v 1,2,34
    static isMy(s) {
        let re = /^[Rr](ef|eference)? +([!:\-+$.@\w\d]+) *-> *(.*)$/;
        let r = re.exec(s);
        if (!r) return undefined;

        let name = r[2];
        let cmds = undefined;
        for (let cmdCls of combinedRefCommands) {
            cmds = cmdCls.isMy(r[3]);
            if (cmds) break;
        }
        if (!cmds) return undefined;
        let refCmd = new CreateVariable(
            () => new RefecenceVariable(name, undefined, nullRef, 0));
        refCmd.line = "Ref " + name;

        let cmd = cmds[0];
        let rname = cmd.name;
        cmd.line = r[3];

        let refToCmd = new ReferenceTo(name, rname);
        refToCmd.line = name + " -> " + rname;
        let retcmds = [refCmd];
        retcmds = retcmds.concat(cmds);
        retcmds.push(refToCmd);
        return retcmds;
        // return [refCmd, cmd, refToCmd];
    }
}

class AssignTo extends Command {
    // Assign value to predefined variable
    // see: https://regex101.com/r/bPn7zX/latest
    // syntax: a = 5
    static isMy(s) {
        let re = /^([$[\].,\w\d]+) *(<-|=|:=) *(.*)$/;
        let r = re.exec(s);
        if (!r) return undefined;
        return [new AssignTo(r[1], r[3])];
    }

    constructor(to, value) {
        super();
        this.error = "";
        this.to = to;
        this.value = value;
    }

    run(variables) {
        let error = "";
        let varTo = variables.findVar(this.to);
        if (!varTo) return `Muuttujaa ${this.to} ei löydy!`;
        if (varTo.isArray()) {
            error += `${this.to} on taulukko. Ei saa sijoittaa koko taulukkoon! `;
            varTo = varTo.vars[0];
        }
        return error + varTo.assign(this.value, variables);
    }
}


class CreateClass extends Command {
    // Creates a class for new structures
    // see: https://regex101.com/r/wWtcI5/1/
    // syntax: class id: v, name: s, address: s
    static isMy(s) {
        let re = /^class ([^: ]*)(: *| +)(.*)$/gm;
        let r = re.exec(s);
        if (!r) return undefined;
        let name = r[1];
        let definition = r[3];
        return [new CreateClass(name, definition)];
    }

    constructor(name, definition) {
        super();
        this.name = name;
        this.definition = definition;
        this.noAnimate = true;
    }

    run(variables) {
        variables.clearAttributes();
        return "";
    }

    prerun(variables) {
        try {
            let defsList = varsStringToJson(this.definition);
            return variables.addClass(this.name, defsList);
        } catch (e) {
            return e;
        }
    }
}

class SetCount extends Command {
    // Set's count for list.  Error for other types
    // see: https://regex101.com/r/sPeXxR/latest
    // syntax:
    //  c.count=4
    //  $4.Count <- 19
    static isMy(s) {
        let re = /^([$._\d\w]+)\.[Cc]ount *(=|:=|<-) *(\S+)$/gm;
        let r = re.exec(s);
        if (!r) return undefined;
        let arrayName = r[1];
        let count = r[3];
        return [new SetCount(arrayName, count)];
    }

    constructor(arrayName, count) {
        super();
        this.arrayName = arrayName;
        this.count = count;
    }

    run(variables) {
        let array = variables.findVar(this.arrayName);

        let varTo = variables.findVar(this.arrayName + ".count");
        if (varTo) {
            return varTo.assign(this.count, variables);
        }


        if (!array) return `Taulukkoa ${this.arrayName} ei löydy! `;
        // Only list accepts this
        return array.setCount(this.count, variables);
    }
}

class SetPhaseNr extends Command {
    // Start a new phase
    // see: https://regex101.com/r/1MX6oE/latest
    // syntax:
    //  phase 1
    //  phase 4 Kissa istuu puussa
    //  phase 3 {"color": "green", "size": 30} Kukkuu
    static isMy(s) {
        let re = /^[Pp](hase)? *(\d+) *({[^}]*})?([^\n]*)?$/gm;
        let r = re.exec(s);
        if (!r) return undefined;
        return [new SetPhaseNr(r[2], r[4], r[3])];
    }

    constructor(phaseNumber, text, options) {
        super();
        this.phaseNumber = phaseNumber;
        this.text = text;
        if (options) {
            options = varsStringToJson(options);
        }
        this.options = options;
        this.noAnimate = true;
    }

    run(variables) {
        if (variables.phaseNumber === undefined && !variables.hasTexts() &&
            !variables.hasVars()) { // not yet used, just change number
            variables.phaseNumber = this.phaseNumber;
            if (this.text)
                variables.addText(this.text, 0, this.options);
            return "";
        }
        //  phase is used, so create new phase with that number
        let rel = variables.variableRelations;
        let phase = new PhaseVariables(rel, this.phaseNumber);
        rel.changePhase(phase);
        if (this.text)
            phase.addText(this.text, 0, this.options);
        return "";
    }
}

class AddPhaseText extends Command {
    // Add text to phase.  f option for footer
    // see: https://regex101.com/r/tj8CeD/latest
    // syntax:
    //  text Aku Ankka
    //  t {"color": "green", "size": 30} Kukkuu
    //  t f {"color": "blue", "size": 20} This is footer text
    static isMy(s) {
        let re = /^t(ext)? *(\$.+?)? (h |f | i |)? *({[^}]*})?([^\n]*)$/gm;
        let r = re.exec(s);
        if (!r) return undefined;
        return [new AddPhaseText(r[2], r[5], r[3], r[4])];
    }

    constructor(name, text, pos, options) {
        super();
        this.text = text;
        this.name = name;
        pos = (pos + " ").trim();
        this.pos = "hf".indexOf(pos);
        if (options) {
            options = varsStringToJson(options);
        }
        this.options = options;
    }

    run(variables) {
        if (this.pos >= 0) {
            variables.addText(this.text, this.pos, this.options);
            return "";
        }
        let textVar = new TextObject(this.name, this.text, this.options);
        variables.add(textVar);
    }
}

class AddSVG extends Command {
    // Add text SVG command
    // see: https://regex101.com/r/2uoneT
    // syntax:
    //  SVG <path>...</path>
    static isMy(s) {
        let re = /^(SVG|svg) * ([^\n]*)$/gm;
        let r = re.exec(s);
        if (!r) return undefined;
        return [new AddSVG(r[2])];
    }

    constructor(svg) {
        super();
        this.svg = svg;
    }

    run(variables) {
        variables.addSVG(this.svg);
        return "";
    }
}


class SetGraphAttributes extends Command {
    // Add text SVG command
    // see: https://regex101.com/r/q6YJDq/latest
    // syntax:
    //  Graph {x: 10, y:20}
    static isMy(s) {
        let re = /^[Gg](raph)? * ([^\n]*)$/gm;
        let r = re.exec(s);
        if (!r) return undefined;
        return [new SetGraphAttributes(r[2])];
    }

    constructor(graphAttributes) {
        super();
        this.graphAttributes = graphAttributes;
        this.noAnimate = true;
    }

    run(variables) {
        try {
            let ga = varsStringToJson(this.graphAttributes);
            variables.setGraphAttributes(ga);
            return "";
        } catch (e) {
            return e;
        }
    }

    prerun(variables) { // to use variables already for classes
        this.run(variables);
    }

}

class SetNamedGraphAttributes extends Command {
    // Add text SVG command
    // see: https://regex101.com/r/LViQAy/latest
    // syntax:
    //  gn $1 {x: 10, y:20}
    static isMy(s) {
        let re = /^[Gg][Nn] ([^ \n]+):? * ([^\n]*)$/gm;
        let r = re.exec(s);
        if (!r) return undefined;
        return [new SetNamedGraphAttributes(r[1], r[2])];
    }

    constructor(name, graphAttributes) {
        super();
        this.name = name;
        this.graphAttributes = graphAttributes;
        this.noAnimate = true;
    }

    run(variables) {
        try {
            let ga = varsStringToJson(this.graphAttributes);
            variables.setNamedGraphAttributes(this.name, ga);
            return "";
        } catch (e) {
            return e;
        }
    }

    prerun(variables) { // to use variables already for classes
        return "";
    }

}



class CodeCommand extends Command {
    // Add a CODE: -line
    // see: https://regex101.com/r/ki09TM/latest
    // syntax:
    //  Code: int a = 5;
    static isMy(s, variables) {
        let re = /^[Cc](ode|ODE)?[: ]+ *(.*)$/gm;
        let r = re.exec(s);
        if (!r) return undefined;
        let code = "" + r[2];
        if (variables.isAnimateCommands() &&
            code !== "" &&
            !code.startsWith("#") &&
            !code.startsWith("//")
        ) {
            code = "CODE: " + code;
        }
        // variables.addAllCode(code);
        return [new CodeCommand(code)];
    }

    constructor(code) {
        super();
        this.code = code;
        this.line = code;
    }

    run(variables) {
        variables.addCode(this.code);
        return "";
    }
}

class UnknownCommand extends Command {
    // This commad takes all commands that are not known
    static isMy(s) {
        return [new UnknownCommand(s)];
    }

    constructor(line) {
        super();
        this.line = line;
    }

    run(variables) {
        return `${this.line} - ei tunneta tai ei hyväksytä`;
    }
}


const knownCommands = [
    CreateClass,
    ReferenceTo,
    CreateValueVariable,
    CreateNullVariable,
    CreateRefecenceVariable,
    CreateObjectVariable,
    CreateInitializedStructVariable,
    SetCount,
    AssignTo,
    CreateReferenceAndObject,
    CreateInitialized2DArrayVariable,
    CreateInitializedArrayVariable,
    AddPhaseText,
    SetPhaseNr,
    SetGraphAttributes,
    SetNamedGraphAttributes,
    CodeCommand,
    AddSVG,
    UnknownCommand,
];


/*!
 * A phase is a list of variables and releted texts.
 * It may have phaseNumber, but in most cases there
 * is only one phese without phaseNumber.
 * The whole drawing (variableRelations) may include
 * one or more phases.  Many option and other things
 * are in variableRelations, so reference to that parent
 * is needed.  Variables/Commands communicates thru
 * phase (variables) to the parent.
 * Only SetPhaseNr uses the parent directly.
 */
class PhaseVariables {
    constructor(variableRelations, phaseNumber) {
        this.variableRelations = variableRelations;
        this.varsmap = {};
        this.vars = [];
        this.flatvarsmap = {};
        this.flatvars = [];
        this.phaseNumber = phaseNumber;
        this.texts = [[], []];
        this.autoNumber = 1;
        this.lastRank = 0;
        this.defaultRank = undefined;
        this.namedGraphAttributes = {};
    }

    addText(s, pos, options) {
        this.texts[pos].push({text: s, options: options});
    }

    addSVG(svg) {
        this.variableRelations.svgs.push(svg);
    }

    moveName(obj, from, to) {
        let obf = obj[from];
        if (obf === undefined) return;
        obj[to] = obf;
        delete obj[from];
    }

    clearAttributes() {
        this.graphAttributes = undefined; // TODO: clear only ontimers
    }

    setNamedGraphAttributes(name, ga) {
        this.moveName(ga, "r", "rank");
        this.moveName(ga, "rd", "rankdir");
        name = this.cleanName(name);
        this.namedGraphAttributes[name] = ga;
    }

    setGraphAttributes(ga) {
        if (ga) { // handle aliases
            this.moveName(ga, "r", "rank");
            this.moveName(ga, "rd", "rankdir");
            if ( ga.rgdx !== undefined ) this.variableRelations.rankGlobalMoveDx = ga.rgdx;
            if ( ga.rgdy !== undefined ) this.variableRelations.rankGlobalMoveDy = ga.rgdy;
        }

        this.graphAttributes = {...this.graphAttributes, ...ga};  // TODO: join previous
        if (ga.rank !== undefined) {
            if (ga.rank < 0 || ga.rank === "" ||
                ga.rank === null || ga.rank === "null") {
                this.defaultRank = undefined;
                ga.rank = undefined;
            } else this.defaultRank = ga.rank;
        }

    }

    isMarkErrors() {
        return this.variableRelations.isMarkErrors();
    }

    isShowErrors() {
        return this.variableRelations.isShowErrors();
    }

    isAllowLazy() {
        return this.variableRelations.allowLazy;
    }


    addFlat(variable) {
        let name = this.cleanName(variable.name, variable);
        this.flatvarsmap[name] = variable;
        this.flatvars.push(variable);
        if (variable.name2) {
            this.flatvarsmap[this.cleanName(variable.name2)] = variable;
        }
    }

    cleanName(s, variable) {
        // clear $-+ from name and if variable then
        // change force and deny attributes
        if (!s || s.length === 0) return "";
        let force = false;
        let deny = false;
        let forceParentName = false;
        let denyDollarName = false;
        let denyBox = false;
        let label = undefined;
        let i = s.indexOf(":");
        if (i>=0) {
            label = s.substring(i+1);
            s = s.substring(0,i);
        }
        let dollar = "";
        while (true) {
            let f = s[0];
            if (!"*+-$!".includes(f)) break;
            // if (variable !== undefined) {
                if (s[0] === '$') { denyDollarName = true; dollar = "$"; }
                if (s[0] === '+') force = true;
                if (s[0] === '-') deny = true;
                if (s[0] === '*') forceParentName = true;
                if (s[0] === '!') denyBox = true;
            // }
            s = s.substring(1);
        }
        if (variable !== undefined) {
            if (force) {
                variable.denynames = false;
                variable.forcenames = true;
            } else if (deny) {
                variable.denynames = true;
                variable.forcenames = false;
            }
            if (!variable.denynames && !variable.parent) variable.forcenames = true;
            if (label !== undefined)
                variable.name = label;
            else
                variable.name = s;
            if ( denyBox ) variable.denyBox = true;
            if ( forceParentName ) variable.forceParentName = true;
            else if ( denyDollarName && !force) variable.denyDollarName = true;
            variable.label = dollar+variable.name;
            if (variable.label.endsWith(".")) variable.label = variable.label.slice(0,-1);
            if (variable.label.endsWith(",")) variable.label = variable.label.slice(0,-1);
        }
        return s;
    }

    add(variable) {
        this.vars.push(variable);
        if (!variable.name) {
            variable.name = "$a$" + this.autoNumber++;
        }
        let i = variable.name.indexOf("@");
        if (i > 0) {
            let rs = variable.name.substring(i + 1);
            variable.name = variable.name.substring(0, i);
            variable.rank = parseInt(rs);
        } else { // use default
            if (this.defaultRank !== undefined) {
                variable.rank = this.defaultRank;
            }
        }
        if (variable.rank !== undefined) this.lastRank = variable.rank;
        variable.graphAttributes = this.graphAttributes;
        this.clearAttributes();
        if (variable.sclass) {
            if (variable.sclass.graphAttributes) {
                variable.graphAttributes = {...variable.graphAttributes,
                  ...variable.sclass.graphAttributes};
            }
        }
        this.addFlat(variable); // cleans the name
        this.varsmap[variable.name] = variable;
    }

    findVar(name) {
        name = this.cleanName(name);
        [name,] = name.split("@");
        if (name === "null") return nullRef;
        let ret = this.flatvarsmap[name];
        if (!ret) {
           if (name.endsWith(",")) ret = this.flatvarsmap[name.replace(",", ".")];
           if (name.endsWith(".")) ret = this.flatvarsmap[name.replace(".", ",")];
        }
        return ret;
    }

    addClass(name, defList) {
        let cls = {name: name,
                   defList: defList,
                   graphAttributes: this.graphAttributes};
        this.clearAttributes();
        return this.variableRelations.addClass(name, cls);
    }

    findClass(name) {
        return this.variableRelations.findClass(name);
    }

    hasTexts() {
        for (let s of this.texts)
            if (s.length > 0) return true;
        return false;
    }

    hasVars() {
        return this.vars.length > 0;
    }

    addCode(code) {
        this.variableRelations.codeList.push(code);
    }

    addAllCode(code, stepnr) {
        // let lnr = this.variableRelations.linenumber;
        this.variableRelations.allCodeList.push({
            code: code,
            stepnumber: stepnr
        });
    }

    isAnimateCommands() {
        return this.variableRelations.isAnimateCommands();
    }

    isAnimateCode() {
        return this.variableRelations.isAnimateCode();
    }

    isStaticMode() {
        return this.variableRelations.isStaticMode();
    }

    isStepMode() {
        return this.variableRelations.isStepMode();
    }

    solveLazy() {
        for (let v of this.flatvars) {
            let error = v.solveLazy(this);
            if (error) this.variableRelations.addError(error);
        }
    }

}


/*!
 * Class for phase list and commands
 */
class VariableRelations {

    addError(s) {
        this.errors += s + "\n"
    }

    init() {
        this.phaseList = [new PhaseVariables(this, undefined)];
        this.codeList = [];
        this.currentPhase = this.phaseList[0];
        this.errors = this.createErrors;
        this.svgs = [];
    }

    /*!
     * Convert string to list of variables
     * \fn constructor(s, params, knownCommands)
     * \param string s variables as a string representation
     * \param object params
     *     - mode: "step" allow change ref value
     *             "static" adda extra wrong ref when new ref assign
     *     - errorlevel: 0 = no errors show
     *                   1 = no textual error but wrong refs red
     *                   2 = just textual errors
     *                   3 = textual and visual errors
     *     - animate:    code = lines marked as CODE:
     *                   commands = original commands
     * \param knownCommands list of classes that are know
     *
     * After creation there is JSON variables structure:
     *   commands: list of created commands
     *
     * After runUntil there is
     *
     *   phaseList
     *     - phase 1
     *         - var 1
     *         - var 2
     *     - pahse 2
     *         - var 1
     *         - var 2
     *         - var 3
     *   errors: list of errors during run
     */
    constructor(s, params, knownCommands) {
        this.classes = {};
        this.createErrors = "";
        this.mode = "static";
        this.errorlevel = 3;
        this.animate = 0;
        this.allowLazy = false;
        if (params) {
            if (params.allowLazy !== undefined) this.allowLazy = params.allowLazy;
            if (params.mode) this.mode = params.mode;
            if (params.errorlevel !== undefined)
                this.errorlevel = params.errorlevel;
            if (params.animate === "code") this.animate = 1;
            if (params.animate === "commands") this.animate = 2;
        }
        if (this.animate > 0) this.mode = "step";
        this.errorlevel = Math.max(0, Math.min(this.errorlevel, 3));
        this.commands = [];
        this.allCodeList = [];
        this.linenumber = 0; // this is original linenumber in input
        this.minStepnumber = 1000000;
        this.maxStepnumber = 0;
        this.init();
        if (s) this.addCommands(s, knownCommands);
    }

    changePhase(phase) {
        // this.currentPhase.solveLazy();
        this.currentPhase = phase;
        this.phaseList.push(phase);
    }


    addClass(name, cls) {
        let exists = this.findClass(name);
        this.classes[name] = cls;
        if (exists) return `Luokka ${name} on jo olemassa`;
        return ""
    }

    findClass(name) {
        return this.classes[name];
    }

    isAnimateCode() {
        return this.animate === 1;
    }

    isAnimateCommands() {
        return this.animate === 2;
    }

    isStaticMode() {
        return this.mode === "static";
    }

    isStepMode() {
        return this.mode === "step";
    }

    isMarkErrors() {
        return this.errorlevel % 2 === 1;
    }

    isShowErrors() {
        return this.errorlevel > 1;
    }

    maxStep() {
        return this.commands.length - 1;
    }

    addCreatedCommands(cmds, line) {
        let lnr = this.linenumber;
        let cmdline = line;
        let cmd;
        for (cmd of cmds) {
            cmd.linenumber = lnr;
            if (cmd.line) cmdline = cmd.line; else cmdline = line;
            this.commands.push(cmd);
            cmd.stepnumber = this.commands.length;
            if (this.isAnimateCommands())
                this.currentPhase.addAllCode(cmdline, cmd.stepnumber);
            if (this.isAnimateCode() && cmd.code)
                this.currentPhase.addAllCode(cmdline, cmd.stepnumber);
        }
        this.minStepnumber = Math.min(cmd.stepnumber, this.minStepnumber);
        this.maxStepnumber = Math.max(cmd.stepnumber, this.maxStepnumber);
    }

    /*!
     * Convert string to list of commands
     * \fn addCommands(s, knownCommands)
     * \param string s variables as a string representation
     * \param knownCommands list of classes that are know
     * \return JSON resulting variables structure
     */
    addCommands(s, knownCommands) {
        // Start checking lines from string representation
        this.linenumber = 0;
        for (let line of s.split("\n")) {
            try {
                line = line.trim();
                this.linenumber++;
                if (!line) continue; // forget empty lines
                if (line.startsWith("#")) continue; // forget comments
                let cmds = undefined;
                for (let cls of knownCommands) {
                    cmds = cls.isMy(line, this.currentPhase);
                    if (cmds) break;
                }
                if (!cmds) {  // this should not happend if there is UnknownCommand last
                    this.addError(`${this.linenumber}: ${line} - ei tunneta tai ei hyväksytä`);
                    continue; // did not match any know types
                }

                this.addCreatedCommands(cmds, line);

                for (let cmd of cmds) {
                    let error = cmd.prerun(this.currentPhase);
                    if (error) this.addError(`${this.linenumber}: ${error}`);
                }
            } catch (e) {
                this.addError(`${this.linenumber}: ${e}`);
            }
        }
        // this.currentPhase.solveLazy();
        if (this.animate > 0) // add extra END line to show "program" ended
            this.addCreatedCommands(CodeCommand.isMy("CODE: END", this.currentPhase));
        if (!this.isShowErrors()) this.createErrors = "";
        else this.createErrors = this.errors;
    }

    runUntil(n) {
        // Runs the command list from begining until in step n
        // If n not defined, run all commands
        this.init();
        let nr = 0;
        let lastlinenr = 0;
        if (n === undefined) n = this.maxStep();
        for (let cmd of this.commands) {
            if (nr >= n) break;
            let error = cmd.run(this.currentPhase);
            lastlinenr = cmd.linenumber;
            if (error && this.isShowErrors()) this.addError(`${cmd.linenumber}: ${error}`);
            nr++;
        }
        if (nr >= this.commands.length) { // all runned, check counts
            for (let phase of this.phaseList) {
                for (let v of phase.vars) {
                    let error = v.checkCount(phase);
                    if (error && this.isShowErrors()) this.addError(`${error}`);
                }
                phase.solveLazy();
            }
        }
        return nr;
    }

    isLastPhase(phase) {
        return phase === this.phaseList[this.phaseList.length - 1];
    }
} // VariableRelations
// ------------------ Variables END -----------------------------------


class SVGUtils {
    static box(id, w, h, dx, dy, x, y, fill) {
        // draw one svg 3D box
        if (x === undefined) x = 0;
        if (y === undefined) y = 0;
        if (fill === undefined) fill = "#fff";
        return `
<g id="${id}" transform="translate(${x - w / 2.0} ${y - h / 2.0})">
    <path d="M 0 0 L 0 ${h} L ${w} ${h} L ${w + dx} ${h - dy} L ${w + dx} ${-dy} L ${dx} ${-dy} L 0 0 Z"
       fill=${fill} stroke="#000000" stroke-miterlimit="10"  pointer-events="all"/>
    <path d="M ${w} 0 L ${w + dx} ${-dy} L ${w + dx} ${h - dy} L ${w} ${h} Z"
       fill-opacity="0.15" fill="#000000" stroke="none"  pointer-events="all"/>
    <path d="M 0 0 L ${dx} ${-dy} L ${w + dx} ${-dy} L ${w} 0 Z"
       fill-opacity="0.2" fill="#000000" stroke="none"  pointer-events="all"/>
    <path d="M 0 0 L ${w} 0 L ${w + dx} ${-dy} M ${w} 0 L ${w} ${h}"
       fill="none" stroke="#000000" stroke-miterlimit="10"  pointer-events="all"/>
</g>
`;
    }

    static ebox(id, w, h, dx, dy, color) {
        // error box: draw red "hat" for box
        return `
<g id="ebox" transform="translate(-16 -11)">
    <path d="M 0 0 L ${dx} ${-dy} L ${w + dx} ${-dy} L ${w} ${0} L ${0} ${0}  Z"
       fill="${color}"  stroke-miterlimit="10"  pointer-events="all"/>
</g>
`;
    }

    static lineends() {
        // dot, arrow end and null ref end markers
        return `
<marker id="endarrow" markerWidth="10" viewBox="-10 0 10 12"
     markerHeight="9" refX="0" refY="4.5" orient="auto" markerUnits="strokeWidth">
    <polygon points="-10 0, 1.1 4.5, -10 9"  />
</marker>
<marker id="startarrow" viewBox="0 0 12 12" refX="6" refY="6"
     markerWidth="6" markerHeight="6">
    <circle cx="6" cy="6" r="6" fill="black" />
</marker>
<marker id="nullarrow" markerWidth="3" viewBox="-4 0 4 8"
     markerHeight="16" refX="0" refY="0.5" orient="auto" markerUnits="strokeWidth">
    <polygon points="-3 -8, -3 8, 0 8, 0 -8"  />
</marker>
`;
    }

    static joinOptions(options, def) {
        let opt = options;
        if (opt) opt = Object.assign({}, def, opt);
        else opt = def;
        return opt;
    }

    static drawSVGText(s, options, x, y) {
        // draw one svg text using options
        if (s === undefined) return ["", 0];
        let opt = this.joinOptions(options, {
            "x": x, "y": y,
            "color": "black",
            "size": 14, font: "Helvetica", "weight": "normal",
            "base": "middle",
            "align": "left",
            "sx": 0,
            "sy": 0,
        });
        if (!isNaN(Number(opt.size))) opt.size += "px";
        let svg = `<text x="${opt.x + opt.sx}" y="${opt.y + opt.sy}" fill="${opt.color}" font-family="${opt.font}" font-weight="${opt.weight}" font-size="${opt.size}" text-anchor="${opt.align}" alignment-baseline="${opt.base}">${s}</text>\n`;
        let sz = parseFloat(opt.size.replace("px", ""));
        let dy = sz * 1.2;
        if (options && (options.y || options.sy))
            dy = 0; // no y move if set pos
        return [svg, dy];
    }

    static drawSVGTexts(texts, x, y, h) {
        // draw list of text. One item has text and options as JSON
        let svg = "";
        if (!texts) return [svg, y];
        for (let text of texts) {
            let [s1, dy] = SVGUtils.drawSVGText(text.text, text.options, x, y - h);
            svg += s1;
            y += dy;
        }
        return [svg, y];
    }


    static drawParentName(obj) {
        // draw variable names if needed
        if (!obj) return "";
        if (!(obj.name && obj.forceParentName)) return "";
        let textalign = "end";
        let textsize = 12;
        let text = obj.label;
        let textx = obj.left().x - 15;
        let texty = obj.y - 20;
        let opt = SVGUtils.joinOptions(obj.nameOptions,
            {align: textalign, font: "Helvetica", size: textsize});
        let [svg1,] = SVGUtils.drawSVGText(text, opt, textx, texty);
        return svg1;
    }


    static debug(sl, opt, cmd, dir, p, d, color) {
        if (!opt || !opt.debug) return;
        if (!opt.debug.includes(cmd)) return;
        if (dir === 0 || dir === 2) {
            let [sv, ] = SVGUtils.drawSVGText(p.y, {}, d, p.y);
            sl.push(sv);
            sl.push(`<line x1="${d + 30}" y1="${p.y}" x2="${1000}" y2="${p.y}" stroke="${color}" stroke-width="0.5"  />`);
        }

        if (dir === 1 || dir === 2) {
            let [sv, ] = SVGUtils.drawSVGText(p.x, {}, p.x, d);
            sl.push(sv);
            sl.push(`<line x1="${p.x}" y1="${d + 15}" x2="${p.x}" y2="${1000}" stroke="${color}" stroke-width="0.5"  />`);
        }
    }
}


const svgTextMixin = {
    showCount() {
        return "";
    },

    toSVG(x, y, w) {
        let svg = "";
        let s = this.text.trim();
        if ( s.startsWith("$") && this.lastObj !== undefined) {
            if (s === "$name") s = this.lastObj.name;
            else if (s === "$class" && this.lastObj.sclass !== undefined) {
                s = this.lastObj.sclass.name;
            }
        }
        let [s1, dy] = SVGUtils.drawSVGText(s, this.textOptions, x, y);
        svg += s1;
        this.x = x;
        this.y = y;
        this.width = w;
        this.height = dy;
        if (dy === 0) this.height = 0;
        return svg;
    },

    left() {
        return {x: this.x - this.width / 2, y: this.y};
    },
    right() {
        return {x: this.x + this.width / 2, y: this.y};
    }
}

/*
 * Mixins for drawing as SVG
 * Add these to variables to make them SVG drawble
 */
const svgVariableMixin = {
    showCount(dy) {
        // show varibales count-attribute.  By red if in error state
        if (this.count !== undefined) {
            let p = this.left();
            p.x -= 5;
            // p.y -= this.height / 2 + dy;
            p.y -= 22 / 2 + dy;
            let fill = "black";
            if (this.countError) fill = "red";
            return `<text x="${p.x}" y="${p.y}" fill="${fill}" font-family="Helvetica" font-size="12px" text-anchor="middle" alignment-baseline="middle">${this.count}</text>\n`;
        }
        return "";
    },

    toSVG(x, y, w, h, dx, dy, nobox, align, forcedw) {
        // draw variable as a box, it's name and content
        // if in error state, draw red box also
        // align 1 aligns from left
        if (this.graphAttributes) {
            if (this.graphAttributes.x) x = this.graphAttributes.x;
            if (this.graphAttributes.y) y = this.graphAttributes.y;
            if (this.graphAttributes.sx) x += this.graphAttributes.sx;
            if (this.graphAttributes.sy) y += this.graphAttributes.sy;
            //  if (this.graphAttributes.h) hfactor = this.graphAttributes.h;
        }
        if (this.denyBox) nobox = true;
        this.x = x;
        this.y = y;
        this.width = w;
        this.height = h;
        let size = "1";
        let val = this.value;
        if (val !== undefined) { // TODO: should we draw "" string???
            val = "" + val;
            if (val.length > 4) {
                size = "2";
                this.width = 2 * w;
            }
            if (val.length > 9) {
                size = "4";
                this.width = 4 * w;
            }
        }
        if (forcedw) {
            size = "" + forcedw;
            this.width = forcedw * w;
        }
        if (align === 1 && this.width > w) {
            this.x += (this.width - w) / 2;
            x = this.x;
        }
        let svg = "";
        if (!nobox && dx > 0 && dy > 0) {
            svg += `<use href="#rbox${size}" x="${x}" y="${y}" />\n`;
        }
        if ((dx === 0 && dy === 0) || (nobox && this.error)) { // f.ex ref boxes inside struct
            let fill = undefined;
            if (this.error) fill = "red";
            svg += SVGUtils.box(this.name, w, h, 0, 0, x, y, fill)
        } else if (this.error) {
            svg += `<use href="#ebox" x="${x}" y="${y}" />\n`;
        }

        if (val) {
            let opt = SVGUtils.joinOptions(this.textOptions,
                {align: "middle", font: "Helvetica", size: "12px"});
            let [svg1,] = SVGUtils.drawSVGText(val, opt, x, y);
            svg += svg1;
        }

        let textx = this.left().x - 20;
        if ( this.parent ) textx = this.parent.left().x - 10;
        let texty = y;
        let textalign = "end";
        let textsize = "12px";
        let text = this.label;
        if (this.parent && !this.parent.vertical) {
            textx = x;
            texty = y - this.parent.height - 3;
            textalign = "middle";
            textsize = "10px";
        }
        if (this.parent) {
            text = text.replace(this.parent.name + ".", "");
            textsize = "10px";
        }

        // draw variable names if needed
        if (text && this.forcenames && !this.denynames && !this.denyDollarName) {
            let opt = SVGUtils.joinOptions(this.nameOptions,
                {align: textalign, font: "Helvetica", size: textsize});
            let [svg1,] = SVGUtils.drawSVGText(text, opt, textx, texty);
            svg += svg1;
        }
        svg += this.showCount(dy)
        let left = this.left();
        this.snappoints = [
            {x: left.x, y: left.y},
            {x: this.x + dx / 2, y: left.y - this.height / 2 - dy / 2},
            {x: left.x + this.width + dx / 2, y: left.y - dy / 2},
            {x: this.x, y: left.y + 11}, // because def box is 22 height
        ];
        return svg;
    },

    left() {
        // left coordinate for variable
        return {x: this.x - this.width / 2, y: this.y};
    },

    right() {
        // left coordinate for variable
        return {x: this.x + this.width / 2, y: this.y};
    },

    closest(p) {
        // find closest snappoint to p
        let dmin = 1000000;
        let result = this.left();
        for (let i = 0; i < this.snappoints.length; i++) {
            if (this.useSnapPoints)
                if (!(this.useSnapPoints.includes(i))) continue;
            let p2 = this.snappoints[i];
            if (!p2) continue;
            let dx = p2.x - p.x;
            let dy = p2.y - p.y;
            let d = dx * dx + dy * dy;
            if (d < dmin) {
                dmin = d;
                result = p2;
            }
        }
        return result;
    },

    refToSVG() {
        // draw reference arrow from variable to object
        // if variable in error state, draw arrow as red
        // null ref has psecail handling
        let svg = "";
        if (!this.refs || this.refs.length === 0) {
            return "";
        }
        for (let ref of this.refs) {
            let stroke = "#000";
            if (this.error) stroke = "red";
            if (ref === nullRef) {
                if (this.isVertical) continue; // todo draw better
                if (this.parent &&
                    this.parent.isStruct() &&
                    this.parent.vertical &&
                    !this.isLast) continue;
                let x2 = this.x;
                let y2 = this.y + 22; //this.height;
                svg += `<line x1="${this.x}" y1="${this.y}" x2="${x2}" y2="${y2}" stroke="${stroke}" stroke-width="1"  marker-end="url(#nullarrow)" marker-start="url(#startarrow)" />`;
                continue;
            }
            let p = ref.closest(this);
            let x2 = p.x;
            let y2 = p.y;
            svg += `<line x1="${this.x}" y1="${this.y}" x2="${x2}" y2="${y2}" stroke="${stroke}" stroke-width="1"  marker-end="url(#endarrow)" marker-start="url(#startarrow)" />`;
        }
        return svg;
    }
}

const svgNullVariableMixin = {
    toSVG(x, y, w, h) {
        // draw variable as a box, it's name and content
        // if in error state, draw red box also
        // align 1 aligns from left
        let svg = "";
        if (this.graphAttributes) {
            if (this.graphAttributes.x) x = this.graphAttributes.x;
            if (this.graphAttributes.y) y = this.graphAttributes.y;
            if (this.graphAttributes.sx) x += this.graphAttributes.sx;
            if (this.graphAttributes.sy) y += this.graphAttributes.sy;
            //  if (this.graphAttributes.h) hfactor = this.graphAttributes.h;
        }
        this.x = x;
        this.y = y;
        this.width = w;
        this.height = h;
        if ( this.vals === "0") {
            this.noMove = true;
            // this.width = 0;
            // this.height = 0;
        }

        this.snappoints = [
            {x: x, y: y},
        ];
        return svg;
    },

    left() {
        // left coordinate for variable
        return {x: this.x, y: this.y};
    },

    right() {
        // left coordinate for variable
        return {x: this.x, y: this.y};
    },
}

const svgSimpleReferenceVariableMixin = {
    toSVG(x, y, w, h) {
        // draw variable as a box, it's name and content
        // if in error state, draw red box also
        // align 1 aligns from left
        let svg = "";
        if (this.graphAttributes) {
            if (this.graphAttributes.x) x = this.graphAttributes.x;
            if (this.graphAttributes.y) y = this.graphAttributes.y;
            if (this.graphAttributes.sx) x += this.graphAttributes.sx;
            if (this.graphAttributes.sy) y += this.graphAttributes.sy;
            //  if (this.graphAttributes.h) hfactor = this.graphAttributes.h;
        }
        this.x = x;
        this.y = y;
        this.width =  20; // h; // w;
        this.height = h;

        let textx = this.left().x - 20;
        if ( this.parent ) textx = this.parent.left().x - 10;
        let texty = y;
        let textalign = "end";
        let textsize = "12px";
        let text = this.name;
        if (this.parent && !this.parent.vertical) {
            textx = x;
            texty = y - this.parent.height - 3;
            textalign = "middle";
            textsize = "10px";
        }
        if (this.parent) {
            text = text.replace(this.parent.name + ".", "");
            textsize = "10px";
        }

        // draw variable names if needed
        if (this.name && this.forcenames && !this.denynames) {
            let opt = SVGUtils.joinOptions(this.nameOptions,
                {align: textalign, font: "Helvetica", size: textsize});
            let [svg1,] = SVGUtils.drawSVGText(text, opt, textx, texty);
            svg += svg1;
        }
        this.snappoints = [
            {x: x, y: y},
        ];
        return svg;
    },

    left() {
        // left coordinate for variable
        return {x: this.x, y: this.y};
    },

    right() {
        // left coordinate for variable
        return {x: this.x, y: this.y};
    },
}

const svgArrayVariableMixin = {
    toSVG(x, y, w, h, dx, dy) {
        // draw array as variables side by side
        // and indesies over it. If there is over/under indexing
        // draw those before and after array
        let nw = undefined;
        let dir = undefined;
        if (this.graphAttributes) {
            if (this.graphAttributes.x) x = this.graphAttributes.x;
            if (this.graphAttributes.y) y = this.graphAttributes.y;
            if (this.graphAttributes.sx) x += this.graphAttributes.sx;
            if (this.graphAttributes.sy) y += this.graphAttributes.sy;
            if (this.graphAttributes.w) nw = this.graphAttributes.w;
            if (this.graphAttributes.dir !== undefined ) dir = this.graphAttributes.dir;
            // if (this.graphAttributes.h) hfactor = this.graphAttributes.h;
        }
        if (this.dir !== undefined && this.dir > " ") dir = this.dir;
        if (dir === undefined) dir = 1;
        let vertical = false;
        if ( ('vV'.includes(dir) || dir === 0)) vertical = true;
        this.w1 = w;

        let svg1 = "";
        let y1 = 0;
        this.x = x;
        this.y = y;
        let xv = x;
        let yv = y;
        let svg = "";
        this.snappoints = [];
        // defaults for horizontal
        let mx = 1;
        let my = 0;
        let starti = 0;
        let n = this.vars.length;
        let endi = n - 1;
        this.height = h;
        let indexx = dx;
        let indexy = -h / 2 - dy - 4;
        let mi = 1;

        if (vertical) { // vertical
            if (nw === undefined ) nw = 1;
            // TODO: find nw?
            indexx = -nw * w * 0.7;
            indexy = 0;
            mx = 0;
            my = -1;
            starti = endi;
            mi = -1;
            this.height = n * h;
            yv = y + endi * h;
            vertical = true;
        }
        this.vertical = vertical;
        let lastrx = xv;

        for (let i = starti; 0 <= i && i <= endi; i += mi) {
            let v = this.vars[i];
            if (vertical) v.isVertical = true;
            let xvl = xv;
            let yvl = yv;
            if ( this.phase ) {
                let gn = this.phase.namedGraphAttributes[v.name];
                if (gn) {
                    if (gn.tsx) xvl = xv + gn.tsx;
                    if (gn.tsy) yvl = yv + gn.tsy;
                }
            }
            svg += v.toSVG(xvl, yvl, w, h, dx, dy, false, 1, nw);
            xv += mx * v.width;
            yv += my * v.height;
            lastrx = Math.max(lastrx, v.right().x);
            this.snappoints = this.snappoints.concat(v.snappoints);
        }
        this.rightx = lastrx;
        for (let i = 0; i < this.vars.length; i++) {
            let v = this.vars[i];
            if (v.forcenames || v.denynames) continue; // do no tprint i's if name printed
            let opt = SVGUtils.joinOptions(this.indexOptions,
                {align: "end", font: "Helvetica", size: 8});
            [svg1, y1] = SVGUtils.drawSVGText(`${i}`, opt,
                v.x + indexx, v.y + indexy);
            svg += svg1;
            // svg += `<text x="${v.x + indexx}" y="${v.y + indexy}" fill="#000000" font-family="Helvetica" font-size="8px" text-anchor="middle" alignment-baseline="middle">${i}</text>\n`;
        }


        svg += SVGUtils.drawParentName(this);
        svg += this.over[0].toSVG(x - w, y, w, h, dx, dy, true);
        svg += this.over[1].toSVG(xv, y, w, h, dx, dy, true);
        this.width = xv - x;
        svg += this.showCount(dy);
        let midx = (this.rightx + this.left().x)/2;
        if (vertical) {
            let v1 = this.vars[0];
            let v2 = this.vars[this.vars.length-1];
            this.snappoints = [
                v1.snappoints[0],
                v1.snappoints[1],
                v1.snappoints[2],
                v2.snappoints[3]
            ]

        } else {
            this.snappoints = [
                this.left(),
                {x: midx, y: this.y - this.height / 2 - dy / 2},
                {x: this.right().x + dx / 2, y: this.y},
                {x: midx, y: this.y + this.height / 2},
            ]
        }
        return svg;
    },

    refToSVG() {
        // draw all array's references and also
        // over and under indexed refreneces
        let svg = "";
        for (let ref of this.vars)
            svg += ref.refToSVG();
        for (let ref of this.over)
            svg += ref.refToSVG();
        return svg;
    },

    left() {
        // left coordinate for array
        // if (!this.vars || this.vars.length === 0)
            return {x: this.x -this.w1/2, y: this.y};
        // let v = this.vars[0];
        // return {x: v.x - v.width / 2, y: v.y};
        // return v.left(); when going from bottom to up, [0] has not yet attributes
        // return {x: this.x -this.w1/2, y: this.y};
    },

    right() {
        // left coordinate for array
        if (!this.vars || this.vars.length === 0)
            return {x: this.x, y: this.y};
        // let v = this.vars[0];
        // return {x: v.x - v.width / 2, y: v.y};
        return {x: this.rightx, y: this.y};
    },
}

const svgStructVariableMixin = {
    toSVG(x, y, w, h, dx, dy) {
        // draw array as variables side by side
        // and indesies over it. If there is over/under indexing
        // draw those before and after array
        let nh = 1;
        let nw = undefined;
        let dir = 0;
        if ( this.sclass && this.sclass.graphAttributes) {
            const ca = this.sclass.graphAttributes;
            if (ca.dir) dir = ca.dir;
        }
        if (this.graphAttributes) {
            if (this.graphAttributes.x) x = this.graphAttributes.x;
            if (this.graphAttributes.y) y = this.graphAttributes.y;
            if (this.graphAttributes.sx) x += this.graphAttributes.sx;
            if (this.graphAttributes.sy) y += this.graphAttributes.sy;
            if (this.graphAttributes.w) nw = this.graphAttributes.w;
            if (this.graphAttributes.h) nh = this.graphAttributes.h;
            if (this.graphAttributes.dir) dir = this.graphAttributes.dir;
        }
        if (this.dir !== undefined && this.dir > " ") dir = this.dir;
        let vertical = true;
        if ('hH'.includes(dir) || dir === 1) vertical = false;
        if (nw === undefined) nw = vertical ? 2 : 1;

        this.x = x;
        this.y = y;
        let svg = "";
        this.snappoints = [];
        let n = this.vars.length;
        let ymul = 0.55;
        let fh = (n) * h * ymul + (22 - ymul * h);  // 22 is def box height

        // Defs for horizontal struct
        let mx = 1;
        let my = 0;
        let starti = 0;
        let endi = n - 1;
        this.height = h;
        let indexx = dx;
        let indexy = -h / 2 - dy - 4;
        let mi = 1;
        let bw = nw * w;
        this.height = nh * h;
        this.width = n * nw * w;
        let ey = 0; // extra y for refs inside struct
        let xv = x - 0.5*(n-1)*bw;
        let yv = y + (nh - 1) * h / 2;
        let clientw = bw;
        let forcedw = undefined;
        let textalign = 0;

        if (vertical) { // vertical stacking
            this.width = (n) * bw;
            yv = y + (n - 1) * h * ymul;
            xv = x; //  - w / 2;
            indexx = -nw * w * 0.7;
            indexy = 0;
            mx = 0;
            my = -1;
            starti = endi;
            mi = -1;  // move i direction
            this.width = w * nw;
            this.height = (n) * h * ymul + (22 - ymul * h);  // 22 is def box height
            // yv = y + endi*h;
            fh = this.height;
            ey = 0;
            forcedw = 2;
            textalign = 0;
        }
        this.vertical = vertical;
        fh = this.height;

        this.leftx = this.x - this.width / 2;
        this.lefty = this.y + fh / 2 - h / 2;
        this.rightx = this.x + this.width / 2;
        this.righty = this.y + fh / 2 - h / 2;
        let refsn = 0;
        let lastRefi = -1;
        if ( vertical ) {  // vertical last ref is drawn a bit lower if only ref
            for (let v of this.vars) {
                if (v.isRef()) refsn++;
            }
            if (refsn === 1 && this.vars.length > 1) {
                ey = 6;
                lastRefi = this.vars.length -1;
            }
        }

        if (!this.denyBox)
            svg += SVGUtils.box(this.name, this.width, fh, dx, dy, x, y + fh / 2 - h / 2.0)
        // svg += this.vars[0].toSVG(xv, y, w, h, dx, dy, false, 1 );
        for (let i = starti; 0 <= i && i <= endi; i += mi) {
            let v = this.vars[i];
            v.textOptions = {size: 10};
            if (v.isRef()) {
                let refw = Math.min(clientw, w);
                let dropy = 0;
                if (i === lastRefi) dropy = ey;
                // draw a ref a bit lower, smaller and without shadow
                svg += v.toSVG(xv, yv + dropy, refw, h * ymul * 0.8, 0, 0, true, textalign, forcedw);
            } else {
                svg += v.toSVG(xv, yv, clientw, h * ymul, dx, dy, true, textalign, forcedw);
            }
            xv += mx * bw; // v.width;
            yv += my * v.height;
            this.snappoints = this.snappoints.concat(v.snappoints);
        }

        svg += SVGUtils.drawParentName(this);

        /*
        for (let i = 0; i < this.vars.length; i++) {
            let v = this.vars[i];
            svg += `<text x="${v.x + dx}" y="${v.y - h / 2 - dy - 4}" fill="#000000" font-family="Helvetica" font-size="8px" text-anchor="middle" alignment-baseline="middle">${i}</text>\n`;
        }
        */

        let left = this.left();
        this.snappoints = [
            {x: left.x, y: left.y},
            {x: this.x, y: left.y - fh / 2 - dy / 2},
            {x: left.x + this.width + dx / 2, y: left.y - dy / 2},
            {x: this.x, y: left.y + fh / 2}, // because def box is 22 height
        ];

        svg += this.showCount(dy);
        return svg;
    },

    refToSVG() {
        // draw all array's references and also
        // over and under indexed refreneces
        let svg = "";
        for (let ref of this.vars)
            svg += ref.refToSVG();
        for (let ref of this.over)
            svg += ref.refToSVG();
        return svg;
    },

    left() {
        return {x: this.leftx, y: this.lefty};
        // left coordinate for array
        /*
        if (!this.vars || this.vars.length === 0)
            return {x: this.x, y: this.y};
        let v = this.vars[0];
        return {x: v.x - this.width / 2, y: v.y};
        // return v.left();
         */
    },

    right() {
        return {x: this.rightx, y: this.righty};
    },
}

// Add visual methods to variable and array varaible
Object.assign(TextObject.prototype, svgTextMixin);
Object.assign(Variable.prototype, svgVariableMixin);
Object.assign(NullVariable.prototype, svgNullVariableMixin);
Object.assign(SimpleRefecenceVariable.prototype, svgSimpleReferenceVariableMixin);
Object.assign(ArrayVariable.prototype, svgArrayVariableMixin);
Object.assign(StructVariable.prototype, svgStructVariableMixin);


/*!
 * Class for SVG representation of Variables
 */
class VisualSVGVariableRelations {
    getElementPos(el) {
        let lx = 0, ly = 0
        for (;
            el != null;
            lx += el.offsetLeft, ly += el.offsetTop, el = el.offsetParent) {
        }
        return {x: lx, y: ly};
    }

    svgclick(e) {
        if (!this.elements.coordspan) return;
        // let d = this.getElementPos(this.elements.svgdiv);
        // let x = e.clientX; // - d.x;
        // let y = e.clientY; // - d.y;
        let x = e.offsetX;
        let y = e.offsetY; // - d.y;
        // errspan.innerText = `${x},${y} ${e.clientX},${e.clientY}` +
        //                     `${e.pageX},${e.pageY} ${d.x},${d.y}`;
        this.elements.coordspan.innerText = `tx:${x},ty:${y}`;
    }

    setSVG(svg, svgDiv) {
        svgDiv.innerHTML = svg;
    }

    drawSVG(svg) {
        this.setSVGCallback(svg, this.elements.svgdiv);
        // TODO: height??
        // this.elements.svgdiv.innerHTML = svg;
        // errspan.innerText = "";
        // errspan.innerText = error.message;
    }

    addError(err) {
        if (this.elements.errspan)
            this.elements.errspan.innerText += err;
    }

    addCode(code) {
        if (this.elements.codediv)
            this.elements.codediv.innerHTML += code;
    }

    clearError() {
        if (this.elements.errspan)
            this.elements.errspan.innerText = "";
    }

    encodeHTML(s) {
        if (!s) return "";
        return s.replace(/[\u00A0-\u9999<>&]/gim, function (i) {
            return '&#' + i.charCodeAt(0) + ';';
        });
    }

    constructor(variableRelations, args, elements) {
        this.elements = elements;
        this.clearError();
        this.variableRelations = variableRelations;
        this.args = args; // not yet used
        this.svg = undefined;
        this.svglist = [];
        if (this.elements.svgdiv)
            this.elements.svgdiv.addEventListener("click", (e) => this.svgclick(e));
    }

    getPrevNextStepNumbers(stepnr) {
        // returns an exact interval when to show current line
        // This is one of the most difficult and error prone code here!!!
        let vars = this.variableRelations;
        let result = [0, vars.maxStepnumber + 1];
        if (vars.isAnimateCommands()) {
            for (let code of vars.allCodeList) {
                if (code.stepnumber <= stepnr) {
                    result[0] = Math.min(code.stepnumber + 1, vars.maxStepnumber);
                }
                if (code.stepnumber > stepnr) {
                    result[1] = Math.max(code.stepnumber, 1);
                    break;
                }
            }
            return result;
        }
        stepnr++;
        for (let code of vars.allCodeList) {
            if (code.stepnumber <= stepnr) {
                result[0] = Math.min(code.stepnumber, vars.maxStepnumber);
            }
            if (code.stepnumber > stepnr) {
                result[1] = Math.min(code.stepnumber - (result[0] > 0 ? 1 : 0), vars.maxStepnumber);
                break;
            }
        }

        return result;
    }


    /*!
     * Converts variableRelations variables to string SVG notation
     * \fn makeSVG()
     * \return string svg
     */
    makeSVG() {
        let w = 32;
        let h = 22;
        let dx = 8;
        let dy = 8;
        let s1 = ""; // helper for text calls
        let y1 = 0;  // helper for text calls

        this.svglist = [];
        this.svglist.push( '<defs>' +
            SVGUtils.box("rbox1", w, h, dx, dy) +
            SVGUtils.box("rbox2", 2 * w, h, dx, dy) +
            SVGUtils.box("rbox3", 3 * w, h, dx, dy) +
            SVGUtils.box("rbox4", 4 * w, h, dx, dy) +
            SVGUtils.box("rbox5", 5 * w, h, dx, dy) +
            SVGUtils.box("rbox6", 6 * w, h, dx, dy) +
            SVGUtils.ebox("ebox", w, h, dx, dy, "red") +
            SVGUtils.lineends() +
            '</defs>\n');


        let rankBeginx = 110;
        let rankDx = this.variableRelations.rankGlobalMoveDx || 143;
        let rankBeginy = 30;

        const rankStack = [];
        function pushRank(rank) {
            rankStack.push(JSON.stringify(rank));
        }

        function popRank(rank) { // default if no on in stack
            if (rankStack.length === 0) return rank;
            const r = rankStack.pop();
            const res = JSON.parse(r);
            ranks[currentRank] = res;
            return res;
        }

        const modelRank = {
                x: rankBeginx,
                y: rankBeginy,
                ax: rankBeginx,
                ay: rankBeginy,
                dir: 0,
                dx: this.variableRelations.rankGlobalMoveDx,
                dy: this.variableRelations.rankGlobalMoveDy,
                max: {x : 0,y : 0},
            };

        const modelRankS = JSON.stringify(modelRank);

        let ranks = []; // 0 = stack, 1 = heap
        for (let i = 0; i < 10; i++) {
            let r = JSON.parse(modelRankS);
            r.x += i * rankDx;
            r.ax += i * rankDx;
            ranks.push(r);
        }

        let currentRank = 0;
        function getRank(ranks, rank, r) {
            if (r === undefined || r < 0) return rank;
            currentRank = r;
            let result = ranks[r];
            if (result !== undefined) return result;
            result = JSON.parse(modelRankS)
            ranks[r] = result;
            return result;
        }


        function getRankMaxs(ranks, maxs) {
            let result = {x: 0, y: 0};
            for (let r of ranks) {
                if (r === undefined) continue;
                if (r.x > result.x) result.x = r.x;
                if (r.y > result.y) result.y = r.y;
            }
            if (maxs.x > result.x) result.x = maxs.x;
            if (maxs.y > result.y) result.y = maxs.y;
            return result;
        }

        let maxs = {x: 0, y: rankBeginy};
        let maxx = 0;
        let maxy = 0;
        let lastObj = undefined;

        let linenr = 1;
        if (this.elements.codediv) {
            this.elements.codediv.innerHTML = "";
            let [prev, next] = this.getPrevNextStepNumbers(this.stepnumber);
            for (let code of this.variableRelations.allCodeList) {
                let ns = String(linenr).padStart(2, '0');
                let curr = "";
                let text = this.encodeHTML(code.code);
                if (text === "CODE: END" || text === "END") {
                    ns = "  ";
                    text = "";
                }
                if (prev <= code.stepnumber && code.stepnumber <= next)
                    curr = "current";
                this.addCode(`<pre class="step ${curr}">${ns} ${text}</pre>`);
                linenr++;
            }
        }

        let gopts;

        for (let phase of this.variableRelations.phaseList) {

            for (let r of ranks) r.y = maxs.y;

            let rank = ranks[0];
            [s1,] = SVGUtils.drawSVGText(phase.phaseNumber,
                {
                    color: "red", size: 16,
                    font: "Helvetica",
                    weight: "bold"
                }, 0, rank.y - h);
            this.svglist.push(s1);

            // draw possible header texts
            [s1, y1] = SVGUtils.drawSVGTexts(phase.texts[0], 20, rank.y, h);
            this.svglist.push(s1);
            for (let r of ranks) r.y = y1;

            for (let v of phase.vars) { // stack vars (local normal vars)
                // join named attributes before in place attributes
                let gn = phase.namedGraphAttributes[v.name];
                if (gn !== undefined) {
                    v.graphAttributes = {...gn, ... v.graphAttributes};
                    let r = v.graphAttributes.rank;
                    if ( r !== undefined) v.rank = r;
                }
                rank = getRank(ranks, rank, v.rank);
                let oldp = {x: rank.x, y: rank.y};

                gopts = v.graphAttributes;
                if (gopts) {
                    if (gopts.pop !== undefined) {
                        let rm = rank.max;
                        rank = popRank(rank);
                        countRankPosPop(rank, rm);
                    }
                    if (gopts.push !== undefined)
                        pushRank(rank);
                    if (gopts.rank !== undefined) {
                        rank = getRank(ranks, rank, gopts.rank);
                    }
                    if (gopts.tx !== undefined) gopts.x = gopts.tx;
                    if (gopts.ty !== undefined) gopts.y = gopts.ty;
                    if (gopts.tsx !== undefined) gopts.sx = gopts.tsx;
                    if (gopts.tsy !== undefined) gopts.sy = gopts.tsy;
                    if (gopts.trx !== undefined) gopts.rx = gopts.trx;
                    if (gopts.try !== undefined) gopts.ry = gopts.try;

                    if (gopts.rankdir !== undefined) {
                        rank.dir = gopts.rankdir;
                    }
                    if (gopts.snap !== undefined) {
                        let snap = "" + gopts.snap;
                        if (snap === "a") rank.snappoints = undefined;
                        else {
                            if (!Array.isArray(snap)) snap = snap.split(/[ ,]+/);
                            let jsnap = [];
                            for (let c of snap) jsnap.push(parseInt(c));
                            rank.snappoints = jsnap;
                        }
                    }
                    oldp = {x: rank.x, y: rank.y};

                    if (gopts.w !== undefined) rank.w = gopts.w;
                    if (gopts.h !== undefined) rank.h = gopts.h;
                    if (gopts.ax !== undefined) rank.ax = gopts.ax;
                    if (gopts.ay !== undefined) rank.ay = gopts.ay;

                    if (gopts.rx !== undefined) rank.x = rank.ax + gopts.rx;
                    if (gopts.ry !== undefined) rank.y = rank.ay + gopts.ry;
                    if (gopts.rdx !== undefined) rank.dx = gopts.rdx;
                    if (gopts.rdy !== undefined) rank.dy = gopts.rdy;
                    if (gopts.rev !== undefined) rank.reversePos =
                        gopts.rev === 1 ? this.svglist.length : 0;
                } else {
                    v.graphAttributes = {};
                    gopts = v.graphAttributes;
                }
                if (rank.snappoints) v.useSnapPoints = rank.snappoints;
                if (rank.w) gopts.w = rank.w;
                if (rank.h) gopts.h = rank.h;

                if (v.text) v.lastObj = lastObj;
                v.phase = phase;
                // Here we draw the object
                //const p = {x: rank.x, y: rank.y, w: w, h: h, dx: dx, dy: dy};
                let svg = v.toSVG(rank.x, rank.y, w, h, dx, dy);

                // in rdx or rdy case use old x or y
                if (gopts.trx !== undefined) rank.x = oldp.x;
                if (gopts.try !== undefined) rank.y = oldp.y;

                function countRankPosPop(rank, rm) {
                    // for rank pop top return correct place
                    let vw = rm.x - rank.x;
                    let vh = rm.y - rank.y;

                    let xextra = 40 + w/2;
                    let yextra = 25 + h/2;

                    let xadd = rank.dir ? vw + xextra : 0;
                    let yadd = (rank.dir || !vh) ? 0 : vh + yextra;

                    if (xadd !== 0 && rank.dx !== undefined) xadd = rank.dx;
                    if (yadd !== 0 && rank.dy !== undefined) yadd = rank.dy;

                    rank.y += yadd;
                    rank.x += xadd;
                }

                function countRankPos(rank, v) {
                    let xextra = 40;
                    let yextra = 25;

                    if (v.text) { // tekstin tapauksessa ei ylm lisää
                        xextra = yextra = 0;
                    } else {
                        lastObj = v;
                    }

                    let xadd = rank.dir ? v.width + xextra : 0;
                    let yadd = (rank.dir || !v.height) ? 0 : v.height + yextra;

                    if (xadd !== 0 && rank.dx !== undefined) xadd = rank.dx;
                    if (yadd !== 0 && rank.dy !== undefined) yadd = rank.dy;

                    if (gopts.dx !== undefined) xadd = gopts.dx;
                    if (gopts.dy !== undefined) yadd = gopts.dy;

                    if (v.noMove) {  // mosty for null variable
                        xadd = 0;
                        yadd = 0;
                    }
                    if (gopts.ty === undefined && gopts.tsy === undefined && gopts.try === undefined)
                        rank.y = v.y + yadd;  // TODO: add last element height
                    if (gopts.tx === undefined && gopts.tsx === undefined && gopts.trx === undefined)
                        rank.x = v.x + xadd;
                }

                countRankPos(rank, v);


                if (rank.reversePos)
                    this.svglist.splice(rank.reversePos, 0, svg);
                else
                    this.svglist.push(svg);

                maxs = getRankMaxs(ranks, maxs);
                maxx = Math.max(maxx, v.right().x);
                maxy = Math.max(maxy, v.left().y + v.height);
                rank.max.x = Math.max(rank.max.x, v.right().x);
                rank.max.y = Math.max(rank.max.y, v.left().y + v.height/2);

                SVGUtils.debug(this.svglist, gopts, "c", 2, v, 40, "green");
                SVGUtils.debug(this.svglist, gopts, "r", 2, rank.max, 5, "red");
            }

            for (let v of phase.vars) { // reference arrows
                if (!v.isRef()) continue;
                let svg = v.refToSVG();
                this.svglist.push(svg);
            }


            // draw possible footer texts
            [s1, y1] = SVGUtils.drawSVGTexts(phase.texts[1], 20, maxs.y + 10, h);
            maxs.y = Math.max(maxs.y, y1);
            this.svglist.push(s1);

            // Draw a line between phases
            if (!this.variableRelations.isLastPhase(phase) && phase.hasVars()) {
                let stroke = "#C0C0C0";
                this.svglist.push(`<line x1="${0}" y1="${maxs.y}" x2="${maxs.x}" y2="${maxs.y}" stroke="${stroke}" stroke-width="2"  />`);
                maxs.y += 2 * h;
            }
        }

        for (let sv of this.variableRelations.svgs) {
            this.svglist.push(sv + "\n");
        }

        // TODO: find width correctly
        let width = Math.max(maxx + 20, 100); // Math.max(maxs.x, 100);
        let height = Math.max(maxs.y,100, maxy ) ;

        SVGUtils.debug(this.svglist, gopts, "m", 2, {x:width-1, y: height-1}, 5, "blue");

        this.svglist.push(`</svg>`);


        // add svg size info
        this.svg = `<svg width="${width}px" height="${height}px"
                         viewBox="-0.5 -0.5 ${width} ${height}" >\n`;
        for (let svg of this.svglist)
            this.svg += svg;

    }


    /*!
     * Draw all variables.
     * \fn draw()
     */
    draw() {
        this.drawSVG(this.svg);
        if (this.variableRelations.errors) {
            this.addError(this.variableRelations.errors);
        }
    }


    update() {
        // clear errors an rerun to the current step
        this.clearError();
        let step = this.variableRelations.runUntil(this.step);
        this.stepnumber = step;
        this.step = step;
        this.makeSVG();
        this.draw();
    }

    maxStep() {
        return this.variableRelations.maxStep();
    }

    reset() {
        this.stopAnimate();
        this.step = 0;
    }

    move(dir) {
        let cmd;
        do {
            this.step += dir;
            if (this.step <=  0) return false;
            if (this.step >=  this.maxStep()) return false;
            cmd = this.variableRelations.commands[this.step];
        } while (cmd.noAnimate);
        return true;
    }

    forward() {
        return this.move(1);
    }

    backward() {
       return this.move(-1);
    }

    stopAnimate() {
        clearInterval(this.timer);
    }

    startAnimate(moveOneStepToDirection, until) {
        // start animation and call moveOneStepToDirection
        // in every intervall.  Stop if condtion until
        // reached.
        let visual = this;
        this.stopAnimate();
        this.timer = setInterval(function () {
            if (until()) {
                visual.stopAnimate();
                return;
            }
            if (!moveOneStepToDirection()) {
                visual.stopAnimate();
            }
            visual.update();
        }, 500)
    }

    animate(n, move, until) {
        this.stopAnimate();
        let visual = this;
        if (n === undefined) {
            this.startAnimate(move, until)
            return;
        }
        this.step = Math.min(visual.step, visual.maxStep());
        let cmd1 = this.variableRelations.commands[visual.step];

        function check() {
            // run until different CodeCommand than in start
            // and not simple comment.  Return true when to stop
            let cmd2 = visual.variableRelations.commands[visual.step];
            if (!cmd2) return false;
            if (cmd2.code === undefined) return false;
            if (cmd2 === cmd1) return false;
            if (cmd2.noAnimate) return false;
            const code = cmd2.code.trim();
            if (code === "") return false;
            if (code.startsWith("///")) return true;
            if (code.startsWith("//")) return false;
            return !code.startsWith("#");
        }

        this.startAnimate(move, check)
    }

} // Visual


class Animation {
    constructor(visual, buttonDiv) {
        this.visual = visual;
        this.buttonDiv = buttonDiv;
        this.createButtons();
    }

    jumpToStart() {
        this.visual.stopAnimate();
        this.visual.reset();
        this.visual.update();
    }

    stepFwd() {
        this.visual.stopAnimate();
        this.visual.forward();
        this.visual.update();
    }

    stepBack() {
        this.visual.stopAnimate();
        this.visual.backward();
        this.visual.update();
    }


    animateFwd(n) {
        this.visual.animate(n, () => this.visual.forward(),
            () => this.visual.step >= this.visual.maxStep());
    }


    animateBack(n) {
        this.visual.animate(n, () => this.visual.backward(),
            () => this.visual.step <= 0);
    }


    jumpToEnd() {
        this.visual.stopAnimate();
        this.visual.step = this.visual.maxStep();
        this.visual.update();
    }

    createButton(jump, text, title) {
        let button = document.createElement("button");
        button.innerText = text;
        button.onclick = jump;
        button.title = title;
        this.buttonDiv.appendChild(button);
    }

    createButtons() {
        if (!this.buttonDiv) return;
        if (this.buttonDiv.hasChildNodes()) return;
        this.createButton(() => this.jumpToStart(), "|<", "reset");
        this.createButton(() => this.animateBack(), "<<<", "animate back to start");
        this.createButton(() => this.animateBack(1), "<<", "animate one step back");
        this.createButton(() => this.stepBack(), "<", "back minor step");
        this.createButton(() => this.stepFwd(), ">", "do minor step");
        this.createButton(() => this.animateFwd(1), ">>", "animate one step");
        this.createButton(() => this.animateFwd(), ">>>", "animate to end");
        this.createButton(() => this.jumpToEnd(), ">|", "run to end");
        this.buttonDiv.classList.remove("hidden");
    }
}

function ensureElement(parent, id, cls, type) {
    let elem = parent.querySelector(`#${id}`)
    if (elem) return elem;
    if (!cls) cls = id;
    let divelem = document.createElement("div");
    if (type) {
        let innerelem = document.createElement(type);
        divelem.appendChild(innerelem);
        divelem.classList.add(cls);
        elem = innerelem;
    } else {
        elem = divelem;
    }
    elem.id = id;
    elem.classList.add(cls);
    parent.appendChild(divelem);
    return elem;
}


function getElements(params) {
    let elements = undefined;
    let variablesDiv = undefined;
    let getButtons = false;
    if (params) {
        elements = params.elements;
        variablesDiv = params.variablesDiv;
        getButtons = params.animate;
    }
    if (elements) return elements;

    elements = {};
    if (!variablesDiv) variablesDiv = document.getElementById('variablesDiv');
    if (!variablesDiv) {
        variablesDiv = ensureElement(document.body, "variablesDiv");
    }
    elements.codediv = ensureElement(variablesDiv, 'codediv');
    elements.errspan = ensureElement(variablesDiv, 'errorspan', 'varerror', 'span');
    elements.svgdiv = ensureElement(variablesDiv, 'svgdiv');
    if (getButtons)
        elements.buttondiv = ensureElement(variablesDiv, 'buttondiv');
    elements.coordspan = ensureElement(variablesDiv, 'coord');
    elements.coordspan.contenteditable = true;
    elements.coordspan.onclick = () => {
        // elements.coordspan.selectAll();
        let range = document.createRange();
        range.selectNodeContents(elements.coordspan);
        let sel = window.getSelection();
        sel.removeAllRanges();
        sel.addRange(range);
        document.execCommand("copy");
    }
    return elements;
}


function setData(data) {
    if (data === undefined) return;
    let params = data.params || {};
    let elements = getElements(params);
    let variableRelations = new VariableRelations(data.code,
        params, knownCommands);
    let newCall = true;
    let visual = elements.svgdiv.visual; // is it allreadu created?
    if (!visual) {
        visual = new VisualSVGVariableRelations(variableRelations,
            data.args,
            elements);
        visual.setSVGCallback = params.setSVGCallback ? params.setSVGCallback : visual.setSVG;
        // if ( params.setSVGCallback ) visual.setSVGCallback = param.setSVGCallback;
    } else { // yes it was.  Just init it
        visual.variableRelations = variableRelations;
        visual.elements = elements;
        visual.clearError();
        newCall = false;
    }
    elements.svgdiv.visual = visual;  // to find next time
    let step1 = visual.maxStep() + 1;
    if ( params.animate) {
        if (newCall) new Animation(visual, elements.buttondiv);
        step1 = 0;
    }
    let step = variableRelations.runUntil(step1);
    visual.stepnumber = step;
    visual.step = step;
    visual.makeSVG();
    visual.draw();
}

// TODO: for struct
// auto width
// struct names
// svg draw callback

export {setData, varsStringToJson};
// export default setData;