// noinspection DuplicatedCode

// For Lexer see: https://github.com/aaditmshah/lexer
class Lexer {
// Create lexer, add regexp rules where to match
// call lexer.setInput(input) to give input
// call lexer.lex() to get on token at time until undefined
// longest match is used if not /g in rule.
    deferror(chr) {  // default error handler
        throw new Error(`Unexpected character at index ${this.index - 1}: ${chr}`);
    };


    constructor(defunct) {
        this.defunc = defunct;
        if (typeof defunct !== "function") this.defunct = this.deferror;

        try {
            Lexer.engineHasStickySupport = typeof /(.)/.sticky == 'boolean';
        } catch (ignored) {
            Lexer.engineHasStickySupport = false;
        }
        try {
            Lexer.engineHasUnicodeSupport = typeof /(.)/.unicode == 'boolean';
        } catch (ignored) {
            Lexer.engineHasUnicodeSupport = false;
        }

        this.tokens = [];
        this.rules = [];
        this.setInput("")
    }

    addRule(pattern, action, start) {
        let global = pattern.global;

        if (!global || Lexer.engineHasStickySupport && !pattern.sticky) {
            let flags = Lexer.engineHasStickySupport ? "gy" : "g";
            if (pattern.multiline) flags += "m";
            if (pattern.ignoreCase) flags += "i";
            if (Lexer.engineHasUnicodeSupport && pattern.unicode) flags += "u";
            pattern = new RegExp(pattern.source, flags);
        }

        if (Object.prototype.toString.call(start) !== "[object Array]") start = [0];

        this.rules.push({
            pattern: pattern,
            global: global,
            action: action,
            start: start
        });

        return this;
    }

    setInput(input) {
        this.remove = 0;
        this.state = 0;
        this.index = 0;
        this.tokens.length = 0;
        this.input = input;
        return this;
    }

    lex() {
        // There might be tokens from last action returned array
        if (this.tokens.length > 0) return this.tokens.shift();

        this.reject = true;

        while (this.index <= this.input.length) {
            let token;  // remove in next is for removing used empty matches
            let matches = this.scan().splice(this.remove);
            let index = this.index;

            while (matches.length > 0) {
                if (!this.reject) break;
                let match = matches.shift();
                this.index += match.length;
                this.reject = false;
                this.remove++;  // this is used for empty matches

                token = match.action.apply(this, match.result);
                if (this.reject) { // action may change this.reject!
                    this.index = match.result.index;
                    continue;
                }
                if (token === undefined)
                    continue;
                if (Array.isArray(token)) { // save rest if many
                    this.tokens = token.slice(1);
                    token = token[0]; // and return first one
                }
                if (match.length > 0) this.remove = 0;
                return token;
            }

            if (index >= this.input.length) {
                if (matches.length === 0) return undefined;
                this.reject = true; // opportunity for empty match at end
                continue;
            }

            if (this.reject) {
                this.remove = 0;
                token = this.defunct.call(this, this.input.charAt(this.index++));
                if (token === undefined)  continue;
                if (Array.isArray(token)) {
                    this.tokens = token.slice(1);
                    token = token[0];
                }
                return token;
            }

            if (this.index !== index) this.remove = 0;
            this.reject = true;
        }
        return undefined;
    }

    scan() {
        let matches = [];
        let index = 0;

        let state = this.state;
        let lastIndex = this.index;
        let input = this.input;

        for (const rule of this.rules) {
            let start = rule.start;
            let states = start.length;

            // The use of states is not very clear???
            if ((states > 0 && start.indexOf(state) < 0) &&
                (state % 2 === 0 || states > 1 || start[0] === 1)) continue;

            rule.pattern.lastIndex = lastIndex;
            let result = rule.pattern.exec(input);
            if (!result || result.index !== lastIndex) continue;

            let j = matches.push({
                result: result,
                action: rule.action,
                length: result[0].length
            });

            if (rule.global) index = j; // do not sort prior this

            // move to place according to its length, longest first
            while (--j > index) {
                let k = j - 1;

                if (matches[j].length > matches[k].length) {
                    let temple = matches[j];
                    matches[j] = matches[k];
                    matches[k] = temple;
                }
            }
        }

        return matches;
    }
}

// Dijkstra's shunting yard algorithm
// Parser see: https://gist.github.com/aaditmshah/6683499
class Parser {

    constructor() { }

    parse(tokens) {
        let output = [];
        let stack = [];

        for (let token of tokens) {
            if (token.name() ==="(") {
                stack.push(token);
                continue;
            }
            if (token.name() === ")") {
                while (stack.length) {
                    token = stack.pop();
                    if (token.name() === "(") break;
                    output.push(token);
                }

                if (token.name() !== "(")
                    throw new Error("Missing (.");
                continue;
            }

            if (token.type() === undefined) {
                output.push(token);
                continue;
            }
            while (stack.length > 0) {
                let top = stack[stack.length-1];
                if (top.name() === "(") break;
                let operator = token.type();
                let precedence = operator.precedence;
                let antecedence = top.type().precedence;

                if (precedence > antecedence ||
                    precedence === antecedence &&
                    operator.associativity === "right") break;
                output.push(stack.pop());
            }

            stack.push(token);
        }

        while (stack.length > 0) {
            let token = stack.pop();
            if (token.name() !== "(") output.push(token);
            else throw new Error("Missing ).");
        }

        return output;
    }
}

function roundToNearest(num, decimals) {
    decimals = decimals || 0;
    let p = Math.pow(10, decimals);
    let p2 = p;
    while (Math.abs(num) > 10) { // normalize
        p2 /= 10;
        num /= 10;
    }
    let n = (num * p) * (1 + Number.EPSILON);
    return Math.round(n) / p2;
}

// Base class for all calculator operations
class CalculatorOp {

    static power = {
        precedence: 5,
        associativity: "left"
    };

    static sign = {
        precedence: 7,
        associativity: "right"
    };

    static factor = {
        precedence: 3,
        associativity: "left"
    };

    static term = {
        precedence: 1,
        associativity: "left"
    };

    static func = {
        precedence: 3,
        associativity: "right"
    };

    constructor(calculator) {
        this.calculator = calculator;
    }

    type() {   }           // if parser should order, return on of above
    reg() { return / /;  }  // regexp to match those operation

    pop() {                // pop value from calc stack
        if (this.calculator.stack.length === 0)
            return this.calculator.lastResult;
        return this.calculator.stack.pop();
    }

    push(v) { this.calculator.stack.push(this.r(v)); }
    name() { return this.reg().source.replace("\\", ""); } // needed if name is different from op
    lexname() { return this.name(); } // name to be used in allowed or illegals list
    output(extraBefore) { return ` ${this.name()} `;  } // text to output
    extraSpaceAfter() { return ""; } // need space before some operation?
    doCalc() {  }              // helper method for doing calc
    calc() { this.doCalc(); }  // what to do when run
    r(v) { return roundToNearest(v, this.calculator.params.decimals);  }
    lexfunc(lexme, lexer) { return this;  }  // what to be returned for lexer if match
    allowSignRight() { return true; } // does allow sign operation on right side
    checkSign(lastToken) { return this; } // deny 2-3 as a sign and change to bin operator -

    getnum(s) { // convert s as numeric using p, mem or parse
        if (s === undefined) return this.calculator.lastResult;
        s = (""+s).trim();
        if (s === "") return this.calculator.lastResult;
        if (s.match(/^p[0-9]*$/)) { // from pX calc index for r
            if (s==="p") return this.calculator.lastResult;
            let idx = this.calculator.row - parseInt(s.substring(1));
            idx = Math.max(0, Math.min(idx, this.calculator.row-1));
            return this.calculator.mem["r"+idx] || 0;
        }
        if (s[0].match(/^[a-z]/)) { // use mem value
            let val = this.calculator.mem[s];
            if (val === undefined) throw new Error(`Unknown variable '${s}'`);
            return val;
        }
        return parseFloat(s.replace(",", "."));
    }
}

class Command extends  CalculatorOp {
    output(extraBefore) { return extraBefore + this.name() + " "; }
}

class FuncRROperation extends  CalculatorOp {
    type() { return CalculatorOp.func; }
    doCalc(a) { return 0; }
    output(extraBefore) { return extraBefore + this.name(); }
    extraSpaceAfter() { return " "; }

    calc() {
        let a = this.pop();
        this.push(this.doCalc(a));
    }
}

class BracketOperation extends  CalculatorOp {
    output(extraBefore) { return this.name(); }
}

class ValueOperation extends  CalculatorOp {
    constructor(calculator) {
        super(calculator);
        this.value = "";
    }
    name() { return ""+this.value; }
    output(extraBefore) { return extraBefore + this.name(); }
    calc() { this.push(this.value);}
    allowSignRight() { return false; }
    extraSpaceAfter() { return " "; }
}


class BinOperation extends  CalculatorOp {
    type() { return CalculatorOp.term; }
    doCalc(a,b) { return 0; }
    calc() {
        let b = this.pop();
        let a = this.pop();
        this.push(this.doCalc(a,b));
    }
}

class SignOperation extends FuncRROperation{
    type() { return CalculatorOp.sign; }
    extraSpaceAfter() { return ""; }
    output(extraBefore) { return extraBefore + this.name().trim(); }
    newCalcOperation() { return new BinOperation(this.calculator); }
}

// These two are outside of operations list, because
// they are referred inside the list (SignMinus and SignPlus)
class Plus extends BinOperation {
    reg() { return / *\+ /; }
    name() { return "+"; }
    doCalc(a, b) { return a + b; }
}

class Minus extends BinOperation {
    reg() { return / *- /; }
    name() { return "-"; }
    doCalc(a, b) { return a - b; }
}

const operations = [

class LeftBracketOperation extends  BracketOperation {
    reg() { return /\(/;}
    extraSpaceAfter() { return ""; }
},

class RightBracketOperation extends  BracketOperation {
    reg() { return /\)/;}
    allowSignRight() { return false; }
},

class SignMinus extends SignOperation {
    reg() { return / *-/; }
    name() { return " -"; }
    lexname() { return "signm"}
    calc() {
        let a = this.pop();
        this.push(-a);
    }
    newCalcOperation() { return new Minus(this.calculator); }
    checkSign(lastToken) {
        if (lastToken.allowSignRight()) return this;
        return this.newCalcOperation();
    }
},

class SignPlus extends SignOperation {
    reg() { return / *\+/; }
    name() { return " +"; }
    lexname() { return "signp"}
    calc() {  }
    newCalcOperation() { return new Plus(this.calculator); }
    checkSign(lastToken) {
        if (lastToken.allowSignRight()) return this;
        return this.newCalcOperation();
    }
},

Plus,
Minus,

class Mul extends BinOperation {
    reg() { return /\*/; }
    type() { return CalculatorOp.factor; }
    doCalc(a, b) { return a * b; }
},

class Div extends BinOperation {
    reg() { return /\//; }
    type() { return CalculatorOp.factor; }
    doCalc(a, b) { return a / b; }
},

class Pow extends BinOperation {
    reg() { return /\^/; }
    type() { return CalculatorOp.power; }
    extraSpaceAfter() { return ""; }
    output(extraBefore) { return this.name().trim(); }
    doCalc(a, b) { return Math.pow(a , b); }
},

class Sin extends FuncRROperation {
    reg() { return /sin/; }
    doCalc(a) { return Math.sin(this.calculator.toAngle(a)); }
},

class Cos extends FuncRROperation {
    reg() { return /cos/; }
    doCalc(a) { return Math.cos(this.calculator.toAngle(a)); }
},

class Tan extends FuncRROperation {
    reg() { return /tan/; }
    doCalc(a) { return Math.tan(this.calculator.toAngle(a)); }
},

class ASin extends FuncRROperation {
    reg() { return /asin/; }
    doCalc(a) { return this.calculator.fromAngle(Math.asin(a)); }
},

class ACos extends FuncRROperation {
    reg() { return /acos/; }
    doCalc(a) { return this.calculator.fromAngle(Math.acos(a)); }
},


class ATan extends FuncRROperation {
    reg() { return /atan/; }
    doCalc(a) { return this.calculator.fromAngle(Math.atan(a)); }
},

class Sqrt extends FuncRROperation {
    reg() { return /sqrt/; }
    doCalc(a) { return Math.sqrt(a); }
},

class Ln extends FuncRROperation {
    reg() { return /ln/; }
    doCalc(a) { return Math.log(a); }
},

class Log10 extends FuncRROperation {
    reg() { return /log10/; }
    doCalc(a) { return Math.log10(a); }
},

class Rad extends Command {
    reg() { return /rad/; }
    calc() { this.calculator.deg = false; }
},

class Deg extends Command {
    reg() { return /deg/; }
    calc() { this.calculator.deg = true; }
},

class RPN extends Command {
    reg() { return /rpn/; }
    calc() { this.calculator.rpn = true; }
},

class Infix extends Command {
    reg() { return /infix/; }
    calc() { this.calculator.rpn = false; }
},

class NumOperation extends  ValueOperation {
    reg() {
        // return "((?:(?:[-][0-9]+)|(?:[0-9]*))(?:[.,][0-9]*)?(?:[eE]-?[0-9]+)?)"
        return /([0-9]+(?:[.,][0-9]*)?(?:[eE]-?[0-9]+)?)/;
    }
    lexname() { return "num"; }
    lexfunc(lexme, lexer) {
        const oper = new NumOperation(this.calculator);
        oper.value = parseFloat(lexme.replace(",", "."))
        return oper;
    }
},

class PiOperation extends  ValueOperation {
    reg() { return /pi|π/ }
    lexname() { return "pi"; }
    lexfunc(lexme, lexer) {
        const oper = new PiOperation(this.calculator);
        oper.value = Math.PI;
        return oper;
    }
},

class EOperation extends  ValueOperation {
    reg() { return /e/ }
    lexname() { return "e"; }
    lexfunc(lexme, lexer) {
        const oper = new EOperation(this.calculator);
        oper.value = Math.E;
        return oper;
    }
},

class MemOperation extends  ValueOperation {
    reg() { return /[a-z][a-z0-9]*/ }
    lexname() { return "mem"; }
    lexfunc(lexme, lexer) {
        const oper = new MemOperation(this.calculator);
        oper.value = this.getnum(lexme)
        return oper;
    }
},
];

// Add new operation class to index
// if index not defined, insert before MemOperation (that
// matches almost any name)
function addOperation(oper, index) {
    if (index === undefined) index = operations.length-1;
    if (index < 0) return;
    if (index >= operations.length) index = operations.length-1;
    operations.splice(index,0, oper);
}


/*
const operations = [
    LeftBracketOperation,
    RightBracketOperation,
    Plus,
    Minus,
    SignMinus,
    SignPlus,
    Mul,
    Div,
    Pow,
    Sin,
    Cos,
    Sqrt,
    Deg,
    Rad,
    NumOperation,
    PiOperation,
    MemOperation,
];
*/

class Calculator {
    rpnparse(tokens) {
        let output = [];
        let lasttoken = new BracketOperation(this);
        for (const token of tokens) {
            if (lasttoken instanceof SignOperation) {
                const lt = output.pop();
                output.push(token);
                output.push(lt)
            } else output.push(token);
            lasttoken = token;
        }
        if (lasttoken instanceof SignOperation) {
            output.pop();
            output.push(lasttoken.newCalcOperation());
        }
        return output;
    }


    isIn(reglist, cmd, def) {
        let name = cmd.lexname();
        if (!reglist || reglist.length === 0) return def;
        for (let rs of reglist) {
            if (rs === "+" || rs === "*") rs = "\\" + rs;
            const re = new RegExp("^" + rs + "$");
            if (re.test(name)) {
                return true;
            }
        }
        return false;
    }

    constructor(params) {
        this.params = params || {};
        this.params.decimals = this.params.decimals || 13;
        this.rpn = this.params.rpn || false;
        if (this.params.usemem === undefined) this.params.usemem = true;
        this.deg = this.params.deg;
        if (this.deg === undefined) this.deg = true;
        this.lastResult = 0;
        this.operations = [];
        this.mem = {};
        this.row = 0;
        this.stack = [];
        this.lexer = new Lexer();

        for (const opClass of operations) {
            const oper = new opClass(this);
            if (!this.isIn(this.params.allowed, oper, true)) {
                continue;
            }
            if (this.isIn(this.params.illegals, oper, false)) {
                continue;
            }
            this.operations.push(oper);
            this.lexer.addRule(oper.reg(),function (lexme) {
               return oper.lexfunc(lexme, this);  // this is lexer!
            })
        }
        this.lexer.addRule(/\s+/, function () {
             /* skip whitespace */
        });
        this.parser = new Parser();
    }

    toAngle(a) {
        if (!this.deg) return a;
        return a*Math.PI/180;
    }

    fromAngle(a) {
        if (!this.deg) return a;
        return a/(Math.PI/180);
    }

    calcOne(s) {
        try {
            this.stack = [];
            this.lexer.setInput(s);
            let tokens = [], token;
            let lastToken = new BracketOperation(this);
            while (token = this.lexer.lex()) {
                if (!this.rpn) token = token.checkSign(lastToken);
                tokens.push(token);
                lastToken = token;
            }
            let cmds;
            if (this.rpn) cmds = this.rpnparse(tokens);
            else cmds = this.parser.parse(tokens);

            for (const cmd of cmds) {
                cmd.calc();
            }

            let calc = "";
            let extraBefore = "";
            for (const cmd of tokens) {
                calc += cmd.output(extraBefore);
                extraBefore = cmd.extraSpaceAfter();
            }

            let res = "";
            if (this.stack.length > 0) {  // is some result
                res = this.stack.pop();
                this.lastResult = res;
                calc += " = " + res;
                if (this.stack.length > 0) { // is too much left
                    calc += " extra values!"
                    res = undefined;
                }
            }

            return {res: res, calc: calc};
        } catch (e) {
            return {res: undefined, calc: e.message};
        }
    }

    calc(s) {
        let result = [];
        let lines = s.split("\n");
        let i = 0;
        for (const line of lines) {
            i++;
            this.row = i;
            this.mem["r"] = this.lastResult;
            // remove comments # and trim
            let tline = line.replace(/ *#.*/, "").trim();
            if (!tline) continue;
            if (tline === "list") {
                for (const cmd of this.operations) {
                    const name = cmd.name();
                    let ex = cmd.lexname();
                    if (name === ex) ex = ""; else ex = "  " +ex
                    result.push({res: "", calc: name + ex});
                }
                continue;
            }
            let parts = tline.split("===");
            tline = parts[0].trim();
            let expected = null;
            if (parts.length > 1) expected = parts[1].trim();

            let toMem = "";
            parts = [];
            // check if mem is used
            if (this.params.usemem) {
                parts = tline.split("->");
                const mi = tline.indexOf("->");
                if (mi >= 0) {
                    toMem = " " + tline.substring(mi);
                }
                tline = parts[0].trim();
            }
            let r = {res: this.lastResult, calc: ""};
            if (tline)
                r = this.calcOne(tline);
            r.calc = `r${i}: ${r.calc}${toMem}`;
            if (expected !== null) {
                let res = r.res;
                if ( typeof res === 'number') res = (Math.round(res*1000000)/1000000);
                if (""+res === expected) r.calc = ""; // OK, do not display
                else r.calc += " expected " + expected;
            }
            if (r.calc) result.push(r);
            this.mem["r"+i] = r.res;
            this.mem["r"] = this.lastResult;
            for (let i = 1; i < parts.length; i++) {
                const mem = parts[i].trim();
                if (mem) this.mem[mem] = r.res;
            }
        }
        return result;
    }
}

