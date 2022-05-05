function roundToNearest(num, decimals) {
  decimals = decimals || 0;
  var p = Math.pow(10, decimals);
  var n = (num * p) * (1 + Number.EPSILON);
  return Math.round(n) / p;
}

class CalculatorOp {
    constructor(calculator) {
        this.calculator = calculator;
    }

    op() {
        return "";
    }

    name() {
        return this.op(); // needed if name is different than op
    }

    doCalc() {
        return undefined;
    }

    r(v) {
        return roundToNearest(v, this.calculator.params.decimals);
    }

    getnum(s) {
        if (s === undefined) return this.calculator.lastResult;
        s = (""+s).trim();
        if (s === "") return this.calculator.lastResult;
        if (s === "pi" || s === "π") return Math.PI;
        if (s === "-pi" || s === "-π") return -Math.PI;
        if (s.startsWith("p")) {
            if (s==="p") return this.calculator.lastResult;
            let idx = this.calculator.row - parseInt(s.substring(1));
            idx = Math.max(0, Math.min(idx, this.calculator.row-1));
            return this.calculator.mem["r"+idx] || 0;
        }
        if (s[0].match(/[a-z]/)) {
            let val = this.calculator.mem[s];
            return val || 0;
        }
        return parseFloat(s.replace(",", "."));
    }

}

// See https://regex101.com/r/e5iqja/latest
const num = "((?:(?:[-+][0-9]+)|(?:[0-9]*))(?:[.,][0-9]*)?(?:[eE]-?[0-9]+)?|-?pi|-?π|[a-z][a-z0-9]*)";

class Command extends  CalculatorOp {
    calc(s) {
        let re = new RegExp("^ *("+this.op()+")?$", "i")
        let r = re.exec(s);
        if (!r) return undefined;
        let o = r[1];
        let res = this.doCalc();
        return {res: res, calc: `${o}`};
    }
}

class FuncRROperation extends  CalculatorOp {
    doCalc(a) {
        return 0;
    }

    calc(s) {
        let re = new RegExp("^ *("+this.op()+") *\\(? *" + num + "?\\)?$", "i")
        let r = re.exec(s);
        if (!r) return undefined;
        let o = r[1];
        let a = this.getnum(r[2]);
        let res = this.r(this.doCalc(a));
        return {res: res, calc: `${o} ${a} = ${res}`};
    }
}

class BinOperation extends  CalculatorOp {
    doCalc(a,b) {

    }

    calc(s) {
        let re = new RegExp("^ *" + num + "? *("+this.op()+") *" + num + "$", "i")
        let r = re.exec(s);
        if (!r) return undefined;
        let a = this.getnum(r[1]);
        let o = r[2];
        let b = this.getnum(r[3]);
        if (a==="" || a === undefined) a = this.calculator.lastResult;
        let res = this.r(this.doCalc(a,b));
        return {res: res, calc: `${a} ${o} ${b} = ${res}`};
    }
}

class Deg extends Command {
    op() { return "deg"; }
    doCalc() {
        this.calculator.deg = true;
        return undefined;
    }
}


class Rad extends Command {
    op() { return "rad"; }
    doCalc() {
        this.calculator.deg = false;
        return undefined;
    }
}


class Plus extends BinOperation {
    op() { return "\\+"; }
    name() { return "+"; }
    doCalc(a, b) { return a + b; }
}

class Minus extends BinOperation {
    op() { return "-"; }
    doCalc(a, b) { return a - b; }
}

class Mul extends BinOperation {
    op() { return "\\*"; }
    name() { return "*"; }
    doCalc(a, b) { return a * b; }
}

class Div extends BinOperation {
    op() { return "/"; }
    doCalc(a, b) { return a / b; }
}

class Sin extends FuncRROperation {
    op() { return "sin"; }
    doCalc(a) { return Math.sin(this.calculator.toAngle(a)); }
}

class Cos extends FuncRROperation {
    op() { return "cos"; }
    doCalc(a) { return Math.cos(this.calculator.toAngle(a)); }
}

class Sqrt extends FuncRROperation {
    op() { return "sqrt"; }
    doCalc(a) { return Math.sqrt(a); }
}

class Ln extends FuncRROperation {
    op() { return "ln"; }
    doCalc(a) { return Math.log(a); }
}

class Log10 extends FuncRROperation {
    op() { return "log10"; }
    doCalc(a) { return Math.log10(a); }
}

const operations = [
  Plus,
  Minus,
  Mul,
  Div,
  Sin,
  Cos,
  Sqrt,
  Deg,
  Rad,
  Ln,
  Log10,
];


class Calculator {
    isIn(reglist, cmd, def, err) {
        let name = cmd.name();
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
        this.deg = this.params.deg || true;
        this.lastResult = 0;
        this.operations = [];
        this.mem = {};
        this.row = 0;
        for (const op of operations) {
            const oper = new op(this);
            if (!this.isIn(this.params.allowed, oper, true, "not in allowed commands")) {
                continue;
            }
            if (this.isIn(this.params.illegals, oper, false, "in illegal commands")) {
                continue;
            }
            this.operations.push(oper);
        }
    }

    toAngle(a) {
        if (!this.deg) return a;
        return a*Math.PI/180;
    }

    calcOne(s) {
        for (const op of this.operations) {
            let r = op.calc(s);
            if (r) {
                if (r.res !== undefined) this.lastResult = r.res;
                return r;
            }
        }
        return {res: "Error!", calc: "Error: " + s};
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
            const parts = tline.split("->");
            let toMem = "";
            const mi = tline.indexOf("->");
            if (mi >= 0) {
                toMem = " " + tline.substring(mi);
            }
            tline = parts[0].trim();
            let r = this.calcOne(tline);
            r.calc = `r${i}: ${r.calc}${toMem}`;
            result.push(r);
            this.mem["r"+i] = r.res;
            this.mem["r"] = this.lastResult;
            for (let i=1; i<parts.length; i++) {
                const mem = parts[i].trim();
                if (mem) this.mem[mem] = r.res;
            }
        }
        return result;
    }
}

