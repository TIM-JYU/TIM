class CalculatorOp {
    constructor(calculator) {
        this.calculator = calculator;
    }
}

// See https://regex101.com/r/e5iqja/latest
const num = "((?:(?:[-+][0-9]+)|(?:[0-9]*))(?:[.,][0-9]*)?(?:[eE]-?[0-9]+)?|-?pi|-?π)";

function getnum(s) {
    s = ""+s;
    if (s.toUpperCase().startsWith("E")) s = "1"+s;
    if (s === "pi" || s === "π") return Math.PI;
    return parseFloat(s.replace(",", "."));
}

class Command extends  CalculatorOp {
    op() {
        return "";
    }

    doCalc() {
        return undefined;
    }

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
    op() {
        return "";
    }

    doCalc(a) {
        return 0;
    }

    calc(s) {
        let re = new RegExp("^ *("+this.op()+") *\\(? *" + num + "?\\)?$", "i")
        let r = re.exec(s);
        if (!r) return undefined;
        let o = r[1];
        let a = r[2];
        if (a==="" || a === undefined) a = this.calculator.lastResult;
        let res = this.doCalc(getnum(a));
        return {res: res, calc: `${o} ${a} = ${res}`};
    }
}

class BinOperation extends  CalculatorOp {
    op() {
        return "";
    }

    doCalc(a,b) {

    }

    calc(s) {
        let re = new RegExp("^ *" + num + "? *("+this.op()+") *" + num + "$", "i")
        let r = re.exec(s);
        if (!r) return undefined;
        let a = r[1];
        let o = r[2];
        let b = r[3];
        if (a==="" || a === undefined) a = this.calculator.lastResult;
        let res = this.doCalc(getnum(a),getnum(b));
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
    doCalc(a, b) { return a + b; }
}

class Minus extends BinOperation {
    op() { return "-"; }
    doCalc(a, b) { return a - b; }
}

class Mul extends BinOperation {
    op() { return "\\*"; }
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


class Calculator {
    constructor(params) {
        this.params = params;
        this.deg = true;
        this.lastResult = 0;
        this.operations = [];
        this.operations.push(new Plus(this));
        this.operations.push(new Minus(this));
        this.operations.push(new Mul(this));
        this.operations.push(new Div(this));
        this.operations.push(new Sin(this));
        this.operations.push(new Cos(this));
        this.operations.push(new Sqrt(this));
        this.operations.push(new Deg(this));
        this.operations.push(new Rad(this));
    }

    toAngle(a) {
        if (!this.deg) return a;
        return a*Math.PI/180;
    }

    calcOne(s) {
        for (const op of this.operations) {
            let r = op.calc(s);
            if (r) {
                this.lastResult = r.res;
                return r;
            }
        }
        return {res: "Error!", calc: "Error: " + s};
    }

    calc(s) {
        let result = [];
        let lines = s.split("\n");
        for (const line of lines) {
            let tline = line.replace(/ *#.*/, "").trim();
            if (!tline) continue;
            let r = this.calcOne(tline);
            result.push(r);
        }
        return result;
    }
}

