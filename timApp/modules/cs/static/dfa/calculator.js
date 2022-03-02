class CalculatorOp {
    constructor(calculator) {
        this.calculator = calculator;
    }
}

class Command extends  CalculatorOp {
    op() {
        return "";
    }

    doCalc() {
        return undefined;
    }

    calc(s) {
        let num = "(-?[0-9]*(?:\.[0-9])?)";
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
        let num = "(-?[0-9]*(?:\.[0-9])?)";
        let re = new RegExp("^ *("+this.op()+") *\\(? *" + num + "?\\)?$", "i")
        let r = re.exec(s);
        if (!r) return undefined;
        let o = r[1];
        let a = r[2];
        if (a==="" || a === undefined) a = this.calculator.lastResult;
        let res = this.doCalc(parseFloat(a));
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
        let num = "(-?[0-9]*(?:\.[0-9])?)";
        let re = new RegExp("^ *" + num + "? *("+this.op()+") *" + num + "$", "i")
        let r = re.exec(s);
        if (!r) return undefined;
        let a = r[1];
        let o = r[2];
        let b = r[3];
        if (a==="" || a === undefined) a = this.calculator.lastResult;
        let res = this.doCalc(parseFloat(a),parseFloat(b));
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
        return {res: "Error!", calc: s};
    }

    calc(s) {
        let result = [];
        let lines = s.split("\n");
        for (const line of lines) {
            let tline = line.trim();
            if (!tline) continue;
            let r = this.calcOne(tline);
            result.push(r);
        }
        return result;
    }
}

