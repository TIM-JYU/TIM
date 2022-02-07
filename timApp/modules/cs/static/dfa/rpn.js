/*!
* Class for Reverce Polish Notation
*/



// Start non visual part of variables
/*!
 * base Class for any program object
 */
class RPNCommand {
    static check(s, name, cls) {
        let names = name.split(";");
        for (name of names) {
            let re = new RegExp('^' + name + '$', 'gmi')
            let r = re.exec(s);
            if (!r) continue;
            let cmd = new cls();
            cmd.name = name.replace("\\", "");
            return [cmd];
        }
        return undefined;
    }

    constructor() {
        this.createError = undefined;
        this.error = "";
        this.name = "???";
        this.expl = "";
    }

    showText() {
        return this.name;
    }

    run(rpn) {
        rpn.stepnumber++;
    }

    isEnd() {
        return false;
    }

    isLabel(_) { return false; }

    prerun(_rpn) {
        return "";
    }
}


class End extends RPNCommand {
    // Push command
    // syntax: --
    constructor() {
        super();
        this.name = "--";
    }

    isEnd() {
        return true;
    }
}


class Push extends RPNCommand {
    // Push command
    // see: https://regex101.com/r/PuQciP/latest
    // syntax: push 5
    static isMy(s) {
        let re = /^push +(-?[0-9]+)$/gmi;
        let r = re.exec(s);
        if (!r) return undefined;
        return [new Push("PUSH", r[1].trim())];
    }

    constructor(name, value) {
        super();
        this.name = name;
        this.value = parseInt(value);
    }

    showText() {
        return this.name + " " + this.value;
    }

    run(rpn) {
        super.run(rpn);
        rpn.stack.push(this.value);
    }
}


class BinOperator extends RPNCommand {
    calc(a,b) {
        return 0;
    }

    run(rpn) {
       let b = rpn.pop();
       let a = rpn.pop();
       super.run(rpn);
       rpn.stack.push(this.calc(a,b));
    }
}

class Add extends BinOperator {
    static isMy(s) {
        return this.check(s, "ADD;\\+", Add)
    }

    calc(a,b) {
        return a + b;
    }
}

class Sub extends BinOperator {
    static isMy(s) {
        return this.check(s, "SUB;-", Sub)
    }

    calc(a,b) {
        return a - b;
    }
}

class Mul extends BinOperator {
    static isMy(s) {
        return this.check(s, "MUL;\\*", Mul)
    }

    calc(a,b) {
        return a * b;
    }
}

class Pop extends RPNCommand {
    static isMy(s) {
        return this.check(s, "POP", Pop)
    }

    run(rpn) {
        super.run(rpn);
        rpn.pop();
    }
}

class Double extends RPNCommand {
    static isMy(s) {
        return this.check(s, "DOUBLE", Double)
    }

    run(rpn) {
        super.run(rpn);
        let a = rpn.pop();
        rpn.stack.push(a);
        rpn.stack.push(a);
    }
}

class Swap extends RPNCommand {
    static isMy(s) {
        return this.check(s, "SWAP", Swap)
    }

    run(rpn) {
        super.run(rpn);
        let a = rpn.pop();
        let b = rpn.pop();
        rpn.stack.push(a);
        rpn.stack.push(b);
    }
}

class Down extends RPNCommand {
    static isMy(s) {
        return this.check(s, "DOWN", Down)
    }

    run(rpn) {
        super.run(rpn);
        if ( rpn.isEmpty() ) return;
        let a = rpn.stack.shift();
        rpn.stack.push(a);
    }
}

class Up extends RPNCommand {
    static isMy(s) {
        return this.check(s, "UP", Up)
    }

    run(rpn) {
        super.run(rpn);
        let a = rpn.pop();
        rpn.stack.unshift(a);
    }
}

class CommandWithLabel extends RPNCommand {
    static check(s, name, cls) {
        let re = new RegExp('^' + name + ' +([A-Za-z])$', 'gmi')
        let r = re.exec(s);
        if (!r) return undefined;
        let cmd = new cls();
        cmd.name = name;
        cmd.label = r[1].toUpperCase();
        return [cmd];
    }

    showText() {
        return super.showText() + " " + this.label;
    }
}

class Label extends CommandWithLabel {
    static isMy(s) {
        return this.check(s, "LABEL", Label)
    }

    isLabel(label) { return this.label === label; }
}

class JmpCommand extends CommandWithLabel {
    static check(s, name, cls, n, cond) {
        let cmds = super.check(s, name, cls);
        if (!cmds) return cmds;
        cmds[0].cond = cond;
        cmds[0].n = n; // what is second peek
        return cmds;
    }

    find(rpn) {
        for (let cmd of rpn.commands) {
            if (cmd.isLabel(this.label)) {
                rpn.stepnumber = cmd.stepnumber - 1;
                return undefined;
            }
        }
        rpn.addError("Label " + this.label + " not found!")
        super.run(rpn)
        return undefined;
    }

    run(rpn) {
        if (!this.cond) return this.find(rpn);
        let a = rpn.peek(1);
        let b = rpn.peek(this.n);
        if ( this.cond(a,b) ) return this.find(rpn);
        super.run(rpn);
    }
}

class Jump extends JmpCommand {
    static isMy(s) {
        return this.check(s, "JUMP", Jump, 0, undefined);
    }
}

class JZero extends JmpCommand {
    static isMy(s) {
        return this.check(s, "JZERO", JZero, 1,(a,_) => a === 0);
    }
}

class JPos extends JmpCommand {
    static isMy(s) {
        return this.check(s, "JPOS", JPos, 1, (a,_) => a > 0);
    }
}

class JGtr extends JmpCommand {
    static isMy(s) {
        return this.check(s, "JGTR", JGtr, 2,(a,b) => a > b);
    }
}


const knownCommands = [
    Push,
    Add,
    Sub,
    Mul,
    Pop,
    Double,
    Swap,
    Down,
    Up,
    Label,
    Jump,
    JZero,
    JPos,
    JGtr,
];

class RPN {

    addError(err) {
        this.errors += err + "\n";
    }

    /*!
     * Convert string to dfa
     * \fn constructor(s)
     * \param string s RPN program as a string representation
     * \param dict params, syntaxes what syntaxes area allowed
     * \param list knownCmds, syntaxes what syntaxes area allowed
     * \return JSON resulting RPN structure
     */
    constructor(s, params) {
        params = params || {};
        this.params = params;
        let initial = params["initial"] || "";
        this.allowed = params["allowed"] || [];
        this.illegals = params["illegals"] || [];
        this.errorlevel = 3;
        this.stack = [];
        this.commands = [];
        this.errors = "";
        this.createErrors = "";
        this.linenumber = 0; // this is original linenumber in input
        this.minStepnumber = 1000000;
        this.maxStepnumber = 0;
        this.explcount = 0;
        this.values = initial.split(",");
        this.init();
        if (s) this.addCommands(s, knownCommands);
    }

    maxStep() {
        return 10000;
    }

    isIn(reglist, cmd, def, err) {
        if (cmd.isEnd()) return def;
        let name = cmd.name;
        if (!reglist) return def;
        for (let rs of reglist) {
            if (rs === "+" || rs === "*") rs = "\\" + rs;
            const re = new RegExp("^" + rs + "$");
            if (re.test(name)) {
                this.addError(cmd.linenumber + ": " + cmd.name + " " + err);
                return true;
            }
        }
        return false;
    }

    addCreatedCommands(cmds, line, expl) {
        let lnr = this.linenumber;
        for (let cmd of cmds) {
            cmd.linenumber = lnr;
            if (!this.isIn(this.params["allowed"], cmd, true, "not in allowed commands")) {
                continue;
            }
            if (this.isIn(this.params["illegals"], cmd, false, "in illegal commands")) {
                continue;
            }
            this.commands.push(cmd);
            cmd.stepnumber = this.commands.length;
            cmd.expl = expl;
            if (expl) this.explcount++;
            this.minStepnumber = Math.min(cmd.stepnumber, this.minStepnumber);
            this.maxStepnumber = Math.max(cmd.stepnumber, this.maxStepnumber);
        }
    }

    isShowErrors() {
        return this.errorlevel > 1;
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
                let parts = (line+"#").split("#");
                line = parts[0].trim();
                let expl = parts[1].trim();
                let cmds = undefined;
                for (let cls of knownCommands) {
                    cmds = cls.isMy(line);
                    if (cmds) break;
                }
                if (!cmds) {  // this should not happend if there is UnknownCommand last
                    this.addError(`${this.linenumber}: ${line} - unknown or illegal`);
                    continue; // did not match any know types
                }

                this.addCreatedCommands(cmds, line, expl);

                for (let cmd of cmds) {
                    let error = cmd.prerun(this);
                    if (error) this.addError(`${this.linenumber}: ${error}`);
                }
            } catch (e) {
                this.addError(`${this.linenumber}: ${e}`);
            }
        }
        // this.currentPhase.solveLazy();
        this.addCreatedCommands([new End()], "", "");
        if (!this.isShowErrors()) this.createErrors = "";
        else this.createErrors = this.errors;
    }

    init() {
        this.maxStack = 0;
        this.stepnumber = 0;
        this.stack = [];
        this.errors = "";
        for (const s of this.values) {
            let value = s.trim();
            if (value === "") continue;
            if (!/^-?([0-9]+)/.test(value)) this.addError("Wrong number: " + value);
            this.stack.push(parseInt(value));
        }
    }

    isEmpty() {
        if (this.stack.length < 1) {
            this.addError("Stack empty!");
            return true;
        }
        return false;
    }

    pop() {
        if (this.isEmpty()) return 0;
        return this.stack.pop();
    }

    peek(n) {
        if (this.stack.length < n) {
            this.addError("No item " + n + " in stack!");
            return true;
        }
        return this.stack[this.stack.length-n];
    }

    isEnd() {
        let cmd = this.commands[this.stepnumber];
        return cmd.isEnd();
    }

    currentCmd() {
        if (this.commands.length === 0) return undefined;
        return this.commands[this.stepnumber];
    }



    runUntil(n) {
        // Runs the command list from begining until in step n
        // If n not defined, run all commands
        this.init();
        this.maxStack = Math.max(this.stack.length, this.maxStack);
        let nr = 0;
        let lastlinenr = 0;
        if (n === undefined) n = this.maxStep()+1;
        while (nr < n) {
            if (this.stepnumber >= this.commands.length) break;
            let cmd = this.commands[this.stepnumber];
            if (cmd.isEnd()) break;
            let error = cmd.run(this);
            this.maxStack = Math.max(this.stack.length, this.maxStack);
            lastlinenr = cmd.linenumber;
            if (error && this.isShowErrors()) this.addError(`${cmd.linenumber}: ${error}`);
            nr++;
        }
        return nr;
    }

    getErrors() {
        let err = this.createErrors+this.errors;
        return err;
    }

    compare(rpn2, initials) {
        for (let initial of initials) {
            this.values = initial.split(",");
            this.runUntil();
            if (this.getErrors()) return [this.getErrors(), "", "", initial];
            rpn2.values = initial.split(",");
            rpn2.runUntil();
            if (rpn2.getErrors()) return ["2: " + rpn2.getErrors(), "", "", initial];
            let s1 = JSON.stringify(this.stack).replace("[", "").replace("]", "");
            let s2 = JSON.stringify(rpn2.stack).replace("[", "").replace("]", "");
            if (s1 !== s2) return ["!=", s1, s2, initial];
        }
        return ["", "", "", ""];
    }
} // RPN

