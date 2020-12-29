    let svgdiv = document.getElementById('svgdiv');
    svgdiv.addEventListener("click", svgclick);
    let buttondiv = document.getElementById('buttondiv');
    let codediv = document.getElementById('codediv');
    let errspan = document.getElementById('error');
    let coordspan = document.getElementById('coord');

    function getElementPos(el) {
        let lx = 0, ly = 0
        for (;
            el != null;
            lx += el.offsetLeft, ly += el.offsetTop, el = el.offsetParent) {
        }
        return {x: lx, y: ly};
    }

    function svgclick(e) {
        let d = getElementPos(svgdiv);
        let x = e.clientX - d.x;
        let y = e.clientY - d.y;
        // errspan.innerText = `${x},${y} ${e.clientX},${e.clientY} ${e.pageX},${e.pageY} ${d.x},${d.y}`;
        coordspan.innerText = `${x},${y}`;
    }

    function drawSVG(svg) {
        svgdiv.innerHTML = svg;
        // errspan.innerText = "";
        // errspan.innerText = error.message;
    }

    function addError(err) {
        errspan.innerText += err;
    }

    function addCode(code) {
        codediv.innerHTML += code;
    }

    function clearError() {
        errspan.innerText = "";
    }

    function encodeHTML(s) {
        if (!s) return "";
        return s.replace(/[\u00A0-\u9999<>&]/gim, function (i) {
            return '&#' + i.charCodeAt(0) + ';';
        });
    }

    let nullRef = undefined;

    // ------------------ Variables BEGIN -----------------------------------
    // Start non visual part of variables
    /*!
     * base Class for any program object, so variable or command
     */
    class PrgObject {
        constructor() {
        }

        run() {
            return "Not poissble to run baseclass";
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
            let v = variables.findVar(variable.name);
            if (v) error = `Nimi ${v.name} on jo käytössä`;
            variables.add(variable);
            return error;
        }

        run(variables) {  // TODO: catch creation errors
            try {
                let v = this.create(variables);
                this.createdVar = v;
                return this.add(variables, v);
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
        constructor(name, value, ref) {
            super();
            this.name = name;
            this.value = value;
            this.refs = [];
            if (ref) this.refs.push(ref);
            this.error = "";  // errors for this variable
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

        addRef(objTo, index2, variables) {
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
            let isRef = this.isRef();  // Was originally ref?
            if (index2 !== undefined) {
                if (objTo.isArray()) {
                    if (index2 < 0 || index2 >= objTo.vars.length) {
                        error += `${index2} on laiton indeksi. `;
                    } else {
                        objTo = objTo.vars[index2];
                    }
                } else {
                    error += `${objTo.name} ei ole taulukko! `;
                }
            }
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

        assign(value, variables) {
            // assigns value to any variable, but if not value variable,
            // gives an error. In static mode only one initialization is allowed.
            let error = "";
            if (this.value !== undefined &&
                variables.isStaticMode() && !this.allowInit) {
                  error += `${this.name} on jo alustettu. Ei saa alustaa uudelleen! `;
            }
            let notnull = true;
            if (this.isRef()) {
                if (value === "null") {
                    error += this.addRef(nullRef, undefined, variables);
                    notnull = false;
                } else {
                    error += `${this.name} on viitemuuttuja. Ei saa sijoittaa arvoa! `;
                }
            }
            if (notnull) {
                this.value = value;
                this.allowInit = false;
            }
            return this.handleError(error, variables);
        }

        init(value) {
            this.value = value;
            this.allowInit = false;
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

        setCount(n, variables) {
            // allowed only for lists
            this.count = n;
            if (this.isList()) return "";
            let error = `${this.name} ei ole lista, joten ei ole count-ominaisuutta`;
            return this.handleCountError(error, variables);
        }

        checkCount(variables) {
            return ""; // error allready given when set
        }

        run() {
            return "Variable is not runnable";
        }
    }

    class NullVariable extends Variable {
        // Null varible for un assigned refs or null ref
        constructor() {
            super("null", undefined, undefined);
        }

        addRef(objTo, index2, variables) {
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
            return error;
        }
    }

    nullRef = new NullVariable();


    class ValueVariable extends Variable {
        constructor(name, value, ref) {
            super(name, value, ref);
        }
    }


    class CreateValueVariable extends CreateVariable {
        // Creates a value variable
        // see: https://regex101.com/r/uaLk9H/latest
        // syntax: V summa <- -3.5
        static isMy(s) {
            let re = /^[Vv](al|alue)? +([.$\w\d]+) *(<-|=|:=|) *([^\n]+|)$/;
            let r = re.exec(s);
            if (!r) return undefined;
            let name = r[2];
            let value = r[4];
            if (!r[3] && value === "") value = undefined;
            return [new CreateVariable(
                () => new ValueVariable(name, value))];
        }
    }


    class RefecenceVariable extends ValueVariable {
        // Creates a reference variable
        constructor(name, value, ref) {
            super(name, value, ref);
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
            let re = /^[Rr](ef|eference)? +([.\w\d]+)$/;
            let r = re.exec(s);
            if (!r) return undefined;
            return [new CreateVariable(
                () => new RefecenceVariable(r[2], undefined, nullRef))];
        }
    }


    class ObjectVariable extends Variable {
        // Creates an object variable
        constructor(name, value) {
            super(name, value);
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
            let re = /^[Nn][ew]{0,2} +(\$[\S]+) +([^\n]*)$/;
            let r = re.exec(s);
            if (!r) return undefined;
            let name = r[1];
            return [new CreateVariable(
                () => new ObjectVariable(name, r[2]), name)];
        }
    }


    class ArrayVariable extends ObjectVariable {
        // Creates array or list for refs or values
        // value ei not used in this case
        // kind is [ for Array and L for List (ArrayList in Java)
        // type is R for referneces and V for values.
        constructor(name, value, kind, type, len) {
            super(name, value);
            this.kind = kind;
            this.type = type;
            if (this.isList()) this.count = 0;
            this.vars = [];
            this.over = [];  // all under index and over index things goes here
            let nref = undefined;
            let init = undefined;
            let cls;
            if (type === "R")
                cls = RefecenceVariable;
            else
                cls = ValueVariable;
            if (this.kind === "[" && type === "R") nref = nullRef;
            if (this.kind === "[" && type === "V") init = "0";
            for (let i = 0; i < len; i++) {
                let v = new cls(`${name}[${i}]`, init, nref);
                this.vars.push(v);
            }
            this.over[0] = new cls(`${name}.under`, undefined);
            this.over[1] = new cls(`${name}.over`, undefined);
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
            for (let i=0; i < this.count; i++) {
                let v = this.vars[i];
                if ( !v.isRef() ) continue;  // non refs does not matter
                if (!v.refs || v.refs.length === 0) { error += alustamattomia; break; }
            }
            // Is all refs in not used part without reference? nullRef is OK
            for (let i=this.count; i<this.vars.length; i++) {
                let v = this.vars[i];
                if ( !v.isRef() ) continue;  // non refs does not matter
                if (v.refs && v.refs.length > 0) {
                    if (v.refs.length > 1) { error += deferr; break; }
                    else if (v.refs[0] !== nullRef) { error += deferr; break; }
                }
            }
            if (error !== "") error = `${this.name}: ` + error;
            return this.handleCountError(error, variables);
        }
    }


    class CreateArrayVariable extends CreateVariable {
        // Creates array or list (compare Java's ArrayList) for refs or values
        // see: https://regex101.com/r/u9L3lj/latest
        // syntax:
        //    Array $1 R 3
        //    List $1 R 3
        //    [] $1 R 3     // inits nullRef
        //    Array $2 V 5
        static isMy(s) {
            let re = /^([Aa](rray)?|[Ll](ist)?|\[]) *(\$[^ ]+) *([\S]+) *([0-9]+)$/;
            let r = re.exec(s);
            if (!r) return undefined;
            let kind = "[";
            if (r[1].toUpperCase().startsWith("L")) kind = "L";
            let name = r[4];
            let type = r[5].toUpperCase();
            let len = r[6];
            if (!"RV".includes(type)) throw `${name} väärä tyyppi taulukolle.  Pitää olla R tai V! `

            return [new CreateVariable(
                () => new ArrayVariable(name, undefined, kind, type, len), name)];
        }
    }


    class InitializedArrayVariable extends ArrayVariable {
        // Creates initilized array or list

        constructor(name, value, kind, type, vals) {
            let valsarr = vals.split(",");
            super(name, value, kind, type, valsarr.length);
            this.valsarr = valsarr;
        }

        initByVals(variables) {
            let error = "";
            let len = this.vars.length;
            let valsarr = this.valsarr;
            if (!valsarr) {
                error += `${this.name} ei ole alustustietoja! `
                return this.handleError(error, variables);
            }
            if (this.isList()) this.count = len;
            if (this.type === "V") {
                for (let i = 0; i < len; i++) {
                    let v = this.vars[i];
                    v.init(valsarr[i].trim());
                }
                return;
            }
            if (this.type === "R") {
                for (let i = 0; i < len; i++) {
                    let v = this.vars[i];
                    let to = valsarr[i].trim();
                    let [name, index2] = Command.nameAndIndex(to);
                    let objTo = variables.findVar(name);
                    if (!objTo) {
                        error += v.handleError(`Oliota ${name} ei löydy! `, variables)
                        continue;
                    }
                    error += v.addRef(objTo, index2, variables);
                }
            }
            return this.handleError(error, variables);
        }
    }


    class CreateInitializedArrayVariable extends CreateArrayVariable {
        // Creates initilized array or list
        // see: https://regex101.com/r/Hre5iQ/latest
        //      https://regex101.com/r/ZepJ1V/latest
        // syntax:
        //    Array $1 V [3]
        //    l $2 v a,b,d,d
        //    []$3 r $1,$2
        //    L $3 r [$1,$2,$[1]]
        static isMy(s) {
            let re = /^([Aa](rray)?|[Ll](ist)?|\[]) *(\$[^ ]+) *([\S]+) *[{[]?([$\d., \w]+)[}\]]?$/;
            let r = re.exec(s);
            if (!r) { // try if format L $3 r [$1,$2,$[1]]
                re = /^([Aa](rray)?|[Ll](ist)?|\[]) *(\$[^ ]+) *([\S]+) *[{]?([$[\]\d., \w]+)[}]?$/;
                r = re.exec(s);
                if (!r) return undefined;
            }
            let kind = "[";
            if (r[1].toUpperCase().startsWith("L")) kind = "L";
            let name = r[4];
            let type = r[5].toUpperCase();
            let vals = r[6];
            if (vals.startsWith("[")) {
                vals = vals.substring(1,vals.length-1);
            }
            if (!"RV".includes(type)) throw `${name} väärä tyyppi taulukolle.  Pitää olla R tai V! `

            return [new CreateInitializedArrayVariable( () =>
                new InitializedArrayVariable(name, undefined, kind, type, vals), name)];
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
            let re = /^([$.\w\d]+) *-> *(\$?(.\d|\w)+)(\[(-?\d+)])?$/;
            let r = re.exec(s);
            if (!r) return undefined;
            return [new ReferenceTo(r[1], r[2], r[5])];
        }

        constructor(from, to, index2) {
            // index2 is for syntax lista -> $2[1]
            super();
            this.from = from;
            this.to = to;
            this.index2 = index2;
        }

        run(variables) {
            let refFrom = variables.findVar(this.from)
            if (!refFrom) return `Viitemuuttujaa ${this.from} ei löydy!`;
            let objTo = variables.findVar(this.to);
            if (!objTo) return `Oliota ${this.to} ei löydy!`;
            return refFrom.addRef(objTo, this.index2, variables);
        }
    }


    const combinedRefCommands = [
        CreateObjectVariable,
        CreateArrayVariable,
        CreateInitializedArrayVariable,
    ];

    class CreateReferenceAndObject extends CreateRefecenceVariable {
        // Set Crete object and set ref variable to refecence object
        // see: https://regex101.com/r/CmSGH9/latest
        // syntax: ref aku -> new $1 Aku
        // syntax: ref aku -> a $1 v 3
        // syntax: ref aku -> a $1 v 1,2,34
        static isMy(s) {
            let re = /^[Rr](ef|eference)? +([$.\w\d]+) *-> (.*)$/;
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
                () => new RefecenceVariable(name, undefined, nullRef));
            refCmd.line = "Ref " + name;

            let cmd = cmds[0];
            let rname = cmd.name;
            cmd.line = r[3];

            let refToCmd = new ReferenceTo(name, rname);
            refToCmd.line = name + " -> " + rname;

            return [refCmd, cmd, refToCmd];
        }
    }

    class AssignTo extends Command {
        // Assign value to predefined variable
        // see: https://regex101.com/r/bPn7zX/latest
        // syntax: a = 5
        static isMy(s) {
            let re = /^([$.\w\d]+) *(<-|=|:=) *(.*)$/;
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

    class ArrayAssignTo extends AssignTo {
        // Assign value to array by index
        // see: https://regex101.com/r/Bwygiw/2/
        // syntax: $3[2] = 5
        static isMy(s) {
            let re = /^(\$?[\w\d]+)\[(-?\d+)] *(=|:=|<-) *([^\n]*)$/;
            let r = re.exec(s);
            if (!r) return undefined;
            return [new ArrayAssignTo(r[1], r[2], r[4])];
        }

        constructor(to, index, value) {
            super(to, value);
            this.index = index;
        }

        run(variables) {
            let error = "";
            let array = variables.findVar(this.to);
            let varTo;
            if (!array) return `Taulukkoa ${this.to} ei löydy!`;
            if (!array.isArray()) {
                error += `${this.to} ei ole taulukko! `;
                varTo = array;
            } else {
                if (this.index < 0) {
                    error += `Indeksi negatiivinen ${this.index}! `;
                    varTo = array.over[0];
                    if (variables.isMarkErrors())
                        varTo.error = error;
                } else if (array.vars.length <= this.index) {
                    error += `Liian iso indeksi ${this.index}! `;
                    varTo = array.over[1];
                    if (variables.isMarkErrors())
                        varTo.error = error;
                } else varTo = array.vars[this.index];
            }
            return error + varTo.assign(this.value, variables);
        }
    }

    class ArrayReferenceTo extends Command {
        // Assigs array reference by index
        // see: https://regex101.com/r/fUD9yw/latest
        // syntax: $1[0] -> $2
        static isMy(s) {
            let re = /^(\$?[\w\d]+)\[(-?\d+)] *-> *(\$?[\w\d]+)(\[(-?\d+)])?$/gm;
            let r = re.exec(s);
            if (!r) return undefined;
            let arrayName = r[1];
            let index = r[2];
            let to = r[3]
            let index2 = r[5]
            return [new ArrayReferenceTo(arrayName, index, to, index2)];
        }

        constructor(arrayName, index, to, index2) {
            super();
            this.arrayName = arrayName;
            this.index = parseInt(index);
            this.to = to;
            this.index2 = index2;  // index in to variable
        }

        run(variables) {
            let error = "";
            let array = variables.findVar(this.arrayName);
            if (!array) return `Taulukkoa ${this.arrayName} ei löydy!`;
            let objTo = variables.findVar(this.to);
            if (!objTo) return `Kohdetta ${this.to} ei löydy!`;
            if (!array.isArray()) {
                const refTo = new ReferenceTo(this.arrayName, this.to, this.index2);
                if (variables.isMarkErrors())
                    error += `${this.arrayName} ei ole taulukko! `
                // even it is not an array, put value to the varibale and mark error.
                return error + refTo.run(variables);
            }
            let refFrom;
            // if wrong index, put value to extra places
            if (this.index < 0) {
                error += `Indeksi negatiivinen ${this.index}!`;
                refFrom = array.over[0];
            } else if (array.vars.length <= this.index) {
                error += `Liian iso indeksi ${this.index}!`;
                refFrom = array.over[1];
            } else refFrom = array.vars[this.index];
            error += refFrom.addRef(objTo, this.index2, variables);
            return refFrom.handleError(error, variables);
        }
    }

    class SetCount extends Command {
        // Set's count for list.  Error for other types
        // see: https://regex101.com/r/sPeXxR/latest
        // syntax:
        //  c.count=4
        //  $4.Count <- 19
        static isMy(s) {
            let re = /^([$._\d\w]+)\.[Cc]ount *(=|:=|<-) *(\d+)$/gm;
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
                options = JSON.parse(options);
            }
            this.options = options;
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
            rel.currentPhase = phase;
            rel.phaseList.push(phase);
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
            let re = /^[Tt](ext)? * (h |f |)? *({[^}]*})?([^\n]*)$/gm;
            let r = re.exec(s);
            if (!r) return undefined;
            return [new AddPhaseText(r[4], r[2], r[3])];
        }

        constructor(text, pos, options) {
            super();
            this.text = text;
            this.pos = 0;
            if (pos) this.pos = (pos.trim() === "f") ? 1 : 0;
            if (options) {
                options = JSON.parse(options);
            }
            this.options = options;
        }

        run(variables) {
            variables.addText(this.text, this.pos, this.options);
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
        CreateValueVariable,
        CreateRefecenceVariable,
        CreateObjectVariable,
        CreateArrayVariable,
        ReferenceTo,
        ArrayReferenceTo,
        SetCount,
        AssignTo,
        ArrayAssignTo,
        CreateInitializedArrayVariable,
        CreateReferenceAndObject,
        AddPhaseText,
        SetPhaseNr,
        CodeCommand,
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
            this.phaseNumber = phaseNumber;
            this.texts = [[], []];
        }

        addText(s, pos, options) {
            this.texts[pos].push({text: s, options: options});
        }

        isMarkErrors() {
            return this.variableRelations.isMarkErrors();
        }

        isShowErrors() {
            return this.variableRelations.isShowErrors();
        }

        add(variable) {
            this.vars.push(variable);
            this.varsmap[variable.name] = variable;
        }

        findVar(name) {
            if (name === "null") return nullRef;
            return this.varsmap[name];
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
            this.createErrors = "";
            this.mode = "static";
            this.errorlevel = 3;
            this.animate = 0;
            if (params) {
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
            return this.commands.length-1;
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
         * \fn addVariables(s, knownCommands)
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
                } catch (e) {
                    this.addError(`${this.linenumber}: ${e}`);
                }
            }
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
                        let error = v.checkCount(phase)
                        if (error && this.isShowErrors()) this.addError(`${error}`);
                    }
                }
            }
            return [lastlinenr, nr];
        }

        isLastPhase(phase) {
            return phase === this.phaseList[this.phaseList.length - 1];
        }
    } // VariableRelations
    // ------------------ Variables END -----------------------------------


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
                p.y -= this.height / 2 + dy;
                let fill = "black";
                if (this.countError) fill = "red";
                return `<text x="${p.x}" y="${p.y}" fill="${fill}" font-family="Helvetica" font-size="12px" text-anchor="middle" alignment-baseline="middle">${this.count}</text>\n`;
            }
            return "";
        },

        toSVG(x, y, w, h, dx, dy, nobox, align) {
            // draw variable as a box, it's name and content
            // if in error state, draw red box also
            this.x = x;
            this.y = y;
            this.width = w;
            this.height = h;
            let size = "";
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
            if (align === 1 && this.width > w) {
                this.x += (this.width - w) / 2;
                x = this.x;
            }
            let svg = "";
            if (!nobox) {
                svg += `<use xlink:href="#rbox${size}" x="${x}" y="${y}" />\n`;
            }
            if (this.error) svg += `<use xlink:href="#ebox" x="${x}" y="${y}" />\n`;
            if (val) svg += `<text x="${x}" y="${y}" fill="#000000" font-family="Helvetica" font-size="12px" text-anchor="middle" alignment-baseline="middle">${val}</text>\n`;
            if (this.name && !this.name.startsWith('$'))
                svg += `<text x="${x - 52}" y="${y}" fill="#000000" font-family="Helvetica" font-size="12px" text-anchor="middle" alignment-baseline="middle">${this.name}</text>\n`;
            svg += this.showCount(dy)
            let left = this.left();
            this.snappoints = [
                {x: left.x, y: left.y},
                {x: this.x + dx / 2, y: left.y - this.height / 2 - dy / 2},
                {x: left.x + this.width + dx / 2, y: left.y - dy / 2},
                {x: this.x, y: left.y + this.height / 2},
            ];
            return svg;
        },

        left() {
            // left coordinate for variable
            return {x: this.x - this.width / 2, y: this.y};
        },

        closest(p) {
            // find closest snapopoint to p
            let dmin = 1000000;
            let result = this.left();
            for (let p2 of this.snappoints) {
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
                    let x2 = this.x;
                    let y2 = this.y + this.height;
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

    svgArrayVariableMixin = {
        toSVG(x, y, w, h, dx, dy) {
            // draw array as variables side by side
            // and indesies over it. If there is over/under indexing
            // draw those before and after array
            this.x = x;
            this.y = y;
            let xv = x;
            let svg = "";
            this.snappoints = [];
            for (let v of this.vars) {
                svg += v.toSVG(xv, y, w, h, dx, dy, false, 1);
                xv += v.width;
                this.snappoints = this.snappoints.concat(v.snappoints);
            }
            for (let i = 0; i < this.vars.length; i++) {
                let v = this.vars[i];
                svg += `<text x="${v.x + dx}" y="${v.y - h / 2 - dy - 4}" fill="#000000" font-family="Helvetica" font-size="8px" text-anchor="middle" alignment-baseline="middle">${i}</text>\n`;
            }
            svg += this.over[0].toSVG(x - w, y, w, h, dx, dy, true);
            svg += this.over[1].toSVG(xv, y, w, h, dx, dy, true);
            this.width = xv - x;
            this.height = h;
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
            // left coordinate for array
            if (!this.vars) return {x: this.x, y: this.y};
            let v = this.vars[0];
            // return {x: v.x - v.width / 2, y: v.y};
            return v.left();
        },
    }


    // Add visual methods to variable and array varaible
    Object.assign(Variable.prototype, svgVariableMixin);
    Object.assign(ArrayVariable.prototype, svgArrayVariableMixin);


    /*!
     * Class for SCG representation of Variables
     */
    class VisualSVGVariableRelations {
        constructor(variableRelations, args) {
            this.variableRelations = variableRelations;
            this.args = args; // not yet used
            this.svg = undefined;
        }

        box(id, w, h, dx, dy) {
            // draw one svg 3D box
            return `
<g id="${id}" transform="translate(${-w / 2} ${-h / 2})">
    <path d="M 0 0 L 0 ${h} L ${w} ${h} L ${w + dx} ${h - dy} L ${w + dx} ${-dy} L ${dx} ${-dy} L 0 0 Z"
       fill="#ffffff" stroke="#000000" stroke-miterlimit="10"  pointer-events="all"/>
    <path d="M ${w} 0 L ${w + dx} ${-dy} L ${w + dx} ${h - dy} L ${w} ${h} Z"
       fill-opacity="0.15" fill="#000000" stroke="none"  pointer-events="all"/>
    <path d="M 0 0 L ${dx} ${-dy} L ${w + dx} ${-dy} L ${w} 0 Z"
       fill-opacity="0.2" fill="#000000" stroke="none"  pointer-events="all"/>
    <path d="M 0 0 L ${w} 0 L ${w + dx} ${-dy} M ${w} 0 L ${w} ${h}"
       fill="none" stroke="#000000" stroke-miterlimit="10"  pointer-events="all"/>
</g>
`;
        }

        ebox(id, w, h, dx, dy, color) {
            // error box: draw red "hat" for box
            return `
<g id="ebox" transform="translate(-16 -11)">
    <path d="M 0 0 L ${dx} ${-dy} L ${w + dx} ${-dy} L ${w} ${0} L ${0} ${0}  Z"
       fill="${color}"  stroke-miterlimit="10"  pointer-events="all"/>
</g>
`;
        }

        lineends() {
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

        drawSVGText(s, options, x, y) {
            // draw one svg text using options
            if (s === undefined) return 0;
            let def = {
                "x": x, "y": y,
                "color": "black",
                "size": 14, font: "Helvetica", "weight": "normal",
                "base": "middle",
                "align": "left",
            };
            let opt = options;
            if (opt) opt = Object.assign({}, def, opt);
            else opt = def;
            this.svg += `<text x="${opt.x}" y="${opt.y}" fill="${opt.color}" font-family="{$opt.font}" font-weight="${opt.weight}" font-size="${opt.size}px" text-anchor="${opt.align}" alignment-baseline="${opt.base}">${s}</text>\n`;
            if (options && options.y) return 0; // no y move if set pos
            return opt.size * 1.2;
        }

        drawSVGTexts(texts, x, y, h) {
            // draw list of text. One item has text and options as JSON
            if (!texts) return y;
            for (let text of texts) {
                let dy = this.drawSVGText(text.text, text.options, x, y - h);
                y += dy;
            }
            return y;
        }

        getPrevNextStepNumbers(stepnr) {
            // returns an exact interval when to show current line
            // This is one of the most difficult and error prone code here!!!
            let vars = this.variableRelations;
            let result = [0, vars.maxStepnumber+1];
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
                    result[1] = Math.min(code.stepnumber-(result[0]>0?1:0) , vars.maxStepnumber);
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
            this.svg = '<defs>' +
                this.box("rbox", w, h, dx, dy) +
                this.box("rbox2", 2 * w, h, dx, dy) +
                this.box("rbox4", 4 * w, h, dx, dy) +
                this.ebox("ebox", w, h, dx, dy, "red") +
                this.lineends() +
                '</defs>\n';

            let width = 500;

            let ystack = 30;
            let xstack = 110;

            let yheap = 30;
            let xheap = 253;

            let linenr = 1;
            codediv.innerHTML = "";
            let [prev, next] = this.getPrevNextStepNumbers(this.stepnumber);
            for (let code of this.variableRelations.allCodeList) {
                let ns = String(linenr).padStart(2, '0');
                let curr = "";
                let text = encodeHTML(code.code);
                if ( text === "CODE: END" || text === "END") {
                    ns = "  "; text = "";
                }
                if (prev <= code.stepnumber && code.stepnumber <= next)
                    curr = "current";
                addCode(`<pre class="step ${curr}">${ns} ${text}</pre>`);
                linenr++;
            }

            for (let phase of this.variableRelations.phaseList) {

                yheap = ystack;

                this.drawSVGText(phase.phaseNumber, {color: "red", size: 16, weight: "bold"}, 0, ystack - h);

                // draw possible header texts
                ystack = this.drawSVGTexts(phase.texts[0], 20, ystack, h);
                yheap = ystack;

                for (let v of phase.vars) { // stack vars (local normal vars)
                    if (v.isObject()) continue;
                    let svg = v.toSVG(xstack, ystack, w, h, dx, dy);
                    ystack += 45;
                    this.svg += svg;
                }

                for (let v of phase.vars) { // heap vars (objects)
                    if (!v.isObject()) continue;
                    let svg = v.toSVG(xheap, yheap, w, h, dx, dy);
                    yheap += 45;  // TODO: add last element height
                    this.svg += svg;
                }

                for (let v of phase.vars) { // reference arrows
                    if (!v.isRef()) continue;
                    let svg = v.refToSVG();
                    this.svg += svg;
                }

                ystack = Math.max(ystack, yheap);

                // draw possible footer texts
                ystack = this.drawSVGTexts(phase.texts[1], 20, ystack + 10, h);

                // Draw a line between phases
                if (!this.variableRelations.isLastPhase(phase) && phase.hasVars()) {
                    let stroke = "#C0C0C0";
                    this.svg += `<line x1="${0}" y1="${ystack}" x2="${width}" y2="${ystack}" stroke="${stroke}" stroke-width="2"  />`;
                    ystack += 2 * h;
                }
            }
            this.svg += `</svg>`;

            let height = Math.max(ystack, yheap);

            // add svg size info
            this.svg = `<svg width="${width}px" height="${height}px"
                         viewBox="-0.5 -0.5 ${width} ${height}" >\n`
                + this.svg;

        }


        /*!
         * Draw all variables.
         * \fn draw()
         */
        draw() {
            drawSVG(this.svg);
            if (this.variableRelations.errors) {
                addError(this.variableRelations.errors);
            }
        }


        update() {
            // clear errors an rerun to the current step
            clearError();
            let [linenr, step] = this.variableRelations.runUntil(this.step);
            this.stepnumber = step;
            this.step = step;
            this.makeSVG();
            this.draw();
        }

        maxStep() {
            return this.variableRelations.maxStep();
        }

        reset() {
            this.stopTimer();
            this.step = 0;
        }

        forward() {
            this.step++;
            return this.step < this.maxStep();
        }

        backward() {
            this.step--;
            return this.step > 0;
        }

        stopTimer() {
            clearInterval(this.timer);
        }

        startAnimate(moveOneStepToDirection, until) {
            // start animation and call moveOneStepToDirection
            // in every intervall.  Stop if condtion until
            // reached.
            let visual = this;
            this.stopTimer();
            this.timer = setInterval(function () {
                if (until()) {
                    visual.stopTimer();
                    return;
                }
                if (!moveOneStepToDirection()) {
                    visual.stopTimer();
                }
                visual.update();
            }, 500)
        }

        animate(n, move, until) {
            clearInterval(this.timer);
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
                const code = cmd2.code.trim();
                if (code === "") return false;
                if (code.startsWith("///")) return true;
                if (code.startsWith("//")) return false;
                return !code.startsWith("#");
            }

            this.startAnimate(move, check)
        }

    } // Visual

    let visual = null;

    function jumpToStart() {
        visual.reset();
        visual.update();
    }

    function stepFwd() {
        visual.stopTimer();
        visual.step++;
        visual.update();
    }

    function stepBack() {
        visual.stopTimer();
        visual.step--;
        visual.update();
    }


    function animateFwd(n) {
        visual.animate(n, () => visual.forward(), () => visual.step >= visual.maxStep());
    }


    function animateBack(n) {
        visual.animate(n, () => visual.backward(), () => visual.step <= 0);
    }


    function jumpToEnd() {
        visual.stopTimer();
        visual.step = visual.maxStep();
        visual.update();
    }


    function setData(data) {
        clearError();
        let variableRelations = new VariableRelations(data.code, data.params, knownCommands);
        visual = new VisualSVGVariableRelations(variableRelations, data.args);
        let step1 = visual.maxStep()+1;
        if (data.params && data.params.animate) {
            buttondiv.classList.remove("hidden");
            step1 = 0;
        }
        let [linenr, step] = variableRelations.runUntil(step1);
        visual.stepnumber = step;
        visual.step = step;
        visual.makeSVG();
        visual.draw();
    }
