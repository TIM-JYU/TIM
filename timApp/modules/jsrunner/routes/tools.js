class Tools {

    constructor(data, currDoc, markup, aliases) {
        this.data = data;
        this.currDoc = currDoc;
        this.markup = markup;
        this.aliases = aliases;
        this.result = {};
        this.regex = /^[0-9]+\./;
        this.printP = "";
        this.error = "";
    }

    normalizeField(fieldName) {
        if (this.regex.test(fieldName)) {
            return fieldName;
        } else {
            return this.currDoc + fieldName;
        }
    }

    normalizeAndGet(fieldName) {
        if (fieldName in this.aliases) {
            return this.data.fields[fieldName];
        }
        let fn = this.normalizeField(fieldName);
        return this.data.fields[fn];
    }

    normalizeAndSet(fieldName) {
        if (fieldName in this.aliases) {
            return this.normalizeField(this.aliases[fieldName]);
        }
        return this.normalizeField(fieldName);
    }

    getStudentName() {
        return this.data.user.real_name;
    }

    getDouble(fieldName, def = 0) {
        let s= this.normalizeAndGet(fieldName);
        if (!s) return def;
        if (typeof s == 'number') return s;
        let sp = s.toString().replace(",", ".");
        let r = parseFloat(sp);
        if (isNaN(r)) r = def;
        return r;
    }

    getInt(fieldName, def = 0) {
        let s= this.normalizeAndGet(fieldName);
        let r = parseInt(s);
        if (isNaN(r)) r = def;
        return r;
    }

    getString(fieldName, def = "") {
        let s= this.normalizeAndGet(fieldName);
        if (s === null) s = def;
        return s.toString();
    }

    getValue(fieldName, def = "") {
        let s= this.normalizeAndGet(fieldName);
        if (s === null) s = def;
        return s;
    }

    getSum(fieldName, start, end, def = 0) {
        if (isNaN(start) || isNaN(end)) throw new Error("from ... to parameters must be numbers in getSum method");
        let fn = this.normalizeField(fieldName);
        let sum = 0;
        let r = 0;
        for (let i = start; i <= end; i++) {
            let fnn = fn + i.toString();
            let s = this.data.fields[fnn];
            if (s) {
                let sp = s.toString().replace(",", ".");
                r = parseFloat(sp);
                if (isNaN(r)) r = def;
            }
            else r = def;
            sum += r;
        }
        return sum;
    }

    setString(fieldName, content) {
        let fn = this.normalizeAndSet(fieldName);
        this.result[fn] = content.toString();
    }

    setInt(fieldName, content) {
        let fn = this.normalizeAndSet(fieldName);
        let r = parseInt(content);
        if (isNaN(r)) throw new Error("Can't parse " + content + " to integer in " + fieldName);
        this.result[fn] = r;
    }

    setDouble(fieldName, content) {
        let fn = this.normalizeAndSet(fieldName);
        let cont = content.toString().replace(",", ".");
        let r = parseFloat(cont);
        if (isNaN(r)) throw new Error("Can't parse " + content + " to number in " + fieldName);
        this.result[fn] = r;
    }

    getDefaultPoints(def = this.markup.defaultPoints) {
        if (!this.markup.defaultPoints) this.error = "Default points have not been set";
        return def;
    }

    getGrade(points) {
        if (isNaN(points)) throw new Error("Points must be number in getGrade method");
        if (!this.markup.gradingScale) this.error = "Grading scale has not been set";
        const scale = this.markup.gradingScale;
        const values = Object.entries(scale);
        values.sort((a, b) => {return b[1] - a[1]});
        let grade = this.markup.failGrade || "";
        for (const [currGrade, requiredPoints] of values) {
            if (points >= requiredPoints) {
                grade = currGrade;
                break;
            }
        }
        return grade;
    }

    saveGrade(gradeVal, points=0) {
        let d = this.markup.gradeField || "grade";
        let fn = this.normalizeAndSet(d);
        this.result[fn] = gradeVal;
        let c = this.markup.creditField || "credit";
        let fnc = this.normalizeAndSet(c);
        if (this.markup.defaultPoints) points = this.markup.defaultPoints;
        if (arguments.length === 2) points = arguments[1];
        this.result[fnc] = points;
    }

    defineTime(s) {
        // TODO: fix timezone to work locally
        let localDateTime = new Date(s);
        let offset = localDateTime.getTimezoneOffset() * 60;
        return (localDateTime.getTime() / 1000) + offset;
    }

    getDateTime(fieldName, def=NaN) {
        let s= this.normalizeAndGet(fieldName);
        let r = parseInt(s);
        if (isNaN(r)) r = def;
        return r;
    }

    print() {
        for (let i = 0; i < arguments.length; i++) {
            let a = arguments[i].toString();
            this.printP += a + " ";

        }
        this.printP += "\n";
    }

    getResult() {
        return {'user': this.data.user.id, 'fields': this.result};
    }

    getPrint() {
        return this.printP;
    }

    getError() {
        return this.error;
    }
}

module.exports = Tools;
