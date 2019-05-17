class Tools {

    constructor(data, currDoc, markup, aliases) {
        this.data = data;
        this.currDoc = currDoc;
        this.markup = markup;
        this.aliases = aliases;
        this.result = {};
        this.regex = /^[0-9]+\./;
        this.printP = "";
        this.error = ""; // TODO: map for errors
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
        let r = parseFloat(s);
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
        let fn = this.normalizeField(fieldName);
        let sum = 0;
        for (let i = start; i <= end; i++) {
            let fnn = fn + i.toString();
            let s = this.data.fields[fnn];
            let r = parseFloat(s);
            if (isNaN(r)) r = def;
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
        // TODO: if (isNaN(r)) error
        this.result[fn] = r;
    }

    setDouble(fieldName, content) {
        let fn = this.normalizeAndSet(fieldName);
        let r = parseFloat(content);
        // TODO: if (isNaN(r)) error
        this.result[fn] = r;
    }

    getDefaultPoints(def = this.markup.defaultPoints) {
        if (!this.markup.defaultPoints) this.error = "Default points have not been set";
        return def;
    }

    getGrade(points) {
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

    //TODO: fix!! also nomalize
    saveGrade(gradeVal, points) {
        let d = this.markup.gradeField || "grade";
        let fn = this.normalizeField(d);
        this.result[fn] = gradeVal;
        if (arguments.length === 2) { // TODO: ?
            let c = this.markup.creditField || "credit";
            let fnc = this.normalizeAndSet(c);
            this.result[fnc] = points;
        }

    }

    defineTime(s) {
        // TODO: fix so that user can give datetime without offset (2019-05-11 12:13:14)
        return Date.parse(s) / 1000;
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
