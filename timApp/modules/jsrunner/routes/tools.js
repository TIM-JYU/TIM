class Tools {

    constructor(data, currDoc, markup) {
        this.data = data;
        this.currDoc = currDoc;
        this.markup = markup;
        this.result = {};
        this.regex = /^[0-9]+\./;
    }

    normalizeField(fieldName) {
        if (this.regex.test(fieldName)) {
            return fieldName;
        } else {
            return this.currDoc + fieldName;
        }
    }

    getStudentName() {
        return this.data.user['real_name'];
    }

    getDouble(fieldName, def=0) {
        let fn = this.normalizeField(fieldName);
        let s = this.data.fields[fn];
        let r = parseFloat(s);
        if (isNaN(r)) r = def;
        return r;
    }

    getInt(fieldName, def=0) {
        let fn = this.normalizeField(fieldName);
        let s = this.data.fields[fn];
        let r = parseInt(s); // TODO: katkaisee, ei pyöristä, onko OK?
        if (isNaN(r)) r = def;
        return r;
    }

    getString(fieldName, def="") {
        let fn = this.normalizeField(fieldName);
        let r = this.data.fields[fn];
        // TODO: default on jo ""
        return r;
    }

    getValue(fieldName, def="") {
        let fn = this.normalizeField(fieldName);
        let r = this.data.fields[fn];
        // TODO: default on jo ""
        return r;
    }

    /*
    getSum() {
        let sum = 0;
        for (let i = 0; i < arguments.length; i++) {
            sum += arguments[i];
        }
        return sum;
    }
    */

    getSum(fieldName, start, end, def=0) {
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
        let fn = this.normalizeField(fieldName);
        this.result[fn] = content;
    }

    setInt(fieldName, content) {
        let fn = this.normalizeField(fieldName);
        let r = parseInt(content);
        // TODO: if (isNaN(r)) error
        this.result[fn] = r;
    }

    setDouble(fieldName, content) {
        let fn = this.normalizeField(fieldName);
        let r = parseFloat(content);
        // TODO: if (isNaN(r)) error
        this.result[fn] = r;
    }

    getDefaultPoints(def=this.markup.defaultPoints) {
        // TODO: error if default not set
        return def;
    }

    getDefaultCredits(def=this.markup.defaultCredits) {
        // TODO: error if default not set
        return def;
    }

    getGrade(points) {
        const scale = this.markup.gradingScale;
        const values = Object.values(scale);
        values.sort(function(a, b){return b-a});
        let grade = "";
        for (let i = 0; i < values.length; i++) {
            if (points >= values[i]) {
                // grade = scale[points[i]];
                grade = Object.keys(scale).find(key => scale[key] === values[i]);
                break;
            }
            grade = this.markup.failGrade;
        }
        console.log(grade);
        return grade;
    }

    saveGrade(grade, def=this.getDefaultPoints(), defa=this.getDefaultCredits()) {
        let fn = this.normalizeField(this.markup.gradeField);
        // TODO: fix
        this.result[fn] = grade;
    }

    // TODO: print

    getResult() {
        return {'user': this.data.user.id, 'fields':  this.result};
    }
}

module.exports = Tools;
