class Tools {

    constructor(data, currDoc) {
        this.data = data;
        this.currDoc = currDoc;
        this.result = {};
        this.regex = /^[0-9]+\./; //TODO: onko oikein?
    }

    getStudentName() {
        return this.data.user['real_name'];
    }

    getDouble(fieldName, def=0) {
        if (this.regex.test(fieldName)) {
            var fn = fieldName;
        } else {
            fn = this.currDoc + fieldName;
        }
        let s = this.data.fields[fn];
        let r = parseFloat(s);
        if (isNaN(r)) r = def;
        return r;
    }

    getInt(fieldName, def=0) {
        if (this.regex.test(fieldName)) {
            var fn = fieldName;
        } else {
            fn = this.currDoc + fieldName;
        }
        let s = this.data.fields[fn];
        let r = parseInt(s); // TODO: katkaisee, ei pyöristä, onko OK?
        if (isNaN(r)) r = def;
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

    // TODO: tarpeettoman monimutkainen
    getSum(fieldName, start, end, def=0) {
        if (this.regex.test(fieldName)) {
                var fn = fieldName;
            } else {
                fn = this.currDoc + fieldName;
            }
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
        if (this.regex.test(fieldName)) {
            var tidFN = fieldName;
        } else {
            tidFN = this.currDoc + fieldName;
        }
        this.result[tidFN] = content;
    }

    setDouble(fieldName, content) {
        if (this.regex.test(fieldName)) {
            var tidFN = fieldName;
        } else {
            tidFN = this.currDoc + fieldName;
        }
        this.result[tidFN] = content;
    }

    getResult() {
        return {'user': this.data.user.id, 'fields':  this.result};
    }
}

module.exports = Tools;
