class Tools {

    constructor(data) {
        const s = JSON.stringify(data[0]);
        const usersAndFields = JSON.parse(s);
        this.data = usersAndFields;
        this.final_result = {};
    }

    getDouble(fieldName, def=0) {
        console.log(fieldName);
        var fn = '46.'+fieldName;
        var s = this.data.fields[fn];
        console.log(fn + " => " +s);
        console.log(this.data);
        var r = parseFloat(s);
        if (r === NaN) r = def;
        return r;
    }

    getSum(a, b) {
        return a + b;
    }

    setDouble(fieldName, content) {
        var tidFN = '46.'+fieldName;
        this.final_result[tidFN] = content;
    }

    getResult() {
        return {'user': this.data.user.id, 'fields':  this.final_result};
    }
}

module.exports = Tools;
