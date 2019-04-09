class Tools {


    constructor(data) {
        const s = JSON.stringify(data[0]);
        const usersAndFields = JSON.parse(s);
        this.data = usersAndFields;
        this.final_result = {};
    }

    sayHi() {
        return "hi";
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
        //let result = {'user': this.data.user};
        //result[fieldName] = content;
        this.final_result[fieldName] = content;
       // return result;
        //{'user': this.data.user, 'd1': content};
    }

    getResult() {
        return {'user': this.data.user.id, fields:  this.final_result};
    }
}

module.exports = Tools;
