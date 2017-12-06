// source Atkins 8th edition
var KineticData = (function () {
    function KineticData(labels, prefactors, orders, rate_constant, C0, dt, t_max, save_dt) {
        if (save_dt === void 0) { save_dt = 1; }
        this.labels = labels;
        this.prefactors = prefactors;
        this.orders = orders;
        this.rate_constant = rate_constant;
        this.C0 = C0;
        this.dt = dt;
        this.t_max = t_max;
        this.save_dt = save_dt;
        this.time = [];
        this.C = [];
        var n = Math.round(this.t_max / this.dt);
        this.time.push(0);
        for (var i = 0; i < this.labels.length; ++i) {
            this.C.push(new Array());
            this.C[i].push(this.C0[i]);
        }
        // start from one because C0 is already there
        for (var j = 1; j < n; ++j) {
            this.time.push(j * dt);
            var rate = this.rate_constant;
            for (var i = 0; i < this.labels.length; ++i)
                rate *= Math.pow(this.C[i][j - 1], this.orders[i]);
            for (var i = 0; i < this.labels.length; ++i)
                this.C[i].push(this.C[i][j - 1] + this.prefactors[i] * rate * dt);
        }
        var save_every = Math.round(this.save_dt / this.dt);
        var tmp = this.time;
        this.time = [];
        for (var j = 1; j < n; j += save_every)
            this.time.push(tmp[j]);
        for (var i = 0; i < this.labels.length; ++i) {
            var tmp = this.C[i];
            this.C[i] = [];
            for (var j = 1; j < n; j += save_every)
                this.C[i].push(tmp[j]);
        }
    }
    return KineticData;
})();
var kinetic_data = [];
kinetic_data.push(new KineticData(['N<sub>2</sub>O<sub>5</sub>', 'NO<sub>2</sub>', 'O<sub>2</sub>'], [-2, 4, 1], [1, 0, 0], 3.38e-5, [3.0e-3, 0.0, 0.0], 10, 12 * 60 * 60, 60 * 60));
kinetic_data.push(new KineticData(['H<sub>2</sub>', 'I<sub>2</sub>', 'HI'], [-1, -1, 2], [1, 1, 0], 2.42e-2, [3.0e-3, 1.0e-3, 0.0], 1, 12 * 60 * 60, 60 * 60));
kinetic_data.push(new KineticData(['sucrose', 'glucose', 'fructose'], [-1, 1, 1], [1, 0, 0], 6.0e-5, [1.1e-2, 0.0, 0.0], 10, 12 * 60 * 60, 60 * 60));
kinetic_data.push(new KineticData(['ethanol', 'enzyme', 'acetaldehyde', 'enzyme'], [-1, 1e-15, 1, -1e-15], [0, 0, 0, 0], 1e-7, [0.65e-2, 1.23e-3, 0.0, 1.23e-3], 10, 12 * 60 * 60, 60 * 60));
kinetic_data.push(new KineticData(['NO<sub>2</sub>', 'NO', 'O<sub>2</sub>'], [-2, 2, 1], [2, 0, 0], 0.54, [4.0e-1, 0.0, 0.0], .01, 15, 1));
kinetic_data.push(new KineticData(['CH<sub>3</sub>Cl', 'CH<sub>3</sub>O<sup>-</sup>', 'CH<sub>3</sub>OCH<sub>3</sub>', 'Cl<sup>-</sup>'], [-1, -1, 1, 1], [1, 1, 0, 0], 2.29e-6, [6.0, 7.0, 0.0, 0], 1000, 30 * 24 * 60 * 60, 2 * 24 * 60 * 60));
/*
var data = kinetic_data[5];
for(var j = 0; j < data.time.length; ++j) {
    var str = '';
    str += data.time[j];
    for(var i = 0; i < data.C.length; ++i)
    str += '\t ' + data.C[i][j];
    console.log(str);
}
*/
