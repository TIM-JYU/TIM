// M-x load-library typescript
// M-x typescript-mode
var VibrationalBoltzmann = (function () {
    ////////////////////////////////////////////////////////
    function VibrationalBoltzmann(init_type, Nvib, Emax, E) {
        this.init_type = init_type;
        this.Nvib = Nvib;
        this.Emax = Emax;
        this.E = E;
        this.vibrators = null;
        this.energy_sums = null;
        this.population_sums = null;
        this.sample_count = 0;
        this.Mersenne = null;
        this.Mersenne = new MersenneTwister();
        if (this.Nvib <= 1)
            this.Nvib = 2;
        if (this.Emax <= 0)
            this.Emax = 1;
        if (this.E <= 0)
            this.E = 1;
        if (this.E / this.Emax >= this.Nvib)
            this.E = this.Emax * this.Nvib;
        this.vibrators = new Array(this.Nvib);
        for (var i = 0; i < this.vibrators.length; ++i)
            this.vibrators[i] = 0;
        // map doesn't work because it skips 'undefined'
        // this.vibrators.map(function(value,index) : number { console.log(index); this.vibrators[index] = 0; return 0; } );
        this.energy_sums = new Array(this.Nvib);
        for (var i = 0; i < this.energy_sums.length; ++i)
            this.energy_sums[i] = 0;
        this.population_sums = new Array(this.Emax + 1);
        for (var i = 0; i < this.population_sums.length; ++i)
            this.population_sums[i] = 0;
        /////
        if (init_type == "random") {
            for (var n = 0; n < this.E;) {
                var random_index = Math.floor(this.vibrators.length * this.random());
                if (this.vibrators[random_index] < this.Emax) {
                    this.vibrators[random_index] += 1;
                    n += 1;
                }
            }
        }
        else if (init_type == "first") {
            var index = 0;
            for (var n = 0; n < this.E;) {
                if (this.vibrators[index] < this.Emax) {
                    this.vibrators[index] += 1;
                    n += 1;
                }
                else
                    index += 1;
                if (index >= this.vibrators.length) {
                    throw new Error("Too much energy! Cannot to distribute to vibrators.");
                    break;
                }
            }
        }
        else {
            throw new Error("Unknown initialization type: " + init_type);
        }
    }
    ////////////////////////////////////////////////////////
    VibrationalBoltzmann.prototype.random = function () { return this.Mersenne.random(); };
    ////////////////////////////////////////////////////////
    VibrationalBoltzmann.prototype.iterate = function () {
        if (this.E <= 0)
            return;
        var success = false;
        while (!success) {
            var random_index = Math.floor(this.vibrators.length * this.random());
            var random_dir = 1 - 2 * Math.floor(2 * this.random()); // +1 or -1
            var neighbor_index = random_index + random_dir;
            //neighbor_index = Math.floor(this.vibrators.length * this.random());
            // if not valid neighbor
            if (neighbor_index < 0 || neighbor_index >= this.vibrators.length)
                continue;
            // if cannot take energy
            //if ( this.vibrators[random_index] >= this.Emax )
            //	continue;
            // if cannot give energy
            if (this.vibrators[neighbor_index] <= 0)
                continue;
            // else exchange energy
            this.vibrators[random_index] += 1;
            this.vibrators[neighbor_index] -= 1;
            success = true;
        }
    };
    ////////////////////////////////////////////////////////    
    VibrationalBoltzmann.prototype.print = function () {
        console.log("" + this.vibrators);
    };
    ////////////////////////////////////////////////////////    
    VibrationalBoltzmann.prototype.reset_sampling = function () {
        this.sample_count = 0;
        for (var i = 0; i < this.energy_sums.length; ++i)
            this.energy_sums[i] = 0;
        for (var i = 0; i < this.population_sums.length; ++i)
            this.population_sums[i] = 0;
    };
    ////////////////////////////////////////////////////////    
    VibrationalBoltzmann.prototype.sample = function () {
        // save sample
        this.sample_count += 1;
        for (var i = 0; i < this.vibrators.length; ++i)
            this.energy_sums[i] += this.vibrators[i];
        for (var i = 0; i < this.vibrators.length; ++i)
            if (this.vibrators[i] < this.population_sums.length)
                this.population_sums[this.vibrators[i]] += 1;
    };
    ////////////////////////////////////////////////////////    
    VibrationalBoltzmann.prototype.population_distribution = function () {
        var dist = new Array(this.population_sums.length);
        for (var i = 0; i < this.population_sums.length; ++i)
            dist[i] = this.population_sums[i] / (this.sample_count + 1e-15);
        return dist;
    };
    ////////////////////////////////////////////////////////    
    VibrationalBoltzmann.prototype.energy_distribution = function () {
        var dist = new Array(this.energy_sums.length);
        for (var i = 0; i < this.energy_sums.length; ++i)
            dist[i] = this.energy_sums[i] / (this.sample_count + 1e-15);
        return dist;
    };
    return VibrationalBoltzmann;
})();
//var vb : VibrationalBoltzmann = new VibrationalBoltzmann("first", 5, 5, 7);
/*
var vb : VibrationalBoltzmann;
vb = new VibrationalBoltzmann("random", 5, 5, 7);
vb.print();
vb = new VibrationalBoltzmann("first", 5, 5, 7);
vb.print();
for(var i: number = 0; i < 1000; ++i)
    vb.iterate();
vb.reset_sampling();
for(var i: number = 0; i < 10000; ++i)
    vb.iterate();
console.log(vb.population_distribution());
console.log(vb.energy_distribution());
*/
