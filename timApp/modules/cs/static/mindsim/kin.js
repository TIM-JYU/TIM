////////////////////////////////////////////////////////
var Reagent = (function () {
    function Reagent(label, E_0, dE, N) {
        this.label = label;
        this.E_0 = E_0;
        this.dE = dE;
        this.N = N;
    }
    return Reagent;
})();
////////////////////////////////////////////////////////
var Reaction = (function () {
    function Reaction(reagents, prefactors, E_act, frequency) {
        this.reagents = reagents;
        this.prefactors = prefactors;
        this.E_act = E_act;
        this.frequency = frequency;
    }
    return Reaction;
})();
////////////////////////////////////////////////////////
var ReactionKinetics = (function () {
    ////////////////////////////////////////////////////////
    function ReactionKinetics(reagents, reactions, T, dt) {
        this.reagents = reagents;
        this.reactions = reactions;
        this.T = T;
        this.dt = dt;
        this.time = 0.;
        this.R = 8.3144621; // J / (mol K)
        this.Mersenne = null;
        this.Mersenne = new MersenneTwister();
    }
    ////////////////////////////////////////////////////////
    ReactionKinetics.prototype.random = function () { return this.Mersenne.random(); };
    ////////////////////////////////////////////////////////
    ReactionKinetics.prototype.iterate = function () {
        for (var i = 0; i < this.reactions.length; ++i) {
            var index = Math.floor(this.random() * this.reactions.length);
            this.react(this.reactions[index]);
        }
    };
    ////////////////////////////////////////////////////////
    ReactionKinetics.prototype.react = function (reaction) {
        var E0_sum = 0.0;
        var E_sum = 0.0;
        var Ekin = [];
        for (var i = 0; i < reaction.reagents.length; ++i) {
            Ekin.push(-this.R * this.T * Math.log(this.random()));
            // reactant
            if (reaction.prefactors[i] < 0) {
                E0_sum += reaction.reagents[i].E_0;
                E_sum += reaction.reagents[i].E_0 + Ekin[i];
            }
        }
        // these can react
        if (E_sum > reaction.E_act + E0_sum) {
            // how many reacts
            var dN = 1;
            for (var i = 0; i < reaction.reagents.length; ++i) {
                // product of reactants because number of singles or pairs or triples...
                if (reaction.prefactors[i] < 0) {
                    dN *= reaction.reagents[i].N; // total number
                    dN *= reaction.reagents[i].dE; // energy density (hmmm... originally 1/dE, but nope)
                    dN *= Math.exp(-Ekin[i] / (this.R * this.T)); // population
                }
            }
            // reaction frequency
            dN *= reaction.frequency;
            // time step
            dN *= this.dt;
            // change populations
            for (var i = 0; i < reaction.reagents.length; ++i) {
                reaction.reagents[i].N += reaction.prefactors[i] * dN;
            }
        }
        this.time += this.dt;
    };
    ReactionKinetics.prototype.print = function () {
        var str = "" + this.time;
        for (var i = 0; i < this.reagents.length; ++i)
            str += " " + this.reagents[i].N;
        console.log(str);
    };
    return ReactionKinetics;
})();
