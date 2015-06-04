function Laskuri(kohde, arvo) {  // "Muodostaja"
    this.kohde = kohde | "???";
    this.arvo = arvo | 0;
}


Laskuri.prototype.lisaa = function (maara) {
    var m = 1;
    if (maara !== undefined) m = maara;
    this.arvo += m;
};

Laskuri.prototype.toString = function () {
    return this.kohde+': '+this.arvo;
};

var l = new Laskuri('auto');
l.lisaa(); l.lisaa();
console.log(l);

function HienoLaskuri(kohde) {
    this.kohde = kohde;
}

HienoLaskuri.prototype = new Laskuri();
HienoLaskuri.prototype.constructor = HienoLaskuri;

HienoLaskuri.prototype.toString = function () {
    return 'Elegantisti: '+ Laskuri.prototype.toString.call(this);
};

var hl = new HienoLaskuri('ufo',3);
hl.lisaa(); hl.lisaa(4);
console.log(hl);

function foo(e) {
    console.log(foo.maara,e,5);
}

foo.maara = 2;

foo(23);
foo.call(null, 24);

// return { 'auto': l, 'ufo':hl}