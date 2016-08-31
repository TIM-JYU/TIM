// -*- coding: utf-8; tab-width: 4; indent-tabs-mode: nil; -*-
//
// Tauno - taulukot nohevasti
//
// Leikkikenttä taulukoiden käytön opetteluun.
//
// (c)2014 Jonne Itkonen, Vesa Lappalainen GPLv3-lisenssillä


/**********************************************************************
 TODO-kohdat
 - tee undo
   - ei peruutusta, vaan ohjelma ajetaan alusta uudestaan
   - tee ohjelman ajo
     - tee reset-metodi olioille
     - uusiTila palautukseen tarjolle myös NullTila
       tai VirheTila olio (kuin Maybe)
 - this.muuuttujat arvoiksi Muuttuja-oliot, tai tilalle Muuttujat-olio
 - mobiili-UI
 - yhdistä askel ja lause
 - jos vetää solun itseensä tai muuttujan itseensä,
   ei generoidu koodia (=>cancel)
 - if end
 - indeksi ylittyessä lopettaa olemasta indeksi
 - vakionumerot -1 ja n, joka kysyy numeron arvon (raahattavia)
 - scratchin tuplaklikkaus (tai vain klikkaus) sotkee softan toiminnan
 - ohjelman lausekkeet ohjelma-olioon
 - generoituvat suorituslauseet ei saa näkyä ensin
 - if on tila, joka aktivoituu ehtolauseesta,
   jolloin seuraavat tekemiset menee iffin sisään (then osio),
   kunnes käyttäjä lopettaa iffin (ok-nappi tai jotain tilan
   osoittimen luona).
 - 'if' tilalle 'koska'
 - silmukat muotoon 'toista kohdasta (pc) kunnes (ehto)'
   -> gotomaisuus
 - jos muuttujan nimessä on alussa 'int', niin urvelletaan
 - loput 
 - samanakaltaisten lauseiden tunnistus (versiossa 98)
 **********************************************************************/

// Globaalit sanoat, jotka käännetään
var wordMuuttujat = "muuttujat:";
var wordUusiMuuttuja = "uusi muuttuja";
var wordMuuttujatTitle = "muuttujalista, lisää muuttuja yllä olevasta painikkeesta";
var wordOhjelma = "ohjelma";
var wordApua = 'Parametrit:\n' +
              '  help=help in english\n'+
              '  apua=apua suomeksi\n'+
              '  lang=en - käyttöliittymä englanniksi\n'+
              '  s    - simple, ei indeksejä\n'+
              '  mx=v - Luo muuttuja x arvolla v.\n'+
              '  ix=v - Luo indeksimuuttuja x arvolla v.\n'+
              '  ts=n - Aseta taulukon kooksi n.\n'+
              '  t=[x,y,...] - Luo taulukko alkioilla x,y,...\n';
 

/* globaali huutelutaulu */
var echo = null;

// TODO ilkeä, ilkeä globaali virtuaalikone
var vk = null;

// globaali muuttuja debuggauksen ajonaikaiseen sallimiseen/estämiseen
var debis = false;

// Palauttaa ajan merkkijonona, käytetään id:iden luomisessa.
function timestring() { return (new Date()).getTime().toString(16); }

function haeOlio(olioTaiId) { // TODO nimeksi haeElementti???
    /* Hakee olion id:n perusteella,
     * tai jos annettu jo olio, palauttaa sen. */
    var olio = null;
    if (typeof olioTaiId === 'string')
        olio = document.getElementById(olioTaiId);
    else if (typeof olioTaiId === 'object') {
        olio = olioTaiId;
    }
    return olio;
}

function luoDiv(id, attrs) {
    var d = document.createElement('div');
    d.id = id;
    for (var attr in attrs) {
        d[attr]=attrs[attr];
    }
    if (attrs.innerHTML) {
        var s = attrs.innerHTML;
        s = s.replace("<br.*", "");
        vk._tekstina.push(s);
    }
    return d;
}

function parametrit(str) {
    var kaikki = str || window.location.search.substring(1);
    kaikki = kaikki.split('&');
    var tulos = {};

    for (var i=0; i<kaikki.length; ++i) {
        var nv = kaikki[i].split('=');

        if (nv.length==1) {
            tulos[nv[0]]=true;
        } else if (nv.length==2) {
            tulos[nv[0]]=decodeURIComponent(nv[1].replace(/\+/g,' '));
        } else
            continue;
    }

    if (typeof tulos.el === 'undefined') tulos.el = 'r';

    return tulos;
}

// namespace source:
//      http://elegantcode.com/2011/01/26/basic-javascript-part-8-namespaces/
function namespace(namespaceString) {
    var parts = namespaceString.split('.'),
        parent = window,
        currentPart = '';

    for(var i = 0, length = parts.length; i < length; i++) {
        currentPart = parts[i];
        parent[currentPart] = parent[currentPart] || {};
        parent = parent[currentPart];
    }

    return parent;
}


// http://stackoverflow.com/questions/5778020/
//       check-whether-an-input-string-contains-number
function isNumeric(n) {
    return !isNaN(parseInt(n)) && isFinite(n);
}

function isNotNumeric(n) {
    return !isNumeric(n);
}

var Operaatio = {
    mistä: null,
    mihin: null,
    data: {},
    odottaaLähdettä: function () {
        return this.mistä === null;
    },
    asetaLähde: function (l) {
        this.mistä = l;
        l.classList.add('valittu');
        if (this.mihin) {
            this.mihin.classList.remove('valittu');
            this.mihin = null;
        }
    },
    asetaKohde: function (k) {
        this.mihin = k;
        k.classList.add('valittu');
    },
    lähde: function () { return this.mistä; },
    kohde: function () { return this.mihin; },
    tehty: function () {
        if (this.mistä) {
            this.mistä.classList.remove('valittu');
            this.mistä.style.background = "";
            this.mistä = null;
        }
        if (this.mihin) {
            this.mihin.classList.remove('valittu');
            this.mihin.style.background = "";
            this.mihin = null;
        }
    },
    peruuta: function () {
        Operaatio.tehty();
    },
    asetaData: function (d) {
        if (this.odottaaLähdettä()) {
            this.data = d;
        } else {
            for (var k in d) this.data[k] = d[k];
        }
        return this.data;
    },
    haeData: function () {
        if (this.odottaaLähdettä()) return undefined;
        return this.data;
    }
};

function copyViesti(msg) {
    return new Viesti(
        msg.kuka,
        msg.mitä,
        msg.mistä,
        msg.mihin);
}

function Viesti(kuka, mitä, mistä, mihin) {
    this.kuka = kuka;
    this.mitä = mitä;
    this.mistä = mistä;
    this.mihin = mihin;
}

function stringTaiArvo(x) {
    return x;
    if (typeof x !== 'object')
        x = String(x);
    else
        x = String(x.arvo());
    return x;
}

Viesti.prototype.toString = function() {
    var mistä = stringTaiArvo(this.mistä);
    var mihin = stringTaiArvo(this.mihin);
    return '<'+this.kuka.constructor.name+' '+this.mitä+': '+mistä+' ~> '+mihin+'>';
};

function Virtuaalikone() {
    this.pc = 0; // TODO täällä vai Ohjelmassa?

    this._alkutila = new Tila([], {});
    this._tila = this._alkutila.klooni();
    this._ohjelma = [];
    this._tekstina = [];
}

Virtuaalikone.prototype.tila = function (uusiTila) {
    if (typeof uusiTila !== 'undefined') {
        this._tila = uusiTila;
    }
    return this._tila;
};

Virtuaalikone.prototype.ohjelma = function () {
    return this._ohjelma[this._ohjelma.length-1];
};

Virtuaalikone.prototype.ohjelmaTekstinä = function () {
    return this.ohjelma().tekstinä();
};

// Julkinen rajapinta TIMille
function getUserCodeFromTauno () {
    return vk.ohjelmaTekstinä();
}

Virtuaalikone.prototype.lisääOhjelma = function (ohj) {
    this._ohjelma.push(ohj);
    return ohj;
};

Virtuaalikone.prototype.poistaOhjelma = function () {
    return this._ohjelma.pop();
};

Virtuaalikone.prototype.kysyMuuttuja = function(diviin, arvolla) {
    var nimi = prompt("Anna muuttujan nimi:",'');
    if (nimi===null || nimi==='') return null;

    nimi = nimi.trim();

    var validiNimi = false;

    do {
        if (this._tila.löytyyMuuttuja(nimi)) {
            nimi = prompt("Muuttuja "+nimi+" on jo olemassa, anna uusi nimi:",
                          '');
            if (nimi===null || nimi==='') return null;
            nimi = nimi.trim();
            validiNimi = false;
        } else  if (nimi != nimi.match('[a-zA-Z_åäöÅÄÖ][a-zA-Z_åäöÅÄÖ0-9]*')) {
            nimi = prompt("Muuttujan nimi saa sisältää vain kirjaimia "+
                          " ja numeroita, ensin kirjain.  Anna uusi nimi:",'');
            if (nimi===null || nimi==='') return null;
            nimi = nimi.trim();
            validiNimi = false;
        } else
            validiNimi = true;

    } while (!validiNimi);

    var arvo = null;
    if (typeof arvolla === 'undefined') {
        do {
            arvo = prompt("Anna muuttujan arvo (vain luvut, tai tyhjä, "+
                          "kelpaavat arvoksi):",'');
            if (arvo === null || arvo === '') {
                arvo = null;
            } else {
                arvo = new Luku(parseInt(arvo));
            }
        } while (arvo!==null && isNotNumeric(arvo));
    } else {
        arvo = arvolla;
    }
    return this.lisääMuuttuja(nimi, arvo, diviin);
};


function alkaaNumerolla(nimi) {
    var patt = /^[-0-9].*/g;
    var numero = patt.test(nimi);
    return numero;
}

Virtuaalikone.prototype.lisääMuuttuja =
    function(nimi, arvo, diviin, lisääVaikkaNumero) {
        if (diviin == null) {
            diviin = haeOlio('muuttujat');
        }
        var vakio = alkaaNumerolla(nimi);
        if (vakio) {
            var a = parseInt(nimi);
            arvo = a;
        }

        var muuttuja = this.ohjelma().uusiMuuttuja(nimi, arvo, vakio, diviin);

        // XXX XXX ??? if ( !vakio || lisääVaikkaNumero ) muuttuja.liitä(diviin);
        return muuttuja;
    };


Virtuaalikone.prototype.teeMuuttuja = function(diviin, arvolla) {
    var nimi = document.getElementById("uusi-nimi").value;
    var arvo = document.getElementById("uusi-arvo").value;
    if (this._tila.löytyyMuuttuja(nimi)) return null;
    if (arvo === null || arvo === '') { arvo = null;  }
    else { arvo = parseInt(arvo); } // TODO muuttuu Luvuksi lisääMuuttujassa?
    // TODO ei toimi "new Luku(arvo);" koska seuraava rivi }
    if (arvo !== null && isNotNumeric(arvo)) return null;
    if (nimi ) nimi = nimi.trim();
    if (nimi && !nimi.match(/^[a-zöäåA-ZÖÄÅ_][a-zöäåA-ZÖÄÅ_0-9]*$/))
        return null;
    this.piilotaMuuttujanLisäys();
    if ((nimi === null || nimi === '')) {
        if (arvo == null) return null;
        nimi = arvo.toString(); // Joo, hitaampi, mutta selittää paremmin
    }
    return this.lisääMuuttuja(nimi, arvo, diviin);
};

Virtuaalikone.prototype.piilotaMuuttujanLisäys = function(diviin, arvolla) {
    document.getElementById("uuden-muuttujan-alue").style.visibility="collapse";
    document.getElementById("muuttuja-button-alue").style.visibility="visible";
};

// event.type must be keypress
Virtuaalikone.prototype.getChar = function(event) {
    if (event.which === null) {
        return String.fromCharCode(event.keyCode); // IE
    } else if (event.which!==0 && event.charCode!==0) {
        return String.fromCharCode(event.which);   // the rest
    } else {
        return null; // special key
    }
};


Virtuaalikone.prototype.luoMuuttujaKentistä = function(diviin, arvolla) {
    document.getElementById("uuden-muuttujan-alue").style.visibility="visible";
    document.getElementById("muuttuja-button-alue").style.visibility="collapse";
    var uusiNimi =  document.getElementById('uusi-nimi');
    uusiNimi.focus();
    uusiNimi.value = "";
};

Virtuaalikone.prototype.uusiNimiKeypress = function(event,diviin) {
    var ch = this.getChar(event || window.event);
    if (ch.match(/[a-zöäåA-ZÖÄÅ_]/)) return true;
    var uusiNimi =  document.getElementById('uusi-nimi').value;
    if (uusiNimi.length >= 1 && ch.match(/[0-9]/)) return true;
    if (" " < ch) return false;
    var uusiArvo =  document.getElementById('uusi-arvo');
    uusiArvo.focus();
    return false;
};

Virtuaalikone.prototype.uusiArvoKeypress = function(event,diviin) {
    var ch = this.getChar(event || window.event);
    if (ch == "-" || "0" <= ch && ch <= "9") return true;
    if (" " <  ch) return false;
    this.teeMuuttuja(diviin);
    return false;
};


Virtuaalikone.prototype.muuttuja = function(nimi) {
    return haeOlio('muuttuja-'+nimi).olio;
};

Virtuaalikone.prototype.solu = function(indeksi) {
    console.log('dbg.solu: '+ typeof indeksi );
    var s =  haeOlio('solu-'+indeksi).olio; // TODO pois .olio?
    if (!s) return null;
    return s; // TODO lisää .olio?
};

Virtuaalikone.prototype.alkuun = function () {
    this._tila = this._alkutila.klooni();
    return this._tila;
};

Virtuaalikone.prototype.päivitäAlkutila = function () {
    this._alkutila = this._tila.klooni();
    return this._tila;
};

Virtuaalikone.prototype.luoTaulukko = function(parametrit) {
    if (debis) debugger; // XXX
    this.simple = false;
    if (parametrit.s) this.simple = true;
    if (parametrit.t) {
        var alkiot = parametrit.t.split(',');
        alkiot = alkiot.map(function (i) { return parseInt(i); }); // TODO Luku?
        for (var i=0; i<alkiot.length; ++i) {
            this._tila.asetaIndeksiin(i, alkiot[i]);
        }
    } else {
        var alkioita = 6;
        if ( parametrit.ts ) alkioita = parseInt(parametrit.ts) || 0;
        for (var j = 0; j < alkioita; ++j)
            this._tila.asetaIndeksiin(j, Math.floor((Math.random() * 100)));
    }
    this.taulukko = new Taulukko(this); // XXX tää on niiiiin väärin...
    return this.taulukko;
};

Virtuaalikone.prototype.luoIndeksi = function (nimi,inro) {
    var muuttuja = this.lisääMuuttuja(nimi, inro);
    if (!inro) inro = muuttuja.diviArvo.arvo;


    var solu = this.solu(inro);
    if (!solu) return;

    var io = new Indeksi(this, solu);
    io.indeksoi(muuttuja);
    io.div.innerHTML = muuttuja.nimi();

    solu.divSi.appendChild(io.div);
    solu.indeksimuuttujat.push(io);
    io.siirry();
}

Virtuaalikone.prototype.luoMuuttujat = function (parametrit) {
    "use strict";
    if (debis) debugger; // XXX
    var mu = document.getElementById('muuttujat');
    mu.innerHTML = '';

    for (var pn in parametrit) {
        if (pn[0]=='m') {
            var val = new Luku(parametrit[pn]);
            // if ( isNaN(val) ) val = null;
            this.lisääMuuttuja(pn.substring(1), val ,null, true);

            //this.lisääMuuttuja(pn.substring(1), new Luku(parametrit[pn]));
            //parseInt(parametrit[pn])); // TODO Luku?
        } else if (pn[0]=='i') {
            var inro = parseInt(parametrit[pn]); // TODO Luku? Ei, vaan alle.
            var muuttuja = this.lisääMuuttuja(pn.substring(1), new Luku(inro));

            var solu = this.solu(inro);

            var io = this.uusiIndeksi(solu, muuttuja);
        }
    }
    // HUOMIO! Yllä luodut muuttujat voidaan poistaa undolla.
    // Jos tämä ei sovi, korjaa ominaisuus koodista.

    if (vk.simple) {
        var n = vk.tila().taulukko.length;
        for (var i = 0; i < n; i++)
            this.luoIndeksi("" + i, i);
    }
};

Virtuaalikone.prototype.uusiIndeksi = function (solu, muuttuja) {
    var io = new Indeksi(this, solu);
    io.indeksoi(muuttuja);
    io.div.innerHTML = muuttuja.nimi();
    // XXX väärä paikkako, ei, koska nyt Indeksissä ei ole UI-koodia => ongelma silti

    solu.divSi.appendChild(io.div);  // XXX väärä paikka???
    solu.indeksimuuttujat.push(io);
    io.siirry(); // XXX TODO tää tekee jo edelliset 2 riviä

    return io;
};

Virtuaalikone.prototype.poistaMuuttuja = function (muuttujaNimi) {
    this._tila.poistaMuuttuja(muuttujaNimi);
};

function Taulukko(vk) {
    if (vk.tila().taulukko.length < 1) return;
    this.divi = luoDiv('taulukko', {
        className: 'taulukko',
        title: 'Taulukollinen numeroita työstettäväksi, raahailepa niitä.',
        innerHTML: ''});
    this.vm = vk;
    this.divi.tila = this.vm.tila();
    this.vm.tila().taulukko.olio = this;

    var taululause = 'int[] t = {';

    for (var i=0; i<this.vm.tila().taulukko.length; ++i) {
        var v = this.vm.tila().taulukko[i];
        taululause += v+', ';
        var nos = new Solu(this.vm, i, v, true);
        this.divi.appendChild(nos.div);
    }
    // Todo Kaaaaamea kludge alla, ton pitäs olla oma lause

    taululause = taululause.substring(0,taululause.length-2)+'};';
    var tldiv = luoDiv('taulukon_alustus',
                        {className : 'lauseke',
                         title: taululause,
                         innerHTML : taululause});

    haeOlio('ohjelma').appendChild(tldiv);
    haeOlio('taulunaytto').appendChild(this.divi);
}


Taulukko.prototype.reset = function () {
    // jokaiselle alidiville (indeksille)
    var lkm = this.divi.childNodes.length;
    for (var i = 0; i<lkm; ++i) {
        this.divi.childNodes[i].olio.reset();
    }
};


// Taulukko.prototype.toString = function() {
//     var ts = this.taulukko.join(', ');
//     return "t=["+ts+"]";
// };


function Tila(taulukko, muuttujat) {
        this.taulukko = taulukko;
        this.muuttujat = muuttujat;
}

Tila.prototype.klooni = function() {
    // Tämän pitäisi olla tarpeeksi syvä kopio,
    // sillä yksittäiset alkiot ovat (tai pitäs olla)
    // muuttumattomia.
    var uusiTaulukko = this.taulukko.slice(0);
    uusiTaulukko.olio = this.taulukko.olio;

    var uudetMuuttujat = this.muuttujat.constructor();
    for (var attr in this.muuttujat)
        if (this.muuttujat.hasOwnProperty(attr))
            uudetMuuttujat[attr] = this.muuttujat[attr];
    return new Tila(uusiTaulukko, uudetMuuttujat);
};

Tila.prototype.asetaIndeksiin = function(i, v) {
    this.taulukko[i] = v;
};

Tila.prototype.asetaMuuttuja = function(n, v) {
    this.muuttujat[n] = v;
};

Tila.prototype.muuttujassa = function(n) {
    return this.muuttujat[n];
};

Tila.prototype.indeksissa = function(i) {
    return this.taulukko[i];
};

Tila.prototype.muuttujaan = function(i, k) {
    this.muuttujat[k] = this.taulukko[i];
};

Tila.prototype.muuttujasta = function(k, i) {
    this.taulukko[i] = this.muuttujat[k];
};

// muuttujasta k taulukkoon muuttujan ki arvon mukaiseen indeksiin
Tila.prototype.indeksiin = function(k, ki) {
    if (debis) debugger;
    this.taulukko[this.muuttujat[ki]] = this.muuttujat[k];
};

// taulukosta muuttujan ki arvon mukaisesta indeksistä muuttujaan k
Tila.prototype.indeksista = function(ki, k) {
    if (debis) debugger;
    this.muuttujat[k] = this.taulukko[this.muuttujat[ki]];
};

Tila.prototype.löytyyMuuttuja = function(nimi) {
    return this.muuttujat.hasOwnProperty(nimi);
};

Tila.prototype.poistaMuuttuja = function(nimi) {
    delete this.muuttujat[nimi];
};

Tila.prototype.toString = function() {
    var m = [];
    for (var k in this.muuttujat) m.push(k);
    var ts = this.taulukko.join(', ');
    return "["+m.join(', ')+"; "+ts+"]";
};


function siivoaHTML(s) {
    //s = s.replace(/<div.*?<\/div>/, "");
    r = "";
    while (true) {
        s = s.replace(/<div.*?>/, "");
        var i = s.indexOf("<br");
        if (i < 0) break;
        r += s.substring(0, i)+"\n";
        s = s.replace(/.*?<\/div>/, "");
    }
    return r.trim();
}

function Ohjelma(vk) {
    this.askeleet = [];
    this.vk = vk;
    this.pc = 0;
}

Ohjelma.prototype.divinä = function (divid, parentdiv) {
    this.divid = divid;
    this.divi = luoDiv(divid, {className: 'ohjelma', title: wordOhjelma});
    this.divi.olio = this;
    if (typeof parentdiv === 'undefined') this.liitä('oikea');

    return this.divi;
};

Ohjelma.prototype.tekstinä = function (tila) {
    var s = this.divi.textContent;
    if (!s) s = siivoaHTML(this.divi.innerHTML);
    if (s) {
        var i = s.indexOf("int[] t");
        if ( i == 0 ) {
            var i = s.indexOf(";");
            s = s.substring(i + 1);
        }
        if (s.indexOf("\n") < 0) s = s.replace(/;/g, ";\n");
        return s.trim(); // poistetaan taulukkorivi
    }

    return this.askeleet.map(function(x) {
        return x.tekstinä();
    }).join('\n');
};

Ohjelma.prototype.reset = function () {
};

Ohjelma.prototype.aja = function () {
    this.pc = 0;
    var uusiTila = this.vk.alkuun();
    this.vk.taulukko.reset();
    var ok = true;

    vk.lauseke.katoa(); // TODO kludge, tarkista toimivuus

    var muuttujatDiv = haeOlio('muuttujat');
    var children = [];
    for (var mui = 0; mui < muuttujatDiv.childElementCount; ++mui) {
        var mu = muuttujatDiv.childNodes[mui];
        mu.olio.reset();
        children.push(mu);
    };
    for (var c in children) muuttujatDiv.removeChild(children[c]);

    while (this.pc < this.askeleet.length) {
        ok = this.askeleet[this.pc].sovita(uusiTila);
        if (!ok) return false;
        uusiTila = ok; // this.askeleet[this.pc].uusiTila;
        ++this.pc;
    }
    this.vk.tila(uusiTila);
    return true;
};

Ohjelma.prototype.liitä = function (kooste) {
    var koosteolio = haeOlio(kooste);
    koosteolio.appendChild(this.divi);
};

Ohjelma.prototype.irrota = function (kooste) {
    var koosteolio = haeOlio(kooste);
    koosteolio.removeChild(this.divi);
};

Ohjelma.prototype.tila = function () {
    return this.vk.tila();
};

Ohjelma.prototype.tilaksi = function (uusiTila) {
    return this.vk.tila(uusiTila);
};

Ohjelma.prototype.viimeisinAskel = function () {
    return this.askeleet[this.askeleet.length-1];
};

Ohjelma.prototype.poistaViimeisin = function () {
    this.divi.removeChild(this.divi.lastChild);
    var lause = this.askeleet.pop();
    lause.poista();
    return lause;
};

Ohjelma.prototype.ehto = function (ehto, ohjelma) {
    var ohj=this.divi;
    var e = new Ehto(ehto, ohjelma);
    var ok = e.sovita(this.tila());  // TODO entä jos ei onnistu?
    var uusiTila = ok; // e.uusiTila;
    var a = new Askel(e, uusiTila, this.viimeisinAskel());
    this.tilaksi(uusiTila);
    this.askeleet.push(a);

    ohj.appendChild(a.divinä());

    ohj.children[ohj.childElementCount-1].scrollIntoView(false);
};

Ohjelma.prototype.lisääDiviNäkyville = function (uusiDivi) {
    var ohj=this.divi;
    ohj.appendChild(uusiDivi);
    ohj.scrollTop = uusiDivi.offsetTop;
};

Ohjelma.prototype.sijoitus = function (mihin, mistä) {
    if (mistä == mihin) return;
    if (mihin.vakio) return;
    var s = new Sijoitus(mihin, mistä, this.viimeisinAskel());
    var uusiTila = s.sovita(this.tila());  // TODO entä jos ei onnistu?
    this.tilaksi(uusiTila); // XXX Se uusiTilaMaybe s.uusiTila);
    this.askeleet.push(s);

    this.lisääDiviNäkyville(s.divinä());
};

Ohjelma.prototype.unplus = function (mihin, määrä) {
    if (mihin.vakio) return;
    var up = new UnaariPlus(mihin, määrä, this.viimeisinAskel());
    var uusiTila = up.sovita(this.tila());  // TODO entä jos ei onnistu?;
    this.tilaksi(uusiTila);
    this.askeleet.push(up);

    this.lisääDiviNäkyville(up.divinä());
};

Ohjelma.prototype.unmiinus = function (mihin, määrä) {
    if (mihin.vakio) return;
    var um = new UnaariMiinus(mihin, määrä, this.viimeisinAskel());
    var uusiTila = um.sovita(this.tila());  // TODO entä jos ei onnistu?
    this.tilaksi(uusiTila);
    this.askeleet.push(um);

    this.lisääDiviNäkyville(um.divinä());
};


Ohjelma.prototype.uusiMuuttuja = function (nimi, arvo, vakio, diviin) {
    var lm = new LuoMuuttuja(this.vk, nimi, arvo, vakio, diviin);
    // XXX diviin => Muuttujat-olio tarvitaan...
    var uusiTila = lm.sovita(this.tila()); // = lm.uusiTila;
    this.tilaksi(uusiTila);
    if (!alkaaNumerolla(nimi)) {
        this.askeleet.push(lm);
        this.lisääDiviNäkyville(lm.divinä());
    }

    // var muuttuja = new Muuttuja(this.vk, nimi, arvo, true);
    // return muuttuja;
    return lm.muuttuja();
};


function Askel(lause, tila, edellinen) {
    this.lause = lause;
    this.tila = tila;
    this.edellinenAskel = edellinen;
}

Askel.prototype.divinä = function () {
    return this.lause.divinä(this.tila); // TODO tartteeko arg.ina vanhan tilan
};

Askel.prototype.tekstinä = function (tila) {
    return this.lause.tekstinä(this.tila); // TODO kts. Askel.divinä
};


function Lause(edellinen) {
    this._arvo = null;
    this.divi = null;
    this.tila = null;
    this.uusiTila = null;
    this.edellinenAskel = edellinen;
}

Lause.prototype.arvo = function() {
    return this._arvo;
};

Lause.prototype.suorita = function (tila) {
    this.tila = tila;
    this.uusiTila = tila;
    alert('aliolio toteuttaa!');
    return tila;
};

Lause.prototype.poista = function () {
};

Lause.prototype.divinä = function () {
    this.divi = luoDiv('lause'+this.arvo().toString(),
                        {'className': "lause",
                         'title': this.arvo().toString(),
                         'innerHTML': this.arvo().toString()});
    return this.divi;
};

Lause.prototype.tekstinä = function (tila) {
    return this.arvo().toString();
};


function Lohko(ehto, edellinen) {
    this.ehto = ehto;
    this.lauseet = [];
    this.edellinenAskel = edellinen;
}

Lohko.prototype.arvo = function () {
    return this.mistä.arvo();
};

Lohko.prototype = new Lause();
Lohko.prototype.constructor = Lohko;

Lohko.prototype.lisääLause = function(lause) {
    this.lauseet.push(lause);
};

Lohko.prototype.poistaLause = function(lause) {
    return this.lauseet.pop(lause);
};

Lohko.prototype.asetaEhto = function(ehto) {
    this.ehto = ehto;
};

Lohko.prototype.sovita = function(tila) {
    this.tila = tila ? tila : vk.tila();
    var uusiTila = tila.klooni();
    for (var l in this.lauseet) {
        var ok = l.sovita(uusiTila);
        uusiTila = ok; // l.uusiTila;
    }
    return uusiTila; // true; // this.uusiTila;
};




function Sijoitus(mihin, mistä, edellinen) {
    this.mihin = mihin;
    this.mistä = mistä;
    this.edellinenAskel = edellinen;
}

Sijoitus.prototype.arvo = function () {
    return this.mistä.arvo();
};

Sijoitus.prototype = new Lause();
Sijoitus.prototype.constructor = Sijoitus;

Sijoitus.prototype.sovita = function (tila) { // TODO XXX tämäkin voisi tajuta eron luku/indeksiluku
    // this.uusiTila = tila.klooni();
    var uusiTila = tila.klooni();
    var arvo = this.mistä.arvo(uusiTila);
    // TODO pitäs huomioida, jos ei onnistu ANS:
    //         ei voi olla väärin, koska drag-drop
    this.mihin.arvoksi(arvo, uusiTila);
    return uusiTila; // true; // this.uusiTila;
};

Sijoitus.prototype.divinä = function (tila) {
    this.divi = luoDiv('sijoitus'+this.mihin.nimi()+
                       this.mistä.nimi()+this.mistä.arvo(tila), {
                           className: "lause sijoitus",
                           title: this.mihin.nimi()+' = '+this.mistä.arvo(tila),
                           innerHTML: this.mihin.nimi()+' = '+ this.mistä.htmlksi(tila) +';<br/>'});
    return this.divi;
};

Sijoitus.prototype.tekstinä = function (tila) {
    return this.mihin.htmlksi(tila)+' = '+this.mistä.htmlksi(tila)+';';
};


function Ehto(ehto, ohjelma, edellinen) {
    this.ehto = ehto;
    this._ohjelma = ohjelma;
    this.edellinenAskel = edellinen;
}

Ehto.prototype = new Lause();
Ehto.prototype.constructor = Ehto;

Ehto.prototype.arvo = function () {
    return this.ehto.arvo();
};

Ehto.prototype.ohjelma = function () {
    return this._ohjelma;
};

Ehto.prototype.sovita = function (tila) {
    var uusiTila = tila.klooni();
    // jos this.ehto tosi
    // if (this.ehto.sovita(this.uusiTila))
    //   sovita ohjelma tilaan
    //   this.uusiTila = this.ohjelma.sovita(this.uusiTila);
    // muuten vanha tila voimassa

    return uusiTila; // true; // this.uusiTila;
};

Ehto.prototype.divinä = function (tila) {
    this.divi = luoDiv('ehto'+this.ehto.nimi(),
                       {'className': "lause",
                        'title': this.ehto.nimi() });
    this.divi.appendChild(this.ohjelma.divi);
    return this.divi;
};



function UnaariPlus(mihin, määrä) {
    this.mihin = mihin;
    if (typeof määrä === 'undefined')
        this.määrä = new Luku(1);
    else
        this.määrä = new Luku(määrä);
}

UnaariPlus.prototype.arvo = function (tila) {
    return this.mihin.arvo(tila);
};

UnaariPlus.prototype = new Lause();
UnaariPlus.prototype.constructor = UnaariPlus;

UnaariPlus.prototype.sovita = function (tila) {
    var uusiTila = tila.klooni();
    var arvo_mihin = this.mihin;
    var arvo_määrä = this.määrä;
    var arvo = arvo_mihin.add(arvo_määrä, uusiTila);
    this.mihin.arvoksi(arvo, uusiTila);
    return uusiTila; // true;
};

UnaariPlus.prototype.divinä = function (tila) {
    this.divi = luoDiv('uplus'+this.mihin.nimi()+this.mihin.arvo(tila), {
        className: "lause unaari",
        title: this.tekstinä(tila),
        innerHTML: this.mihin.nimi()+' += '+ this.määrä.arvo() +';<br/>'});
    return this.divi;
};

UnaariPlus.prototype.tekstinä = function (tila) {
    return this.mihin.nimi()+' += '+this.määrä.arvo()+';';
};




function UnaariMiinus(mihin, määrä) {
    this.mihin = mihin;

    if (typeof määrä === 'undefined')
        this.määrä = new Luku(1);
    else
        this.määrä = new Luku(määrä);
}

UnaariMiinus.prototype.arvo = function (tila) {
    return this.mihin.arvo(tila);
};

UnaariMiinus.prototype = new Lause();
UnaariMiinus.prototype.constructor = UnaariMiinus;

UnaariMiinus.prototype.sovita = function (tila) {
    var uusiTila = tila.klooni();
    var arvo = this.mihin.arvo(tila);
    arvo -= this.määrä.arvo();
    this.mihin.arvoksi(arvo, uusiTila);
    return uusiTila; // true;
};

UnaariMiinus.prototype.divinä = function (tila) {
    this.divi =
        luoDiv('umiinus'+this.mihin.nimi()+this.mihin.arvo(tila),
               {'className': "lause unaari",
                'title': this.tekstinä(tila),
                'innerHTML': this.mihin.nimi()+' -= '
                           + this.määrä.arvo()+';<br/>'});
    return this.divi;
};

UnaariMiinus.prototype.tekstinä = function (tila) {
    return this.mihin.nimi()+' -= ' + this.määrä.arvo()+';';
};


function LuoMuuttuja(vk, nimi, arvo, vakio, diviin) {
    this.nimi = nimi;
    this._arvo = arvo;
    this.vakio = vakio;
    this.vk = vk;
    this.muuttujatDivi = diviin;
    this._muuttuja = new Muuttuja(vk, nimi, arvo);
}

LuoMuuttuja.prototype.arvo = function () {
    return this._arvo;
};

LuoMuuttuja.prototype = new Lause();
LuoMuuttuja.prototype.constructor = LuoMuuttuja;

LuoMuuttuja.prototype.sovita = function (tila) {
    var uusiTila = tila.klooni();
    uusiTila.muuttujat[this.nimi] = this._arvo;// ÖÖÖ XXX tulis olla  uusiTila.muuttujat[this.nimi] = this._muuttuja;
    // var uusiTila = this._muuttuja.sovita(tila);  // XXX sittenkin tämä pois ja yllä oleva takas XXX-muutoksin?
    this._muuttuja.liitä(this.muuttujatDivi);
    return uusiTila; // true; // uusiTila;
};

LuoMuuttuja.prototype.poista = function () {
    this.vk.poistaMuuttuja(this.nimi);
    this._muuttuja.irrota(this.muuttujatDivi);
};

LuoMuuttuja.prototype.divinä = function (tila) {
    var lause = this.tekstinä(tila);
    this.divi = luoDiv('luo_muuttuja'+this.nimi+this._arvo,
                        {'className': 'lause luo_muuttuja',
                         'title': lause,
                         'innerHTML': lause+'<br/>'});
    return this.divi;
};

LuoMuuttuja.prototype.tekstinä = function (tila) {
    var lause = 'int '+this.nimi;
    if (this._arvo !== null)
        lause += ' = ' + this._arvo;
    lause += ';';
    return lause;
};

LuoMuuttuja.prototype.muuttuja = function () {
    return this._muuttuja;
};




function Luku(arvo) {
    if (typeof arvo === 'string')
        this.sisältö = parseInt(arvo);
    else
        this.sisältö = arvo;
}

Luku.prototype.arvo = function (tila) {
    return this.sisältö;
};

Luku.prototype.add = function(toinen) {
    var tulos = new Luku(this.arvo()+toinen.arvo());
    return tulos;
};

Luku.prototype.sub = function(toinen) {
    var tulos = new Luku(this.arvo()-toinen.arvo());
    return tulos;
};

Luku.prototype.nimi = function () {
    return this.sisältö.toString();
};

Luku.prototype.divinä = function (tila) {
    if (!this.divi)
        this.divi = luoDiv('', { 'className': 'luku',
                                 'title' : this.tekstinä(tila),
                                 'innerHTML' : this.htmlksi()});
    return this.divi;
};

Luku.prototype.htmlksi = function () {
    return this.sisältö.toString();
};

Luku.prototype.tekstinä = function (tila) {
    return this.sisältö.toString();
};

Luku.prototype.toString = function() {
    return this.tekstinä();
};

function Indeksiluku(arvo) { // TODO Korjaa, vai tarvitaanko sittenkään?
    this.sisältö = arvo;
}

Indeksiluku.prototype = new Luku();
Indeksiluku.prototype.constructor = Indeksiluku;

Indeksiluku.prototype.nimi = function() {
    return '<'+this.sisältö.toString()+'>';
};


function pudotaIndeksialueeseen(vm, event, data, mihin, mistä) {
    var nimi   = data.nimi;
    var tyyppi = data.tyyppi;
    var ohj = document.getElementById('ohjelma').olio;

    event.stopPropagation();

    if (mihin.constructor === mistä.constructor) return;

    if (tyyppi==='indeksi') {
        var erotus = mihin.indeksi - mistä.div.solu.indeksi;

        if (erotus<0) ohj.unmiinus(mistä.muuttuja, -erotus);
        else ohj.unplus(mistä.muuttuja, erotus);

        return;
    } else if (tyyppi==='muuttuja') { // XXX XXX XXX ihan sekaisin
        var indeksi = mistä.arvo() === null ? mihin.indeksi : mistä.arvo();
        var muuttujaAlustamatta = false;

        if ((indeksi < 0) || (indeksi >= vm.tila().taulukko.length)) {
            return;
        }

        if (mistä.arvo() === null) {
            mistä.arvoksi(mihin.indeksi); // TODO mihin.indeksi == indeksi
            muuttujaAlustamatta = true;   // TODO yllä '.indeksi' on int,
                                          // -> Indeksiarvo-olioksi?
        }

        var io = vm.uusiIndeksi(mihin, mistä);

        if (muuttujaAlustamatta) {
            // XXX TODO Tämä voisi luoda uuden indeksiarvon lukuarvon sijaan:
            ohj.sijoitus(mistä, new Indeksiluku(mihin.indeksi));
        } else {
            var f = mihin.indeksimuuttujat.filter(
                function (x) {
                    return x.muuttuja.nimi() === mistä.nimi();
                });
            if (f.length>1) {
                mihin.indeksimuuttujat =
                    mihin.indeksimuuttujat.filter(
                        function (x) { return x !== io; });
                mihin.divSi.removeChild(io.div);
            }
        }

        return;
    } else if (tyyppi==='unop') {
        if (nimi === 'miinus') ohj.unmiinus(mihin);
        else if (nimi === 'plus') ohj.unplus(mihin);

        return;
    }
}


function lisääDragKuuntelija(div,fSallittu) {
    if (typeof (fSallittu) != "function") fSallittu = false;
    div.original = div.style.background;

    div.addEventListener('dragover', function (event) {
        if (fSallittu && fSallittu()) return true;
        if (event.preventDefault) { event.preventDefault(); }
        event.dataTransfer.effectAllowed = 'copy';
        return false;
    }, false);

    div.addEventListener('dragenter', function (event) {
        if (fSallittu && fSallittu()) return true;
        event.preventDefault();
        event.dataTransfer.effectAllowed = 'copy';
        if ( event.target.style ) event.target.style.background = "gray";
        return false;
    }, false);
    div.addEventListener('dragleave', function (event) {
        if (fSallittu && fSallittu()) return true;
        event.preventDefault();
        event.dataTransfer.effectAllowed = 'copy';
        if ( event.target.style ) event.target.style.background = "";
        return false;
    }, false);
}

function lisääKuuntelijat(div, asetaLähde, käytäKohde, salliPudotus) {
    if (asetaLähde)
        div.addEventListener('dragstart', function (event) {
            asetaLähde(event);
            event.dataTransfer.effectAllowed = 'copy';
            event.dataTransfer.setData('text/plain',""); // FF ei muuten toimi
        }, false);

    if (käytäKohde)
        div.addEventListener('drop', function (event) {
            käytäKohde(event);
        }, false);

    div.addEventListener('click', function (event) {
        if (Operaatio.odottaaLähdettä()) {
            if (asetaLähde) asetaLähde(event);
        } else {
            if (käytäKohde) käytäKohde(event);
        }
    }, false);

    if (salliPudotus) lisääDragKuuntelija(div, salliPudotus);
}


function Solu(vm, indeksi, arvo) {
    var vakio = alkaaNumerolla(vm._nimi);

    var divid = 'solu-'+indeksi;
    this.divid = divid;
    this.div = luoDiv(divid,                  // solun kehys sisältäen:
                       {'className':'solu'});
    this.divSn = luoDiv('solu-n'+indeksi,     // - solun numero
                          {'className':'solu-n',
                           'innerHTML':indeksi.toString()});
    this.divSa = luoDiv('solu-a'+indeksi,     // - solun arvo
                        {'className':'solu-a',
                         'draggable':true});
    this.divSi = luoDiv('solu-i'+indeksi,     // - soluun osoittavat indeksit
                          {'className':'solu-i',
                           'draggable':false,
                           'innerHTML':''});

    this.indeksimuuttujat = [];

    this.sulje();

    this.kuuntelijat = [];

    this.vm = vm;
    this.indeksi = indeksi;
    this.div.olio = this;
    this.divSa.olio = this;
    this.divSi.olio = this;
    this.arvoksi(arvo||0);
    this._alkuarvo = arvo;

    this.div.solu = this;    // TODO tarvitaanko näitä .soluja?
    this.divSa.solu = this;
    this.divSi.solu = this;

    var self = this;
    function asetaLähde(event) {
        if (self.suljettu()) {
            event.preventDefault();
            event.stopPropagation();
            return;
        }
        if (self.divSi.children.length>1) {
            event.preventDefault();
            event.stopPropagation();
            alert('Solulla on useampia indeksejä, '+
                  'joten siirrä indeksi valitaksesi sopivan.');
            return;
        }
        if (Operaatio.odottaaLähdettä()) {
            Operaatio.asetaLähde(event.target);
            Operaatio.asetaData({tyyppi:  'solu',
                                 indeksi: event.target.solu.indeksi.toString(),
                                 oid:     event.target.id});
            return;
        }
    }

    function käytäKohde(event) {
        Operaatio.asetaKohde(event.target);

        if (self.divSi.children.length > 1) {
            event.preventDefault();
            event.stopPropagation();
            alert('Solulla on useampia indeksejä, '+
                  'joten pudota indeksiin valitaksesi sopivan.');
            return Operaatio.tehty();
        }

        event.preventDefault();
        var data = Operaatio.haeData();
        var tyyppi = data.tyyppi;
        var oid = data.oid;
        var olio = haeOlio(oid).olio;
        var mihin = event.target.olio;
        if (mihin.suljettu && mihin.suljettu() ) { return Operaatio.tehty(); }
        var mistä = olio;
        var ohj = document.getElementById("ohjelma").olio;

        if (tyyppi == "muuttuja" || tyyppi == "solu" ||
            tyyppi === "indeksi" || tyyppi === "vakio") {
            event.stopPropagation();

            ohj.sijoitus(mihin, mistä);
            Operaatio.tehty();
            return;
        } else if (tyyppi === "lauseke") {
            var lausekediv = haeOlio(oid);
            event.stopPropagation();

            ohj.sijoitus(mihin, mistä);
            lausekediv.olio.katoa();
            Operaatio.tehty();
            return;
        } else if (tyyppi === "unop") {
            event.stopPropagation();

            if (data.nimi === "miinus")    ohj.unmiinus(mihin);
            else if (data.nimi === "plus") ohj.unplus(mihin);
            Operaatio.tehty();
            return;
        }
    } // XXX tarkista return arvo

    lisääKuuntelijat(this.divSa, asetaLähde, käytäKohde,
                     function () { return self.suljettu(); } );

    function käytäKohdeSi(event) {
            event.preventDefault();
            Operaatio.asetaKohde(event.target);
            var data = Operaatio.haeData();
            var mihin = event.target.olio;
            var mistä = Operaatio.lähde().olio; // muuta kuin olio...
            var tulos = pudotaIndeksialueeseen(vm, event, data, mihin, mistä);
            Operaatio.tehty();
    }

    lisääKuuntelijat(this.divSi, null, käytäKohdeSi, true);

    this.div.appendChild(this.divSn);
    this.div.appendChild(this.divSa);
    if ( !vk.simple )  this.div.appendChild(this.divSi);
}

Solu.prototype.sidoIndeksi = function(i) { // XXX tätä ei kutsuta
    this.kuuntelijat.push(i);
};

Solu.prototype.vapautaIndeksi = function (i) {
    this.kuuntelijat = this.kuuntelijat.filter(
        function (x) { return x !== i; });
};

Solu.prototype.päivitäIndeksit = function (tila) {
    var _tila = tila || this.vm.tila();
    this.kuuntelijat.map(
        function (x) { x.muuttujaPäivitetty(this, _tila); });
};

Solu.prototype.poistaIndeksimuuttuja = function (im) {
    if (this.diviSi.children.length>0)
        this.diviSi.removeChild(im.div);
};

Solu.prototype.arvoksi = function (x, tila) {
    tila = tila ? tila : this.vm.tila();
    // XXX x voi olla null, jos alustamattoman muuttujan sijoittaa taulukkoon
    if (x != null) { // TODO järkevämmäksi: alustamattomat muuttujat pois?
        tila.asetaIndeksiin(this.indeksi,  x);  // XXX ZZZ TODO vai asetaIndeksiin
        this.divSa.innerHTML = x.toString();
    }
    return tila.indeksissa(this.indeksi);
};

Solu.prototype.arvo = function (tila) {
    return new Luku(tila ? tila.indeksissa(this.indeksi)
                    : this.vm.tila().indeksissa(this.indeksi));
};

Solu.prototype.reset = function () {
    this.indeksimuuttujat.forEach(function (im) { im.reset(); });
    // TODO KUUN this.indeksimuuttujat = [];
    // TODO XXX poistuvatko indeksimuuttujat? tarvitseeko?
    // pitäiskö tässä nollata kuuntelijat?
    this.sulje();
    return this.arvoksi(this._alkuarvo);
};

Solu.prototype.add = function(mitä, tila) {
    var _tila = tila || this.vm.tila();
    var lkm = this.arvo(tila);
    return lkm.add(mitä);
};

Solu.prototype.nimi = function (r) {
    // TODO XXX vuoden kludge, joka estää rekursion
    if ((this.indeksimuuttujat.length !== 0) && (r!==this)) {
        // ei osaa "uudelleenajossa"
        // esim sorsatulostuksessa huomioida indeksisyyttä oikein 
        return this.indeksimuuttujat[0].nimi(this);
    }
    return 't['+this.indeksi+']';  // katso Indeksi.prototype.nimi
};

Solu.prototype.sovita = function (tila) {
    return tila; // true;
};

Solu.prototype.htmlksi = function () {
    return this.nimi();
};

Solu.prototype.suljettu = function () {
    var sulj = (this.divSi.innerHTML === '');
    // käy läpi lapset ja kutsu niille validi()

    var validi = false;
    if (!sulj) {
        for (var i = this.divSi.firstChild; i !== null; i = i.nextSibling)
            validi = i.olio.validi() || validi;
    }
    return sulj || !validi;
};

Solu.prototype.avaa = function () {
    if (!this.suljettu()) {
        this.divSa.classList.remove('suljettu');
        this.divSa.classList.add('avattu');
    }
};

Solu.prototype.sulje = function () {
    if (this.suljettu()) {
        this.divSa.classList.remove('avattu');
        this.divSa.classList.add('suljettu');
    }
};

Solu.prototype.muuttujaPäivitetty = function (a,b) {
    // TODO keskeneräisen lausekkeen t[i] arvo pitää päivittyä
};




function Indeksi(vm, solu) {
    var vakio = alkaaNumerolla(vm._nimi);
    var divid = 'indeksi'+timestring();
    this.divid = divid;
    this.div = luoDiv(divid, {
        'className':'indeksi',
        'draggable':true,
        'title': 'siirrä tähän muuttuja, niin voit viitata '+
            'taulukon alkioihin\n' +
            'muuttujan arvon muuttaminen tai tämän raahaaminen '+
            'siirtää viittausta',
        'innerHTML':'&nbsp;'});
    this.vm = vm;
    this.muuttuja = null;
    this.div.olio = this;

    this.div.solu = solu;

    function asetaLähde(event) {
        Operaatio.asetaLähde(event.target);
        event.stopPropagation();
        Operaatio.asetaData({tyyppi: 'indeksi',
                             indeksi: event.target.solu.indeksi.toString(),
                             oid:event.target.id});
    }

    function käytäKohde(event) {
        Operaatio.asetaKohde(event.target);
        event.stopPropagation();
        event.preventDefault();

        var data = Operaatio.haeData();

        var tyyppi = data.tyyppi;
        var oid = data.oid;

        var olio = haeOlio(oid).olio;
        var mihin = event.target.olio;
        var mistä = olio;
        var ohj = document.getElementById('ohjelma').olio;

        if (tyyppi == "muuttuja") {
            if (mihin.div.solu.divSi.children.length === 0) {
                mihin.indeksoi(mistä);
                mihin.div.innerHTML = mistä.nimi(); //
                // TODO tänne seuraava lause &
                // indeksin siirto muuttujan arvon mukaan
                mihin.siirry();
                // ohj.sijoitus(mihin, mistä);
            } else {
                ohj.sijoitus(mihin, mistä);
            }
        } else if (tyyppi === "indeksi" || tyyppi == "solu" ||
                   tyyppi === "vakio" ) {
            ohj.sijoitus(mihin, mistä);
        } else if (tyyppi === "lauseke") {
            var lausekediv = haeOlio(oid);
            ohj.sijoitus(mihin, mistä);
            lausekediv.olio.katoa();
        } else if (tyyppi === "unop") {
            var nimi = data.nimi;
            if (nimi === "miinus") ohj.unmiinus(mihin);
            else if (nimi === "plus") ohj.unplus(mihin);
        }
        Operaatio.tehty();
    }

    lisääKuuntelijat(this.div, asetaLähde, käytäKohde, true);
}

Indeksi.prototype.validi = function (tila) {
    if ((typeof this.muuttuja === 'undefined') ||
        (this.muuttuja === null))
        return false;

    var taulukko = tila ? tila.taulukko : this.vm.tila().taulukko;
    var ind = this.muuttuja.arvo(tila);

    // TODO XXX ind voi olla myös null

    if ((ind < 0) || (ind >= taulukko.length)) return false;

    return true;
};

Indeksi.prototype.muuttujaPäivitetty = function (m, tila) {
    this.siirry(tila);
};

Indeksi.prototype.reset = function () {
    if (this.muuttuja !== null) {
        var indeksiDivi = this.div.solu.divSi;
        if (indeksiDivi.children.length>0) {
            indeksiDivi.removeChild(this.div);
        }
        // this.div.solu.vapautaIndeksi(this);
        // this.muuttuja.vapautaIndeksi(this);
        // this.muuttuja = null;
    }
};

Indeksi.prototype.arvoksi = function (x, tila) {
    tila = tila ? tila : this.vm.tila();
    if (!this.validi(tila)) return null;

    var ind = this.muuttuja.arvo(tila);
    var solu = this.vm.solu(ind);

    return solu.arvoksi(x, tila);
};

Indeksi.prototype.arvo = function (tila) {
    tila = tila ? tila : this.vm.tila();
    if (!this.validi(tila)) return null;

    return tila.indeksissa(this.muuttuja.arvo());
};

Indeksi.prototype.siirry = function (tila) {
    if (!this.validi(tila)) return;

    var ind = this.muuttuja.arvo(tila);
    if (ind === null) return;
    // XXX uudelleen alustettaessa muuttuja voi olla alustamaton,
    //     vaikka tätä kutsutaan.
    // TODO XXX jos ind==null, saattaa indeksi olla muuttujassa
    //          kuuntelijana. poistetaanko nyt vai muualla?
    var s = this.vm.solu(ind);
    var d = this.div.solu;
    var ti = this;

    if (d.divSi.children.length>0) {
        // resetin jälkeen lapsia ei ole,
        // mutta alla appendChild luo ensimmäisen, jep kludge
        d.divSi.removeChild(this.div);
    }
    d.indeksimuuttujat =
        d.indeksimuuttujat.filter(function (x) { return x !== ti; });

    s.divSi.appendChild(this.div);
    s.indeksimuuttujat.push(this);

    d.sulje();
    s.avaa();

    this.div.solu = s;
};

Indeksi.prototype.indeksoi = function (muuttuja, tila) {
    //if (!this.validi(tila)) return;
    if (this.muuttuja === muuttuja) return;

    if (this.muuttuja !== null)
        this.muuttuja.vapautaIndeksi(this);
    this.muuttuja = muuttuja;
    this.muuttuja.sidoIndeksi(this);
};

Indeksi.prototype.päivitä = function (tila) {
    var ind = this.muuttuja.arvo(tila);
    var solu = this.vm.solu(ind);
};

Indeksi.prototype.nimi = function (r) {
    // XXX TODO vuoden kludge, joka estää rekursion
    if (this.muuttuja === null) return '?';
    return 't['+this.muuttuja.nimi()+']'; // zzz
    // if (typeof r === 'undefined')
    //     return this.muuttuja.nimi();
    // return 't['+this.muuttuja.nimi(r)+']';  // katso Solu.prototype.nimi
};

Indeksi.prototype.htmlksi = function (tila) {
    return this.nimi();
};


function add_drag_drop_new(divi, callbacks) {
    divi.addEventListener('dragstart', callbacks.dragstart ||
                          function (event) {}, false);
    divi.addEventListener('dragenter', callbacks.dragenter ||
                          function (event) {}, false);
    divi.addEventListener('dragleave', callbacks.dragleave ||
                          function (event) {}, false);
    divi.addEventListener('dragover', callbacks.dragover ||
                          function (event) {
                              if (event.preventDefault) { event.preventDefault(); }
                              event.dataTransfer.effectAllowed = 'copy';
                          }, false);
    divi.addEventListener('drop', callbacks.drop ||
                          function (event) {
                              if (event.stopPropagating) { event.stopPropagating(); }
                              event.dataTransfer.dropEffect = 'copy';
                              return false;
                          }, false);
}


function Vakio(value, vm) {
    var divid = 'vakio'+value;
    this.divid = divid;
    this.vm = vm;
    this.value = value;

    this.divi = luoDiv(this.divid, {
        className: 'const',
        title: 'raahaan tämä arvoon, tai tähän arvo, '+
               'muuttaaksesi sen arvoksi '+this.value});
    this.divi.innerHTML = '_ &larr; '+this.value.toString();

    function asetaLähde(event) {
            Operaatio.asetaLähde(event.target);
            Operaatio.asetaData({tyyppi: 'vakio',
                                 nimi: value.toString(),
                                 oid: divid});
    }
    function käytäKohde(event) {
            Operaatio.asetaKohde(event.target);
            var data = Operaatio.haeData();
            var tyyppi = data.tyyppi;
            var nimi  = data.nimi;
            var oid = data.oid;
            var ohj = document.getElementById('ohjelma').olio;
            var olio = haeOlio(oid).olio;
            var mihin = event.target.olio;
            var mistä = olio;

            var kohde = mistä;

            if ((tyyppi === 'muuttuja') || (tyyppi === 'solu') ||
                (tyyppi === 'indeksi')) {
                event.stopPropagation();

                var luku = new Luku(value);
                ohj.sijoitus(kohde, luku);
            }
            Operaatio.tehty();
    }

    lisääKuuntelijat(this.divi, asetaLähde, käytäKohde, false);

    this.divi.olio = this;
}

Vakio.prototype.arvo = function (tila) {
    return this.value;
};

Vakio.prototype.nimi = function (tila) {
    return this.arvo(tila).toString();
};

Vakio.prototype.htmlksi = function (tila) {
    return this.arvo(tila).toString();
};

Vakio.prototype.liitä = function (kooste) {
    var koosteolio = haeOlio(kooste);
    koosteolio.appendChild(this.divi);
};

Vakio.prototype.irrota = function (kooste) {
    var koosteolio = haeOlio(kooste);
    koosteolio.removeChild(this.divi);
};


function UnOpPlus(vm) {
    var divid = 'unopplus-'+timestring();
    this.divid = divid;
    this.vm = vm;

    this.divi = luoDiv(this.divid, {
        className: 'unop',
        title:
        'siirrä tämä arvoon, tai tähän arvo, kasvattaaksesi arvoa yhdellä',
        'draggable': true});
    this.divi.innerHTML ='_ + 1';
    this.divi.olio = this;

    function asetaLähde(event) {
        Operaatio.asetaLähde(event.target);
        Operaatio.asetaData({
            tyyppi: 'unop',
            nimi: 'plus',
            oid: divid});
    }

    function käytäKohde(event) {
        Operaatio.asetaKohde(event.target);
        var data = Operaatio.haeData();
        var tyyppi = data.tyyppi;
        var nimi  = data.nimi;
        var oid = data.oid;
        var ohj = document.getElementById('ohjelma').olio;

        var olio = haeOlio(oid).olio;
        var mihin = event.target.olio;
        var mistä = olio;

        var kohde = mistä;

        if ((tyyppi === 'muuttuja') || (tyyppi === 'solu') ||
            (tyyppi === 'indeksi')) {
            event.stopPropagation();

            var lauseke = new Lauseke();
            lauseke.asetaOper('+');
            lauseke.asetaArg1(kohde);
            lauseke.asetaArg2(new Luku(1));

            ohj.sijoitus(kohde, lauseke);
        }
        Operaatio.tehty();
    }

    lisääKuuntelijat(this.divi, asetaLähde, käytäKohde, true);
}

UnOpPlus.prototype.liitä = function (kooste) {
    var koosteolio = haeOlio(kooste);
    koosteolio.appendChild(this.divi);
};

UnOpPlus.prototype.irrota = function (kooste) {
    var koosteolio = haeOlio(kooste);
    koosteolio.removeChild(this.divi);
};

function UnOpMiinus(vm) {
    var divid = 'unopmiinus-'+timestring();
    this.divid = divid;
    this.vm = vm;

    this.divi = luoDiv(this.divid, {
        className: 'unop',
        title:
        'raahaa tähän arvo, tai tämä arvoon, vähentääksesi arvoa yhdellä',
        'draggable': true});
    this.divi.innerHTML ='_ - 1';
    this.divi.olio = this;

    function asetaLähde(event) {
        Operaatio.asetaLähde(event.target);
        Operaatio.asetaData({tyyppi: 'unop',
                             nimi: 'miinus',
                             oid: divid});
    }

    function käytäKohde(event) {
        Operaatio.asetaKohde(event.target);
        var data = Operaatio.haeData();
        var tyyppi = data.tyyppi;
        var nimi  = data.nimi;
        var oid = data.oid;
        var ohj = document.getElementById('ohjelma').olio;

        var olio = haeOlio(oid).olio;
        var mihin = event.target.olio;
        var mistä = olio;

        var kohde = mistä;

        if ((tyyppi === 'muuttuja') || (tyyppi === 'solu') ||
            (tyyppi === 'indeksi')) {
            event.stopPropagation();

            var lauseke = new Lauseke();
            lauseke.asetaOper('-');
            lauseke.asetaArg1(kohde);
            lauseke.asetaArg2(new Luku(1));

            ohj.sijoitus(kohde, lauseke);
        }
        Operaatio.tehty();
    }

    lisääKuuntelijat(this.divi, asetaLähde, käytäKohde, true);
}

UnOpMiinus.prototype.liitä = function (kooste) {
    var koosteolio = haeOlio(kooste);
    koosteolio.appendChild(this.divi);
};

UnOpMiinus.prototype.irrota = function (kooste) {
    var koosteolio = haeOlio(kooste);
    koosteolio.removeChild(this.divi);
};


function Muuttuja(vm, nimi, arvo, vakio) {
    var divid = 'muuttuja-'+nimi;
    this.divid = divid;
    this.vm = vm;
    this._nimi = nimi;
    this.vakio = vakio;
    if (alkaaNumerolla(nimi)) this.vakio = true;

    this._alkuarvo = arvo;

    this.kuuntelijat = [];

    this.divi = luoDiv(this.divid+'-b', {});
    this.divi.olio = this;


    var ntext = nimi + ":";
    if (vakio) ntext = "(dbg:"+vakio+"):"; // "";
    var no = luoDiv(this.divid,
                     {'className': 'muuttuja',
                      'innerHTML': ntext});

    var ma = luoDiv(this.divid+'-a',
                     {'className': 'muuttuja-arvo',
                     'draggable': true
                     });

    ma.innerHTML = arvo!==null ? arvo.toString() : '&nbsp;&nbsp;';
    ma.arvo = arvo;
    ma.nimi = nimi;

    // ma.addEventListener('dblclick', muutaMuuttujanArvoa);

    function asetaLähde(event) {
        Operaatio.asetaLähde(event.target);
        Operaatio.asetaData({tyyppi: "muuttuja",
                             nimi:   event.target.olio.nimi(),
                             oid:    divid});
        return;
    }

    function käytäKohde(event) {
        if (vakio) return Operaatio.tehty();

        Operaatio.asetaKohde(event.target);

        var data = Operaatio.haeData();
        var tyyppi = data.tyyppi;
        var nimi  = data.nimi;
        var oid = data.oid;
        var ohj = document.getElementById('ohjelma').olio;

        var olio = haeOlio(oid).olio;
        var mihin = event.target.olio;
        var mistä = olio;

        if ((tyyppi === "solu")
            || (tyyppi=== 'indeksi')
            || (tyyppi=== 'vakio')
            || ((tyyppi === "muuttuja") && (nimi != event.target.nimi))) {

            event.stopPropagation();

            ohj.sijoitus(mihin, mistä);

        } else if (tyyppi === "lauseke") {
            var lausekediv = haeOlio(oid);

            event.stopPropagation();

            ohj.sijoitus(mihin, mistä);
            lausekediv.olio.katoa();

        } else if (tyyppi == 'unop') {
            event.stopPropagation();

            if (nimi==='miinus')
                ohj.unmiinus(mihin);
            else if (nimi==='plus')
                ohj.unplus(mihin);

        } else {
            return;
        }

        Operaatio.tehty();
        return;
    } // XXX return value?

    lisääKuuntelijat(ma, asetaLähde, käytäKohde, !vakio);
    ma.muuttujat = this.vm.tila().muuttujat; // XXX ei toimi näin ÖÖÖ
    ma.nimi = nimi;

    this.divi.appendChild(no);
    this.divi.appendChild(ma);
    ma.olio = this;
    no.olio = this;
    this.diviArvo = ma;

    this.indeksoiva = false; // XXX Kludge

    this.arvoksi(arvo);
    Muuttuja.prototype.instanssit.push(this); // XXX Pois?
}

Muuttuja.prototype.instanssit = [];

Muuttuja.prototype.reset = function() {
    this.arvoksi(this._alkuarvo);
    // TODO KUUN this.kuuntelijat = [];
};

Muuttuja.prototype.sidoIndeksi = function(i) {
    this.kuuntelijat.push(i);
    this.indeksoiva = true;
};

Muuttuja.prototype.vapautaIndeksi = function (i) {
    this.kuuntelijat = this.kuuntelijat.filter(
        function (x) { return x !== i; });
    this.indeksoiva = false;
};

Muuttuja.prototype.päivitäIndeksit = function (tila) {
    var _tila = tila || this.vm.tila();
    this.kuuntelijat.map(
        function (x) { x.muuttujaPäivitetty(this, _tila); });
};

Muuttuja.prototype.nimi = function() {
    return this._nimi;
};

Muuttuja.prototype.htmlksi = function () {
    return this.nimi();
};

Muuttuja.prototype.arvo = function(tila) {
    tila = tila ? tila : this.vm.tila();
    return tila.muuttujassa(this.nimi());
};

Muuttuja.prototype.arvoksi = function (x, tila) {
    if (x === null) {
        console.log("x = null"); if (debis) debugger;
    } else {
        console.log("x type "+(typeof x)+" "+(typeof x === 'object' && x !== null ? x.constructor.name : '-')+" "+x.toString()); if (debis) debugger;
    }
    // if (typeof x !== 'object') debugger;
    tila = tila ? tila : this.vm.tila();
    // var muuttujat = tila ? tila.muuttujat : this.vm.tila().muuttujat;
    // muuttujat[this.nimi()] = x;
    var vanha = this.arvo();
    tila.asetaMuuttuja(this.nimi(), x);
    if (x!==null)
        this.diviArvo.innerHTML = x.toString();
    else
        this.diviArvo.innerHTML = '&nbsp;&nbsp;';

    this.päivitäIndeksit(tila); // XXX TODO ei tee mitään?

    return tila.muuttujassa(this.nimi());
};

Muuttuja.prototype.add = function (toinen, tila) {
    var _tila = tila ? tila : this.vm.tila();
    var lkm = this.arvo(_tila);
    return lkm.add(toinen, _tila);
};

Muuttuja.prototype.sovita = function (tila) {
    return tila;
    // var uusiTila = tila.klooni();
    // uusiTila.muuttujat[this.nimi] = this._arvo;
    // return uusiTila;
};

Muuttuja.prototype.nimeä = function (n) { // TODO Mietipä vähän 
    this._nimi = n;
};

Muuttuja.prototype.liitä = function (kooste) {
    var koosteolio = haeOlio(kooste);
    koosteolio.appendChild(this.divi);
};

Muuttuja.prototype.irrota = function (kooste) {
    var koosteolio = haeOlio(kooste);
    koosteolio.removeChild(this.divi);
};

function Echo(id) {
    this.divi = document.getElementById(id);
    this.divi.addEventListener('click',
                               function (event) { echo.puhdista(); }, false);
}

Echo.prototype.puhdista = function () {
    if (this.divi)
        this.divi.innerHTML = '';
};

Echo.prototype.aseta = function (str) {
    if (this.divi)
        this.divi.innerHTML = str.toString(); // varmistetaan merkkijonous
    else
        console.log('Echo: '+str.toString());
};

Echo.prototype.lisää = function (str) {
    if (this.divi)
        this.divi.innerHTML += str.toString();
    else
        console.log('Echo: '+str.toString());
};


function luoOperaattorit(divid, kohde, opers) {
    var divi = luoDiv(divid, {'className': 'op'});
    var sel = document.createElement('select');
    for (var on in opers) {
        var op = document.createElement('option');
        op.value = on;
        op.label = on;
        op.text = on;  // firefox vaatii tämänkin, webkitille riittää ylemmät...
        op.accessKey = on; // elvistelyä
        op.fn = opers[on];

        sel.appendChild(op);
    }
    sel.addEventListener('change',
                         function (event) { kohde.asetaOper(sel.value); },
                         false);
    divi.appendChild(sel);
    return divi;
}


function Nappi(divid, tunnus, toimintoFn) {
    this.divid = divid;
    this.tunnus = tunnus;

    this.elementti = document.createElement('button');
    this.elementti.innerHTML = this.tunnus;
    this.elementti.onclick = toimintoFn;
    this.elementti.olio = this;
}

Nappi.prototype.liitä = function (kooste) {
    var koosteolio = haeOlio(kooste);
    koosteolio.appendChild(this.elementti);
};

Nappi.prototype.irrota = function (kooste) {
    var koosteolio = haeOlio(kooste);
    koosteolio.removeChild(this.elementti);
};

Nappi.prototype.aktivoi = function (aktiivinen) {
    this.elementti.disabled = !aktiivinen;
};


function Lauseke(divid) {
    if (typeof divid === 'undefined') {
        divid = 'lauseke'+timestring();
    }

    this.divid = divid;
    this.arg1 = null;
    this.operators = {'+': function (a,b) { return a.add(b); },
                      '-': function (a,b) { return a.sub(b); } // ,
                     //  '<': function (a,b) { return a<b; }
                     };
    this.operator = '+';
    this.arg2 = null;

    this.divi     = luoDiv(divid, {
        className:'lauseke',
        title: 'Laittamalla tähän arvoja suoritat laskutoimituksen,\n'+
            'jonka tuloksen voit siirtää muuttujaan tai taulukkoon.'});
    this.diviA1  = luoDiv(divid+'-a1',
                            {'className':'arg'});
    this.diviOp  = luoOperaattorit(divid+'-op', this, this.operators);
    this.diviA2  = luoDiv(divid+'-a2',
                            {'className':'arg'});
    this.diviEq  = luoDiv(divid+'-eq',
                            {'className':'equals-literal'});
    this.diviVal = luoDiv(divid+'-v',
                          {'className':'arvo',
                           'draggable': true});

    this.diviA1.innerHTML = "&nbsp; &nbsp; &nbsp;";
    this.diviA2.innerHTML = "&nbsp; &nbsp; &nbsp;";
    this.diviEq.innerHTML = "=";
    this.diviVal.innerHTML = "&nbsp; &nbsp; &nbsp;";

    this.divi.appendChild(this.diviA1);
    this.divi.appendChild(this.diviOp);
    this.divi.appendChild(this.diviA2);
    this.divi.appendChild(this.diviEq);
    this.divi.appendChild(this.diviVal);

    this.divi.olio = this;
    this.diviA1.olio = this;
    this.diviOp.olio = this;
    this.diviA2.olio = this;
    this.diviVal.olio = this;

    var self = this;

    function asetaLähde(event) {
        Operaatio.asetaLähde(event.target);
        if (!self.validi()) {
            alert('Vain valmiin lausekkeen voi siirtää!');
            echo.aseta('Vain valmiin lausekkeen voi siirtää!');
            event.preventDefault();
            Operaatio.peruuta();
            return;
        }
        Operaatio.asetaData({tyyppi: "lauseke",
                             oid: self.divi.id});
    }

    lisääKuuntelijat(this.diviVal, asetaLähde, null, false);

    var asetus = function (asfn) {
        return function (event) {
            if (!Operaatio.odottaaLähdettä()) {
                Operaatio.asetaKohde(event.target);
                var data = Operaatio.haeData();
                var tyyppi = data.tyyppi;

                if (tyyppi == "solu"
                    || tyyppi == "muuttuja"
                    || tyyppi === 'indeksi') {
                    event.stopPropagation();

                    var oid = data.oid;
                    var olio = document.getElementById(oid).olio;
                    event.target.olio[asfn](olio);
                }
                event.preventDefault();
                Operaatio.tehty();
            }
        };
    };

    lisääKuuntelijat(this.diviA1, null, asetus('asetaArg1'), true);
    lisääKuuntelijat(this.diviA2, null, asetus('asetaArg2'), true);

    // this.diviA1.addEventListener('click', asetus('asetaArg1'), false);
    // this.diviA2.addEventListener('click', asetus('asetaArg2'), false);
}

Lauseke.prototype.validi = function () {
    return this.arg1 && this.operator && this.arg2;
};


Lauseke.prototype.sovita = function (tila) {
    return tila;
//    return this.arvo(tila) !== null;
};

Lauseke.prototype.arvo = function (tila) {
    if (!this.validi()) return null;
    return this.operators[this.operator](this.arg1.arvo(tila),
                                         this.arg2.arvo(tila));
};

Lauseke.prototype.asetaArg1 = function (arvo) { // XXXXXX tila ei välity!!!
    this.arg1 = arvo;
    if (debis) debugger;
    // XXX Lausekkeen argumentit eivät tyhjene, kun ajo aloitetaan uusiksi
    var nimiTaiIndeksi = arvo.nimi();
    //                || ('t['+arvo.indeksi+"]"); // TODO Kauhee kludge!
    if (arvo.vakio) nimiTaiIndeksi = "(dbg:vakio)";
    this.diviA1.innerHTML = nimiTaiIndeksi+':'+arvo.arvo();
    this.diviVal.innerHTML = this.validi()? this.arvo().toString() : '__';
};

Lauseke.prototype.asetaArg2 = function (arvo) {
    this.arg2 = arvo;
    var nimiTaiIndeksi = arvo.nimi();
    //                || ('t['+arvo.indeksi+"]"); // TODO Kauhee kludge!
    if (arvo.vakio) nimiTaiIndeksi = "(dbg:vakio)";
    this.diviA2.innerHTML = nimiTaiIndeksi+':'+arvo.arvo();
    this.diviVal.innerHTML = this.validi()? this.arvo().toString() : '__' ;
};

Lauseke.prototype.asetaOper = function (symboli) {
    this.operator = symboli;
    this.diviVal.innerHTML = this.arvo().toString();
};

Lauseke.prototype.liitä = function (kooste) {
    var koosteolio = haeOlio(kooste);
    koosteolio.appendChild(this.divi);
};

Lauseke.prototype.irrota = function (kooste) {
    var koosteolio = haeOlio(kooste);
    koosteolio.removeChild(this.divi);
};

Lauseke.prototype.katoa = function () {
    var parent = this.divi.parentElement;
    if (parent != null) {
        parent.removeChild(this.divi);  // XXX null is not an object
        var lauseke = new Lauseke();
        lauseke.liitä(parent);
    }
};

Lauseke.prototype.nimi = function () {
    return this.arg1.nimi()+this.operator+this.arg2.nimi();
};

Lauseke.prototype.htmlksi = function () {
    return this.arg1.nimi()
        +' '+this.operator.replace('>','&lt;')
        +' '+this.arg2.nimi();
};

function uusiLauseke(diviin) {
    var lauseke = new Lauseke();
    lauseke.liitä(diviin);
    return lauseke;
}


function ehtolause(event) {
    var ohj = new Ohjelma(vk);
    ohj.divinä('ehto'+timestring());
    var ihvi = new Ehto(new Lauseke(), ohj);
    ohj = vk.ohjelma();
    // var ask = new Askel();
    alert('ei tee mitään vielä');
}

function ehtolauseLoppu(event) {
    alert('ei tee mitään vielä');
}

function undo() {
    vk.ohjelma().poistaViimeisin();
    vk.ohjelma().aja();
}

function alustaTauno(event) {
    var naytto = haeOlio('naytto');
    var tnimi = '<p class="muuttujan-nimi">t:</p>';
    var prt = parametrit();
    if (prt.ts && prt.ts == "0") tnimi = "";
    if (prt.lang && prt.lang == "en" || prt.help ) {
       wordMuuttujat = "variables:";
       wordUusiMuuttuja = "new variable";
       wordMuuttujatTitle = "list of variables, add varibale from button above";
       wordOhjelma = "program";
       wordApua = "Parameters:\n" +
              "  help=help in english\n"+
              "  apua=apua suomeksi\n"+
              "  lang=en - user interface as english\n"+
              "  s    - simple, no indeces\n"+
              "  mx=v - variable x by value v.\n"+
              "  ix=v - index variable x by value v.\n"+
              "  ts=n - set array size as n.\n"+
            "  t=[x,y,...] - Create array by values x,y,...\n";
    }

    naytto.innerHTML =
        '      <div id="tiedot" class="tiedotd">taulukko: '+
        'int[<span id="taulukon_pituus" '+
        'title="tähän tulee taulukon pituus joskus">...</span>] t = '+
        '        <div id="taulunaytto" class="syottod">'+
        ' '+
        '        </div> '+
        '      </div> '+
        ' '+
        '      <div id="toiminta" class="toiminta"  ondragover="event.preventDefault();" '+
        '         ondragleave="event.preventDefault();" ondragenter="event.preventDefault();"> '+
        '        <div id="vasen" class="vasen">'+
        '          <div class="vasen-top">'+
        '            <div id="muuttuja-button-alue" class="muuttuja-button-alue">'+
        '              '+ wordMuuttujat +
        '                <button id="uusi-muuttuja" onclick="vk.luoMuuttujaKentistä(\'muuttujat\')">'+
        '                  '+ wordUusiMuuttuja +'</button>'+
        '            </div>'+
        '            <div id="uuden-muuttujan-alue" class="uuden-muuttujan-alue">'+
        '                <label>nimi:</label>'+
        '                <input id="uusi-nimi" style="width:6em;" '+
        '                       onkeypress="return vk.uusiNimiKeypress(event,\'muuttujat\')"/>'+
        '                <label>arvo:</label>'+
        '                <input id="uusi-arvo"  style="width:2em;" '+
        '                       onkeypress="return vk.uusiArvoKeypress(event,\'muuttujat\')"/>'+
        '                <button id="uuden-lisays" onclick="vk.teeMuuttuja(\'muuttujat\')">lisää'+
        '                </button>'+
        '            </div>'+
        '            <div id="muuttujat" class="muuttujatd" '+
        '                 title="' + wordMuuttujatTitle + '"' +
        '                 onclick="vk.piilotaMuuttujanLisäys()">'+
        '              <!-- hyi, globaali vk... TODO XXX -->'+
        '            </div>'+
        '          </div>'+
        '          <div class="scratch" id="scratch"> '+
        '            <div class="compound-statements" id="compound-statements" '+
        '                 title="täytä lausekkeen puuttuvat osat raahaamalla, ja raahaa sitten tulos kohteeseen"> '+
        '            </div> '+
        '            <div id="tools" class="tools"> '+
        '            </div> '+
        '          </div> '+
        '        </div> '+
        '        <div id="oikea" class="oikea">' + wordOhjelma + /*': <button onclick="alert(getUserCodeFromTauno());">lähdekoodi</button>'+*/
        '                                               <button onclick="undo();">undo</button>'+
        '        </div> '+
        '      </div> '+
        ' '+
        '      <div id="echo" class="echo" title="Viestialue, johon ohjelma tulostaa viestejä toiminnastaan. Puhdista klikkaamalla."></div> '+
        '';

    if (('help' in prt)||('apua' in prt)) {
        alert(wordApua);
        // 'Parametrit:\n'+
        //      '  mx=v - Luo muuttuja x arvolla v.\n'+
        //      '  ix=v - Luo indeksimuuttuja x arvolla v.\n'+
        //      '  ts=n - Aseta taulukon kooksi n.\n'+
        //      '  t=[x,y,...] - Luo taulukko alkioilla x,y,...\n');
    }

    // vk on globaali... hyi...
    vk = new Virtuaalikone();

    var ohjelma = new Ohjelma(vk);
    var ohjelmadivi = ohjelma.divinä('ohjelma');
    vk.lisääOhjelma(ohjelma);

    vk.luoTaulukko(prt);  // XXX TODO taulukon ja muuttujat voi luoda vasta
    vk.luoMuuttujat(prt); // XXX TODO ohjelman jälkeen... gui-riippuvuus, hyi!

    // HUOMIO HUOMIO
    // Jos alkutilan päivittää, jotta parametrina annetut muuttujat säilyy,
    // tulisi huomioida tämä myös tilan 'resetoinnissa'.
    // Tämä on kuitenkin hassua:
    //  - muuttujat voi generoida nopeasti uudestaan uudelleenlataamalla
    //  - muuttujien luonti on aina ensimmäinen vaihe
    // kannattaa seuraava komento pitää poiskommentoituna.
    // // // vk.päivitäAlkutila();
    // ja jos tuon joskus aktivoi, tulee korjata edellä esitetty ominaisuus.

    vk.lauseke = uusiLauseke('compound-statements');
    var up = new UnOpPlus(vk);
    up.liitä('tools');
    var um = new UnOpMiinus(vk);
    um.liitä('tools');
    var nolla = new Vakio(0, vk);
    nolla.liitä('tools');
    var yksi = new Vakio(1, vk);
    yksi.liitä('tools');
    // var iffi = new Nappi('if-nappi', 'if', ehtolause);
    // iffi.liitä('tools');
    // var endiffi = new Nappi('endif-nappi', 'end if', ehtolauseLoppu);
    // endiffi.liitä('tools');
    if (echo===null) echo = new Echo('echo');
    echo.lisää(navigator.platform+'§ ');
    echo.lisää(navigator.userAgent+'§ ');
    echo.lisää(navigator.appVersion+'§§');
}


if (navigator.userAgent.indexOf('MSIE')>-1 &&
    navigator.userAgent.indexOf('MSIE 10')<0) {
    alert('Ajat liian vanhaa IE:tä tai IE10 on yhteensopivuustilassa. '+
          'Tauno vaatii IE10:n modernina (ei compat view)!');
    if (echo===null) echo = new Echo('echo');
    echo.aseta('IE10 compatibility-näkymässä, sammuta se!');
} else if (typeof QUnit === "undefined") { // Emmehän ole testausympäristössä?

    // latauksen jälkeen ajettava koodi
    window.addEventListener('load', alustaTauno, false);

    // myös tämä pitää määritellä onloadin lisäksi
    window.addEventListener('unload', function (event) {}, false);

} else { /* yes, qunit go go go */ }

