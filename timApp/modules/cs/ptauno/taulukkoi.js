// [[file:~/Sites/taulukko/tauno.org::*Runko][Runko:1]]

// -*- coding: utf-8; tab-width: 4;  -*-

// [[file:~/Sites/taulukko/tauno.org::*Teht%C3%A4v%C3%A4t][todo]]
/**********************************************************************
 ** TODO-kohdat [8/27]
 - [ ] jos vetää solun itseensä tai muuttujan itseensä, ei generoidu koodia (=>cancel)
 - [ ] yhdistä askel ja lause, tee ohjelman ajo 
 - [ ] if end
 - [ ] indeksi ylittyessä lopettaa olemasta indeksi
 - [X] vakiot 0 ja 1
 - [ ] vakionumerot -1 ja n, joka kysyy numeron arvon (raahattavia)
 - [X] indeksoi-metodiin indeksin nimeäminen & indeksin siirto muuttujan arvon mukaan
 - [ ] scratchin tuplaklikkaus (tai vain klikkaus) sotkee softan toiminnan
 - [ ] lisää parametri, joka sallii tiputtaa muuttujien laatikkoon arvoja
 - [ ] muuttujian alustus mahdollista tyhjänä
 - [X] korjaa muuttujien divid-sotku
 - [X] muuttujille olio
 - [X] selvitä muuttujan arvo-attribuutin paikka :)
 - [X] taulukon solun arvon pudotus muuttuja-alueelle
       lisää muuttujan
 - [ ] ohjelmasta oma olio
 - [ ] ohjelman lausekkeet ohjelma-olioon
 - [ ] ohjelman lausekkeille olio
 - [X] echo kuntoon!
 - [X] muuttujan / solun lisääminen ohjelman lausekkeeseen
 - [ ] html-tulostukset vastaavalle oliolle?
 - [ ] generoituvat suorituslauseet ei saa näkyä ensin
 - [ ] if on tila, joka aktivoituu ehtolauseesta,
       jolloin seuraavat tekemiset menee iffin sisään (then osio),
       kunnes käyttäjä lopettaa iffin (ok-nappi tai jotain tilan
       osoittimen luona).
 - [ ] 'if' tilalle 'koska'
 - [ ] silmukat muotoon 'toista kohdasta (pc) kunnes (ehto)'
       -> gotomaisuus
 - [ ] jos muuttujan nimessä on alussa 'int', niin urvelletaan
 - [ ] loput :)
 - [ ] samanakaltaisten lauseiden tunnistus (versioss 98)
 **********************************************************************/
// todo ends here

// [[file:~/Sites/taulukko/tauno.org::*Selainten%20v%C3%A4linen%20yhteensopivuus][selainten-erot]]
var hae_data = function (dt) { alert('hae_data: jotain pahasti vialla'); };
var aseta_data = function (dt,d) { alert('aseta_data: jotain pahasti vialla'); };

// Vesa was here

if ((navigator.appVersion.indexOf('MSIE')>-1) ||
    (navigator.appVersion.indexOf('Trident/7')>-1)) {
    /* MSIE käytössä */
    hae_data = function (event) {
        var dt = event.dataTransfer;
        var d = JSON.parse(dt.getData('text'));
        return d;
    };
    aseta_data = function (event, d) {
        var dt = event.dataTransfer;
        var data = JSON.stringify(d);
        dt.setData('text', data);
    };
} else {
    var webkit_types = function (x) {
        if (x==null) {alert("webkit_types x==null"); debugger; };
        return x.filter(function (x) { return x.indexOf('text/x-')!=-1;});
    };
    var firefox_types = function (x) {
        // XXX Firefoxissa types on stringlist, jolla items, ei map/filter
        var ret = [];
        for (var v=0; v<x.length; ++v)
            ret.push(x.item(v));
        return webkit_types(ret);
    };
    var types = null;
    if (navigator.appVersion.indexOf('MSIE')>-1 // XXX TODO pitääkö tähänkin lisätä Trident/7???
        || navigator.appVersion.indexOf('WebKit')>-1) {
        types = webkit_types;
    } else {
        types = firefox_types;
    }
    /* firefox & webkit */
    hae_data = function (event) {
        var dt = event.dataTransfer;
        var tyypit = types(dt.types);
        var avaimet = tyypit.map(function (x) { return x.substring(7); });
        var ret = {};
        tyypit.map(function (x) { ret[x.substring(7)]=dt.getData(x);});
        return ret;
    };
    aseta_data = function (event,d) {
        var dt = event.dataTransfer;
        
        for (var k in d) {
            dt.setData('text/x-'+k, d[k]);
        }
    };
}
// selainten-erot ends here

// [[file:~/Sites/taulukko/tauno.org::*Yleiset%20apufunktiot][yleiset-apufunktiot]]
// Palauttaa ajan merkkijonona, käytetään id:iden luomisessa.
function timestring() { return (new Date()).getTime().toString(16); }

function hae_olio(olio_tai_id) { // TODO nimeksi hae_elementti???
    /* Hakee olion id:n perusteella,
     * tai jos annettu jo olio, palauttaa sen. */
    var olio = null;
    if (typeof olio_tai_id === 'string')
        olio = document.getElementById(olio_tai_id);
    else if (typeof olio_tai_id === 'object') {
        olio = olio_tai_id;
    }
    return olio;
}

function luo_div(id, attrs) {
    var d = document.createElement('div');
    d['id'] = id;
    for (var attr in attrs) {
        d[attr]=attrs[attr];
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
    
    return tulos;
}

// getPosition source: http://www.kirupa.com/html5/get_element_position_using_javascript.htm
function getPosition(element) {
    var xPosition = 0;
    var yPosition = 0;
    
    while(element) {
        xPosition += (element.offsetLeft - element.scrollLeft + element.clientLeft);
        yPosition += (element.offsetTop - element.scrollTop + element.clientTop);
        element = element.offsetParent;
    }
    return { x: xPosition, y: yPosition };
}

// namespace source: http://elegantcode.com/2011/01/26/basic-javascript-part-8-namespaces/
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
// [[file:~/Sites/taulukko/tauno.org::*Apufunktiot%20isNumeric%20ja%20isNotNumeric][is-numeric-or-not]]
// http://stackoverflow.com/questions/5778020/check-whether-an-input-string-contains-number
function isNumeric(n) {
    return !isNaN(parseInt(n)) && isFinite(n);
}

function isNotNumeric(n) {
    return !isNumeric(n);
}
// is-numeric-or-not ends here

// yleiset-apufunktiot ends here

/* globaali huutelutaulu */
var echo = null;

// TODO ilkeä, ilkeä globaali virtuaalikone
var vk = null;

// [[file:~/Sites/taulukko/tauno.org::*Virtuaalikone][virtuaalikone]]
function Virtuaalikone() {
    this.pc = 0; // TODO täällä vai Ohjelmassa?
    
    this.alkutila = new Tila(new Array(), {});
    this._tila = this.alkutila;
    this._ohjelma = [];
}

Virtuaalikone.prototype.tila = function (uusi_tila) {
    if (typeof uusi_tila !== 'undefined') {
        this._tila = uusi_tila;
    }
    return this._tila;
};

Virtuaalikone.prototype.ohjelma = function () {
    return this._ohjelma[this._ohjelma.length-1];
};

Virtuaalikone.prototype.lisää_ohjelma = function (ohj) {
    this._ohjelma.push(ohj);
    return ohj;
};

Virtuaalikone.prototype.poista_ohjelma = function () {
    return this._ohjelma.pop();
};

Virtuaalikone.prototype.kysy_muuttuja = function(diviin, arvolla) {
    var nimi = prompt("Anna muuttujan nimi:",'');
    if (nimi==null || nimi=='') return null;
    // TODO palauta viesti echo-alueelle, ja laita se echo kuntoon :)
    nimi = nimi.trim();
    
    var validi_nimi = false;
    
    do {
        if (this._tila.muuttujat.hasOwnProperty(nimi)) {
            nimi = prompt("Muuttuja "+nimi+" on jo olemassa, anna uusi nimi:",'');
            if (nimi==null || nimi=='') return null;
            nimi = nimi.trim();
            validi_nimi = false;
        } else  if (nimi != nimi.match('[a-zA-Z_åäöÅÄÖ][a-zA-Z\_åäöÅÄÖ0-9]*')) {
            nimi = prompt("Muuttujan nimi saa sisältää vain kirjaimia ja numeroita,"+
                          "ensin kirjain.  Anna uusi nimi:",'');
            if (nimi==null || nimi=='') return null;
            nimi = nimi.trim();
            validi_nimi = false;
        } else
            validi_nimi = true;
        
    } while (!validi_nimi);
    
    var arvo = null;
    if (typeof arvolla === 'undefined') {
        do {
            arvo = prompt("Anna muuttujan arvo (vain luvut, tai tyhjä, "+
                          "kelpaavat arvoksi):",'');
            if (arvo === null || arvo === '') {
                arvo = null;
            } else {
                arvo = parseInt(arvo);
            }
        } while (arvo!=null && isNotNumeric(arvo));
    } else {
        arvo = arvolla;
    }
    return this.lisää_muuttuja(nimi, arvo, diviin);
};

Virtuaalikone.prototype.lisää_muuttuja = function(nimi, arvo, diviin) {
    if (typeof diviin === "undefined") {
        diviin = hae_olio('muuttujat');
    }
    var muuttuja = this.ohjelma().uusi_muuttuja(nimi, arvo);
    muuttuja.liitä(diviin);
    return muuttuja;
};

// TODO solu ja muuttuja vois olla omia funktioitaan, mutta ehkä pitäs
// TODO ennemmin pohtia niiden taltioimista tilan säilömuuttujissa
Virtuaalikone.prototype.muuttuja = function(nimi) {
    // return this.tila().muuttujat[nimi];
    return hae_olio('muuttuja-'+nimi).olio;
};

Virtuaalikone.prototype.solu = function(indeksi) {
    return hae_olio('solu-'+indeksi).olio;
};

Virtuaalikone.prototype.alkuun = function () {
    this._tila = this.alkutila;
    return this.alkutila;
};


Virtuaalikone.prototype.luo_taulukko = function(parametrit) {
    // TODO Testailua varten
    if (parametrit.t) {
        var alkiot = parametrit.t.split(',');
        alkiot = alkiot.map(function (i) { return parseInt(i); });
        for (var i=0;i<alkiot.length; ++i) {
            this._tila.aseta_indeksiin(i, alkiot[i]); 
        }
    } else {
        var alkioita = parseInt(parametrit.ts) || 6;
        for (var i=0; i<alkioita; ++i)
            this._tila.aseta_indeksiin(i, Math.floor((Math.random()*100)));
    }
    return new Taulukko(this); // XXX tää on niiiiin väärin...
};

Virtuaalikone.prototype.luo_muuttujat = function (parametrit) {
    "use strict";
    var mu = document.getElementById('muuttujat');
    mu.innerHTML = '';
    
    // var muuttuja = null;
    for (var pn in parametrit) {
        if (pn[0]=='m') {
            /* muuttuja = */ this.lisää_muuttuja(pn.substring(1), parseInt(parametrit[pn]));
        } else if (pn[0]=='i') {
            var inro = parseInt(parametrit[pn]);
            var muuttuja = this.lisää_muuttuja(pn.substring(1), inro);
            
            var solu = this.solu(inro);
            var io = new Indeksi(this, solu);
            io.indeksoi(muuttuja);
            io.div.innerHTML = muuttuja.nimi();
            
            solu.div_si.appendChild(io.div);
            solu.indeksimuuttujat.push(io);
            io.siirry();
        }
    }
};



// virtuaalikone ends here
// [[file:~/Sites/taulukko/tauno.org::*Taulukko][taulukko]]
function Taulukko(vk) {
    // with jquery a must: _('#taulukko').empty();
    // $("#taulukko").empty();
    // var ta = document.getElementById('taulukko');
    this.divi = luo_div('taulukko', {
        className: 'taulukko',
        title: 'Taulukollinen numeroita työstettäväksi, raahailepa niitä.',
        innerHTML: ''});
    this.vm = vk;
    this.divi.tila = this.vm.tila(); // ÖÖÖ
    // this.divi.innerHTML = ''; // without jquery
    this.vm.tila().taulukko.olio = this;
    
    var taululause = 'int[] t = {';
    
    var par = parametrit();
    
    for (var i=0; i<this.vm.tila().taulukko.length; ++i) {
        var v = this.vm.tila().taulukko[i];
        taululause += v+', ';
        var nos = new Solu(this.vm, i, v, true);
        this.divi.appendChild(nos.div);
    }
    // Todo Kaaaaamea kludge alla, ton pitäs olla oma lause
    
    taululause = taululause.substring(0,taululause.length-2)+'};';
    var tldiv = luo_div('taulukon_alustus',
                        {className : 'lauseke',
                         title: taululause,
                         innerHTML : taululause});
    var o = document.getElementById('ohjelma');
    o.appendChild(tldiv);
    hae_olio('taulunaytto').appendChild(this.divi);
};
// taulukko ends here
// [[file:~/Sites/taulukko/tauno.org::*Ohjelma][ohjelma]]
function Ohjelma(vk) {
    this.askeleet = new Array();
    this.vk = vk;
    this.pc = 0;
}

Ohjelma.prototype.divinä = function (divid, parentdiv) {
    this.divid = divid;
    this.divi = luo_div(divid, {className: 'ohjelma', title: 'Ohjelma'});
    this.divi.olio = this;
    if (typeof parentdiv === 'undefined') this.liitä('oikea');
    
    return this.divi;
};

Ohjelma.prototype.tekstinä = function (mittee) {
    var ohj = '';
	var sep = "";
    for (var pc=0; pc < this.askeleet.length; ++pc) {
        ohj += sep + this.askeleet[pc].tekstinä(); sep ='\n';
    }
    return ohj;
};



Ohjelma.prototype.aja = function (mittee) {
    this.pc = 0;
    var uusi_tila = this.vk.alkuun();
    var ok = true;
    // this.vk.luo_taulukko(); // XXX luo taulukon ja palauttaa, muttei sijoita sitä talteen... BUGI!
    while (this.pc < this.askeleet.length) {
        ok = this.askeleet[this.pc].sovita(uusi_tila);
        if (!ok) return false;
        uusi_tila = this.askeleet[this.pc].uusi_tila;
        ++this.pc;
    }
    this.vk.tila(uusi_tila);
    return true;
};

Ohjelma.prototype.liitä = function (kooste) {
    var koosteolio = hae_olio(kooste);
    koosteolio.appendChild(this.divi);
};

Ohjelma.prototype.irrota = function (kooste) {
    var koosteolio = hae_olio(kooste);
    koosteolio.removeChild(this.divi);
};


Ohjelma.prototype.tila = function () {
    return this.vk.tila();
};

Ohjelma.prototype.tilaksi = function (uusi_tila) {
    return this.vk.tila(uusi_tila);
};

Ohjelma.prototype.viimeisin_askel = function () {
    return this.askeleet[this.askeleet.length-1];
};

Ohjelma.prototype.poista_viimeisin = function () {
    this.divi.removeChild(this.divi.lastChild);
    return this.askeleet.pop();
};

Ohjelma.prototype.ehto = function (ehto, ohjelma) {
    var e = new Ehto(ehto, ohjelma);
    var ok = e.sovita(this.tila());  // TODO entä jos ei onnistu?
    var uusi_tila = e.uusi_tila;
    var a = new Askel(e, uusi_tila, this.viimeisin_askel());
    var vanha_tila = this.tila();
    this.tilaksi(uusi_tila);
    this.askeleet.push(a);

    this.lisaaNakyviin(a.divinä());
};


Ohjelma.prototype.lisaaNakyviin = function (uusiDiv) {
    var ohj=this.divi;
    ohj.appendChild(uusiDiv);
    ohj.scrollTop = uusiDiv.offsetTop;  
}


Ohjelma.prototype.sijoitus = function (mihin, mistä) {
    var s = new Sijoitus(mihin, mistä, this.viimeisin_askel());
    var ok = s.sovita(this.tila());  // TODO entä jos ei onnistu?
    // var a = new Askel(s, uusi_tila, this.viimeisin_askel());
    var vanha_tila = this.tila();
    this.tilaksi(s.uusi_tila);
    this.askeleet.push(s);
    
    this.lisaaNakyviin(s.divinä());
};

Ohjelma.prototype.unplus = function (mihin, määrä) {
    var up = new UnaariPlus(mihin, määrä, this.viimeisin_askel());
    var ok = up.sovita(this.tila());  // TODO entä jos ei onnistu?;
    var uusi_tila = up.uusi_tila;
    // var a = new Askel(up, uusi_tila, this.viimeisin_askel());
    var vanha_tila = this.tila();
    this.tilaksi(uusi_tila);
    this.askeleet.push(up);
    
    this.lisaaNakyviin(up.divinä());
};

Ohjelma.prototype.unmiinus = function (mihin, määrä) {
    var um = new UnaariMiinus(mihin, määrä, this.viimeisin_askel());
    var ok = um.sovita(this.tila());  // TODO entä jos ei onnistu?
    var uusi_tila = um.uusi_tila;
    // var a = new Askel(um, uusi_tila, this.viimeisin_askel());
    var vanha_tila = this.tila();
    this.tilaksi(uusi_tila);
    this.askeleet.push(um);
    
    this.lisaaNakyviin(um.divinä());
};


Ohjelma.prototype.uusi_muuttuja = function (nimi, arvo) {
    var lm = new LuoMuuttuja(nimi, arvo);
    var ok = lm.sovita(this.tila());
    var uusi_tila = lm.uusi_tila;
    // var a = new Askel(lm, uusi_tila, this.viimeisin_askel());
    var vanha_tila = this._tila;
    this.tilaksi(uusi_tila);
    this.askeleet.push(lm);
    
    this.lisaaNakyviin(lm.divinä());
    
    var muuttuja = new Muuttuja(this, nimi, arvo, true);
    return muuttuja;
};
// ohjelma ends here
// [[file:~/Sites/taulukko/tauno.org::*Askel][askel]]
function Askel(lause, tila, edellinen) {
    this.lause = lause;
    this.tila = tila;
    this.edellinen_askel = edellinen;
}

Askel.prototype.divinä = function () {
    return this.lause.divinä(this.tila); // TODO tartteeko arg.ina vanhan tilan
};

Askel.prototype.tekstinä = function () {
    return this.lause.tekstinä(this.tila); // TODO kts. Askel.divinä
};
// askel ends here
// [[file:~/Sites/taulukko/tauno.org::*Lause][lause]]
function Lause(edellinen) {
    this._arvo = null;
    this.divi = null;
    this.tila = null;
    this.uusi_tila = null;
    this.edellinen_askel = edellinen;
}

Lause.prototype.arvo = function() {
    return this._arvo;
};

Lause.prototype.suorita = function (tila) {
    this.tila = tila;
    this.uusi_tila = tila;
    alert('aliolio toteuttaa!');
    return tila;
};

Lause.prototype.divinä = function () {
    this.divi = luo_div('lause'+this.arvo().toString(),
                        {'className': "lause",
                         'title': this.arvo().toString(),
                         'innerHTML': this.arvo().toString()});
    return this.divi;
};

Lause.prototype.tekstinä = function () {
    return this.arvo().toString();
};
// lause ends here
// [[file:~/Sites/taulukko/tauno.org::*Lohko][lohko]]
function Lohko(ehto, edellinen) {
    this.ehto = ehto;
    this.lauseet = new Array();
    this.edellinen_askel = edellinen;
}

Lohko.prototype.arvo = function () {
    return this.mistä.arvo();
};

Lohko.prototype = new Lause();
Lohko.prototype.constructor = Lohko;

Lohko.prototype.lisää_lause = function(lause) {
    this.lauseet.push(lause);
};

Lohko.prototype.poista_lause = function(lause) {
    return this.lauseet.pop(lause);
};

Lohko.prototype.aseta_ehto = function(ehto) {
    this.ehto = ehto;
};

Lohko.prototype.sovita = function(tila) {
    this.tila = tila ? tila : vk.tila();
    this.uusi_tila = tila.klooni();
    for (var l in this.lauseet) {
        var ok = l.sovita(this.uusi_tila);
        this.uusi_tila = l.uusi_tila;
    };
    return true; // this.uusi_tila;
};
// lohko ends here
// [[file:~/Sites/taulukko/tauno.org::*Sijoitus][sijoitus]]
function Sijoitus(mihin, mistä, edellinen) {
    this.mihin = mihin;
    this.mistä = mistä;
    this.edellinen_askel = edellinen;
}

Sijoitus.prototype.arvo = function () {
    return this.mistä.arvo();
};

Sijoitus.prototype = new Lause();
Sijoitus.prototype.constructor = Sijoitus;

Sijoitus.prototype.sovita = function (tila) {
    this.uusi_tila = tila.klooni();
    var arvo = this.mistä.arvo(this.uusi_tila);  // TODO pitäs huomioida, jos ei onnistu
    this.mihin.arvoksi(arvo, this.uusi_tila);
    return true; // this.uusi_tila;
};

Sijoitus.prototype.divinä = function (tila) {
    this.divi = luo_div('sijoitus'+this.mihin.nimi()
                        +this.mistä.nimi()+this.mistä.arvo(tila), {
                            className: "lause sijoitus",
                            title: this.mihin.nimi()+' = '+this.mistä.arvo(tila),
                            innerHTML: this.mihin.nimi()+' = '+ this.mistä.htmlksi(tila) +';<br/>'});
    return this.divi;
};

Sijoitus.prototype.tekstinä = function (tila) {
    return this.mihin.htmlksi(tila)+' = '+this.mistä.htmlksi(tila)+'; // '+this.mihin.nimi()+' = '+this.mistä.arvo(tila);
};
// sijoitus ends here
// [[file:~/Sites/taulukko/tauno.org::*Ehto][ehto]]
function Ehto(ehto, ohjelma, edellinen) {
    this.ehto = ehto;
    this._ohjelma = ohjelma;
    this.edellinen_askel = edellinen;
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
    this.uusi_tila = tila.klooni();
    // jos this.ehto tosi
    // if (this.ehto.sovita(this.uusi_tila))
    //   sovita ohjelma tilaan
    //   this.uusi_tila = this.ohjelma.sovita(this.uusi_tila);
    // muuten vanha tila voimassa
    return true; // this.uusi_tila;
};

Ehto.prototype.divinä = function (tila) {
    this.divi = luo_div('ehto'+this.ehto.nimi(),
                        {'className': "lause",
                         'title': this.ehto.nimi() });
    this.divi.appendChild(this.ohjelma.divi);
    return this.divi;
};

// ehto ends here
// [[file:~/Sites/taulukko/tauno.org::*Lis%C3%A4ys%20ja%20v%C3%A4hennys][lisäys-vähennys]]
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
    this.uusi_tila = tila.klooni();
    var arvo = this.mihin.arvo(tila);
    arvo += this.määrä.arvo();
    this.mihin.arvoksi(arvo, this.uusi_tila);
    return true; // this.uusi_tila;
};

UnaariPlus.prototype.divinä = function (tila) {
    this.divi = luo_div('uplus'+this.mihin.nimi()+this.mihin.arvo(tila), {
        className: "lause unaari",
        title: this.tekstinä(tila),
        innerHTML: this.mihin.nimi()+' += '+ this.määrä.arvo() +';<br/>'});
    return this.divi;
};

UnaariPlus.prototype.tekstinä = function (tila) {
    return  this.mihin.nimi()+' += ' + this.määrä.arvo() +";";
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
    this.uusi_tila = tila.klooni();
    var arvo = this.mihin.arvo(tila);
    arvo -= this.määrä.arvo();
    this.mihin.arvoksi(arvo, this.uusi_tila);
    return true; // this.uusi_tila;
};

UnaariMiinus.prototype.divinä = function (tila) {
    this.divi = luo_div('umiinus'+this.mihin.nimi()+this.mihin.arvo(tila),
                        {'className': "lause unaari",
                         'title': this.tekstinä(tila),
                         'innerHTML': this.mihin.nimi()+' -= '+ this.määrä.arvo() +';<br/>'});
    return this.divi;
};

UnaariMiinus.prototype.tekstinä = function (tila) {
    return  this.mihin.nimi()+' -= ' + this.määrä.arvo() + ";";
};
// lisäys-vähennys ends here
// [[file:~/Sites/taulukko/tauno.org::*Muuttujan%20luonti][muuttujan-luonti]]
function LuoMuuttuja(nimi, arvo) {
    this.nimi = nimi;
    this._arvo = arvo;
}

LuoMuuttuja.prototype.arvo = function () {
    return this._arvo;
};

LuoMuuttuja.prototype = new Lause();
LuoMuuttuja.prototype.constructor = LuoMuuttuja;

LuoMuuttuja.prototype.sovita = function (tila) {
    this.uusi_tila = tila.klooni();
    this.uusi_tila.muuttujat[this.nimi] = this._arvo;
    return true; // this.uusi_tila;
};

LuoMuuttuja.prototype.divinä = function (tila) {
    var lause = this.tekstinä(tila);
    this.divi = luo_div('luo_muuttuja'+this.nimi+this._arvo,
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
// muuttujan-luonti ends here
// [[file:~/Sites/taulukko/tauno.org::*Lukujen%20esitys][luku]]
function Luku(arvo) {
    this.sisältö = arvo;
}

Luku.prototype.arvo = function (tila) {
    return this.sisältö;
};

Luku.prototype.nimi = function () {
    return this.sisältö.toString();
};

Luku.prototype.divinä = function (tila) {
    if (!this.divi)
        this.divi = luo_div('', { 'className': 'luku',
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
// luku ends here
// [[file:~/Sites/taulukko/tauno.org::*Virtuaalikoneen%20tila][tila]]
function Tila(taulukko, muuttujat) {
        this.taulukko = taulukko;
        this.muuttujat = muuttujat;
}

Tila.prototype.klooni = function() {
    // Tämän pitäisi olla tarpeeksi syvä kopio,
    // sillä yksittäiset alkiot ovat (tai pitäs olla)
    // muuttumattomia.
    var uusi_taulukko = this.taulukko.slice(0);
    uusi_taulukko.olio = this.taulukko.olio;

    var uudet_muuttujat = this.muuttujat.constructor();
    for (var attr in this.muuttujat)
        if (this.muuttujat.hasOwnProperty(attr))
            uudet_muuttujat[attr] = this.muuttujat[attr];
    return new Tila(uusi_taulukko, uudet_muuttujat);
};

Tila.prototype.aseta_indeksiin = function(i, v) {
    this.taulukko[i]=v;
};

Tila.prototype.aseta_muuttuja = function(n, v) {
    this.muuttujat[n]=v;
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
    this.taulukko[this.muuttujat[ki]] = this.muuttujat[k];
};

// taulukosta muuttujan ki arvon mukaisesta indeksistä muuttujaan k
Tila.prototype.indeksista = function(ki, k) {
    this.muuttujat[k] = this.taulukko[this.muuttujat[ki]];
};
// tila ends here
// [[file:~/Sites/taulukko/tauno.org::*Solu][solu]]
function Solu(vm, indeksi, arvo, draggable) {
    var divid = 'solu-'+indeksi;
    this.divid = divid;
    this.div = luo_div(divid,
                       {'className':'solu'});
    this.div_sn = luo_div('solu-n'+indeksi,
                          {'className':'solu-n',
                           'innerHTML':indeksi.toString()});
    this.div_sa = luo_div('solu-a'+indeksi,
                          {'className':'solu-a',
                           'draggable':true});
    
    this.div_si = luo_div('solu-i'+indeksi,
                          {'className':'solu-i',
                           'draggable':false,
                           'innerHTML':''});

    this.indeksimuuttujat = [];

    this.sulje();

    this.kuuntelijat = [];

    this.vm = vm;
    this.indeksi = indeksi;
    this.div.olio = this;
    this.div_sa.olio = this;
    this.div_si.olio = this;
    this.arvoksi(arvo||0);

    this.div.solu = this;    // TODO tarvitaanko näitä .soluja?
    this.div_sa.solu = this;
    this.div_si.solu = this;

    this.div_sa.addEventListener('dragover', function (event) {
        if (this.olio.suljettu()) return true;
        event.preventDefault();
        event.dataTransfer.effectAllowed = 'copy';
        return false;
    }, false);
    this.div_sa.addEventListener('dragenter', function (event) {
        if (this.olio.suljettu()) return true;
        event.preventDefault();
        event.dataTransfer.effectAllowed = 'copy';
        return false;
    }, false);

    this.div_sa.addEventListener('drop', function (event) {
        if (this.olio.suljettu()) return true;
        
        if (this.olio.div_si.children.length>1) {
            event.preventDefault();
            event.stopPropagation();
            alert('Solulla on useampia indeksejä, joten raahaa indeksiin valitaksesi sopivan kohteen.');
            return true;
        }
        
        var ev=event;
        ev.preventDefault();
        var data = hae_data(ev);
        var tyyppi = data.tyyppi;
        var oid = data.oid;
        var olio = hae_olio(oid).olio;
        var mihin = ev.target.olio;
        var mistä = olio;
        var ohj = document.getElementById('ohjelma').olio;
        
        if (tyyppi == "muuttuja" || tyyppi == "solu" ||
            tyyppi === 'indeksi' || tyyppi === 'vakio') {
            event.dataTransfer.dropEffect = 'copy';
            
            ohj.sijoitus(mihin, mistä);
            return false;
        } else if (tyyppi=="lauseke") {
            var lausekediv = hae_olio(oid);
            if (ev.stopPropagation) ev.stopPropagation();
            event.dataTransfer.dropEffect = 'copy';
            
            ohj.sijoitus(mihin, mistä);
            lausekediv.olio.katoa();
            ev.target.style.background = ev.target.oldbg;
            return false;
        } else if (tyyppi==='unop') {
            if (ev.stopPropagation) ev.stopPropagation();
            event.dataTransfer.dropEffecct = 'copy';
            if (data.nimi === 'miinus')
                ohj.unmiinus(mihin);
            else if (data.nimi === 'plus')
                ohj.unplus(mihin);
            return false;
        }
        return true;
    }, false);
    this.div_sa.addEventListener('dragstart', function (event) {
        if (this.olio.suljettu()) {
            event.preventDefault();
            event.stopPropagation();
            return true;
        }
        if (this.olio.div_si.children.length>1) {
            event.preventDefault();
            event.stopPropagation();
            alert('Solulla on useampia indeksejä, joten raahaa indekseistä valitaksesi sopivan.');
            return true;
        }
        var ev=event;
        aseta_data(ev, {
            tyyppi:'solu',
            indeksi: ev.target.solu.indeksi.toString(),
            oid:ev.target.id});
        ev.dataTransfer.effectAllowed = 'copy';
        return false;
    }, false);
    add_drag_drop_new(this.div_si, {

        // 'dragover': function (event) {
        //     event.preventDefault();
        //     alert('asdf');
        //     event.dataTransfer.effectAllowed = 'copy';
        //     return false;
        // },
        // 'dragenter': function (event) {
        //     event.preventDefault();
        //     event.dataTransfer.effectAllowed = 'copy';
        //     return false;
        // },
        
        'drop': function (event) {
            //if (this.olio.suljettu()) return;
            var ev=event;
            ev.preventDefault();
            var data = hae_data(ev);
            var tyyppi = data.tyyppi;
            var oid = data.oid;
            var olio = hae_olio(oid).olio;
            var mihin = ev.target.olio;
            var mistä = olio;
            var ohj = document.getElementById('ohjelma').olio;
            
            if (mihin.constructor === mistä.constructor) {
                if (event.stopPropagation) event.stopPropagation();
                return false;
            }
            
            if (tyyppi==='indeksi') {
                var nimi  = data.nimi;
                var kohde = mistä;
                
                if (event.stopPropagation) event.stopPropagation();
                
                var erotus = event.target.olio.indeksi - olio.div.solu.indeksi;
                
                if (erotus<0) {
                    ohj.unmiinus(kohde.muuttuja, -erotus);
                } else {
                    ohj.unplus(kohde.muuttuja, erotus);
                }
                
                return false;
            } else if (tyyppi==='muuttuja') {
                var indeksi = mistä.arvo() === null ? mihin.indeksi : mistä.arvo();
                var luotu_muuttuja = false;
                
                if ((indeksi < 0) || (indeksi >= vm.tila().taulukko.length)) return true;
                
                if (mistä.arvo() === null) {
                    mistä.arvoksi(mihin.indeksi);
                    luotu_muuttuja = true;
                }
                
                var io = new Indeksi(vm, mihin);
                
                io.indeksoi(mistä);
                io.div.innerHTML = mistä.nimi();
                
                mihin.div_si.appendChild(io.div);
                mihin.indeksimuuttujat.push(io);
                io.siirry();
                
                if (luotu_muuttuja) {
                    ohj.sijoitus(mistä, new Luku(mihin.indeksi));
                } else {
                    var f = io.div.solu.indeksimuuttujat.filter(
                        function (x) {
                            return x.muuttuja.nimi()===mistä.nimi();
                        });
                    if (f.length>1) {
                        io.div.solu.indeksimuuttujat =
                            io.div.solu.indeksimuuttujat.filter(function (x) { return x !==io; });
                        io.div.solu.div_si.removeChild(io.div);
                    }
                }
                return false;
            }
            return true;
        }});
    
    this.div.appendChild(this.div_sn);
    this.div.appendChild(this.div_sa);
    this.div.appendChild(this.div_si);
}

Solu.prototype.sido_indeksi = function(i) {
    this.kuuntelijat.push(i);
};

Solu.prototype.vapauta_indeksi = function (i) {
    this.kuuntelijat = this.kuuntelijat.filter(
        function (x) { return x !== i; });
};

Solu.prototype.päivitä_indeksit = function (tila) {
    var _tila = tila || this.vm.tila();
    this.kuuntelijat.map(
        function (x) { x.muuttuja_päivitetty(this, _tila); });
};


Solu.prototype.arvoksi = function (x, tila) {
    tila = tila ? tila : this.vm.tila();
    tila.aseta_indeksiin(this.indeksi,  x);  // XXX ZZZ TODO vai aseta_indeksiin
    this.div_sa.innerHTML = x.toString();
    
    return tila.indeksissa(this.indeksi);
};

Solu.prototype.arvo = function (tila) {
    return tila ? tila.indeksissa(this.indeksi) : this.vm.tila().indeksissa(this.indeksi);
};

Solu.prototype.nimi = function (r) { // TODO XXX vuoden kludge, joka estää rekursion
    if ((this.indeksimuuttujat.length != 0) && (r!==this))
        return this.indeksimuuttujat[0].nimi(this);
    return 't['+this.indeksi+']';  // katso Indeksi.prototype.nimi
};

Solu.prototype.sovita = function (tila) {
    return true; // tila;  // XXX järki?
};

Solu.prototype.htmlksi = function () {
    return this.nimi();
};

Solu.prototype.suljettu = function () {
    var sulj = (this.div_si.innerHTML === '');
    // käy läpi lapset ja kutsu niille validi()
    
    var validi = false;
    if (!sulj) {
        for (var i = this.div_si.firstChild; i !== null; i = i.nextSibling)
            validi = i.olio.validi() || validi;
    }
    return sulj || !validi;
};

Solu.prototype.avaa = function () {
    if (!this.suljettu()) {
        this.div_sa.style.background = '#fdf6e3';
        // this.draggable = true;
        }
};

Solu.prototype.sulje = function () {
    if (this.suljettu()) {
        // this.draggable = false;
        this.div_sa.style.background = '#444';
    }
};
// solu ends here
// [[file:~/Sites/taulukko/tauno.org::*Indeksi][indeksi]]
function Indeksi(vm, solu) {
    var divid = 'indeksi'+timestring();
    this.divid = divid;
    this.div = luo_div(divid, {
        'className':'indeksi',
        'draggable': true,
        'title': 'raahaa tähän muuttuja, niin voit viitata taulukon alkioihin\n' +
            'muuttujan arvon muuttaminen tai tämän raahaaminen siirtää viittausta',
        'innerHTML':'&nbsp;'});
    this.vm = vm;
    this.muuttuja = null;
    this.div.olio = this;
    
    this.div.solu = solu;
    
    // Alla oleva mallina tulevalle
    add_drag_drop_new(this.div, {
        'dragstart': function (event) {
            var ev=event;
            aseta_data(ev, {tyyppi: 'indeksi',
                            indeksi: ev.target.solu.indeksi.toString(),
                            oid:ev.target.id});
            ev.dataTransfer.effectAllowed = 'copy';
        },
        
        'dragover': function (event) {
            event.preventDefault();
            event.dataTransfer.effectAllowed = 'copy';
            return false;
        },
        'dragenter': function (event) {
            event.preventDefault();
            event.dataTransfer.effectAllowed = 'copy';
            return false;
        },
        
        'drop': function (event) {
            var ev=event;
            ev.preventDefault();
            var data = hae_data(ev);
            var tyyppi = data.tyyppi;
            var oid = data.oid;
            var olio = hae_olio(oid).olio;
            var mihin = ev.target.olio;
            var mistä = olio;
            var ohj = document.getElementById('ohjelma').olio;
            
            if (tyyppi == "muuttuja") {
                if (ev.stopPropagation) ev.stopPropagation();
                event.dataTransfer.dropEffect = 'copy';
                if (mihin.div.solu.div_si.children.length===0) {
                    mihin.indeksoi(mistä);
                    mihin.div.innerHTML = mistä.nimi(); //
                    // TODO tänne seuraava lause &
                    // indeksin siirto muuttujan arvon mukaan
                    mihin.siirry();
                    // ohj.sijoitus(mihin, mistä);
                } else {
                    ohj.sijoitus(mihin, mistä);
                }
                ev.target.style.background = ev.target.oldbg;
                return false;
            } else if (tyyppi=="indeksi" || tyyppi == "solu" ||
                       tyyppi === "vakio" ) {
                if (ev.stopPropagation) ev.stopPropagation();
                event.dataTransfer.dropEffect = 'copy';
                
                ohj.sijoitus(mihin, mistä);
                ev.target.style.background = ev.target.oldbg;
                return false;
            } else if (tyyppi=="lauseke") {
                var lausekediv = hae_olio(oid);
                if (ev.stopPropagation) ev.stopPropagation();
                event.dataTransfer.dropEffect = 'copy';
                
                ohj.sijoitus(mihin, mistä);
                lausekediv.olio.katoa();
                ev.target.style.background = ev.target.oldbg;
                return false;
            }
            return true;
        }});
}

Indeksi.prototype.validi = function (tila) {
    if ((typeof this.muuttuja === 'undefined') ||
        (this.muuttuja === null))
        return false;
    
    var taulukko = tila ? tila.taulukko : this.vm.tila().taulukko;
    var ind = this.muuttuja.arvo(tila);
    
    if ((ind < 0) || (ind >= taulukko.length)) return false;
    
    return true;
};

Indeksi.prototype.muuttuja_päivitetty = function (m, tila) {
    this.siirry(tila);
};

Indeksi.prototype.arvoksi = function (x, tila) {
    tila = tila ? tila : this.vm.tila();
    if (!this.validi(tila)) return null;
    
    var ind = this.muuttuja.arvo(tila);
    // var solu = document.getElementById('solu'+ind).solu;
    // var solu = document.getElementById('solu'+ind).olio;
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
    // var s = document.getElementById('solu'+ind).olio;
    var s = this.vm.solu(ind);
    var d = this.div.solu;
    var ti = this;
    
    d.div_si.removeChild(this.div);
    d.indeksimuuttujat =
        d.indeksimuuttujat.filter(function (x) { return x !== ti; });
    
    s.div_si.appendChild(this.div);
    s.indeksimuuttujat.push(this);
    
    s.avaa(); // div_sa.style.background = '#fdf6e3';
    d.sulje(); // div_sa.style.background = 'black';
    
    this.div.solu = s;
};

Indeksi.prototype.indeksoi = function (muuttuja, tila) {
    //if (!this.validi(tila)) return;
    if (this.muuttuja === muuttuja) return;
    
    if (this.muuttuja !== null)
        this.muuttuja.vapauta_indeksi(this);
    this.muuttuja = muuttuja;
    this.muuttuja.sido_indeksi(this);
};

Indeksi.prototype.päivitä = function (tila) {
    var ind = this.muuttuja.arvo(tila);
    // var solu = document.getElementById('solu'+ind);
    var solu = this.vm.solu(ind);
    // var solu_pos = getPosition(solu.div);
    // this.div.style.top = solu_pos.y + 'px';  // TODO + tämän divin korkeus (ainakin)
    // this.div.style.left = solu_pos.x + 'px'; // TODO tarvitseeko lisätä jotain?
};

Indeksi.prototype.nimi = function (r) {
    // XXX TODO vuoden kludge, joka estää rekursion
    if (this.muuttuja === null) return '?';
    return 't['+this.muuttuja.nimi()+']'; // zzz
    if (typeof r === 'undefined')
        return this.muuttuja.nimi();
    return 't['+this.muuttuja.nimi(r)+']';  // katso Solu.prototype.nimi
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
// indeksi ends here
// [[file:~/Sites/taulukko/tauno.org::*Vakio][vakio]]
function Vakio(value, vm, draggable) {
    var divid = 'vakio'+value;
    this.divid = divid;
    this.vm = vm;
    this.value = value;

    this.divi = luo_div(this.divid, {
        className: 'const',
        title: 'raahaan tämä arvoon, tai tähän arvo, muuttaaksesi sen arvoksi '
            + this.value,
        'draggable':draggable});
    this.divi.innerHTML = '_&larr;'+this.value.toString();
    
    add_drag_drop_new(this.divi, {
        'dragstart': function (event) {
            aseta_data(event, {tyyppi: 'vakio',
                               nimi: value.toString(),
                               oid: divid});
            event.dataTransfer.effectAllowed = 'copy'; // TODO vai link?
        },
        'drop': function (event) {
            var data = hae_data(event);
            var tyyppi = data.tyyppi;
            var nimi  = data.nimi;
            var oid = data.oid;
            var ohj = document.getElementById('ohjelma').olio;
            
            var olio = hae_olio(oid).olio;
            var mihin = event.target.olio;
            var mistä = olio;
            
            var kohde = mistä;
            
            if ((tyyppi === 'muuttuja') || (tyyppi === 'solu')
                || (tyyppi === 'indeksi')) {
                if (event.stopPropagation) event.stopPropagation();
                
                var luku = new Luku(value);
                ohj.sijoitus(kohde, luku);
                
                // if (this.nimi==='miinus')
                //     ohj.unmiinus(mihin);
                // else if (this.nimi==='plus')
                //     ohj.unplus(mihin);
                
                event.dataTransfer.dropEffect = 'copy';
                event.target.style.background = '#fdf6e3';
                
                return false;
            }
            return true;
        }});
    
    this.divi.olio = this;
};

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
    var koosteolio = hae_olio(kooste);
    koosteolio.appendChild(this.divi);
};

Vakio.prototype.irrota = function (kooste) {
    var koosteolio = hae_olio(kooste);
    koosteolio.removeChild(this.divi);
};
// vakio ends here
// [[file:~/Sites/taulukko/tauno.org::*UnOpPlus%20-Minus][plus-minus-ui]]
function UnOpPlus(vm, draggable) {
    var divid = 'unopplus-'+timestring();
    this.divid = divid;
    this.vm = vm;
    
    this.divi = luo_div(this.divid, {
        className: 'unop',
        title: 'raahaan tämä arvoon, tai tähän arvo, kasvattaaksesi arvoa yhdellä',
        'draggable':draggable});
    this.divi.innerHTML ='_+1';
    
    add_drag_drop_new(this.divi, {
        'dragstart': function (event) {
            aseta_data(event, {tyyppi: 'unop',
                               nimi: 'plus',
                               oid: divid});
            event.dataTransfer.effectAllowed = 'copy'; // TODO vai link?
        },
        'drop': function (event) {
            var data = hae_data(event);
            var tyyppi = data.tyyppi;
            var nimi  = data.nimi;
            var oid = data.oid;
            var ohj = document.getElementById('ohjelma').olio;
            
            var olio = hae_olio(oid).olio;
            var mihin = event.target.olio;
            var mistä = olio;
            
            var kohde = mistä;
            
            if ((tyyppi === 'muuttuja') || (tyyppi === 'solu')) {
                if (event.stopPropagation) event.stopPropagation();
                
                var lauseke = new Lauseke();
                lauseke.aseta_oper('+');
                lauseke.aseta_arg1(kohde);
                lauseke.aseta_arg2(new Luku(1));
                
                ohj.sijoitus(kohde, lauseke);
                
                // if (this.nimi==='miinus')
                //     ohj.unmiinus(mihin);
                // else if (this.nimi==='plus')
                //     ohj.unplus(mihin);
                
                event.dataTransfer.dropEffect = 'copy';
                event.target.style.background = '#fdf6e3';
                
                return false;
            }
            return true;
        }});
    
    this.divi.olio = this;
};

UnOpPlus.prototype.liitä = function (kooste) {
    var koosteolio = hae_olio(kooste);
    koosteolio.appendChild(this.divi);
};

UnOpPlus.prototype.irrota = function (kooste) {
    var koosteolio = hae_olio(kooste);
    koosteolio.removeChild(this.divi);
};

function UnOpMiinus(vm, draggable) {
    var divid = 'unopmiinus-'+timestring();
    this.divid = divid;
    this.vm = vm;
    
    this.divi = luo_div(this.divid, {
        className: 'unop',
        title:'raahaa tähän arvo, tai tämä arvoon, vähentääksesi arvoa yhdellä',
        'draggable':draggable});
    this.divi.innerHTML ='_-1';
    
    add_drag_drop_new(this.divi, {
        'dragstart': function (event) {
            aseta_data(event, {tyyppi: 'unop',
                               nimi: 'miinus',
                               oid: divid});
            event.dataTransfer.effectAllowed = 'copy'; // TODO vai link?
        },
        'drop': function (event) {
            var data = hae_data(event);
            var tyyppi = data.tyyppi;
            var nimi  = data.nimi;
            var oid = data.oid;
            var ohj = document.getElementById('ohjelma').olio;
            
            var olio = hae_olio(oid).olio;
            var mihin = event.target.olio;
            var mistä = olio;
            
            var kohde = mistä;
            
            if ((tyyppi === 'muuttuja') || (tyyppi === 'solu')) {
                if (event.stopPropagation) event.stopPropagation();
                
                var lauseke = new Lauseke();
                lauseke.aseta_oper('-');
                lauseke.aseta_arg1(kohde);
                lauseke.aseta_arg2(new Luku(1));
                
                ohj.sijoitus(kohde, lauseke);
                
                // if (this.nimi==='miinus')
                //     ohj.unmiinus(mihin);
                // else if (this.nimi==='plus')
                //     ohj.unplus(mihin);
                
                event.dataTransfer.dropEffect = 'copy';
                event.target.style.background = '#fdf6e3';
                
                return false;
            }
            return true;
            
        }});
    
    this.divi.olio = this;
};

UnOpMiinus.prototype.liitä = function (kooste) {
    var koosteolio = hae_olio(kooste);
    koosteolio.appendChild(this.divi);
};

UnOpMiinus.prototype.irrota = function (kooste) {
    var koosteolio = hae_olio(kooste);
    koosteolio.removeChild(this.divi);
};
// plus-minus-ui ends here
// [[file:~/Sites/taulukko/tauno.org::*Muuttujat][muuttujat]]
function pudota_muuttujaan(event) {
    var data = hae_data(event);
    var tyyppi = data.tyyppi;
    var nimi  = data.nimi;
    var oid = data.oid;
    var ohj = document.getElementById('ohjelma').olio;
    
    var olio = hae_olio(oid).olio;
    var mihin = event.target.olio;
    var mistä = olio;
    
    event.dataTransfer.dropEffect = 'copy';
    
    if ((tyyppi === "solu") || (tyyppi=== 'indeksi') || (tyyppi=== 'vakio') ||
        ((tyyppi === "muuttuja") && (nimi != event.target.nimi))) {
        
        if (event.stopPropagation) event.stopPropagation();
        var v = olio.arvo();
        
        ohj.sijoitus(mihin, mistä);
        event.target.style.background = "#fdf6e3"; // event.target.oldbg;
        //event.stopPropagation();
        return false;
    } else if (tyyppi==="lauseke") {
        var lausekediv = hae_olio(oid);
        //event.target.innerHTML = lausekediv.olio.arvo();
        //lausekediv.olio.liitä(event.target);  // XXXXX
        //event.target.olio.arvoksi(lausekediv.olio.arvo());
        // lausekediv.olio.katoa(); // XXXXX
        if (event.stopPropagation) event.stopPropagation();
        
        ohj.sijoitus(mihin, mistä);
        lausekediv.olio.katoa();
        event.target.style.background = "#fdf6e3"; // event.target.oldbg;
        return false;
    } else if (tyyppi == 'unop') {
        if (event.stopPropagation) event.stopPropagation();
        
        if (nimi==='miinus')
            ohj.unmiinus(mihin);
        else if (nimi==='plus')
            ohj.unplus(mihin);
        
        event.target.style.background = '#fdf6e3';
        return false;
    }
    
    return true;
};

function Muuttuja(vm, nimi, arvo, draggable) {
    var divid = 'muuttuja-'+nimi;
    this.divid = divid;
    this.vm = vm;
    this._nimi = nimi;
    
    this.kuuntelijat = [];
    
    this.divi = luo_div(this.divid+'-b', {'draggable':false});
    
    var no = luo_div(this.divid,
                     {'className': 'muuttuja',
                      'draggable': false,
                      'innerHTML': nimi+':'});
    
    var ma = luo_div(this.divid+'-a',
                     {'className': 'muuttuja-arvo',
                      'draggable': (draggable||false)});
    
    ma.innerHTML = arvo!=null ? arvo.toString() : '&nbsp;&nbsp;';
    ma.arvo = arvo;
    ma.nimi = nimi;
    
    no.addEventListener('dragstart', function (event) {
        event.preventDefault();
        return false;
    },false);
    
    add_drag_drop_new(ma,
                      {dragstart:  function (event) {
                          var ev=event;
                          aseta_data(ev, {tyyppi: "muuttuja",
                                          nimi: ev.target.olio.nimi(),
                                          oid: divid});
                          ev.dataTransfer.effectAllowed = 'copy';
                      },
                       dragenter: function (event) {
                           ma.oldbg = ma.style.background;
                           ma.style.background = '#839496';
                       },
                       dragleave: function (event) {
                           ma.style.background = "#fdf6e3"; // ma.oldbg;
                       },
                       drop: pudota_muuttujaan});
    
    ma.muuttujat = this.vm.tila().muuttujat; // XXX ei toimi näin
    ma.nimi = nimi;
    
    this.divi.appendChild(no);
    this.divi.appendChild(ma);
    ma.olio = this;
    no.olio = this;
    this.divi_arvo = ma;
    
    this.arvoksi(arvo);
}

Muuttuja.prototype.sido_indeksi = function(i) {
    this.kuuntelijat.push(i);
};

Muuttuja.prototype.vapauta_indeksi = function (i) {
    this.kuuntelijat = this.kuuntelijat.filter(
        function (x) { return x !== i; });
};

Muuttuja.prototype.päivitä_indeksit = function (tila) {
    var _tila = tila || this.vm.tila();
    this.kuuntelijat.map(
        function (x) { x.muuttuja_päivitetty(this, _tila); });
};

Muuttuja.prototype.nimi = function() {
    return this._nimi;
};

Muuttuja.prototype.htmlksi = function () {
    return this.nimi();
};

Muuttuja.prototype.arvo = function(tila) {
    // var muuttujat = tila ? tila.muuttujat : this.vm.tila().muuttujat;
    tila = tila ? tila : this.vm.tila();
    return tila.muuttujassa(this.nimi());
};

Muuttuja.prototype.arvoksi = function (x, tila) {
    tila = tila ? tila : this.vm.tila();
    // var muuttujat = tila ? tila.muuttujat : this.vm.tila().muuttujat;
    // muuttujat[this.nimi()] = x;
    tila.aseta_muuttuja(this.nimi(), x);
    if (x!=null)
        this.divi_arvo.innerHTML = x.toString();
    else
        this.divi_arvo.innerHTML = '&nbsp;&nbsp;';
    
    this.päivitä_indeksit(tila);
    
    return tila.muuttujassa(this.nimi());
};

Muuttuja.prototype.sovita = function (tila) {
    return true; // tila; XXX järki?
};

// Muuttuja.prototype.aseta = function (v) {
//     this.arvo(v);
// };

Muuttuja.prototype.nimeä = function (n) { // TODO Mietipä vähän :)
    this._nimi = n;
};

Muuttuja.prototype.liitä = function (kooste) {
    var koosteolio = hae_olio(kooste);
    koosteolio.appendChild(this.divi);
};

Muuttuja.prototype.irrota = function (kooste) {
    var koosteolio = hae_olio(kooste);
    koosteolio.removeChild(this.divi);
};

function muuttujiin_pudotus(event, kohde) {
    return;
    // ei pudotusta Taunossa
    if (window.location.pathname.indexOf('tauno/')>=0)
        return;
    
    // debugversio sallii pudotuksen ;)
    
    var data = hae_data(event);
    var tyyppi = data.tyyppi;
    if (tyyppi=="muuttuja" || tyyppi=="solu") {
        var oid = data.oid;
        var elementti = hae_olio(oid);
        var arvo = elementti.olio;//.arvo();
        if (!vk.kysy_muuttuja(kohde, arvo)) {
            echo.lisää(" Muuttujaa ei lisätty.");
        }
    } else {
        echo.lisää(" Muuttujiin pudotettu muuta kuin solun tai muuttujan arvo.");
    }
}
// muuttujat ends here
// [[file:~/Sites/taulukko/tauno.org::*Echo][echo]]
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
// echo ends here
// [[file:~/Sites/taulukko/tauno.org::*Nappi][nappi]]
function luo_operaattorit(divid, kohde, opers) {
    var divi = luo_div(divid, {'className': 'op', 'draggable' : false});
    var sel = document.createElement('select');
    for (var on in opers) {
        var op = document.createElement('option');
        op.value = on;
        op.label = on;
        op.text = on;      // firefox vaatii tämänkin, webkitille riittää ylemmät...
        op.accessKey = on; // elvistelyä
        op.fn = opers[on];
        
        sel.appendChild(op);
    }
    sel.addEventListener('change',
                         function (event) { kohde.aseta_oper(sel.value); }, false);
    divi.appendChild(sel);
    return divi;
}

function Nappi(divid, tunnus, toiminto_fn) {
    this.divid = divid;
    this.tunnus = tunnus;
    
    this.elementti = document.createElement('button');
    this.elementti.innerHTML = this.tunnus;
    this.elementti.onclick = toiminto_fn;
    this.elementti.olio = this;
}

Nappi.prototype.liitä = function (kooste) {
    var koosteolio = hae_olio(kooste);
    koosteolio.appendChild(this.elementti);
};

Nappi.prototype.irrota = function (kooste) {
    var koosteolio = hae_olio(kooste);
    koosteolio.removeChild(this.elementti);
};

Nappi.prototype.aktivoi = function (aktiivinen) {
    this.elementti.disabled = !aktiivinen;
};
// nappi ends here
// [[file:~/Sites/taulukko/tauno.org::*Lauseke][lauseke]]
function drag_to_arg1(event) {
    var data = hae_data(event);
    var tyyppi = data.tyyppi;
    if (tyyppi == "solu" || tyyppi == "muuttuja" || tyyppi === 'indeksi') {
        if (event.stopPropagation) event.stopPropagation();
        var oid = data.oid;
        var olio = document.getElementById(oid);
        event.target.olio.aseta_arg1(olio.olio);
        return false;
    } else {
        event.preventDefault();
    }
    return true;
}

function Lauseke(divid) {
    if (typeof divid === 'undefined') {
        divid = 'lauseke'+timestring();
    }
    
    this.divid = divid;
    this.arg1 = null;
    this.operators = {'+': function (a,b) { return a+b; },
                      '-': function (a,b) { return a-b; },
                      '<': function (a,b) { return a<b; }};
    this.operator = '+';
    this.arg2 = null;
    
    this.divi     = luo_div(divid, {
        className:'lauseke',
        title: 'Raahaamalla tähän arvoja suoritat laskutoimituksen,\n'+
            'jonka tuloksen voit raahata muuttujaan tai taulukkoon.',
        'draggable':true});
    this.divi_a1  = luo_div(divid+'-a1',
                            {'className':'arg',
                             'draggable':false});
    this.divi_op  = luo_operaattorit(divid+'-op', this, this.operators);
    this.divi_a2  = luo_div(divid+'-a2',
                            {'className':'arg',
                             'draggable':false});
    this.divi_eq  = luo_div(divid+'-eq',
                            {'className':'equals-literal',
                             'draggable':false});
    this.divi_val = luo_div(divid+'-v',
                            {'className':'arvo',
                             'draggable':true});
    
    this.divi_a1.innerHTML = "&nbsp; &nbsp; &nbsp;";
    this.divi_a2.innerHTML = "&nbsp; &nbsp; &nbsp;";
    this.divi_eq.innerHTML = "=";
    this.divi_val.innerHTML = "&nbsp; &nbsp; &nbsp;";
    
    this.divi.appendChild(this.divi_a1);
    this.divi.appendChild(this.divi_op);
    this.divi.appendChild(this.divi_a2);
    this.divi.appendChild(this.divi_eq);
    this.divi.appendChild(this.divi_val);
    
    this.divi.olio = this;
    this.divi_a1.olio = this;
    this.divi_op.olio = this;
    this.divi_a2.olio = this;
    
    add_drag_drop_new(this.divi, {
        'dragstart': function (event) {
            if (!this.olio.validi()) {
                alert('vain valmiin lausekkeen voi siirtää!');
                event.preventDefault();
                return false;
            }
            aseta_data(event, {tyyppi: "lauseke", oid: this.id});
            event.dataTransfer.effectAllowed = 'copy';
            return true;
        },
        'dragenter': function (event) {
            event.preventDefault();
            return true;
        },
        'dragleave': function (event) {
            event.preventDefault();
            return true;
        },
        'drageover': function (event) {
            event.preventDefault();
            event.dataTransfer.effectAllowed = 'copy';
            return true;
        },
        'drop': function (event) {
            event.preventDefault();
            event.dataTransfer.dropEffect = 'copy';
            return true;
        }});
    
    add_drag_drop_new(this.divi_a1, {
        'dragstart': null,
        'dragenter': function (event) {
            event.preventDefault();
            event.dataTransfer.effectAllowed = 'copy';
            return true;
        },
        'dragleave': function (event) {
            event.preventDefault();
            return true;
        },
        'dragover': function (event) {
            event.preventDefault();
            event.dataTransfer.effectAllowed = 'copy';
            return true;
        },
        'drop': drag_to_arg1
    });
    
    add_drag_drop_new(this.divi_a2, {
        'dragstart': null,
        'dragenter': function (event) {
            event.preventDefault();
            event.dataTransfer.effectAllowed = 'copy';
            return true;
        },
        'dragleave': function (event) {
            event.preventDefault();
            return true;
        },
        'dragover': function (event) {
            event.preventDefault();
            event.dataTransfer.effectAllowed = 'copy';
            return true;
        },
        'drop': function (event) {
            var data = hae_data(event);
            var tyyppi = data.tyyppi;
            if (tyyppi == "solu" || tyyppi == "muuttuja" || tyyppi === 'indeksi') {
                if (event.stopPropagation) event.stopPropagation();
                event.dataTransfer.dropEffect = 'copy';
                var oid = data.oid;
                var olio = document.getElementById(oid).olio;
                event.target.olio.aseta_arg2(olio);
                event.preventDefault();
                return false;
            } else {
                event.preventDefault();
                return false;
            }
            return true;
        }});
    
}

Lauseke.prototype.validi = function () {
    return this.arg1 && this.operator && this.arg2;
};


Lauseke.prototype.sovita = function (tila) {
    return this.arvo(tila) != null;
};

Lauseke.prototype.arvo = function (tila) {
    if (!this.validi()) return null;
    return this.operators[this.operator](this.arg1.arvo(tila), this.arg2.arvo(tila));
};

Lauseke.prototype.aseta_arg1 = function (arvo) { // XXXXXX tila ei välity!!!
    this.arg1 = arvo;
    var nimi_tai_indeksi = arvo.nimi();// || ('t['+arvo.indeksi+"]"); // TODO Kauhee kludge!
    this.divi_a1.innerHTML = nimi_tai_indeksi+':'+arvo.arvo();
    this.divi_val.innerHTML = ""+(this.validi()?this.arvo():'__');
};

Lauseke.prototype.aseta_arg2 = function (arvo) {
    this.arg2 = arvo;
    var nimi_tai_indeksi = arvo.nimi() ; // || ('t['+arvo.indeksi+"]"); // TODO Kauhee kludge!
    this.divi_a2.innerHTML = nimi_tai_indeksi+':'+arvo.arvo();
    this.divi_val.innerHTML = ""+(this.validi()?this.arvo():'__');
};

Lauseke.prototype.aseta_oper = function (symboli) {
    this.operator = symboli;
    this.divi_val.innerHTML = ""+(this.arvo());
};

Lauseke.prototype.liitä = function (kooste) {
    var koosteolio = hae_olio(kooste);
    koosteolio.appendChild(this.divi);
};

Lauseke.prototype.irrota = function (kooste) {
    var koosteolio = hae_olio(kooste);
    koosteolio.removeChild(this.divi);
};

Lauseke.prototype.katoa = function () {
    var parent = this.divi.parentElement;
    parent.removeChild(this.divi);
    var lauseke = new Lauseke();
    lauseke.liitä(parent);
};

Lauseke.prototype.nimi = function () {
    return this.arg1.nimi()+this.operator+this.arg2.nimi();
};

Lauseke.prototype.htmlksi = function () {
    return this.arg1.nimi()+' '+this.operator.replace('>','&lt;')+' '+this.arg2.nimi();
};


function lisää_lauseke(event, diviin) {
    if (event.target.id=="compound-statements") { // TODO Kevyt kludge, siisti joskus!
        uusi_lauseke(diviin);
        event.preventDefault();
        return false;
    }
    return true;
}

function uusi_lauseke(diviin) {
    var lauseke = new Lauseke();
    lauseke.liitä(diviin);
    return lauseke;
}


// function lausekkeisiin_pudotus(event, kohde) {
//     if (event.target.id=="compound-statements") {
//              // TODO Kevyt kludge, siisti joskus!
//              var olio = hae_olio(data.oid).olio;  // XXX XXX ZZZ zzz TODO Vikaa!!!
//              var lauseke = uusi_lauseke(kohde);      // XXX TODO vikkoo, vikkoo
//              lauseke.aseta_arg1(olio); // XXX TODO vikkoo, vikkoo
//              //lauseke.divi_a1.innerHTML = olio.arvo().toString();
//              // TODO pitäskö olla aseta_arg-metodien sisällä?

//              event.preventDefault();
//              return false;
//     }
//     return true;
// }
// lauseke ends here
// [[file:~/Sites/taulukko/tauno.org::*Ehtolause][ehtolause]]
function ehtolause(event) {
    var ohj = new Ohjelma(vk);
    ohj.divinä('ehto'+timestring());
    var ihvi = new Ehto(new Lauseke(), ohj);
    ohj = vk.ohjelma();
    // var ask = new Askel();
    alert('ei tee mitään vielä');
};

function ehtolause_loppu(event) {
    alert('ei tee mitään vielä');
};
// ehtolause ends here

// [[file:~/Sites/taulukko/tauno.org::*Alustus][alusta_tauno]]
function alusta_tauno(event) {
    var naytto = hae_olio('naytto');
    naytto.innerHTML = ''+
        '      <div id="tiedot" class="tiedotd">taulukko: int[<span id="taulukon_pituus" title="tähän tulee taulukon pituus joskus">...</span>] t = '+
        '        <div id="taulunaytto" class="syottod">'+
        ' '+
        '        </div> '+
        '      </div> '+
        ' '+
        '      <div id="toiminta" class="toiminta"> '+
        '        <div id="vasen" class="vasen">muuttujat: <button id="uusi-muuttuja" onclick="vk.kysy_muuttuja(\'muuttujat\')">uusi muuttuja</button> '+
        '          <div draggable="false" ondrop="muuttujiin_pudotus(event,this)" ondragover="event.preventDefault();" '+
        '               ondragleave="event.preventDefault();" ondragenter="event.preventDefault();" '+
        '               id="muuttujat" class="muuttujatd" '+
        '               title="muuttujalista, lisää muuttuja yllä olevasta painikkeesta"><!-- hyi, globaali vk... TODO XXX --></div> '+
        '          <div class="scratch" id="scratch"> '+
        '            <div class="compound-statements" id="compound-statements" draggable="false" '+
        '                 ondrop-not="lausekkeisiin_pudotus(event,this)" ondragover="event.preventDefault();" '+
        '                 ondragleave="event.preventDefault();" ondragenter="event.preventDefault();" '+
        '                 title="täytä lausekkeen puuttuvat osat raahaamalla, ja raahaa sitten tulos kohteeseen"> '+
        '            </div> '+
        '            <div id="tools" class="tools"> '+
        '            </div> '+
        '          </div> '+
        '        </div> '+
        '        <div id="oikea" class="oikea">ohjelma: '+
        '        </div> '+
        '      </div> '+
        ' '+
        '      <div id="echo" class="echo" title="Viestialue, johon ohjelma tulostaa viestejä toiminnastaan. Puhdista klikkaamalla."></div> '+
        '';

    var prt = parametrit();


    // vk on globaali... hyi...
    vk = new Virtuaalikone();

    var ohjelma = new Ohjelma(vk);
    var ohjelmadivi = ohjelma.divinä('ohjelma');
    vk.lisää_ohjelma(ohjelma);

    vk.luo_taulukko(prt); // XXX TODO taulukon ja muuttujat voi luoda vasta ohjelman jälkeen... gui-riippuvuus, hyi!
    vk.luo_muuttujat(prt);

    uusi_lauseke('compound-statements');
    var up = new UnOpPlus(vk, true);
    up.liitä('tools');
    var um = new UnOpMiinus(vk, true);
    um.liitä('tools');
    var nolla = new Vakio(0, vk, true);
    nolla.liitä('tools');
    var yksi = new Vakio(1, vk, true);
    yksi.liitä('tools');
    // var iffi = new Nappi('if-nappi', 'if', ehtolause);
    // iffi.liitä('tools');
    // var endiffi = new Nappi('endif-nappi', 'end if', ehtolause_loppu);
    // endiffi.liitä('tools');
    if (echo===null) echo = new Echo('echo');
}
// alusta_tauno ends here
  
// [[file:~/Sites/taulukko/tauno.org::*P%C3%A4%C3%A4ohjelma][main]]
if (navigator.userAgent.indexOf('MSIE')>-1 &&
    navigator.userAgent.indexOf('MSIE 10')<0) {
    alert('Ajat liian vanhaa IE:tä tai IE10 on yhteensopivuustilassa. '+
          'Tauno vaatii IE10:n modernina (ei compat view)!');
    if (echo===null) echo = new Echo('echo');
    echo.aseta('IE10 compatibility-näkymässä, sammuta se!');
} else if (typeof QUnit === "undefined") { // Emmehän ole testausympäristössä?
    
    // latauksen jälkeen ajettava koodi
    window.addEventListener('load', alusta_tauno, false);
    
    // myös tämä pitää määritellä onloadin lisäksi
    window.addEventListener('unload', function (event) {}, false);
    
} else { /* yes, qunit go go go */ };
// main ends here

// Runko:1 ends here

function getUserCodeFromTauno() {
    return vk.ohjelma().tekstinä();
}

function koodivalistus(v) {
    alert(vk.ohjelma().tekstinä());
}
