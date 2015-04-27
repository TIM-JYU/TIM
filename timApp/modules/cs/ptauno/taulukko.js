/*************************************************************************
    Pauno, A playground for teaching arrays
    Copyright (C) 2014 Jonne Itkonen

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/
 
/**********************************************************************
 TODO:
 [ ]  indeksoi-metodiin indeksin nimeäminen & indeksin siirto muuttujan arvon mukaan
 [ ]  scratchin tuplaklikkaus (tai vain klikkaus) sotkee softan toiminnan
 [ ]  lisää parametri, joka sallii tiputtaa muuttujien laatikkoon arvoja
 [ ]  muuttujian alustus mahdollista tyhjänä
 [+]  korjaa muuttujien divid-sotku
 [+]  muuttujille olio
 [+]  selvitä muuttujan arvo-attribuutin paikka :)
 [+]  taulukon solun arvon pudotus muuttuja-alueelle
      lisää muuttujan
 [ ]  ohjelmasta oma olio
 [ ]  ohjelman lausekkeet ohjelma-olioon
 [ ]  ohjelman lausekkeille olio
 [ ]  vakionumerot -1,0,1 ja n, joka kysyy numeron arvon (raahattavia)
 [+]  echo kuntoon!
 [+]  muuttujan / solun lisääminen ohjelman lausekkeeseen
 [ ]  html-tulostukset vastaavalle oliolle?
 [ ]  generoituvat suorituslauseet ei saa näkyä ensin
 [ ]  if on tila, joka aktivoituu ehtolauseesta,
      jolloin seuraavat tekemiset menee iffin sisään (then osio),
      kunnes käyttäjä lopettaa iffin (ok-nappi tai jotain tilan
      osoittimen luona).
 [ ]  'if' tilalle 'koska'
 [ ]  silmukat muotoon 'toista kohdasta (pc) kunnes (ehto)'
       -> gotomaisuus
 [ ]  jos muuttujan nimessä on alussa 'int', niin urvelletaan
 [ ]  loput :)
 [ ]  samanakaltaisten lauseiden tunnistus (versioss 98)
 **********************************************************************/

var hae_data = function (dt) { alert('hae_data: jotain pahasti vialla'); };
var aseta_data = function (dt,d) { alert('aseta_data: jotain pahasti vialla'); };

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
	return x.filter(function (x) { return x.indexOf('text/x-')!=-1;});
    };
    var firefox_types = function (x) {  // XXX Firefoxissa types on stringlist, jolla items, ei map/filter
	var ret = [];
	for (var v=0; v<x.length; ++v)
	    ret.push(x.item(v));
	return webkit_types(ret);
    };
    var types = null;
    if (navigator.appVersion.indexOf('MSIE')>-1 || navigator.appVersion.indexOf('WebKit')>-1) {
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

/* globaali huutelutaulu */
var echo = null;

// TODO ilkeä, ilkeä globaali virtuaalikone
var vk = null;

// Palauttaa ajan merkkijonona, käytetään id:iden luomisessa.
function timestring() { return (new Date()).getTime().toString(16); }

function hae_olio(olio_tai_id) { // TODO nimeksi hae_elementti???
    /* Hakee olion id:n perusteella,
     * tai jos annettu jo olio, palauttaa sen. */
    var olio = null;
    if (typeof olio_tai_id == 'string')
	olio = document.getElementById(olio_tai_id);
    else if (typeof olio_tai_id == 'object') {
	olio = olio_tai_id;
    }
    return olio;
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

function Virtuaalikone(ohjelmaid) {
    this.pc = 0; // TODO täällä vai Ohjelmassa?

    this._tila=new Tila();
    this.ohjelma = new Ohjelma(ohjelmaid, this);

    // TODO Testailua varten
    var prt = parametrit();
    var alkioita = parseInt(prt['ts']) || 6;
    for (var i=0; i<alkioita; ++i)
	this._tila.aseta(i, Math.floor((Math.random()*100)));
}

Virtuaalikone.prototype.tila = function (uusi_tila) {
    if (typeof uusi_tila !== 'undefined') {
	this._tila = uusi_tila;
    }
    return this._tila;
};

Virtuaalikone.prototype.luo_muuttujat = function () {
    "use strict";
    var mu = document.getElementById('muuttujat');
    mu.innerHTML = '';
    // $("#muuttujat").empty();

    for (var k in this._tila.muuttujat) {
        var v = this._tila.muuttujat[k];
	var muk = new Muuttuja(this, k, v, true);
	muk.liitä(mu);
    }
};

// http://stackoverflow.com/questions/5778020/check-whether-an-input-string-contains-number
function isNumeric(n) {
    return !isNaN(parseInt(n)) && isFinite(n);
}

function isNotNumeric(n) {
    return !isNumeric(n);
}


Virtuaalikone.prototype.lisää_muuttuja = function(diviin, arvolla) {
    var nimi = prompt("Anna muuttujan nimi:",'');
    if (nimi==null || nimi=='') return null; // TODO palauta viesti echo-alueelle, ja laita se echo kuntoon :)

    while (this._tila.muuttujat.hasOwnProperty(nimi)) {
	nimi = prompt("Muuttuja "+nimi+" on jo olemassa, anna uusi nimi:",'');
	if (nimi==null || nimi=='') return null;
    }

    var arvo = null;
    if (typeof arvolla === 'undefined') {
	do {
	    arvo = prompt("Anna muuttujan arvo (vain luvut kelpaavat arvoksi):",'');
	    if (arvo === null || arvo === '') {
		arvo = null;
	    } else {
		arvo = parseInt(arvo);
	    }
	} while (arvo!=null && isNotNumeric(arvo));
    } else {
	arvo = arvolla;
    }
    // ***** Vesan muutokset alkaa ***** //
    return this.lisää_muuttuja_nakyviin(diviin,nimi,arvo);
}
    
Virtuaalikone.prototype.lisää_muuttuja_nakyviin = function(diviin, nimi, arvo) {
    this.ohjelma.uusi_muuttuja(nimi, arvo);
    var muuttuja = new Muuttuja(this, nimi, arvo, true); // TODO saisko tän edeltävän sisään?
    muuttuja.liitä(diviin);
    return muuttuja;
};



Virtuaalikone.prototype.lisää_muuttuja2_tee = function(diviin, arvolla) {
    nimi = document.getElementById("uusiNimi").value.trim();
    arvo = document.getElementById("uusiArvo").value.trim();
    if (this._tila.muuttujat.hasOwnProperty(nimi) ) return this.naytaUusiMessage("Muuttuja " + nimi + " on jo olemassa!","red");
    if (arvo === null || arvo === '') { arvo = null;  } 
    else { arvo = parseInt(arvo); }
    if ( arvo != null && isNotNumeric(arvo) ) return this.naytaUusiMessage("Arvon pitää olla numeerinen!","red");
    if ( nimi ) nimi = nimi.trim();
    if ( nimi && !nimi.match(/^[a-zA-Z_][a-zA-Z_0-9]*$/) ) return this.naytaUusiMessage("Nimessä saa olla vain kirjaimia, numeroita ja _!","red");
    this.piilotaMuuttujanLisays();
	if (nimi==null || nimi=='') return null;
    nappi = document.getElementById("uudenLisays");
    nappi.focus(); // Muuten WP( IE pistää fokuksen minne sattuu
    return this.lisää_muuttuja_nakyviin(diviin,nimi,arvo);
};
 
Virtuaalikone.prototype.piilotaMuuttujanLisays = function(diviin, arvolla) {
    document.getElementById("uudenMuuttujanAlue").style.visibility="collapse";
    document.getElementById("muuttujaButtonAlue").style.visibility="visible";
}

// event.type must be keypress
Virtuaalikone.prototype.getChar = function(event) {
  if (event.which == null) {
    return String.fromCharCode(event.keyCode) // IE
  } else if (event.which!=0 && event.charCode!=0) {
    return String.fromCharCode(event.which);   // the rest
  } else {
    return null; // special key
  }
}


Virtuaalikone.prototype.naytaUusiMessage = function(teksti,vari) {
    var message = document.getElementById("uusiMessage");
    message.innerText = teksti;
    message.style.color=vari;
    return null;
}

Virtuaalikone.prototype.lisää_muuttuja2 = function(diviin, arvolla) {
    document.getElementById("uudenMuuttujanAlue").style.visibility="visible";
    document.getElementById("muuttujaButtonAlue").style.visibility="collapse";
    var uusiNimi =  document.getElementById('uusiNimi');
    uusiNimi.focus();
    uusiNimi.value = "";
    var message = document.getElementById("uusiMessage");
    this.naytaUusiMessage("Anna uuden muuttujan nimi ja arvo","black");
};

Virtuaalikone.prototype.uusiNimiKeyPress = function(event,diviin) {
    var ch = this.getChar(event || window.event)
    if  ( ch.match(/[a-zA-Z_]/) ) return true;
    var uusiNimi =  document.getElementById('uusiNimi').value;
    if ( uusiNimi.length >= 1 && ch.match(/[0-9]/) ) return true;
    if ( " " < ch ) return false; 
    var uusiArvo =  document.getElementById('uusiArvo');
    uusiArvo.focus();
    return false;
}

Virtuaalikone.prototype.uusiArvoKeyPress = function(event,diviin) {
    var ch = this.getChar(event || window.event)
    if ( "0" <= ch && ch <= "9" ) return true; 
    if ( " " <  ch  ) return false; 
    this.lisää_muuttuja2_tee(diviin);
    return false;
}

    // ***** Vesan muutokset loppuu ***** //



Virtuaalikone.prototype.luo_taulukko = function() {
    // with jquery a must: _('#taulukko').empty();
    // $("#taulukko").empty();
    var ta = document.getElementById('taulukko');
    ta.tila = this._tila;
    ta.innerHTML = ''; // without jquery

    var taululause = 'int[] t = {';

    for (var i=0; i<this._tila.taulukko.length; ++i) {
	var v = this._tila.taulukko[i];
	taululause += v+', ';
	var nos = new Solu(this, i, v, true);
	var no = nos.div;

	ta.appendChild(no);
    }

    // TODO Kaaaaamea kludge alla, ton pitäs olla oma lause

    taululause = taululause.substring(0,taululause.length-2)+'};';
    var tldiv = luo_div('taulukon_alustus',
			{className : 'lauseke',
			 title: taululause,
			 innerHTML : taululause});
    var o = document.getElementById('ohjelma');
    o.appendChild(tldiv);
};


function Ohjelma(divi, vk) {
    this.divi = hae_olio(divi);
    this.divi.olio = this;

    this.askeleet = new Array();
    this.vk = vk;
    this.pc = 0;
}

Ohjelma.prototype.tekstinä = function (mittee) {
    var ohj = '';
	var sep = '';
    for (var pc=0; pc < this.askeleet.length; ++pc) {
        ohj += sep + this.askeleet[pc].tekstinä(); sep = '\n';
    }
    return ohj;
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


Ohjelma.prototype.lisaaNakyviin = function (uusiDiv) {
    var ohj=this.divi; // document.getElementById('ohjelma');
    ohj.appendChild(uusiDiv);
    ohj.scrollTop = uusiDiv.offsetTop;  
}


Ohjelma.prototype.sijoitus = function (mihin, mistä) {
    var s = new Sijoitus(mihin, mistä);
    var uusi_tila = s.sovita(this.tila());  // TODO entä jos ei onnistu?
    var a = new Askel(s, uusi_tila, this.viimeisin_askel());
    var vanha_tila = this.tila();
    this.tilaksi(uusi_tila);
    this.askeleet.push(a);

    this.lisaaNakyviin(a.divinä());
};

Ohjelma.prototype.unplus = function (mihin) {
    var up = new UnaariPlus(mihin);
    var uusi_tila = up.sovita(this.tila());  // TODO entä jos ei onnistu?
    var a = new Askel(up, uusi_tila, this.viimeisin_askel());
    var vanha_tila = this.tila();
    this.tilaksi(uusi_tila);
    this.askeleet.push(a);

    this.lisaaNakyviin(a.divinä());
};

Ohjelma.prototype.unmiinus = function (mihin) {
    var um = new UnaariMiinus(mihin);
    var uusi_tila = um.sovita(this.tila());  // TODO entä jos ei onnistu?
    var a = new Askel(um, uusi_tila, this.viimeisin_askel());
    var vanha_tila = this.tila();
    this.tilaksi(uusi_tila);
    this.askeleet.push(a);

    this.lisaaNakyviin(a.divinä());
};


Ohjelma.prototype.uusi_muuttuja = function (nimi, arvo) {
    var lm = new LuoMuuttuja(nimi, arvo);
    var uusi_tila = lm.sovita(this.tila());
    var a = new Askel(lm, uusi_tila, this.viimeisin_askel());
    var vanha_tila = this._tila;
    this.tilaksi(uusi_tila);
    this.askeleet.push(a);

    this.lisaaNakyviin(a.divinä());
};

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

function Lause() {
    this._arvo = null;
    this.divi = null;
}

Lause.prototype.lval = function () {
    
};

Lause.prototype.rval = function () {
    
};

Lause.prototype.arvo = function() {
    return this._arvo;
};

Lause.prototype.suorita = function (tila) {
    return tila;
};

Lause.prototype.divinä = function () {
    this.divi = luo_div('lause'+this.arvo().toString(),
    			{'className': "lause",
    			 'title': this.arvo().toString(),
    			 'innerHTML': this.arvo().toString()});
    return this.divi;
};

function Sijoitus(mihin, mistä) {
    this.mihin = mihin;
    this.mistä = mistä;
}

Sijoitus.prototype.arvo = function () {
    return this.mistä.arvo();
};

Sijoitus.prototype = new Lause();
Sijoitus.prototype.constructor = Sijoitus;

Sijoitus.prototype.sovita = function (tila) {
    var uusi_tila = tila.klooni();
    var arvo = this.mistä.arvo(tila);  // TODO pitäs huomioida, jos ei onnistu
    this.mihin.arvoksi(arvo, uusi_tila);
    return uusi_tila;
};

Sijoitus.prototype.divinä = function (tila) {
    this.divi = luo_div('sijoitus'+this.mihin.nimi()+this.mistä.nimi()+this.mistä.arvo(tila),
    			{'className': "lause sijoitus",
    			 'title': this.mihin.nimi()+'='+this.mistä.arvo(tila),
    			 'innerHTML': this.mihin.nimi()+' = '+ this.mistä.htmlksi() +';<br/>'});
    return this.divi;
};

Sijoitus.prototype.tekstinä = function (tila) {
    return this.mihin.htmlksi(tila)+' = '+this.mistä.htmlksi(tila)+';';
};

function UnaariPlus(mihin) {
    this.mihin = mihin;
}

UnaariPlus.prototype.arvo = function (tila) {
    return this.mihin.arvo(tila);
};

UnaariPlus.prototype = new Lause();
UnaariPlus.prototype.constructor = UnaariPlus;

UnaariPlus.prototype.sovita = function (tila) {
    var uusi_tila = tila.klooni();
    var arvo = this.mihin.arvo(tila);
    this.mihin.arvoksi(++arvo, uusi_tila);
    return uusi_tila;
};

UnaariPlus.prototype.divinä = function (tila) {
    this.divi = luo_div('uplus'+this.mihin.nimi()+this.mihin.arvo(tila),
    			{'className': "lause unaari",
    			 'title':     '++'+this.mihin.nimi()+';',
    			 'innerHTML': '++'+this.mihin.nimi()+';<br/>'});
    return this.divi;
};

/*
UnaariPlus.prototype.tekstinä = function (tila) {
    return  this.mihin.nimi()+' += ' + this.määrä.arvo();
};
*/
UnaariPlus.prototype.tekstinä = function (tila) {
    return  '++' + this.mihin.nimi() +';';
};

function UnaariMiinus(mihin) {
    this.mihin = mihin;
}

UnaariMiinus.prototype.arvo = function (tila) {
    return this.mihin.arvo(tila);
};

UnaariMiinus.prototype = new Lause();
UnaariMiinus.prototype.constructor = UnaariMiinus;

UnaariMiinus.prototype.sovita = function (tila) {
    var uusi_tila = tila.klooni();
    var arvo = this.mihin.arvo(tila);
    this.mihin.arvoksi(--arvo, uusi_tila);
    return uusi_tila;
};

UnaariMiinus.prototype.divinä = function (tila) {
    this.divi = luo_div('umiinus'+this.mihin.nimi()+this.mihin.arvo(tila),
    			{'className': "lause unaari",
    			 'title':     '--'+this.mihin.nimi()+';',
    			 'innerHTML': '--'+this.mihin.nimi()+';<br/>'});
    return this.divi;
};

UnaariMiinus.prototype.tekstinä = function (tila) {
    return  '--' + this.mihin.nimi() +';';
};


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
    var uusi_tila = tila.klooni();
    uusi_tila.muuttujat[this.nimi] = this._arvo;
    return uusi_tila;
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


function Luku(arvo) {
    this.sisältö = arvo;
}

Luku.prototype.arvo = function (tila) {
    return this.sisältö;
};

Luku.prototype.divinä = function (tila) {
    if (!this.divi)
	this.divi = luo_div('', { 'className': 'luku',
				  'title' : this.sisältö.toString(),
				  'innerHTML' : this.sisältö.toString()});
    return this.divi;
};

Luku.prototype.tekstinä = function (tila) {
    return this.sisältö.toString();
};

function Tila(taulukko, muuttujat) {
  this.taulukko = taulukko || new Array();
  this.muuttujat = muuttujat || {};
}

Tila.prototype.klooni = function() {
    // Tämän pitäisi olla tarpeeksi syvä kopio,
    // sillä yksittäiset alkiot ovat (tai pitäs olla)
    // muuttumattomia.
    var uusi_taulukko = this.taulukko.slice(0);
    var uudet_muuttujat = this.muuttujat.constructor();
    for (var attr in this.muuttujat)
	if (this.muuttujat.hasOwnProperty(attr))
	    uudet_muuttujat[attr] = this.muuttujat[attr];
    return new Tila(uusi_taulukko, uudet_muuttujat);
};

Tila.prototype.aseta = function(i, v) {
  this.taulukko[i]=v;
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


function luo_div(id, attrs) {
    var d = document.createElement('div');
    d['id'] = id;
    for (var attr in attrs) {
	d[attr]=attrs[attr];
    }
    return d;
}

function Solu(vm, indeksi, arvo, draggable) {
    var divid = 'solu'+indeksi;
    this.divid = divid;
    this.div = luo_div(divid+'-k', {className: 'solu', 'draggable':false});
    this.ndiv = luo_div(divid+'-n', {className: 'solu-i', 'draggable':false, innerHTML:indeksi});
    this.sdiv = luo_div(divid,
		       {'className':'solu-a',
			'draggable':(draggable||false)});
    this.div.appendChild(this.ndiv);
    this.div.appendChild(this.sdiv);
    this.ndiv.olio = this;
    this.ndiv.solu = this;
    this.sdiv.olio = this;
    this.sdiv.solu = this;
    this.vm = vm;
    this.indeksi = indeksi;
    this.div.olio = this;
    this.arvoksi(arvo||0);


    this.div.solu = this;

    this.sdiv.addEventListener('dragover', function (event) {
	event.preventDefault();
	event.dataTransfer.effectAllowed = 'copy';
	return false;
    }, false);
    this.sdiv.addEventListener('dragenter', function (event) {
	event.preventDefault();
	event.dataTransfer.effectAllowed = 'copy';
	return false;
    }, false);
    // this.div.ondrop = function () {
    this.sdiv.addEventListener('drop', function (event) {
	var ev=event;
	ev.preventDefault();
	var data = hae_data(ev);
	var tyyppi = data.tyyppi;
	var oid = data.oid;
	var olio = hae_olio(oid).olio;
	var mihin = ev.target.olio;
	var mistä = olio;
	var ohj = document.getElementById('ohjelma').olio;

	if (tyyppi == "muuttuja" || tyyppi == "solu") {
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
    this.sdiv.addEventListener('dragstart', function (event) {
	var ev=event;
	aseta_data(ev, {'tyyppi':'solu', indeksi: ev.target.solu.indeksi.toString(), oid:ev.target.id});
	ev.dataTransfer.effectAllowed = 'copy';
	return false;
    }, false);

}

Solu.prototype.arvoksi = function (x, tila) {
    var taulukko = tila ? tila.taulukko : this.vm.tila().taulukko;
    taulukko[this.indeksi] = x;
    this.sdiv.innerHTML = x.toString();
    return taulukko[this.indeksi];
};

Solu.prototype.arvo = function (tila) {
    var taulukko = tila ? tila.taulukko : this.vm.tila().taulukko;
    return taulukko[this.indeksi];
};

Solu.prototype.nimi = function () {
    return 't['+this.indeksi+']';
};

Solu.prototype.sovita = function (tila) {
    return tila;
};

Solu.prototype.htmlksi = function () {
    return this.nimi();
};


function add_drag_drop_old(divi, start_fn, enter_fn, leave_fn, over_fn, drop_fn) {
    if (start_fn)
	divi.addEventListener('dragstart', start_fn, false);

    if (enter_fn)
	divi.addEventListener('dragenter', enter_fn, false);

    if (leave_fn)
	divi.addEventListener('dragleave', leave_fn, false);

    if (over_fn)
	divi.addEventListener('dragover', over_fn, false);

    if (drop_fn)
	divi.addEventListener('drop', drop_fn, false);
}

function add_drag_drop_new(divi, callbacks) {
    divi.addEventListener('dragstart', callbacks['dragstart']|| function (event) {}, false);
    divi.addEventListener('dragenter', callbacks['dragenter']|| function (event) {}, false);
    divi.addEventListener('dragleave', callbacks['dragleave']|| function (event) {}, false);
    divi.addEventListener('dragover', callbacks['dragover']
			  || function (event) {
			      if (event.preventDefault) { event.preventDefault(); };
			      event.dataTransfer.effectAllowed = 'copy';
			  }, false);
    divi.addEventListener('drop', callbacks['drop']
			  || function (event) {
			      if (event.stopPropagating) { event.stopPropagating(); };
			      event.dataTransfer.dropEffect = 'copy';
			      return false;
			  }, false);
}

function UnOpPlus(vm, draggable) {
    var divid = 'unopplus-'+timestring();
    this.divid = divid;
    this.vm = vm;

    this.divi = luo_div(this.divid, {className: 'unop', 'draggable':draggable});
    this.divi.innerHTML ='++';

    add_drag_drop_new(this.divi, {
	'dragstart': function (event) {
	    aseta_data(event, {tyyppi: 'unop',
			       nimi: 'plus',
			       oid: divid});
	    event.dataTransfer.effectAllowed = 'copy'; // TODO vai link?
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

    this.divi = luo_div(this.divid, {className: 'unop', 'draggable':draggable});
    this.divi.innerHTML ='--';

    add_drag_drop_new(this.divi, {
	'dragstart': function (event) {
	    aseta_data(event, {tyyppi: 'unop',
			       nimi: 'miinus',
			       oid: divid});
	    event.dataTransfer.effectAllowed = 'copy'; // TODO vai link?
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


function pudota_muuttujaan(event) {
    var ev = event;
    var v = null;

    // ev.preventDefault();
    // ev.target.style.background ="white";
    var data = hae_data(event);
    var tyyppi = data.tyyppi;
    var nimi  = data.nimi;
    var oid = data.oid;
    var ohj = document.getElementById('ohjelma').olio;

    var olio = hae_olio(oid).olio;
    var mihin = ev.target.olio;
    var mistä = olio;




    if (tyyppi == "solu") {
	if (ev.stopPropagation) ev.stopPropagation();
	v = olio.arvo();


	ohj.sijoitus(mihin, mistä);
	ev.target.style.background = "#fdf6e3"; // ev.target.oldbg;
	//ev.stopPropagation();
	return false;
    } else if ((tyyppi == "muuttuja") &&
	       (nimi != ev.target.nimi)) {
	if (ev.stopPropagation) ev.stopPropagation();
	v = olio.arvo();

	ohj.sijoitus(mihin, mistä);
	ev.target.style.background = "#fdf6e3"; // ev.target.oldbg;

	return false;
    } else if (tyyppi=="lauseke") {
	var lausekediv = hae_olio(oid);
	//ev.target.innerHTML = lausekediv.olio.arvo();
	//lausekediv.olio.liitä(ev.target);  // XXXXX
	//ev.target.olio.arvoksi(lausekediv.olio.arvo());
	// lausekediv.olio.katoa(); // XXXXX
	if (ev.stopPropagation) ev.stopPropagation();

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

Muuttuja.prototype.nimi = function() {
    return this._nimi;
};

Muuttuja.prototype.htmlksi = function () {
    return this.nimi();
};

Muuttuja.prototype.arvo = function(tila) {
    var muuttujat = tila ? tila.muuttujat : this.vm.tila().muuttujat;
    return muuttujat[this.nimi()];
};

Muuttuja.prototype.arvoksi = function (x, tila) {
    var muuttujat = tila ? tila.muuttujat : this.vm.tila().muuttujat;
    muuttujat[this.nimi()] = x;
    if (x!=null)
	this.divi_arvo.innerHTML = x.toString();
    else
	this.divi_arvo.innerHTML = '&nbsp;&nbsp;';
    return muuttujat[this.nimi()];
};

Muuttuja.prototype.sovita = function (tila) {
    return tila;
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
	if (!vk.lisää_muuttuja(kohde, arvo)) {
	    echo.lisää(" Muuttujaa ei lisätty.");
	}
    } else {
	echo.lisää(" Muuttujiin pudotettu muuta kuin solun tai muuttujan arvo.");
    }
}


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

function drag_to_arg1(event) {
    var data = hae_data(event);
    var tyyppi = data.tyyppi;
    if (tyyppi == "solu" || tyyppi == "muuttuja") {
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

    this.divi     = luo_div(divid,
			    {'className':'lauseke',
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
    //this.divi_op.innerHTML = "◌"; // &#x25cc;
    this.divi_a2.innerHTML = "&nbsp; &nbsp; &nbsp;";
    this.divi_eq.innerHTML = "=";
    this.divi_val.innerHTML = "__";

    this.divi.appendChild(this.divi_a1);
    this.divi.appendChild(this.divi_op);
    this.divi.appendChild(this.divi_a2);
    this.divi.appendChild(this.divi_eq);
    this.divi.appendChild(this.divi_val);

    this.divi.olio = this;
    this.divi_a1.olio = this;
    this.divi_op.olio = this;
    this.divi_a2.olio = this;

    add_drag_drop_old(this.divi,
		  function (event) {
		      if (!this.olio.validi()) {
			  alert('vain valmiin lausekkeen voi siirtää!');
			  event.preventDefault();
			  return false;
		      }
		      aseta_data(event, {tyyppi: "lauseke", oid: this.id});
		      event.dataTransfer.effectAllowed = 'copy';
		      return true;
		  },
		  function (event) {
		      event.preventDefault();
		      return true;
		  },
		  function (event) {
		      event.preventDefault();
		      return true;
		  },
		  function (event) {
		      event.preventDefault();
		      event.dataTransfer.effectAllowed = 'copy';
		      return true;
		  },
		  function (event) {
		      event.preventDefault();
		      event.dataTransfer.dropEffect = 'copy';
		      return true;
		  });

    add_drag_drop_old(this.divi_a1,
		  null,
		  function (event) {
		      event.preventDefault();
		      event.dataTransfer.effectAllowed = 'copy';
		      return true;
		  },
		  function (event) {
		      event.preventDefault();
		      return true;
		  },
		  function (event) {
		      event.preventDefault();
		      event.dataTransfer.effectAllowed = 'copy';
		      return true;
		  },
		  drag_to_arg1);

    add_drag_drop_old(this.divi_a2,
		  null,
		  function (event) {
		      event.preventDefault();
		      event.dataTransfer.effectAllowed = 'copy';
		      return true;
		  },
		  function (event) {
		      event.preventDefault();
		      return true;
		  },
		  function (event) {
		      event.preventDefault();
		      event.dataTransfer.effectAllowed = 'copy';
		      return true;
		  },
		  function (event) {
		      var data = hae_data(event);
		      var tyyppi = data.tyyppi;
		      if (tyyppi == "solu" || tyyppi == "muuttuja") {
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
		  });

}

Lauseke.prototype.validi = function () {
    return this.arg1 && this.operator && this.arg2;
};


Lauseke.prototype.sovita = function (tila) {
    return this.arvo(tila);
};

Lauseke.prototype.arvo = function (tila) {
    if (!this.validi()) return null;
    return this.operators[this.operator](this.arg1.arvo(tila), this.arg2.arvo(tila));
};

Lauseke.prototype.aseta_arg1 = function (arvo) { // XXXXXX tila ei välity!!!
    this.arg1 = arvo;
    var nimi_tai_indeksi = arvo.nimi() || ("t["+arvo.indeksi+"]"); // TODO Kauhee kludge!
    this.divi_a1.innerHTML = nimi_tai_indeksi+':'+arvo.arvo();
    this.divi_val.innerHTML = ""+(this.validi()?this.arvo():'__');
};

Lauseke.prototype.aseta_arg2 = function (arvo) {
    this.arg2 = arvo;
    var nimi_tai_indeksi = arvo.nimi() || ("t["+arvo.indeksi+"]"); // TODO Kauhee kludge!
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


function lausekkeisiin_pudotus(event, kohde) {
    if (event.target.id=="compound-statements") { // TODO Kevyt kludge, siisti joskus!
	var olio = hae_olio(data.oid).olio;
	var lauseke = uusi_lauseke(kohde);      // XXX TODO vikkoo, vikkoo
	lauseke.aseta_arg1(olio); // XXX TODO vikkoo, vikkoo
	//lauseke.divi_a1.innerHTML = olio.arvo().toString(); // TODO pitäskö olla aseta_arg-metodien sisällä?

	event.preventDefault();
	return false;
    }
    return true;
}

if (navigator.userAgent.indexOf('MSIE')>-1 &&
    navigator.userAgent.indexOf('MSIE 10')<0) {
    alert('Ajat liian vanhaa IE:tä tai IE10 on yhteensopivuustilassa.  Tauno vaatii IE10:n modernina (ei compat view)!');
    if (echo==null) echo = new Echo('echo');
    echo.aseta('IE10 compatibility-näkymässä, sammuta se!');
} else {

// latauksen jälkeen ajettava koodi
window.addEventListener('load', function (event) {
    vk = new Virtuaalikone('ohjelma');
    vk.luo_muuttujat();
    vk.luo_taulukko();
    uusi_lauseke('compound-statements');
    var up = new UnOpPlus(vk,true);
    up.liitä('tools');
    var um = new UnOpMiinus(vk,true);
    um.liitä('tools');
    if (echo==null) echo = new Echo('echo');
}, false);

// myös tämä pitää määritellä onloadin lisäksi
window.addEventListener('unload', function (event) {}, false);

// vaihtoehto edeltävälle:
// document.onreadystatechange = function () {
//     if (document.readyState == "complete") {
// 	luo_muuttujat();
// 	luo_taulukko();
//     }
// };

};

function getUserCodeFromTauno() {
    return vk.ohjelma.tekstinä();
}

function koodivalistus(v) {
    alert(vk.ohjelma.tekstinä());
}
