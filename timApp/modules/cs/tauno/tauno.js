// -*- coding: utf-8; tab-width: 4;  -*-
//
// Tauno - taulukot nohevasti
//
// Leikkikentta taulukoiden kayton opetteluun.
//
// (c)2014 Jonne Itkonen, GPLv3-lisenssilla
// (c)2025 vesal
// http://www.gnu.org/licenses/gpl-3.0.html

/**********************************************************************
 TODO-kohdat
 - mobiili-UI
 - yhdista askel ja lause
 - tee ohjelman ajo
 + uusi muuttuja uusiksi, jottei tartte prompt()-funktiota
 + jos vetaa solun itseensa tai muuttujan itseensa, ei generoidu koodia (=>cancel)
 - if end
 - indeksi ylittyessa lopettaa olemasta indeksi
 - vakionumerot -1 ja n, joka kysyy numeron arvon (raahattavia)
 - scratchin tuplaklikkaus (tai vain klikkaus) sotkee softan toiminnan
 - ohjelman lausekkeet ohjelma-olioon
 - generoituvat suorituslauseet ei saa nakya ensin
 - if on tila, joka aktivoituu ehtolauseesta,
 jolloin seuraavat tekemiset menee iffin sisaan (then osio),
 kunnes kayttaja lopettaa iffin (ok-nappi tai jotain tilan
 osoittimen luona).
 - 'if' tilalle 'koska'
 - silmukat muotoon 'toista kohdasta (pc) kunnes (ehto)'
 -> gotomaisuus
 - jos muuttujan nimessa on alussa 'int', niin urvelletaan
 - loput :)
 - samanakaltaisten lauseiden tunnistus (versiossa 98)

 Muutoksia vesal 12.9.2025:

 + siirrytty ES6-luokkiin
 + koodin siistimistä (Pycharm varoitukset pois)
 + undo-toiminto (poista viimeisin askel)
 + lauseke säilyttää tilansa kun sitä käytetään
 + *, /, %, -operaatiot
 + v-optio, jolla voi määritellä vakioita

 **********************************************************************/

// Globaalit sanoat, jotka kaannetaan 
let wordMuuttujat = "muuttujat:";
let wordUusiMuuttuja = "uusi muuttuja";
let wordMuuttujatTitle = "muuttujalista, lisää muuttuja yllä olevasta painikkeesta";
let wordOhjelma = "ohjelma";
let wordApua = 'Parametrit:\n' +
    '  help=help in english\n' +
    '  apua=apua suomeksi\n' +
    '  lang=en - kayttöliittymä englanniksi\n' +
    '  s    - simple, ei indeksejä\n' +
    '  mx=v - Luo muuttuja x arvolla v.\n' +
    '  ix=v - Luo indeksimuuttuja x arvolla v.\n' +
    '  ts=n - Aseta taulukon kooksi n.\n' +
    '  t=[x,y,...] - Luo taulukko alkioilla x,y,...\n' +
    '  v=x,y,...   - Vakioita x,y,...\n';
let wordNimi = "nimi:";
let wordArvo = "arvo:";
let wordLisaa = "Lisää";

/* globaali huutelutaulu */
let echo = null;

// TODO ilkea, ilkea globaali virtuaalikone
let vk = null;

// Julkinen rajapinta TIMille
function getUserCodeFromTauno() {
    return vk.ohjelmaTekstina();
}


// Palauttaa ajan merkkijonona, kaytetaan id:iden luomisessa.
function timestring() {
    return (new Date()).getTime().toString(16);
}

function haeElementti(olioTaiId) { // TODO nimeksi haeElementti???
    /* Hakee olion id:n perusteella,
     * tai jos annettu jo olio, palauttaa sen. */
    let olio = null;
    if (typeof olioTaiId === 'string')
        olio = document.getElementById(olioTaiId);
    else if (typeof olioTaiId === 'object') {
        olio = olioTaiId;
    }
    return olio;
}

function alkaaNumerolla(nimi) {
    let patt = /^[-0-9].*/g;
    return patt.test(nimi);
}


function luoDiv(id, attrs) {
    let d = document.createElement('div');
    d.id = id;
    for (let attr in attrs) {
        d[attr] = attrs[attr];
    }
    if (attrs.innerHTML) {
        let s = attrs.innerHTML;
        s = s.replace("<br.*", "");
        vk.tekstina.push(s);
    }
    return d;
}

function parametrit(str) {
    let kaikki = str || window.location.search.substring(1);
    kaikki = kaikki.split('&');
    let tulos = {};

    for (let i = 0; i < kaikki.length; ++i) {
        let nv = kaikki[i].split('=');

        if (nv.length == 1) {
            tulos[nv[0]] = true;
        } else if (nv.length == 2) {
            tulos[nv[0]] = decodeURIComponent(nv[1].replace(/\+/g, ' '));
        }
    }

    if (tulos.el === undefined) tulos.el = 'r';

    return tulos;
}


// http://stackoverflow.com/questions/5778020/check-whether-an-input-string-contains-number
function isNumeric(n) {
    return !isNaN(parseInt(n)) && isFinite(n);
}

function isNotNumeric(n) {
    return !isNumeric(n);
}

let Operaatio = {
    mista: null,
    mihin: null,
    data: {},
    odottaaLahdetta: function () {
        return this.mista === null;
    },
    asetaLahde: function (l) {
        this.mista = l;
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
    lahde: function () {
        return this.mista;
    },
    kohde: function () {
        return this.mihin;
    },
    tehty: function () {
        if (this.mista) {
            this.mista.classList.remove('valittu');
            this.mista.style.background = "";
            this.mista = null;
        }
        if (this.mihin) {
            this.mihin.classList.remove('valittu');
            this.mihin.style.background = "";
            this.mihin = null;
        }
        vk.virkista();
    },
    peruuta: function () {
        Operaatio.tehty();
    },
    asetaData: function (d) {
        if (this.odottaaLahdetta()) {
            this.data = d;
        } else {
            for (let k in d) this.data[k] = d[k];
        }
        return this.data;
    },
    haeData: function () {
        if (this.odottaaLahdetta()) return undefined;
        return this.data;
    }
};

class Virtuaalikone {
    constructor() {
        this.pc = 0;
        this.alkutila = new Tila([], {});
        this._tila = this.alkutila;
        this._ohjelma = [];
        this.tekstina = [];
        this.lausekkeet = [];
        this.muuttujat = {};
        this.taulukko = null;
    }

    tila(uusiTila) {
        if (typeof uusiTila !== 'undefined') {
            this._tila = uusiTila;
        }
        return this._tila;
    };

    ohjelma() {
        return this._ohjelma[this._ohjelma.length - 1];
    };

    ohjelmaTekstina() {
        return this.ohjelma().tekstina();
    };

    lisaaOhjelma(ohj) {
        this._ohjelma.push(ohj);
        return ohj;
    };

    poistaOhjelma() {
        return this._ohjelma.pop();
    };


    lisaaMuuttuja(nimi, arvo, diviin, lisaaVaikkaNumero) {
        if (this._tila.muuttujat.hasOwnProperty(nimi)) return null;
        if (!diviin) {
            diviin = haeElementti('muuttujat');
        }
        let vakio = alkaaNumerolla(nimi);
        if (vakio) {
            arvo = parseInt(nimi);
        }

        let muuttuja = this.ohjelma().uusiMuuttuja(nimi, arvo, vakio);

        if (!vakio || lisaaVaikkaNumero) muuttuja.liita(diviin);
        vk.muuttujat[nimi] = muuttuja;
        return muuttuja;
    };

    teeMuuttuja(diviin) {
        let nimi = document.getElementById("uusi-nimi").value;
        let arvo = document.getElementById("uusi-arvo").value;
        if (arvo === null || arvo === '') {
            arvo = null;
        } else {
            arvo = parseInt(arvo);
        }
        if (arvo !== null && isNotNumeric(arvo)) return null;
        if (nimi) nimi = nimi.trim();
        if (nimi && !nimi.match(/^[a-zA-Z_][a-zA-Z_0-9]*$/)) return null;
        if (nimi === "int") return null;
        if ((nimi === null || nimi === '')) {
            if (arvo == null) return null;
            nimi = "" + arvo;
        }
        if (this._tila.muuttujat.hasOwnProperty(nimi)) return null;
        this.piilotaMuuttujanLisays();
        return this.lisaaMuuttuja(nimi, arvo, diviin, true);
    };

    piilotaMuuttujanLisays(_diviin, _arvolla) {
        document.getElementById("uuden-muuttujan-alue").style.visibility = "collapse";
        document.getElementById("muuttuja-button-alue").style.visibility = "visible";
    };

    // event.type must be keypress
    getChar(event) {
        if (event.key.length === 1) {
            return event.key;
        }
        return null; // special key
    };

    luoMuuttujaKentista(_diviin, _arvolla) {
        document.getElementById("uuden-muuttujan-alue").style.visibility = "visible";
        document.getElementById("muuttuja-button-alue").style.visibility = "collapse";
        let uusiNimi = document.getElementById('uusi-nimi');
        uusiNimi.focus();
        uusiNimi.value = "";
        return true;
    };

    poistaMuuttujat() {
        document.querySelectorAll('div[id^="indeksi"]').forEach(div => div.remove());
    }

    isEditKey(event) {
        let ch = event.key;
        return ch === "Backspace" || ch === "Delete" || ch === "ArrowLeft" || ch === "ArrowRight" || ch === "Tab";
    }

    uusiNimiKeypress(event, diviin) {
        const uusiArvo = document.getElementById('uusi-arvo');
        if (event.key==="Enter") return uusiArvo.focus();
        if (this.isEditKey(event)) return true;
        let ch = this.getChar(event);
        if (!ch) return false;
        if (ch.match(/[a-zA-Z_]/)) return true;
        let uusiNimi = document.getElementById('uusi-nimi').value;
        if (uusiNimi.length >= 1 && ch.match(/[0-9]/)) return true;
        if (" " < ch) return false;
        uusiArvo.focus();
        return false;
    };

    uusiArvoKeypress(event, diviin) {
        if (event.key==="Enter") return this.teeMuuttuja(diviin);
        if (this.isEditKey(event)) return true;
        let ch = this.getChar(event);
        if (ch === "-" || "0" <= ch && ch <= "9") return true;
        if (" " < ch) return false;
        this.teeMuuttuja(diviin);
        return false;
    };


    muuttuja(nimi) {
        return haeElementti('muuttuja-' + nimi).olio;
    };

    solu(indeksi) {
        let s = haeElementti('solu-' + indeksi);
        if (!s) return null;
        return s.olio;
    };

    alkuun() {
        this._tila = this.alkutila;
        return this.alkutila;
    };

    luoTaulukko(parametrit) {
        this.simple = !!parametrit.s;
        if (parametrit.t) {
            parametrit.t = parametrit.t.replace(/[\[\]]/g, '');
            let alkiot = parametrit.t.split(',');
            alkiot = alkiot.map(function (i) {
                return parseInt(i);
            });
            for (let i = 0; i < alkiot.length; ++i) {
                this._tila.asetaIndeksiin(i, alkiot[i]);
            }
        } else {
            let alkioita = 6;
            // noinspection JSUnresolvedVariable
            if (parametrit.ts) alkioita = parseInt(parametrit.ts) || 0;
            for (let j = 0; j < alkioita; ++j)
                this._tila.asetaIndeksiin(j, Math.floor((Math.random() * 100)));
        }
        this.taulukko = new Taulukko(this); // XXX taa on niiiiin vaarin...
        return this.taulukko;
    };

    luoIndeksi(nimi, iNro) {
        let muuttuja = this.lisaaMuuttuja(nimi, iNro);
        if (!iNro) iNro = muuttuja.diviArvo.arvo;

        let solu = this.solu(iNro);
        if (!solu) return;

        let io = new Indeksi(this, solu);
        io.indeksoi(muuttuja);
        muuttuja.indeksi = io;
        io.div.innerHTML = muuttuja.nimi();

        solu.divSi.appendChild(io.div);
        solu.indeksimuuttujat.push(io);
        io.siirry();
    }

    luoMuuttujat(parametrit) {
        let mu = document.getElementById('muuttujat');
        mu.innerHTML = '';

        for (let pn in parametrit) {
            if (pn[0] == 'm') {
                let val = parseInt(parametrit[pn]);
                if (isNaN(val)) val = null;
                let nimi = pn.substring(1);
                if (nimi === "int") continue;
                if (nimi === "") {
                    if (val === null) continue;
                    nimi = "" + val;
                }
                this.lisaaMuuttuja(nimi, val, null, true);
            } else if (pn[0] == 'i') {
                let iNro = parseInt(parametrit[pn]);
                this.luoIndeksi(pn.substring(1), iNro);
            }
        }

        const v = parametrit.v;
        if (v) {
            const vakioArvot = v.split(',').map(x => parseInt(x)).filter(x => !isNaN(x));
            for (const arvo of vakioArvot) {
                this.lisaaMuuttuja("" + arvo, arvo, null, true);
            }
        }

        if (!vk.simple) return;
        let n = vk.tila().taulukko.length;
        for (let i = 0; i < n; i++)
            this.luoIndeksi("" + i, i);
    }

    kirjaaValmiidenMaara() {
        this._ohjelma[0].kirjaaValmiidenMaara();
    }

    virkista() {
        this.taulukko?.virkista();
        for (const m of Object.values(this.muuttujat)) m.virkista();
        for (const lauseke of this.lausekkeet) lauseke.virkista();
    }

}


class Taulukko {
    constructor(vk) {

        if (vk.tila().taulukko.length < 1) return;
        this.divi = luoDiv('taulukko', {
            className: 'taulukko',
            title: 'Taulukollinen numeroita tyostettavaksi, raahailepa niita.',
            innerHTML: ''
        });
        this.vm = vk;
        this.divi.tila = this.vm.tila();
        this.vm.tila().taulukko.olio = this;

        let taululause = 'int[] t = {';

        for (let i = 0; i < this.vm.tila().taulukko.length; ++i) {
            let v = this.vm.tila().taulukko[i];
            taululause += v + ', ';
            let nos = new Solu(this.vm, i, v, true);
            this.divi.appendChild(nos.div);
        }
        // Todo Kaaaaamea kludge alla, ton pitas olla oma lause

        taululause = taululause.substring(0, taululause.length - 2) + '};';
        let tldiv = luoDiv('taulukon_alustus',
            {
                className: 'lauseke',
                title: taululause,
                innerHTML: taululause
            });

        haeElementti('ohjelma').appendChild(tldiv);
        haeElementti('taulunaytto').appendChild(this.divi);
    }

    virkista(tila) {
        if (!tila) tila = this.vm.tila();
        for (let i = 0; i < tila.taulukko.length; ++i) {
            let v = this.vm.tila().taulukko[i];
            let s = this.vm.solu(i);
            if (s) s.arvoksi(v, tila);
        }
    }
}


class Ohjelma {

    constructor(vk) {
        this.askeleet = [];
        this.vk = vk;
        this.pc = 0;
    }

    divina(divid, parentdiv) {
        this.divid = divid;
        this.divi = luoDiv(divid, {className: 'ohjelma', title: wordOhjelma});
        this.divi.olio = this;
        if (typeof parentdiv === 'undefined') this.liita('oikea');

        return this.divi;
    };

    tekstina(_tila) {
        let s = this.divi.textContent;
        if (!s) s = siivoaHTML(this.divi.innerHTML);
        if (s) {
            let i = s.indexOf("int[] t");
            if (i == 0) {
                let i = s.indexOf(";");
                s = s.substring(i + 1);
            }
            if (s.indexOf("\n") < 0) s = s.replace(/;/g, ";\n");
            return s.trim(); // poistetaan taulukkorivi
        }


        return this.askeleet.map(function (x) {
            return x.tekstina();
        }).join('\n');
    };

    aja() {
        this.pc = 0;
        let uusiTila = this.vk.alkuun();
        let ok = true;

        while (this.pc < this.askeleet.length) {
            ok = this.askeleet[this.pc].sovita(uusiTila);
            if (!ok) return false;
            uusiTila = this.askeleet[this.pc].uusiTila;
            ++this.pc;
        }
        this.vk.tila(uusiTila);
        return true;
    };

    liita(kooste) {
        let koosteolio = haeElementti(kooste);
        koosteolio.appendChild(this.divi);
    };

    irrota(kooste) {
        let koosteolio = haeElementti(kooste);
        koosteolio.removeChild(this.divi);
    };


    tila() {
        return this.vk.tila();
    };

    tilaksi(uusiTila) {
        return this.vk.tila(uusiTila);
    };

    viimeisinAskel() {
        return this.askeleet[this.askeleet.length - 1];
    };

    kirjaaValmiidenMaara() {
        this.valmiita = this.askeleet.length;
    }

    poistaViimeisin() {
        // this.divi.removeChild(this.divi.lastChild);
        if (this.askeleet.length <= this.valmiita) return;
        const last = this.askeleet.pop();
        last?.divi?.remove();
        last?.muuttuja?.divi?.remove();
        if (last.muuttuja) {
            delete this.vk.muuttujat[last.muuttuja.nimi()];
            if (last.muuttuja.indeksi) {
                last.muuttuja.indeksi.div.remove();
                last.muuttuja.vapautaIndeksi(last.muuttuja.indeksi);
                last.muuttuja.indeksi = null;
            }
        }
        this.aja();
        vk.virkista();
    };

    ehto(ehto, ohjelma) {
        let ohj = this.divi;
        let e = new Ehto(ehto, ohjelma);
        e.sovita(this.tila());  // TODO enta jos ei onnistu?
        let uusiTila = e.uusiTila;
        let a = new Askel(e, uusiTila, this.viimeisinAskel());
        this.tilaksi(uusiTila);
        this.askeleet.push(a);

        ohj.appendChild(a.divina());

        ohj.children[ohj.childElementCount - 1].scrollIntoView(false);
    };

    lisaaDiviNakyville(uusiDivi) {
        let ohj = this.divi;
        ohj.appendChild(uusiDivi);
        ohj.scrollTop = uusiDivi.offsetTop;
    };

    sijoitus(mihin, mista) {
        if (mista == mihin) return;
        if (mihin.vakio) return;
        let s = new Sijoitus(mihin, mista, this.viimeisinAskel());
        s.sovita(this.tila());  // TODO enta jos ei onnistu?
        this.tilaksi(s.uusiTila);
        this.askeleet.push(s);

        this.lisaaDiviNakyville(s.divina());
    };

    unplus(mihin, maara) {
        if (mihin.vakio) return;
        let up = new UnaariPlus(mihin, maara);
        up.sovita(this.tila());  // TODO enta jos ei onnistu?;
        let uusiTila = up.uusiTila;
        this.tilaksi(uusiTila);
        this.askeleet.push(up);

        this.lisaaDiviNakyville(up.divina());
    };

    unmiinus(mihin, maara) {
        if (mihin.vakio) return;
        let um = new UnaariMiinus(mihin, maara);
        um.sovita(this.tila());  // TODO enta jos ei onnistu?
        let uusiTila = um.uusiTila;
        this.tilaksi(uusiTila);
        this.askeleet.push(um);

        this.lisaaDiviNakyville(um.divina());
    };


    uusiMuuttuja(nimi, arvo, vakio) {
        let lm = new LuoMuuttuja(nimi, arvo, vakio);
        lm.sovita(this.tila());
        let uusiTila = lm.uusiTila;
        this.tilaksi(uusiTila);
        if (!alkaaNumerolla(nimi) || true) {
            this.askeleet.push(lm);
            this.lisaaDiviNakyville(lm.divina());
        }

        let muuttuja = new Muuttuja(this, nimi, arvo, vakio);
        lm.muuttuja = muuttuja;
        return muuttuja;
    }
}

class Askel {
    constructor(lause, tila, edellinen) {
        this.lause = lause;
        this.tila = tila;
        this.edellinenAskel = edellinen;
    }

    divina() {
        return this.lause.divina(this.tila); // TODO tartteeko arg.ina vanhan tilan
    };

    tekstina(_tila) {
        return this.lause.tekstina(this.tila); // TODO kts. Askel.divina
    };
}

class Lause {
    constructor(edellinen) {
        this._arvo = null;
        this.divi = null;
        this.tila = null;
        this.uusiTila = null;
        this.edellinenAskel = edellinen;
    }

    arvo() {
        return this._arvo;
    }

    suorita(tila) {
        this.tila = tila;
        this.uusiTila = tila;
        alert('aliolio toteuttaa!');
        return tila;
    }

    divina() {
        this.divi = luoDiv('lause' + this.arvo().toString(),
            {
                'className': "lause",
                'title': this.arvo().toString(),
                'innerHTML': this.arvo().toString()
            });
        return this.divi;
    }

    tekstina(_tila) {
        return this.arvo().toString();
    }
}


class Lohko extends Lause {
    constructor(ehto, edellinen) {
        super(edellinen);
        this.ehto = ehto;
        this.lauseet = [];
        this.edellinenAskel = edellinen;
    }

    arvo() {
        return this.mista.arvo();
    }

    lisaaLause(lause) {
        this.lauseet.push(lause);
    }

    poistaLause(_lause) {
        return this.lauseet.pop();
    }

    asetaEhto(ehto) {
        this.ehto = ehto;
    }

    sovita(tila) {
        this.tila = tila ? tila : vk.tila();
        this.uusiTila = tila.klooni();
        for (let l of this.lauseet) {
            l.sovita(this.uusiTila);
            this.uusiTila = l.uusiTila;
        }
        return true; // this.uusiTila;
    }
}


class Sijoitus extends Lause {
    constructor(mihin, mista, edellinen) {
        super(edellinen);
        this.mihin = mihin;
        this.mista = mista;
        this.edellinenAskel = edellinen;
    }

    arvo() {
        return this.mista.arvo();
    }

    sovita(tila) {
        this.uusiTila = tila.klooni();
        let arvo = this.mista.arvo(this.uusiTila);  // TODO pitas huomioida, jos ei onnistu
        this.mihin.arvoksi(arvo, this.uusiTila);
        return true; // this.uusiTila;
    }

    divina(tila) {
        this.divi = luoDiv('sijoitus' + this.mihin.nimi() +
            this.mista.nimi() + this.mista.arvo(tila), {
            className: "lause sijoitus",
            title: this.mihin.nimi() + ' = ' + this.mista.arvo(tila),
            innerHTML: this.mihin.nimi() + ' = ' + this.mista.htmlksi(tila) + ';<br/>'
        });
        return this.divi;
    }

    tekstina(tila) {
        return this.mihin.htmlksi(tila) + ' = ' + this.mista.htmlksi(tila) + ';';
    }
}


class Ehto extends Lause {
    constructor(ehto, ohjelma, edellinen) {
        super(edellinen);
        this.ehto = ehto;
        this._ohjelma = ohjelma;
        this.edellinenAskel = edellinen;
    }

    arvo() {
        return this.ehto.arvo();
    }

    ohjelma() {
        return this._ohjelma;
    }

    sovita(tila) {
        this.uusiTila = tila.klooni();
        // if (this.ehto.sovita(this.uusiTila)) {
        //     this.uusiTila = this.ohjelma.sovita(this.uusiTila);
        // }
        return true; // this.uusiTila;
    }

    divina(_tila) {
        this.divi = luoDiv('ehto' + this.ehto.nimi(), {
            className: "lause",
            title: this.ehto.nimi()
        });
        this.divi.appendChild(this.ohjelma.divi);
        return this.divi;
    }
}

class UnaariOperaatio extends Lause {
    constructor(mihin, maara) {
        super();
        this.mihin = mihin;
        this.maara = typeof maara === 'undefined' ? new Luku(1) : new Luku(maara);
    }

    arvo(tila) {
        return this.mihin.arvo(tila);
    }

    sovita(tila) {
        this.uusiTila = tila.klooni();
        let arvo = this.mihin.arvo(tila);
        arvo = this.op(arvo, this.maara.arvo());
        this.mihin.arvoksi(arvo, this.uusiTila);
        return true;
    }

    divina(tila) {
        this.divi = luoDiv(this.divPrefix() + this.mihin.nimi() + this.mihin.arvo(tila), {
            className: "lause unaari",
            title: this.tekstina(tila),
            innerHTML: this.mihin.nimi() + ' ' + this.opSymbol() + '= ' + this.maara.arvo() + ';<br/>'
        });
        return this.divi;
    }

    tekstina(_tila) {
        return this.mihin.nimi() + ' ' + this.opSymbol() + '= ' + this.maara.arvo() + ';';
    }

    // Methods to override
    op(a, b) { return a; }
    opSymbol() { return '?'; }
    divPrefix() { return 'uop'; }
}

class UnaariPlus extends UnaariOperaatio {
    op(a, b) { return a + b; }
    opSymbol() { return '+'; }
    divPrefix() { return 'uplus'; }
}


class UnaariMiinus extends UnaariOperaatio {
    op(a, b) { return a - b; }
    opSymbol() { return '-'; }
    divPrefix() { return 'umiinus'; }
}

class LuoMuuttuja extends Lause {
    constructor(nimi, arvo, vakio) {
        super();
        this.nimi = nimi;
        this._arvo = arvo;
        this.vakio = vakio;
    }

    arvo() {
        return this._arvo;
    }

    sovita(tila) {
        // if (this.vakio) return true;
        this.uusiTila = tila.klooni();
        this.uusiTila.muuttujat[this.nimi] = this._arvo;
        return true; // this.uusiTila;
    }

    divina(tila) {
        let lause = this.tekstina(tila);
        let lisaClass = '';
        if (alkaaNumerolla(this.nimi)) {
          lause = '';
          lisaClass = ' piilotettu';
        }
        this.divi = luoDiv('luo_muuttuja' + this.nimi + this._arvo, {
            className: 'lause luo_muuttuja' + lisaClass,
            title: lause,
            innerHTML: lause + '<br/>'
        });
        return this.divi;
    }

    tekstina(_tila) {
        let lause = 'int ' + this.nimi;
        if (this._arvo !== null)
            lause += ' = ' + this._arvo;
        lause += ';';
        if (alkaaNumerolla(this.nimi)) lause = '';
        return lause;
    }
}

class Luku {
    constructor(arvo) {
        this.sisalto = arvo;
        this.divi = null;
    }

    arvo(_tila) {
        return this.sisalto;
    }

    nimi() {
        return this.sisalto.toString();
    }

    divina(tila) {
        if (!this.divi)
            this.divi = luoDiv('', {
                className: 'luku',
                title: this.tekstina(tila),
                innerHTML: this.htmlksi()
            });
        return this.divi;
    }

    htmlksi() {
        return this.sisalto.toString();
    }

    tekstina(_tila) {
        return this.sisalto.toString();
    }
}

class Tila {
    constructor(taulukko, muuttujat) {
        this.taulukko = taulukko;
        this.muuttujat = muuttujat;
    }

    klooni() {
        // Deep enough copy: elements should be immutable.
        let uusiTaulukko = this.taulukko.slice(0);
        uusiTaulukko.olio = this.taulukko.olio;

        let uudetMuuttujat = this.muuttujat.constructor();
        for (let attr in this.muuttujat)
            if (this.muuttujat.hasOwnProperty(attr))
                uudetMuuttujat[attr] = this.muuttujat[attr];
        return new Tila(uusiTaulukko, uudetMuuttujat);
    }

    asetaIndeksiin(i, v) {
        this.taulukko[i] = v;
    }

    asetaMuuttuja(n, v) {
        this.muuttujat[n] = v;
    }

    muuttujassa(n) {
        return this.muuttujat[n];
    }

    indeksissa(i) {
        return this.taulukko[i];
    }

    muuttujaan(i, k) {
        this.muuttujat[k] = this.taulukko[i];
    }

    muuttujasta(k, i) {
        this.taulukko[i] = this.muuttujat[k];
    }

    indeksiin(k, ki) {
        this.taulukko[this.muuttujat[ki]] = this.muuttujat[k];
    }

    indeksista(ki, k) {
        this.muuttujat[k] = this.taulukko[this.muuttujat[ki]];
    }
}


class Solu {
    constructor(vm, indeksi, arvo) {
        let divid = 'solu-' + indeksi;
        this.divid = divid;
        this.div = luoDiv(divid, {'className': 'solu'});
        this.divSn = luoDiv('solu-n' + indeksi, {
            'className': 'solu-n',
            'innerHTML': indeksi.toString()
        });
        this.divSa = luoDiv('solu-a' + indeksi, {
            'className': 'solu-a',
            'draggable': true
        });
        this.divSi = luoDiv('solu-i' + indeksi, {
            'className': 'solu-i',
            'draggable': false,
            'innerHTML': ''
        });

        this.indeksimuuttujat = [];
        this.sulje();
        this.kuuntelijat = [];
        this.vm = vm;
        this.indeksi = indeksi;
        this.div.olio = this;
        this.divSa.olio = this;
        this.divSi.olio = this;
        this.arvoksi(arvo || 0);

        this.div.solu = this;
        this.divSa.solu = this;
        this.divSi.solu = this;

        let tama = this;

        function asetaLahde(event) {
            if (tama.suljettu()) {
                event.preventDefault();
                event.stopPropagation();
                return;
            }
            if (tama.divSi.children.length > 1) {
                event.preventDefault();
                event.stopPropagation();
                alert('Solulla on useampia indekseja, joten raahaa indekseista valitaksesi sopivan.');
                return;
            }
            if (Operaatio.odottaaLahdetta()) {
                Operaatio.asetaLahde(event.target);
                Operaatio.asetaData({
                    tyyppi: 'solu',
                    indeksi: event.target.solu.indeksi.toString(),
                    oid: event.target.id
                });
            }
        }

        function kaytaKohde(event) {
            Operaatio.asetaKohde(event.target);
            if (tama.divSi.children.length > 1) {
                event.preventDefault();
                event.stopPropagation();
                alert('Solulla on useampia indekseja, joten pudota indeksiin valitaksesi sopivan.');
                return Operaatio.tehty();
            }
            event.preventDefault();
            let data = Operaatio.haeData();
            let tyyppi = data.tyyppi;
            let oid = data.oid;
            let olio = haeElementti(oid).olio;
            let mihin = event.target.olio;
            if (mihin.suljettu && mihin.suljettu()) {
                return Operaatio.tehty();
            }
            let mista = olio;
            let ohj = document.getElementById('ohjelma').olio;

            if (tyyppi == "muuttuja" || tyyppi == "solu" ||
                tyyppi === 'indeksi' || tyyppi === 'vakio') {
                event.stopPropagation();
                ohj.sijoitus(mihin, mista);
                Operaatio.tehty();
                return;
            } else if (tyyppi == "lauseke") {
                let lausekediv = haeElementti(oid);
                event.stopPropagation();
                ohj.sijoitus(mihin, mista);
                lausekediv.olio.katoa();
                Operaatio.tehty();
                return;
            } else if (tyyppi === 'unop') {
                event.stopPropagation();
                if (data.nimi === 'miinus') ohj.unmiinus(mihin);
                else if (data.nimi === 'plus') ohj.unplus(mihin);
                Operaatio.tehty();
                return;
            }
            return true;
        }

        lisaaKuuntelijat(this.divSa, asetaLahde, kaytaKohde, function () {
            return tama.suljettu();
        });

        function kaytaKohdeSi(event) {
            event.preventDefault();
            Operaatio.asetaKohde(event.target);
            let data = Operaatio.haeData();
            let mihin = event.target.olio;
            let mista = Operaatio.lahde().olio;
            pudotaIndeksialueeseen(vm, event, data, mihin, mista);
            Operaatio.tehty();
        }

        lisaaKuuntelijat(this.divSi, null, kaytaKohdeSi, true);

        this.div.appendChild(this.divSn);
        this.div.appendChild(this.divSa);
        if (!vk.simple) this.div.appendChild(this.divSi);
    }

    sidoIndeksi(i) {
        this.kuuntelijat.push(i);
    }

    vapautaIndeksi(i) {
        this.kuuntelijat = this.kuuntelijat.filter(x => x !== i);
    }

    paivitaIndeksit(tila) {
        let _tila = tila || this.vm.tila();
        this.kuuntelijat.map(x => x.muuttujaPaivitetty(this, _tila));
    }

    arvoksi(x, tila) {
        tila = tila ? tila : this.vm.tila();
        tila.asetaIndeksiin(this.indeksi, x);
        this.divSa.innerHTML = x.toString();
        return tila.indeksissa(this.indeksi);
    }

    arvo(tila) {
        return tila ? tila.indeksissa(this.indeksi) : this.vm.tila().indeksissa(this.indeksi);
    }

    nimi(r) {
        if ((this.indeksimuuttujat.length !== 0) && (r !== this))
            return this.indeksimuuttujat[0].nimi(this);
        return 't[' + this.indeksi + ']';
    }

    sovita(_tila) {
        return true;
    }

    htmlksi() {
        return this.nimi();
    }

    suljettu() {
        let sulj = (this.divSi.innerHTML === '');
        let validi = false;
        if (!sulj) {
            for (let i = this.divSi.firstChild; i !== null; i = i.nextSibling)
                validi = i.olio.validi() || validi;
        }
        return sulj || !validi;
    }

    avaa() {
        if (!this.suljettu()) {
            this.divSa.classList.remove('suljettu');
            this.divSa.classList.add('avattu');
        }
    }

    sulje() {
        if (this.suljettu()) {
            this.divSa.classList.remove('avattu');
            this.divSa.classList.add('suljettu');
        }
    }
}

class Indeksi {
    constructor(vm, solu) {
        let divid = 'indeksi' + timestring();
        this.divid = divid;
        this.div = luoDiv(divid, {
            className: 'indeksi',
            draggable: true,
            title: 'raahaa tahan muuttuja, niin voit viitata taulukon alkioihin\n' +
                'muuttujan arvon muuttaminen tai taman raahaaminen siirtaa viittausta',
            innerHTML: '&nbsp;'
        });
        this.vm = vm;
        this.muuttuja = null;
        this.div.olio = this;
        this.div.solu = solu;

        const asetaLahde = (event) => {
            Operaatio.asetaLahde(event.target);
            event.stopPropagation();
            Operaatio.asetaData({
                tyyppi: 'indeksi',
                indeksi: event.target.solu.indeksi.toString(),
                oid: event.target.id
            });
        };

        const kaytaKohde = (event) => {
            Operaatio.asetaKohde(event.target);
            event.stopPropagation();
            event.preventDefault();
            let data = Operaatio.haeData();
            let tyyppi = data.tyyppi;
            let oid = data.oid;
            let olio = haeElementti(oid).olio;
            let mihin = event.target.olio;
            let mista = olio;
            let ohj = document.getElementById('ohjelma').olio;

            if (tyyppi == "muuttuja") {
                if (mihin.div.solu.divSi.children.length === 0) {
                    mihin.indeksoi(mista);
                    mihin.div.innerHTML = mista.nimi();
                    mihin.siirry();
                } else {
                    ohj.sijoitus(mihin, mista);
                }
            } else if (tyyppi == "indeksi" || tyyppi == "solu" || tyyppi === "vakio") {
                ohj.sijoitus(mihin, mista);
            } else if (tyyppi == "lauseke") {
                let lausekediv = haeElementti(oid);
                ohj.sijoitus(mihin, mista);
                lausekediv.olio.katoa();
            } else if (tyyppi === 'unop') {
                let nimi = data.nimi;
                if (nimi === 'miinus') ohj.unmiinus(mihin);
                else if (nimi === 'plus') ohj.unplus(mihin);
            }
            Operaatio.tehty();
        };

        lisaaKuuntelijat(this.div, asetaLahde, kaytaKohde, true);
    }

    validi(tila) {
        if ((typeof this.muuttuja === 'undefined') || (this.muuttuja === null))
            return false;
        let taulukko = tila ? tila.taulukko : this.vm.tila().taulukko;
        let ind = this.muuttuja.arvo(tila);
        return !((ind < 0) || (ind >= taulukko.length));
    }

    muuttujaPaivitetty(m, tila) {
        this.siirry(tila);
    }

    arvoksi(x, tila) {
        tila = tila ? tila : this.vm.tila();
        if (!this.validi(tila)) return null;
        let ind = this.muuttuja.arvo(tila);
        let solu = this.vm.solu(ind);
        return solu.arvoksi(x, tila);
    }

    arvo(tila) {
        tila = tila ? tila : this.vm.tila();
        if (!this.validi(tila)) return null;
        return tila.indeksissa(this.muuttuja.arvo());
    }

    sulje() {
        // noinspection JSUnresolvedVariable
        let d = this.div.solu;
        d?.sulje();
    }

    siirry(tila) {
        if (!this.validi(tila)) return;
        let ind = this.muuttuja.arvo(tila);
        let s = this.vm.solu(ind);
        // noinspection JSUnresolvedVariable
        let d = this.div.solu;
        let ti = this;
        if (s == null) return;
        d?.divSi?.removeChild(this.div);
        if (d && d.indeksimuuttujat)
            // noinspection JSIncompatibleTypesComparison
            d.indeksimuuttujat = d.indeksimuuttujat.filter(x => x !== ti);

        s?.divSi?.appendChild(this.div);
        s?.indeksimuuttujat?.push(this);

        d?.sulje();
        s?.avaa();

        this.solu = s;
        this.div.solu = s;
    }

    indeksoi(muuttuja, _tila) {
        if (this.muuttuja === muuttuja) return;
        if (this.muuttuja !== null)
            this.muuttuja.vapautaIndeksi(this);
        this.muuttuja = muuttuja;
        this.muuttuja.sidoIndeksi(this);
    }

    paivita(tila) {
        let ind = this.muuttuja.arvo(tila);
        this.vm.solu(ind);
    }

    nimi(_r) {
        if (this.muuttuja === null) return '?';
        return 't[' + this.muuttuja.nimi() + ']';
    }

    htmlksi(_tila) {
        return this.nimi();
    }
}


class Vakio {
    constructor(value, vm) {
        let divid = 'vakio' + value;
        this.divid = divid;
        this.vm = vm;
        this.value = value;

        this.divi = luoDiv(this.divid, {
            className: 'const',
            title: 'raahaan tama arvoon, tai tahan arvo, ' +
                'muuttaaksesi sen arvoksi ' + this.value,
            draggable: true
        });
        this.divi.innerHTML = '_&larr;' + this.value.toString();

        const asetaLahde = (event) => {
            Operaatio.asetaLahde(event.target);
            Operaatio.asetaData({
                tyyppi: 'vakio',
                nimi: value.toString(),
                oid: divid
            });
        };

        const kaytaKohde = (event) => {
            Operaatio.asetaKohde(event.target);
            let data = Operaatio.haeData();
            let tyyppi = data.tyyppi;
            let oid = data.oid;
            let ohj = document.getElementById('ohjelma').olio;
            let kohde = haeElementti(oid).olio;

            if ((tyyppi === 'muuttuja') || (tyyppi === 'solu') ||
                (tyyppi === 'indeksi')) {
                event.stopPropagation();
                let luku = new Luku(value);
                ohj.sijoitus(kohde, luku);
            }
            Operaatio.tehty();
        };

        lisaaKuuntelijat(this.divi, asetaLahde, kaytaKohde, false);

        this.divi.olio = this;
    }

    arvo(_tila) {
        return this.value;
    }

    nimi(tila) {
        return this.arvo(tila).toString();
    }

    htmlksi(tila) {
        return this.arvo(tila).toString();
    }

    liita(kooste) {
        let koosteolio = haeElementti(kooste);
        koosteolio.appendChild(this.divi);
    }

    irrota(kooste) {
        let koosteolio = haeElementti(kooste);
        koosteolio.removeChild(this.divi);
    }
}

class UnOp {
    constructor(vm, opSymbol, opName, title, innerHTML) {
        let divid = 'unop' + opName + '-' + timestring();
        this.divid = divid;
        this.vm = vm;

        this.divi = luoDiv(this.divid, {
            className: 'unop',
            title: title,
            draggable: true
        });
        this.divi.innerHTML = innerHTML;
        this.divi.olio = this;

        const asetaLahde = (event) => {
            Operaatio.asetaLahde(event.target);
            Operaatio.asetaData({
                tyyppi: 'unop',
                nimi: opName,
                oid: divid
            });
        };

        const kaytaKohde = (event) => {
            Operaatio.asetaKohde(event.target);
            let data = Operaatio.haeData();
            let tyyppi = data.tyyppi;
            let oid = data.oid;
            let ohj = document.getElementById('ohjelma').olio;

            let kohde = haeElementti(oid).olio;

            if ((tyyppi === 'muuttuja') || (tyyppi === 'solu') ||
                (tyyppi === 'indeksi')) {
                event.stopPropagation();

                let lauseke = new Lauseke();
                lauseke.asetaOper(opSymbol);
                lauseke.asetaArg1(kohde);
                lauseke.asetaArg2(new Luku(1));

                ohj.sijoitus(kohde, lauseke);
            }
            Operaatio.tehty();
        };

        lisaaKuuntelijat(this.divi, asetaLahde, kaytaKohde, true);
    }

    liita(kooste) {
        let koosteolio = haeElementti(kooste);
        koosteolio.appendChild(this.divi);
    }

    irrota(kooste) {
        let koosteolio = haeElementti(kooste);
        koosteolio.removeChild(this.divi);
    }
}

class UnOpPlus extends UnOp {
    constructor(vm) {
        super(
            vm,
            '+',
            'plus',
            'raahaa tämä arvoon, tai tähän arvo, kasvattaaksesi arvoa yhdellä',
            '_+1'
        );
    }
}

class UnOpMiinus extends UnOp {
    constructor(vm) {
        super(
            vm,
            '-',
            'miinus',
            'raahaa tämä arvoon, tai tähän arvo, vähentääksesi arvoa yhdellä',
            '_-1'
        );
    }
}


class Echo {
    constructor(id) {
        this.divi = document.getElementById(id);
        this.divi.addEventListener('click', (_event) => {
            echo.puhdista();
        }, false);
    }

    puhdista() {
        if (this.divi)
            this.divi.innerHTML = '';
    }

    aseta(str) {
        if (this.divi)
            this.divi.innerHTML = str.toString();
        else
            console.log('Echo: ' + str.toString());
    }

    lisaa(str) {
        if (this.divi)
            this.divi.innerHTML += str.toString();
        else
            console.log('Echo: ' + str.toString());
    }
}

class Muuttuja {
    constructor(vm, nimi, arvo, vakio) {
        let divid = 'muuttuja-' + nimi;
        this.divid = divid;
        this.vm = vm;
        this._nimi = nimi;
        this.vakio = vakio;
        this.indeksi = null;
        if (alkaaNumerolla(nimi)) this.vakio = true;

        this.kuuntelijat = [];

        this.divi = luoDiv(this.divid + '-b', {});

        let ntext = vakio ? "" : nimi + ":";
        let no = luoDiv(this.divid, {
            className: 'muuttuja',
            innerHTML: ntext
        });

        let ma = luoDiv(this.divid + '-a', {
            className: 'muuttuja-arvo',
            draggable: true
        });

        ma.innerHTML = arvo !== null ? arvo.toString() : '&nbsp;&nbsp;';
        ma.arvo = arvo;
        ma.nimi = nimi;

        const asetaLahde = (event) => {
            Operaatio.asetaLahde(event.target);
            Operaatio.asetaData({
                tyyppi: "muuttuja",
                nimi: event.target.olio.nimi(),
                oid: divid
            });
        };

        const kaytaKohde = (event) => {
            if (vakio) return Operaatio.tehty();
            Operaatio.asetaKohde(event.target);

            let data = Operaatio.haeData();
            let tyyppi = data.tyyppi;
            let nimi = data.nimi;
            let oid = data.oid;
            let ohj = document.getElementById('ohjelma').olio;

            let olio = haeElementti(oid).olio;
            let mihin = event.target.olio;
            let mista = olio;

            if ((tyyppi === "solu") || (tyyppi === 'indeksi') || (tyyppi === 'vakio') ||
                ((tyyppi === "muuttuja") && (nimi != event.target.nimi))) {
                event.stopPropagation();
                ohj.sijoitus(mihin, mista);
            } else if (tyyppi === "lauseke") {
                let lausekediv = haeElementti(oid);
                event.stopPropagation();
                ohj.sijoitus(mihin, mista);
                lausekediv.olio.katoa();
            } else if (tyyppi == 'unop') {
                event.stopPropagation();
                if (nimi === 'miinus')
                    ohj.unmiinus(mihin);
                else if (nimi === 'plus')
                    ohj.unplus(mihin);
            }
            Operaatio.tehty();
        };

        lisaaKuuntelijat(ma, asetaLahde, kaytaKohde, !vakio);

        ma.muuttujat = this.vm.tila().muuttujat;
        ma.nimi = nimi;

        this.divi.appendChild(no);
        this.divi.appendChild(ma);
        ma.olio = this;
        no.olio = this;
        this.diviArvo = ma;

        this.arvoksi(arvo);
    }

    sidoIndeksi(i) {
        this.kuuntelijat.push(i);
        this.indeksi = i;
    }

    vapautaIndeksi(i) {
        this.kuuntelijat = this.kuuntelijat.filter(x => x !== i);
        this.indeksi?.sulje();
        this.indeksi = null;
    }

    paivitaIndeksit(tila) {
        let _tila = tila || this.vm.tila();
        this.kuuntelijat.map(x => x.muuttujaPaivitetty(this, _tila));
    }

    nimi() {
        return this._nimi;
    }

    htmlksi() {
        return this.nimi();
    }

    arvo(tila=null) {
        tila = tila ? tila : this.vm.tila();
        return tila.muuttujassa(this.nimi());
    }

    arvoksi(x, tila) {
        tila = tila ? tila : this.vm.tila();
        tila.asetaMuuttuja(this.nimi(), x);
        this.virkista(tila);
        this.paivitaIndeksit(tila);
        return tila.muuttujassa(this.nimi());
    }

    virkista(tila=null) {
        let x = this.arvo(tila);
        if (x != null)
            this.diviArvo.innerHTML = x.toString();
        else
            this.diviArvo.innerHTML = '&nbsp;&nbsp;';
        if (x==null && this.indeksi) {
            this.indeksi.div.remove();
            this.vapautaIndeksi(this.indeksi);
        }
        this.paivitaIndeksit(tila);
    }
    sovita(_tila) {
        return true;
    }

    nimea(n) {
        this._nimi = n;
    }

    liita(kooste) {
        let koosteolio = haeElementti(kooste);
        koosteolio.appendChild(this.divi);
    }

    irrota(kooste) {
        let koosteolio = haeElementti(kooste);
        koosteolio.removeChild(this.divi);
    }
}


class Nappi {
    constructor(divid, tunnus, toimintoFn) {
        this.divid = divid;
        this.tunnus = tunnus;

        this.elementti = document.createElement('button');
        this.elementti.innerHTML = this.tunnus;
        this.elementti.onclick = toimintoFn;
        this.elementti.olio = this;
    }

    liita(kooste) {
        let koosteolio = haeElementti(kooste);
        koosteolio.appendChild(this.elementti);
    }

    irrota(kooste) {
        let koosteolio = haeElementti(kooste);
        koosteolio.removeChild(this.elementti);
    }

    aktivoi(aktiivinen) {
        this.elementti.disabled = !aktiivinen;
    }
}

class Lauseke {
    constructor(divid) {
        if (typeof divid === 'undefined') {
            divid = 'lauseke' + timestring();
        }

        this.divid = divid;
        this.arg1 = null;
        this.operators = {
            '+': (a, b) => a + b,
            '-': (a, b) => a - b,
            '*': (a, b) => a * b,
            '/': (a, b) => Math.trunc(a / b),
            '%': (a, b) => a % b,
            ' ': (_a, _b) => '',  // No-op for easier UI handling
            // '<': (a, b) => a < b
        };
        this.operator = '+';
        this.arg2 = null;
        this.sel = null;
        this.diviOp = null;

        this.divi = luoDiv(divid, {
            className: 'lauseke',
            title: 'Raahaamalla tähän arvoja suoritat laskutoimituksen,\n' +
                'jonka tuloksen voit raahata muuttujaan tai taulukkoon.'
        });
        this.diviA1 = luoDiv(divid + '-a1', { className: 'arg' });
        [this.diviOp, this.sel] = luoOperaattorit(divid + '-op', this, this.operators);
        this.diviA2 = luoDiv(divid + '-a2', { className: 'arg' });
        this.diviEq = luoDiv(divid + '-eq', { className: 'equals-literal' });
        this.diviVal = luoDiv(divid + '-v', { className: 'arvo', draggable: true });

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

        const asetaLahde = (event) => {
            Operaatio.asetaLahde(event.target);
            if (!this.validi()) {
                alert('Vain valmiin lausekkeen voi siirtää!');
                echo.aseta('Vain valmiin lausekkeen voi siirtää!');
                event.preventDefault();
                Operaatio.peruuta();
                return;
            }
            Operaatio.asetaData({
                tyyppi: "lauseke",
                oid: this.divi.id
            });
        };

        lisaaKuuntelijat(this.diviVal, asetaLahde, null, false);

        const asetus = (asfn) => (event) => {
            if (!Operaatio.odottaaLahdetta()) {
                Operaatio.asetaKohde(event.target);
                let data = Operaatio.haeData();
                let tyyppi = data.tyyppi;
                if (tyyppi == "solu" || tyyppi == "muuttuja" || tyyppi === 'indeksi') {
                    event.stopPropagation();
                    let oid = data.oid;
                    let olio = document.getElementById(oid).olio;
                    event.target.olio[asfn](olio);
                }
                event.preventDefault();
                Operaatio.tehty();
            }
        };

        lisaaKuuntelijat(this.diviA1, null, asetus('asetaArg1'), true);
        lisaaKuuntelijat(this.diviA2, null, asetus('asetaArg2'), true);
    }

    validi() {
        return this.arg1 && this.operator && this.arg2;
    }

    sovita(tila) {
        return this.arvo(tila) !== null;
    }

    arvo(tila) {
        if (!this.validi()) return "";
        return this.operators[this.operator](this.arg1.arvo(tila), this.arg2.arvo(tila));
    }


    asetaArg(i, arvo) {
        if (!arvo) return;
        this["arg"+i] = arvo;
        let nimiTaiIndeksi = arvo.nimi() + ":";
        if (arvo.vakio) nimiTaiIndeksi = "";
        let a = arvo.arvo();
        if (a == null) { a = '__'; nimiTaiIndeksi = ''; this["arg"+i] = null; }
        this["diviA"+i].innerHTML = nimiTaiIndeksi + a;
        this.diviVal.innerHTML = "" + (this.validi() ? this.arvo() : '__');
    }
    asetaArg1(arvo) {
        this.asetaArg(1, arvo);
    }

    asetaArg2(arvo) {
        this.asetaArg(2, arvo);
    }

    asetaOper(symboli) {
        this.operator = symboli;
        if (symboli === ' ') {
             this.tyhjenna();
             return;
        }
        this.diviVal.innerHTML = "" + (this.arvo());
    }

    liita(kooste) {
        let koosteolio = haeElementti(kooste);
        koosteolio.appendChild(this.divi);
    }

    irrota(kooste) {
        let koosteolio = haeElementti(kooste);
        koosteolio.removeChild(this.divi);
    }

    katoa() {
        // let parent = this.divi.parentElement;
        // parent.removeChild(this.divi);
        // let lauseke = new Lauseke();
        // lauseke.liita(parent);
    }

    tyhjenna() {
        this.arg1 = null; this.diviA1.innerHTML = "&nbsp;";
        this.arg2 = null; this.diviA2.innerHTML = "&nbsp;";
        this.operator = '+';
        this.sel.value = this.operator;
        this.diviVal.innerHTML = "&nbsp;"
    }

    nimi() {
        return this.arg1.nimi() + this.operator + this.arg2.nimi();
    }

    htmlksi() {
        return this.arg1.nimi() + ' ' + this.operator.replace('>', '&lt;') + ' ' + this.arg2.nimi();
    }

    virkista() {
        this.asetaArg(1, this.arg1);
        this.asetaArg(2, this.arg2);
        this.asetaOper(this.operator);
    }
} // Lauseke

function siivoaHTML(s) {
    //s = s.replace(/<div.*?<\/div>/, "");
    let r = "";
    while (true) {
        s = s.replace(/<div.*?>/, "");
        let i = s.indexOf("<br");
        if (i < 0) break;
        r += s.substring(0, i) + "\n";
        s = s.replace(/.*?<\/div>/, "");
    }
    return r.trim();
}

function pudotaIndeksialueeseen(vm, event, data, mihin, mista) {
    let tyyppi = data.tyyppi;
    let ohj = document.getElementById('ohjelma').olio;

    event.stopPropagation();

    if (mihin.constructor === mista.constructor) return;

    if (tyyppi === 'indeksi') {
        let erotus = mihin.indeksi - mista.div.solu.indeksi;

        if (erotus < 0) ohj.unmiinus(mista.muuttuja, -erotus);
        else ohj.unplus(mista.muuttuja, erotus);


        return;
    }
    if (tyyppi === 'muuttuja') {
        let indeksi = mista.arvo() === null ? mihin.indeksi : mista.arvo();
        let luotuMuuttuja = false;

        if ((indeksi < 0) || (indeksi >= vm.tila().taulukko.length)) return;

        if (mista.arvo() === null) {
            mista.arvoksi(mihin.indeksi);
            luotuMuuttuja = true;
        }

        let io = new Indeksi(vm, mihin);
        io.indeksoi(mista);
        io.div.innerHTML = mista.nimi();

        mihin.divSi.appendChild(io.div);
        mihin.indeksimuuttujat.push(io);
        io.siirry();

        if (luotuMuuttuja) {
            ohj.sijoitus(mista, new Luku(mihin.indeksi));
        } else {
            let f = io.solu.indeksimuuttujat.filter(
                function (x) {
                    return x.muuttuja.nimi() === mista.nimi();
                });
            if (f.length > 1) {
                io.solu.indeksimuuttujat =
                    io.solu.indeksimuuttujat.filter(function (x) {
                        // noinspection JSIncompatibleTypesComparison
                        return x !== io;
                    });
                io.solu.divSi.removeChild(io.div);
            }
        }

        return;
    }
    if (tyyppi === 'unop') {
        //  if (nimi === 'miinus') ohj.unmiinus(mihin);
        //  else if (nimi === 'plus') ohj.unplus(mihin);

    }
}


function lisaaDragKuuntelija(div, fSallittu) {

    if (typeof (fSallittu) != "function") fSallittu = false;
    div.original = div.style.background;

    div.addEventListener('dragover', function (event) {
        if (fSallittu && fSallittu()) return true;
        if (event.preventDefault) {
            event.preventDefault();
        }
        event.dataTransfer.effectAllowed = 'copy';
        return false;
    }, false);

    div.addEventListener('dragenter', function (event) {
        if (fSallittu && fSallittu()) return true;
        event.preventDefault();
        event.dataTransfer.effectAllowed = 'copy';
        if (event.target.style) event.target.style.background = "gray";
        return false;
    }, false);
    div.addEventListener('dragleave', function (event) {
        if (fSallittu && fSallittu()) return true;
        event.preventDefault();
        event.dataTransfer.effectAllowed = 'copy';
        if (event.target.style) event.target.style.background = "";
        return false;
    }, false);
}

function lisaaKuuntelijat(div, asetaLahde, kaytaKohde, salliPudotus) {
    if (asetaLahde) div.addEventListener('dragstart', function (event) {
        asetaLahde(event);
        event.dataTransfer.effectAllowed = 'copy';
        event.dataTransfer.setData('text/plain', ""); // FF ei muuten toimi
    }, false);

    if (kaytaKohde) div.addEventListener('drop', function (event) {
        kaytaKohde(event);
    }, false);

    div.addEventListener('click', function (event) {
        if (Operaatio.odottaaLahdetta()) {
            if (asetaLahde) asetaLahde(event);
        } else {
            if (kaytaKohde) kaytaKohde(event);
        }
    }, false);

    if (salliPudotus) lisaaDragKuuntelija(div, salliPudotus);
}

/*
function add_drag_drop_new(divi, callbacks) {
    divi.addEventListener('dragstart', callbacks.dragstart ||
        function (event) {
        }, false);
    divi.addEventListener('dragenter', callbacks.dragenter ||
        function (event) {
        }, false);
    divi.addEventListener('dragleave', callbacks.dragleave ||
        function (event) {
        }, false);
    divi.addEventListener('dragover', callbacks.dragover ||
        function (event) {
            if (event.preventDefault) {
                event.preventDefault();
            }
            event.dataTransfer.effectAllowed = 'copy';
        }, false);
    divi.addEventListener('drop', callbacks.drop ||
        function (event) {
            if (event.stopPropagating) {
                event.stopPropagating();
            }
            event.dataTransfer.dropEffect = 'copy';
            return false;
        }, false);
}
 */

function luoOperaattorit(divid, kohde, opers) {
    let divi = luoDiv(divid, {'className': 'op'});
    let sel = document.createElement('select');
    for (let on in opers) {
        let op = document.createElement('option');
        op.value = on;
        op.label = on;
        op.text = on;      // firefox vaatii tamankin, webkitille riittaa ylemmat...
        op.accessKey = on; // elvistelya
        op.fn = opers[on];

        sel.appendChild(op);
    }
    sel.addEventListener('change',
        function (_event) {
            kohde.asetaOper(sel.value);
        }, false);
    divi.appendChild(sel);
    return [divi, sel];
}

function uusiLauseke(diviin) {
    let lauseke = new Lauseke();
    lauseke.liita(diviin);
    vk.lausekkeet.push(lauseke);
    return lauseke;
}


function ehtolause(_event) {
    alert('ei tee mitaan viela');
}

function ehtolauseLoppu(_event) {
    alert('ei tee mitaan viela');
}

function undo() {
    vk.ohjelma().poistaViimeisin();
}


function alustaTauno(_event) {
    let naytto = haeElementti('naytto');
    let tnimi = '<p class="muuttujan-nimi">t:</p>';
    let prt = parametrit();
    // noinspection JSUnresolvedVariable
    if (prt.ts && prt.ts == "0") tnimi = "";


    if (prt.lang && prt.lang == "en" || prt.help) {
        wordMuuttujat = "variables:";
        wordUusiMuuttuja = "new variable";
        wordMuuttujatTitle = "list of variables, add variable from button above";
        wordOhjelma = "program";
        wordApua = 'Parameters:\n' +
            '  help=help in english\n' +
            '  apua=apua suomeksi\n' +
            '  lang=en - user interface as english\n' +
            '  s    - simple, no indices\n' +
            '  mx=v - variable x by value v.\n' +
            '  ix=v - index variable x by value v.\n' +
            '  ts=n - set array size as n.\n' +
            '  t=[x,y,...] - Create array by values x,y,...\n' +
            '  v=x,y,... - Create contants by values x,y,...\n';
        wordNimi = "name:";
        wordArvo = "value:";
        wordLisaa = "Add";
   }

naytto.innerHTML = `
    <div id="tiedot" class="tiedotd">${tnimi}
        <div id="taulunaytto" class="syottod">
        </div>
    </div>
    <div id="toiminta" class="toiminta" >
        <div id="vasen" class="vasen">
          <div class="vasen-top">
            <div id="muuttuja-button-alue" class="muuttuja-button-alue">
              ${wordMuuttujat}
                <button id="uusi-muuttuja" onclick="vk.luoMuuttujaKentista('muuttujat')">
                  ${wordUusiMuuttuja}</button>
            </div>
            <div id="uuden-muuttujan-alue" class="uuden-muuttujan-alue">
                <label>${wordNimi} <input id="uusi-nimi" style="width:6em;" /></label>
                <label>${wordArvo} <input id="uusi-arvo"  style="width:2em;" /></label>
                <button id="uuden-lisays" onclick="vk.teeMuuttuja('muuttujat')">${wordLisaa}</button>
            </div>
            <div id="muuttujat" class="muuttujatd"
                 title="${wordMuuttujatTitle}"
                 onclick="vk.piilotaMuuttujanLisays()">
              <!-- hyi, globaali vk... TODO XXX -->
            </div>
          </div>
          <div class="scratch" id="scratch">
            <div class="compound-statements" id="compound-statements"
                 title="tayta lausekkeen puuttuvat osat raahaamalla, ja raahaa sitten tulos kohteeseen">
            </div>
            <div id="tools" class="tools">
            </div>
          </div>
        </div>
        <div id="oikea" class="oikea">${wordOhjelma}:
        <span class="undo" onclick="undo()"  title="Peruuta edellinen toiminto">Undo</span>
        </div>
      </div>
      <div id="echo" class="echo" title="Viestialue, johon ohjelma tulostaa viesteja toiminnastaan. Puhdista klikkaamalla."></div>
`;

    document.getElementById('toiminta').addEventListener('dragover', function(event) {
        event.preventDefault();
    });
    document.getElementById('toiminta').addEventListener('dragleave', function(event) {
        event.preventDefault();
    });
    document.getElementById('toiminta').addEventListener('dragenter', function(event) {
        event.preventDefault();
    })
    document.getElementById('uusi-nimi').addEventListener('keydown', function(event) {
        const ret = vk.uusiNimiKeypress(event, 'muuttujat');
        if (!ret) event.preventDefault();
        return ret;
    });
    document.getElementById('uusi-arvo').addEventListener('keydown', function(event) {
        const ret = vk.uusiArvoKeypress(event, 'muuttujat');
        if (!ret) event.preventDefault();
        return ret;
    });

    // let prt = parametrit();
    if (('help' in prt) || ('apua' in prt)) {
        alert(wordApua);
    }

    // vk on globaali... hyi...
    vk = new Virtuaalikone();

    let ohjelma = new Ohjelma(vk);
    ohjelma.divina('ohjelma');
    vk.lisaaOhjelma(ohjelma);

    vk.luoTaulukko(prt); // XXX TODO taulukon ja muuttujat voi luoda vasta ohjelman jalkeen... gui-riippuvuus, hyi!
    vk.luoMuuttujat(prt);
    vk.kirjaaValmiidenMaara();

    uusiLauseke('compound-statements');
    // uusiLauseke('compound-statements'); // voisi olla useampiakin

    let up = new UnOpPlus(vk);
    up.liita('tools');
    let um = new UnOpMiinus(vk);
    um.liita('tools');
    let nolla = new Vakio(0, vk);
    nolla.liita('tools');
    let yksi = new Vakio(1, vk);
    yksi.liita('tools');
    // let iffi = new Nappi('if-nappi', 'if', ehtolause);
    // iffi.liita('tools');
    // let endiffi = new Nappi('endif-nappi', 'end if', ehtolauseLoppu);
    // endiffi.liita('tools');
    if (echo === null) echo = new Echo('echo');
    // echo.lisaa(navigator.platform + '§ ');
    // echo.lisaa(navigator.userAgent + '§ ');
    // echo.lisaa(navigator.appVersion + '§§');
}


if (navigator.userAgent.indexOf('MSIE') > -1 &&
    navigator.userAgent.indexOf('MSIE 10') < 0) {
    alert('Ajat liian vanhaa IE:ta tai IE10 on yhteensopivuustilassa. ' +
        'Tauno vaatii IE10:n modernina (ei compat view)!');
    if (echo === null) echo = new Echo('echo');
    echo.aseta('IE10 compatibility-nakymassa, sammuta se!');
} else
   //noinspection JSUnresolvedVariable
   if (typeof QUnit === "undefined") { // Emmehan ole testausymparistossa?

    // latauksen jalkeen ajettava koodi
    window.addEventListener('load', alustaTauno, false);

    // myos tama pitaa maaritella onloadin lisaksi
    window.addEventListener('unload', function (event) {
    }, false);

} else { /* yes, qunit go go go */
}