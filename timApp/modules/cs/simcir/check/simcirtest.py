# -*- coding:utf-8; -*-
import json


class BinaryTree:
    def __init__(self, val, left=None, right=None):
        self.root = val
        self.left = left or None
        self.right = right or None

    def insertLeft(self, newBranch):
        t = self.left
        if t:
            self.left = BinaryTree(newBranch, self.left, None)
        else:
            self.left = BinaryTree(newBranch, None, None)
        return self

    def insertRight(self, newBranch):
        self.right = BinaryTree(newBranch, None, self.right)
        return self

    def getRootVal(self):
        return self.root

    def setRootVal(self, newVal):
        self.root = newVal

    def getLeftChild(self):
        return self.left

    def getRightChild(self):
        return self.right

    def __str__(self):
        return '(' + str(self.root) + ' ' + str(self.left) + ' ' + str(self.right) + ')'

def AND(*x):
    if len(x) < 2:
        return False
    else:
        return all(x)


def OR(*x): return any(x)


def EOR(*x): return OR(*x) and NOT(AND(*x))


def XOR(*x): return OR(*x) and NOT(AND(*x))


def ENOR(*x): return NOT(XOR(*x))


def XNOR(*x): return NOT(XOR(*x))


def NOT(x): return not x


def NAND(*x): return NOT(AND(*x))


def NOR(*x): return NOT(OR(*x))


def DC(): return True

#funcs = {'AND': AND, 'NOT': NOT, 'OR': OR, 'XOR': XOR, 'XNOR': XNOR, 'EOR': XOR, 'ENOR': XNOR, 'NAND(3in)': NAND}


class Device:
    def __init__(self, dat):
        self.dev_type = dat['type']
        self.dev_id = dat['id']  # I hope these two always exist
        self.label = dat.get('label', None)
        self.x = int(dat['x'])
        self.y = int(dat['y'])
        self.num_inputs = int(dat.get('numInputs', -1))
        #self.func = funcs.get(self.label, None)
        if self.dev_type == 'NAND': self.label = 'NAND'

    def tuple_type_label(self):
        return (self.dev_type, self.label)

    def __str__(self):
        if self.num_inputs > -1:
            numInp = ', "numInputs":"%d"' % self.num_inputs
        else:
            numInp = ''
        return '{"type":"%s", "id":"%s", "label":"%s", "x":"%d", "y":"%d"%s}' \
               % (self.dev_type, self.dev_id, self.label, self.x, self.y, numInp)


class Connector:
    def __init__(self, dat):
        self.from_id, self.from_port = dat['from'].split('.')
        self.to_id, self.to_port = dat['to'].split('.')

    def tuple_to_from(self):
        return (self.to_id, self.from_id)

    def __str__(self):
        return '{"from":"%s.%s","to":"%s.%s"}' \
               % (self.from_id, self.from_port, self.to_id, self.to_port)


class Logik:
    # def __init__(self, data, reverse):
    def __init__(self, data):
        #self.terminals = set()
        self.terminals = [] # use list to catch possible duplicate switches
        # Lista käytetyistä komponenteista, DC mukana lausekkeiden muodostamista varten (jos portti/LED kytketty suoraan DC:hen ohi kytkime
        self.komponentit = ['AND', 'NAND', 'OR', 'NOR', 'XOR', 'XNOR', 'EOR', 'ENOR', 'NOT', 'DC']
        self.portit = set() # Store the types of ports used in the schematic (ehkäpä lista olisi parempi, jos halutaan tallentaa myöhemmin määrää), vertailussa nykyään set kuitenkin muutetaan listaksi
        if data is not None:
            self.devices = {}
            self.ulostulot = []
            self.ledit = []
            portti = 'LED'  ## fix to support others some day
            for d in data['devices']:
                dev = Device(d)
                self.devices[dev.dev_id] = dev
                if dev.dev_type == 'Toggle':  # lisää muita input tyyppejä
                    #self.terminals.add(dev.label)
                    self.terminals.append(dev.label)
                elif dev.dev_type == 'LED':  # verrataan tähän, onko kytketty
                    self.ledit.append(dev.label)
                elif dev.dev_type in self.komponentit and dev.dev_type != 'DC':  # portit, joita kytkennässä käytetty
                    self.portit.add(dev.dev_type)

            self.connectors = []  # data['connectors']
            for c in data['connectors']:
                con = Connector(c)
                self.connectors.append(con)
                if self.devices[con.from_id].dev_type == portti:
                    # save the device id which is connected to LED (portti)
                    # self.ulostulot.append(con.to_id);
                    self.ulostulot.append(con)
                    # self.komponentit = {} # käytetään tämän tilalla self.devices
                    # self.reverse = reverse

    def lausekkeesta_totuustaulu(self, lausekkeet):
        import itertools
        # Terminals sisältää muuttujat.
        # Luodaan yhtä monta totuusarvoa ja käydään
        # näille kaikki kombinaatiot läpi (product),
        # joka tuottaa kätevästi totuustaulun syötteen.
        # Taulun jokainen rivi sitten sovitetaan
        # simcirin dokusta (graafi) generoiduille
        # funktioille.
        terminals = sorted(self.terminals)
        # terminals = sorted(self.terminals, reverse=self.reverse)
        products = itertools.product((False, True), repeat=len(terminals))
        locss = []

        for i in (zip(terminals, prod) for prod in products):
            locs = {}
            for (k, v) in i:
                locs[k] = v
            locss.append(locs)

        def header(locss):
            loc = sorted(locss[0])
            if loc == []: return -4, 0 # tyhjä kaavio
            # loc = sorted(locss[0], reverse=self.reverse)
            label_length = max(len(x) for x in loc)
            frmt = (i.center(label_length) for i in loc)
            labels = str(' '.join(frmt))
            return labels, label_length

        def hianosti(d, label_length):
            k = sorted(d)
            # k = sorted(d, reverse=self.reverse)
            l = []
            for i in k: l.append(d[i])
            return str(' '.join((str(int(i))).center(label_length) for i in l))
            # return str(' '.join((str(i)[0]).center(label_length) for i in l))

        '''
        useampien ulostulojen totuustaulut eri taulukoissa

        tulostettava = ""
        for (nimi,lauseke) in lausekkeet.items():
            # sys.stdout.write(self.komponentit[nimi].label+'\n')
            #tulostettava += self.komponentit[nimi].label+'\n'
            #tulostettava += nimi + '\n' # bring in label directly <-- siirretään muuttujien kanssa samalle riville
            hd,label_length = header(locss)
            # sys.stdout.write(hd+'\n')
            tulostettava += hd + ' ' + nimi + '\n'
            for locs in locss:
                arvo = eval(lauseke, globals(), locs)
                # sys.stdout.write(hianosti(locs, label_length)+' '+str(arvo)+'\n')
                tulostettava += hianosti(locs, label_length)+' '+str(arvo)+'\n'

            # sys.stdout.write('\n')
            tulostettava += '\n'
        '''

        '''
        useampien ulostulojen totuustaulut samassa taulukossa
        '''
        try:
            tulostettava = ""
            hd, label_length = header(locss)
            for (nimi, lauseke) in sorted(lausekkeet.items()):
                hd += ' '.rjust(label_length) + nimi
                # hd += str(' '.join(nimi.center(max(len(nimi),label_length))))
            for locs in locss:
                tulostettava += hianosti(locs, label_length).rstrip()
                for (nimi, lauseke) in sorted(lausekkeet.items()):
                    # jos LED suoraan kytketty DC:hen
                    if lauseke == 'x': # jos LED ei kytketty
                        arvo = 'x'
                        tulostettava += ' '.center(max(len(nimi), label_length)) + arvo
                    else:
                        if lauseke == 'DC':
                            lauseke = 'DC()'
                        arvo = eval(lauseke, globals(), locs)
                        tulostettava += ' '.center(max(len(nimi), label_length)) + str(int(arvo)) # change true,false to 1,0
                tulostettava += '\n'
            tulostettava = hd + '\n' + tulostettava

            return totuustaulu_muotoilu(tulostettava)
        except TypeError:
            return -4 ## palautetaan tyhjä totuustaulu, kaaviosta puuttuu sisäänmenoja ja/tai ulostuloja

    def doit(self):
        # pitää pohtia vielä muuttujien nimemisen vaihtoa
        # for (i,j) in self.komponentit.items():
        #    if j[1]=='B1': self.komponentit[i]=(j[0], 'B1')
        lausekkeet = {}
        testlist = []
        for out in self.ulostulot:
            testlist.append(self.devices[out.from_id].label)

        for c in self.connectors:
            if c.from_id == c.to_id:  # Takaisinkytkentä (suoraan sisäänmenoon ulostulosta, helppo havaita, ei turhaan odoteta RecursionExce)
                return -3, self.portit

        if len(testlist) != len(set(testlist)): # Jos duplikaatti LEDejä
            return -1, self.portit
        if len(self.terminals) != len(set(self.terminals)): # Jos duplikaatti kytkimiä
            return -2, self.portit
        kytkemattomat = list(set(self.ledit) - set(testlist))
        if kytkemattomat != []: # LED kytkemättä
            for k in kytkemattomat:
                lausekkeet[k] = 'x'

        for out in self.ulostulot:
            try:
                r = BinaryTree(out.to_id)
                self.muodostaPuu(self.connectors, r)
                lausekkeet[self.devices[out.from_id].label] = self.yhdista(r)  # use label
            except RecursionError:
                return -3, self.portit # Takaisinkytkentä (useamman komponentin kautta, ei helppo testata, joten napataan poikkeuksella)

        #return self.lausekkeesta_totuustaulu(lausekkeet)
        return self.lausekkeesta_totuustaulu(lausekkeet), self.portit

    def yhdista(self, oksajuuri, lispish=False):
        if lispish:
            alku, par_alku, par_vali = "(", " ", " "
        else:
            alku, par_alku, par_vali = "", "(", ", "

        if not oksajuuri: return

        oksakomponentti = self.devices[oksajuuri.getRootVal()].label

        left = oksajuuri.getLeftChild()
        right = oksajuuri.getRightChild()

        if not left and not right:
            return oksakomponentti

        if not left:
            oikeaOksa = self.yhdista(right)
            return alku + str(oikeaOksa) + par_alku + oksakomponentti + ")"

        if not right:
            vasenOksa = self.yhdista(left)
            return alku + oksakomponentti + par_alku + str(vasenOksa) + ")"

        vasenOksa = self.yhdista(left)
        oikeaOksa = self.yhdista(right)

        return alku + oksakomponentti + par_alku + str(vasenOksa) + par_vali + str(oikeaOksa) + ")"

    def muodostaPuu(self, lines, puu):
        for con in lines:
            if puu.getRootVal() == con.from_id:
                komp = self.devices[con.to_id].dev_type
                if komp == 'DC' and self.devices[puu.getRootVal()].dev_type == 'Toggle':  # Switch connected to DC, continue
                    continue
                if not puu.getLeftChild():
                    puu = puu.insertLeft(con.to_id)
                    self.muodostaPuu(lines, puu.getLeftChild())
                elif not puu.getRightChild():
                    puu = puu.insertRight(con.to_id)
                    self.muodostaPuu(lines, puu.getRightChild())

    def etsi_komponentit(self):
        '''
        Tätä ei tällä hetkellä kutsuta:
            - terminalit lisätään konstruktorissa
            - self.komponentit korvataan self.devices HUOM self.komponentit otettu muuhun käyttöön!!!
            - ei tue vielä 3+ input loogisia portteja, pitää miettiä jos senkin tekee kostruktorissa
        '''
        '''
        for (dev_id, dev) in self.devices.items():
            if dev.dev_type and dev.label:
                if dev.dev_type == 'Toggle':  # lisää muita input tyyppejä
                    self.terminals.add(dev.label)
                if dev.num_inputs < 4:
                    self.komponentit[dev_id] = dev
                else:
                    self.connectors = self.poista_input(dev_id, tyyppi, dev.num_inputs)
                    dev = None
        return self.connectors
        '''

    def poista_input(self, dev_id, tyyppi, num_inputs):
        raise Error("ei toteutettu kunnolla vielä")
        # self.komponentit[dev] = (tyyppi, tyyppi)
        # if tyyppi.startswith('n'):
        #     tyyppi = tyyppi[1:]
        # elif tyyppi == 'XNOR':
        #     tyyppi = 'XOR'

        # self.komponentit[dev+'x'+str(num_inputs)] = (tyyppi, tyyppi)

        # newlines = []

        # for (indeksi,rivi) in enumerate(self.connectors):
        #     if dev+'.in' in rivi:
        #         if indeksi < num_inputs-1:
        #             newlines.append(rivi.replace(dev, dev+'x'+str(num_inputs)))
        #         else:
        #             ind = rivi.index('in')
        #             s = list(rivi)
        #             s[ind+2] = '0'
        #             rivi = "".join(s)
        #             newlines.append(rivi)
        #             newlines.append({'from': dev+'.in2', 'to':dev+'x'+str(num_inputs)+'.out0'})

        # fromlines = [rivi for rivi in from_llines if not dev+'.in' in rivi]

        # print 'Vfrom ', fromlines
        # print 'Vnew ', newlines

        # for item in newlines:
        #     self.connectors.append(item)
        #     dev = dev+'x'+str(num_inputs)
        #     num_inputs -= 1
        # if num_inputs > 2:
        #     return poista_input(kommponentit, dev, tyyppi, self.connectors, num_inputs)
        # else:
        #     return self.connectors


def totuustaulu_kaaviosta(kaavio):
    '''
    Palauttaa totuustaulun simcirjs kaaviosta
    '''
    l = Logik(kaavio)
    return l.doit()


def totuustaulu_lausekkeesta(yhtalot):
    '''
    yhtalot pitaa olla taulukko stringeistä, esim.: ["LED = AND(A,B)"] tai ["Sininen = AND(a,sel)", "Keltainen = AND(a,NOT(sel))"]
    = -merkin vasemalla puolella on ulostulon nimi (label)
    Palauttaa: lausekkeista muodotetun totuustaulun, esim. kahdelle outputille
     a  sel   Keltainen   Sininen
     0   0        0         0
     0   1        0         0
     1   0        1         1
     1   1        1         0

    '''
    l = Logik(None)
    import re
    lausekkeet = {}
    for yhtalo in yhtalot:
        [output, lauseke] = yhtalo.split("=")
        osat = re.findall(r"[\w']+", lauseke)

        for i in osat:
            if i not in l.komponentit:
                #l.terminals.add(i)
                if i not in l.terminals:
                    l.terminals.append(i)
        # l.terminals = set([u'B4', u'A1', u'A3', u'A2', u'A4', u'B2', u'B3', u'B1'])
        lausekkeet[output.strip()] = lauseke
    return l.lausekkeesta_totuustaulu(lausekkeet)


def testaa_totuustaulut(expected, actual):
    '''
    Luotetaan siihen että totuustaulut on samaa muotoa, etsitään poikkeavat totuustaulun rivit
    palauttaa virheiden lukumäärän ja virheelliset totuustaulun rivit
    '''
    import difflib
    expected = expected.splitlines(1)
    actual = actual.splitlines(1)

    virheita = 0
    d = difflib.Differ()
    result = list(d.compare(actual, expected))
    difference = ""
    for line in result:
        if line[0] == '+':
            virheita += 1
            difference += line

    return virheita, difference


def totuustaulu_muotoilu(totuustaulu):
    '''
    muotoillaan totuustaulu samaan muotoon kuin lausekkeesta muotoiltu totuustaulu
    aikamoinen viritys... totuustaulujen samankaltaisuus päätellään rivien samankaltaisuudesta
    pitää miettiä, tekisikö sen joskus arvoilla, kun muotoilu voi mennä pilalle
    '''
    from math import log2, ceil
    # poistetaan mahdolliset tyhjät rivit ja viivalla alkavat rivit (TIM taulukosta)
    try:
        totuustaulu = [line for line in totuustaulu.split('\n') if line.strip() != '' and not line.strip().startswith('-')]
        muokattu = ""
        # Selvitetään muuttujien ja ulostulojen määrä ja nimen pituudet,
        # muuttujien määrä on 2-kantainen logaritmi totuustaulun riveistä (pl. otsikkorivi)
        # pyöristetään ylös, totuustaulussa ei välttämättä kaikkia rivejä
        count = int(ceil(log2(len(totuustaulu) - 1)))
        nimet = totuustaulu[0].split()
        maxlenmuuttujat = max([len(item) for item in nimet[:count]])
        maxlenulostulot = max([len(item) for item in nimet[count:]])
        # Muokkauksia tulostusta varten... sotkuista, mutta kaikki totuustaulut
        # käy tätä kautta, joten tulee samanlainen muotoilu
        frmt1 = (i.center(maxlenmuuttujat) for i in nimet[:count])
        frmt2 = (('   ' + i).center(maxlenmuuttujat) for i in nimet[count:])
        labels = str(' '.join(frmt1) + ''.join(frmt2))
        muokattu = labels + '\n'
        for i in range(1, len(totuustaulu)):
            rivi = totuustaulu[i].split()
            frmt1 = (i.center(maxlenmuuttujat) for i in rivi[:count])
            frmt2 = (i.center(max(maxlenmuuttujat, maxlenulostulot)) for i in rivi[count:])
            labels = str(' '.join(frmt1) + '   ' + ' '.join(frmt2))
            muokattu += labels.rstrip() + '\n'
        return muokattu
    except ValueError:
        return -4 ## palautetaan tyhjä totuustaulu, kaaviosta puuttuu sisäänmenoja ja/tai ulostuloja

def onko_virheita(testitaulu, oikeataulu, maksimipisteet, portit = [], hyvportit = [], tulosta = True):
    """
    :param testitaulu: testattava totuustaulu
    :param oikeataulu: oikea totuustaulu
    :param maksimipisteet: tehtävän maksimipisteet
    :param portit: portit jota käytetty kaaviossa
    :param hyvportit: [] jos saa käyttää mitä tahansa portteja, tai esim. ['NAND'] jos saa käyttää pelkkiä NANDeja
    :param tulosta: Tulostetaanko virheelliset rivit (ja pisteet) opiskelijalle (esim. kokeessa ei tulosteta) - ei toteutettu vielä
    :return: tehtävästä saadut pisteet, virheelliset rivit (tai 'oikein' -teksti tai virheteksti)
    """
    if hyvportit != []:
        # check if portit only contains ports that are listed in hyvportit
        #print(all(any(p == hyvp for hyvp in hyvportit) for p in portit)) Antaa True tai False
        #print([x for x in portit if x not in hyvportit]) # Antaa portit
        ylimportit = [x for x in portit if x not in hyvportit] # Antaa portit
    else:
        ylimportit = []
    oikeatrivit = oikeataulu.splitlines()
    riveja = len(oikeatrivit) - 1  # poistetaan muuttuja/otsikko -rivi
    if isinstance( testitaulu, int ): # jotain vikaa jos testitaulu on (negatiivinen) int
        return tulosta_vai_ei(0.0, testitaulu, tulosta, oikeatrivit[0])

    virheita, virherivit = testaa_totuustaulut(testitaulu, oikeataulu)
    # Tulostetaan vain maxrivit riviä virheitä
    maxrivit = 8
    if virheita > maxrivit:
        virherivit = '\n'.join(virherivit.splitlines()[:maxrivit])
        virherivit += '\nlisäksi ' + str(virheita - maxrivit) + ' muuta virheellistä riviä\n'
    # Lisätään muuttujat ja output virheellisten rivien yläpuolelle
    virheet = '  ' + oikeatrivit[0] + '\n' + virherivit
    # maksimipisteet kerrotaan oikeiden rivien suhteella kaikkiin riveihin
    pistekertoja = (riveja-virheita)/riveja
    pisteet = round(maksimipisteet*pistekertoja*4)/4 # round to .25 intervals

    if virheita == 0:
        if ylimportit == []:
            return tulosta_vai_ei(pisteet, "Oikein, täydet pisteet!\n", tulosta)
        else:
            return tulosta_vai_ei(maksimipisteet/2, "Totuustaulu oikein, mutta puolet maksimipisteistä,\nkoska sallittujen porttien "
                                  + str(hyvportit) + "\nlisäksi käytit myös portteja " + str(ylimportit) + "\n", tulosta)
        #return pisteet, "Oikein, täydet pisteet!\n" # pitäisikö tulostaa oikea totuustaulu?
    if pisteet < 0:
        return tulosta_vai_ei(0.0, -5 , tulosta, oikeatrivit[0])
        #return 0.0, "Kytkennässä on liikaa sisäänmenoja tai ulostuloja.\nSisäänmenot ja ulostulot tulee olla: " + oikeatrivit[0] + "\n"
    # alla tapaukset, joissa totuustaulu väärin, ei syntaksivirheitä
    if ylimportit == []:
        return tulosta_vai_ei(pisteet, virheet, tulosta)
    else:
        return tulosta_vai_ei(0.0, virheet + "\ntotuustaulussa yo. virheet ja sallittujen porttien "
                              + str(hyvportit) + "\nlisäksi käytit myös portteja " + str(ylimportit) + "\n", tulosta)


def tulosta_vai_ei(pisteet, taulu, tulosta = True, teksti = ''):
    # 0 -> -len, 1 -> -(len-1), ...
    # Tulostetaan virhetekstit aina (myös koetilanteessa)
    virhetekstit = [
                    "Kytkennässä on liikaa sisäänmenoja tai ulostuloja.\nSisäänmenot ja ulostulot tulee olla: " + teksti + "\n",
                    "Kytkennästä puuttuu sisäänmenoja tai ulostuloja.\nSisäänmenot ja ulostulot tulee olla: " + teksti + "\n",
                    "Kytkennässä takaisinkytkentä (portin ulostulo kytketty omaan sisäänmenoon,\njoko suoraan tai muiden porttien kautta). Poista takaisinkytkentä.\n",
                    "Kytkennässä samannimisiä sisäänmenoja, poista ylimääräiset.\nSisäänmenot ja ulostulot tulee olla: " + teksti + "\n",
                    "Kytkennässä samannimisiä ulostuloja, poista ylimääräiset.\nSisäänmenot ja ulostulot tulee olla: " + teksti + "\n"
                    ]
    if isinstance( taulu, int ):
        return pisteet, virhetekstit[taulu + len(virhetekstit)]
    if (tulosta):
        return pisteet, taulu
    else:
        return pisteet, ""


def muotoile_lauseke(lauseke):
    """
    Muuttaa LED = a AND b tyyppiset lausekkeet muotoon LED = AND(a,b)
    Poistaa mahdolliset sulut, pyrkii säilyttämään 'puumaisen' rakenteen
    uudessa lausekkeessa, testattu toimivaksi ao. lausekkeella
    (((NOT (A1 XOR B1)) AND (NOT (A2 XOR B2))) AND (NOT (A3 XOR B3)) AND (NOT (A4 XOR B4)))
    eli ensin käsitellään ne portit joiden sisäänmenona on pelkät muuttujat/kytkimet ja NOT:it
    sitten vasta LEDiä lähimpänä puurakenteessa olevat portit
    Tehdään lausekkeesta lista, johon lisätään muokatut termit ja poistetaan niitä vastaavat vanhat muotoilut
    kunnes listassa on enää yksi termi ja se on uusi lauseke. LED irrotetaan alussa ja lisätään lopussa.
    :param lauseke: 'LED = a AND b' tai vastaava
    :return: 'LED = AND(a,b)' tai vastaava
    """
    komponentit = ['AND', 'NAND', 'OR', 'NOR', 'XOR', 'XNOR', 'EOR', 'ENOR', 'NOT', 'DC']
    osat = lauseke.split('=')
    t = osat[1].lstrip().replace('(','').replace(')','').split(' ')
    def muokkaa(lauseke):
        tt = lauseke[a] + '(' + lauseke[a-1] + ',' + lauseke[a+1] + ')'
        lauseke.insert(a-1,tt)
        if a+2 == 0:
            del lauseke[a-1:]
        else:
            del lauseke[a-1:a+2]

    a = -2
    toinen = False
    while len(t) > 1:
        if abs(a) > len(t):
            a = -2
            toinen = True
        if t[a] == 'NOT':
            tt = t[a] + '(' + t[a+1] + ')'
            t.insert(a,tt)
            if a+2 == 0:
                del t[a:]
            else:
                del t[a:a+2]
        elif (t[a] in komponentit and not t[a-1].startswith(tuple(komponentit)) and not t[a+1].startswith(tuple(komponentit))):
            muokkaa(t)
        elif toinen:
            muokkaa(t)
        else:
            a -= 1

    return osat[0].rstrip() + ' = ' + ''.join(t)

def testaa_kaaviot(testikaavio, oikeakaavio, maksimipisteet, hyvportit = [], tulosta = True):
    """
    Testaa opettajan antaman kaavion vs. opiskelijan kytkentäkaavio
    :param testikaavio: JSON string opiskelijan simcirjs kaaviosta
    :param oikeakaavio: JSON string opettajan simcirjs kaaviosta
    :param maksimipisteet: Tehtävästä saatava maksimipistemäärä
    :param hyvportit: Hyväksytyt portit, on [] jos saa käyttää mitä tahansa portteja, tai esim. ['NAND'] jos saa käyttää pelkkiä NANDeja
    :param tulosta: True/False - tulostetaanko tieto arvostelusta vai ei (esim. kokeessa) palauttaa kuitenkin pisteet aina!
    :return: (double, string) - pisteet doublena, ja string:n, jossa 'oikein'-teksti, virheelliset totuustaulun rivit tai muuta virheinformaatiota
    """
    testitaulu, portit = totuustaulu_kaaviosta(json.loads(testikaavio))
    oikeataulu, malliportit = totuustaulu_kaaviosta(json.loads(oikeakaavio)) # opettajan portit, ei käytetä, ehkä myöhemmin voisi antaa infoa opiskelijalle, jos käytti enemmän portteja kuin mallissa, mutta miten sen hoitaisi sitten kun opettaja antaa totuustaulun...

    return onko_virheita(testitaulu, oikeataulu, maksimipisteet, list(portit), hyvportit, tulosta)


def testaa_lauseke_vs_kaavio(testikaavio, oikealauseke, maksimipisteet, hyvportit = [], tulosta = True):
    """
    Testaa opettajan antaman lausekkeen vs. opiskelijan kytkentäkaavio
    :param testikaavio: JSON string opiskelijan simcirjs kaaviosta
    :param oikealauseke: lausekkeet omilla riveillään (järjestyksellä ei väliä)
    Keltainen = AND(a,NOT(sel))
    Sininen = AND(a,sel)
    tai muodossa (saa sisältää sulkuja)
    Sininen = a AND sel
    Keltainen = a AND NOT sel
    niistä muodostetaan taulukko esim. ["Sininen = AND(a,sel)", "Keltainen = AND(a,NOT(sel))"]
    HUOM: muuttujat eivät voi olla pythonin varattuja sanoja esim. 'in'
    :param maksimipisteet: Tehtävästä saatava maksimipistemäärä
    :param hyvportit: Hyväksytyt portit, on [] jos saa käyttää mitä tahansa portteja, tai esim. ['NAND'] jos saa käyttää pelkkiä NANDeja
    :param tulosta: True/False - tulostetaanko tieto arvostelusta vai ei (esim. kokeessa) palauttaa kuitenkin pisteet aina!
    :return: (double, string) - pisteet doublena, ja string:n, jossa 'oikein'-teksti, virheelliset totuustaulun rivit tai muuta virheinformaatiota
    """
    if ',' not in oikealauseke: # a AND b muodossa ei ole pilkkua, kun taas muodossa AND(a,b) on
        oikealauseke = muotoile_lauseke(oikealauseke)
    testitaulu, portit = totuustaulu_kaaviosta(json.loads(testikaavio))
    oikeataulu = totuustaulu_lausekkeesta(oikealauseke.splitlines())

    return onko_virheita(testitaulu, oikeataulu, maksimipisteet, list(portit), hyvportit, tulosta)


def testaa_totuustaulu_vs_kaavio(testikaavio, oikeatotuustaulu, maksimipisteet, hyvportit = [], tulosta = True):
    """
    Testaa opettajan antaman totuustaulun vs. opiskelijan kytkentäkaavio
    Esimerkki totuustaulun rakenteesta, viivojen ja white space -merkkien määrällä ei merkitystä

    a sel Keltainen Sininen
    - --- ---       -
    0   0  0         0
    0   1  0         0
    1   0  1         0
    1   1  0         1

    :param testikaavio: JSON string opiskelijan simcirjs kaaviosta
    :param oikeatotuustaulu: totuustaulu TIM-muodossa, esim. ks. yllä
    :param maksimipisteet: Tehtävästä saatava maksimipistemäärä
    :param hyvportit: Hyväksytyt portit, on [] jos saa käyttää mitä tahansa portteja, tai esim. ['NAND'] jos saa käyttää pelkkiä NANDeja
    :param tulosta: True/False - tulostetaanko tieto arvostelusta vai ei (esim. kokeessa) palauttaa kuitenkin pisteet aina!
    :return: (double, string) - pisteet doublena, ja string:n, jossa 'oikein'-teksti, virheelliset totuustaulun rivit tai muuta virheinformaatiota
    """
    testitaulu, portit = totuustaulu_kaaviosta(json.loads(testikaavio))
    oikeataulu = totuustaulu_muotoilu(oikeatotuustaulu)

    return onko_virheita(testitaulu, oikeataulu, maksimipisteet, list(portit), hyvportit, tulosta)


def testaa(testidata, oikeadata, maksimipisteet, hyvportit = [], tulosta = True):
    """
    Testaa opiskelijan kaavion vs. opettajan kaavio, lauseke tai totuustaulu. Päättelee opettajan
    ratkaisun tyypin sen perusteella sisältääkö se merkit '{' tai '=' vai ei kumpaakaan.
    :param testidata: tekstitiedosto, jossa testattava simcir kaavio JSON muodossa
    :param oikeadata: tekstitiedosto, joko kaavio, lauseke tai totuustaulu. Muotoilu
        Kaavio - JSON simcir kaavio
        Lauseke - jokainen lauseke omalla rivillään muodossa: LED = AND(a,b) - hyväksyy myös muodon LED = a AND b
        Totuustaulu - jokainen totuustaulun rivi omalla rivillään, saa sisältää rivin joka alkaa tavuviivalla (-), ne jätetään huomiotta
    :param maksimipisteet: Tehtävästä saatava maksimipistemäärä
    :param hyvportit: Hyväksytyt portit, on [] jos saa käyttää mitä tahansa portteja,
        tai esim. ['NAND'] jos saa käyttää pelkkiä NANDeja.
        TAI hyväksytyt portit voi tuoda oikeadata -tiedostossa tyhjällä rivillä erotettuna
        pilkulla erotettuna esim. muodossa: NAND, NOR ja noista muodostetaan lista
    :param tulosta: True/False - tulostetaanko tieto arvostelusta vai ei (esim. kokeessa) palauttaa kuitenkin pisteet aina!
    :return: (double, string) - pisteet doublena, ja string:n, jossa 'oikein'-teksti, virheelliset totuustaulun rivit tai muuta virheinformaatiota
    """
    testattava = open(testidata, 'r').read()
    oikea = open(oikeadata, 'r').read()
    if '\n\n' in oikea:
        hyvportit = [item.strip() for item in oikea.split('\n\n')[1].split(',')]
        if hyvportit == ['']: # löytyi useita tyhjiä rivejä (ei pitäisi olla)
            hyvportit = []
        oikea = oikea.split('\n\n')[0]
    oikeantyyppi = ''.join([x for x in ['{', '='] if x in oikea]) or 't'

    return {
        '{': lambda: testaa_kaaviot(testattava, oikea, maksimipisteet, hyvportit, tulosta),
        '=': lambda: testaa_lauseke_vs_kaavio(testattava, oikea, maksimipisteet, hyvportit, tulosta),
        't': lambda: testaa_totuustaulu_vs_kaavio(testattava, oikea, maksimipisteet, hyvportit, tulosta)
    }[oikeantyyppi]()


if __name__ == '__main__':
    import sys

    if len(sys.argv) == 4:
        p,v = testaa(sys.argv[1], sys.argv[2], float(sys.argv[3]))
    elif len(sys.argv) == 5:
        p,v = testaa(sys.argv[1], sys.argv[2], float(sys.argv[3]), sys.argv[4])
    elif len(sys.argv) == 6:
        p,v = testaa(sys.argv[1], sys.argv[2], float(sys.argv[3]), sys.argv[4], sys.argv[5])
    else:
        sys.stdout.write('wrong number or arguments')
        exit()
    sys.stdout.write('Pisteet: ' + str(p) + '\n')
    sys.stdout.write(v+ '\n')
