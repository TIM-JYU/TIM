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
    def __repr__(self): # listattujen olioiden tulostamiseen (testausta varten)
        return str(self)

class Logik:
    # def __init__(self, data, reverse):
    def __init__(self, data):
        #self.terminals = set()
        self.terminals = [] # use list to catch possible duplicate switches
        # Lista käytetyistä komponenteista, DC mukana lausekkeiden muodostamista varten (jos portti/LED kytketty suoraan DC:hen ohi kytkimen
        self.komponentit = ['AND', 'NAND', 'OR', 'NOR', 'XOR', 'XNOR', 'EOR', 'ENOR', 'NOT', 'DC']
        self.portit = set() # Store the types of ports used in the schematic (ehkäpä lista olisi parempi, jos halutaan tallentaa myöhemmin määrää), vertailussa nykyään se kuitenkin muutetaan listaksi
        self.kytkematta = [] # Lista porteista joiden ulostulo(t) tai sisäänmeno(t) on kytkemättä. Ne joissa kaikki kytkemättä erotellaan erilleen
        self.kelluvat =[] # komponentit, joista ei mitään kytketty, eli ei vaikuta millään tavalla kytkentään.
        self.inputkytkematta = [] # Lista kytkimistä, joita ei ole kytketty
        self.porttiennenkytkinta = [] # lista porteista, jotka on DC:n ja kytminen välissä
        if data is not None:
            self.devices = {}
            self.ulostulot = []
            self.ledit = []
            self.connectors = []  # data['connectors']
            portti = 'LED'  ## Mofify to support others some day, make a list of possible output types

            for c in data['connectors']:
                con = Connector(c)
                self.connectors.append(con)

            for d in data['devices']:
                dev = Device(d)
                self.devices[dev.dev_id] = dev
                if dev.dev_type == 'Toggle':  # lisää muita input tyyppejä
                    #self.terminals.add(dev.label)
                    if dev.dev_id in [x.from_id for x in self.connectors] and dev.dev_id in [x.to_id for x in self.connectors]:
                        self.terminals.append(dev.label)
                    else:
                        self.inputkytkematta.append(dev.label)
                        #print(self.inputkytkematta)
                elif dev.dev_type == 'LED':  # verrataan tähän, onko kytketty
                    self.ledit.append(dev.label)
                elif dev.dev_type in self.komponentit and dev.dev_type != 'DC':  # portit, joita kytkennässä käytetty
                    # Tarkastetaan että on oikeasti käytetty, eli ainakin 1 ulostulo ja sisäänmeno kytketty, että lisätään portteihin
                    #print(dev.dev_id in [x.from_id for x in self.connectors])
                    #print(dev.dev_id in [x.to_id for x in self.connectors])
                    #print('dev ' + dev.dev_id)
                    if dev.dev_id in [x.from_id for x in self.connectors] and dev.dev_id in [x.to_id for x in self.connectors]:
                        self.portit.add(dev.dev_type) # ainakin 1 input ja 1 output kytketty
                    elif dev.dev_id in [x.from_id for x in self.connectors] or dev.dev_id in [x.to_id for x in self.connectors]:
                        self.kytkematta.append(dev.dev_type) # joko input tai output kytketty, mutta ei molempia
                    else:
                        self.kelluvat.append(dev.dev_type) # ei yhtään input tai output kytketty

            #print('kytketty ' + str(self.portit))
            #print('kytkemättä ' + str(self.kytkematta))

            for c in self.connectors:
                if self.devices[c.from_id].dev_type == portti:
                    # save the device id which is connected to LED (portti)
                    # self.ulostulot.append(con.to_id);
                    self.ulostulot.append(c)
                    # self.komponentit = {} # käytetään tämän tilalla self.devices
                    # self.reverse = reverse
                # Jos kytkimen ja DC:n välissä on jokin portti
                if self.devices[c.from_id].dev_type == 'Toggle' and self.devices[c.to_id].dev_type != 'DC':
                    self.porttiennenkytkinta.append(self.devices[c.to_id].dev_type)
                    #print(self.devices[c.to_id].dev_type)
                #print(con.from_id in self.portit)
                #if self.devices[con.from_id].dev_type == portti:
                    # save the device id which is connected to LED (portti)
                    # self.ulostulot.append(con.to_id);
                    #self.ulostulot.append(con)
                    # self.komponentit = {} # käytetään tämän tilalla self.devices
                    # self.reverse = reverse
            ## Lopuksi katsotaan että oliko 3+ input komponentti ja vaihdetaan tilalle 3-input komponentit ja vastaavat kytkennät
            # Ei toimi vielä
            # for k, dev in list(self.devices.items()): # list because we modify self.devices
            #     if dev.num_inputs > 2:
            #         # print(dev.num_inputs)
            #        self.poista_input(dev.dev_id, dev.dev_type, dev.num_inputs)
            #print(self.connectors)

                    # print(self.devices)

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
                        try:
                            arvo = eval(lauseke, globals(), locs)
                        except:
                            return -6  ## jokin komponentti kytkennässä jota ei voida evaluoida
                        if callable(arvo): # Jos on funktio, niin LED:ltä ei johdotusta kytkimelle asti
                            return -8
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
        for c in self.connectors:
            if c.from_id == c.to_id:  # Takaisinkytkentä (suoraan sisäänmenoon ulostulosta, helppo havaita, ei turhaan odoteta RecursionExce)
                return -3, self.portit

        if (self.ulostulot == [] or self.terminals == []): ## Kaikki ulostulot tai kytkimet on poistettu
            return -4, self.portit
        for out in self.ulostulot:
            testlist.append(self.devices[out.from_id].label)
        if len(testlist) != len(set(testlist)): # Jos duplikaatti LEDejä
            return -1, self.portit
        if len(self.terminals) != len(set(self.terminals)): # Jos duplikaatti kytkimiä
            return -2, self.portit
        if self.inputkytkematta != []: # Jos kytkin kytkemättä
            return -7, self.inputkytkematta
        # self.kytkematta <-- jos noita niin rangaistaan
        if self.kytkematta != []: # Jos jonkin komponentin (kaikki) inputit tai (kaikki) outputit kytkemättä
            return -9, self.inputkytkematta
        if self.porttiennenkytkinta != []: # Jos portti on DC:n ja kytkimen välissä
            return -10, self.porttiennenkytkinta
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
        #raise Error("ei toteutettu kunnolla vielä")
        print("ei toteutettu kunnolla vielä")
        self.devices[dev_id].label = tyyppi # change label to type, because we use basic components (AND) to replace n-input ones e.g. label AND(3in)
        # Käsitellään negatoidut komponentit erikseen, tehdään muutos ilman negatointeja, ja lopputulos vasta negatoidaan.
        # Esim LED = NAND(a,b,c) --> LED = NOT(AND(a,AND(b,c)))
        if tyyppi.startswith('n'):
            tyyppi = tyyppi[1:]
        elif tyyppi == 'XNOR':
            tyyppi = 'XOR'

        print(self.devices[dev_id])
        # Lisätään uusia virtuaalisia (x) portteja kompensoimaan n-input portin muutos 2-input porteiksi
        # poistetaan muutettava komponentti myöhemmin...

        # Tässä pitäisi luoda uusi Device ja sitten lisätä se devices:iin ja sitten lopulta poistaa alkuperäinen n-input dev
        self.devices[dev_id+'x'+str(num_inputs)] = (tyyppi, tyyppi)

        newlines = []

        # for (indeksi,rivi) in enumerate(self.connectors):
        # for c in self.connectors:
        #     print(c)
            # if dev_id+'.in' in rivi:
            #     print (rivi)
                # if indeksi < num_inputs-1:
                #     newlines.append(rivi.replace(dev_id, dev_id+'x'+str(num_inputs)))
                # else:
                #     ind = rivi.index('in')
                #     s = list(rivi)
                #     s[ind+2] = '0'
                #     rivi = "".join(s)
                #     newlines.append(rivi)
                #     newlines.append({'from': dev_id+'.in2', 'to':dev_id+'x'+str(num_inputs)+'.out0'})

        # print(self.devices)
        ## Tässä pitäisi self. connectorista poistaa dev:iä vastaavat rivit
        ##fromlines = [rivi for rivi in from_llines if not dev_id+'.in' in rivi]
        ## Ja myöhemmin lisätä uudet rivit...
        ## TODO: mieti arin.py versiossa oleva rivien käsittely versus tässä kun käsitellään self.connectors luokkaa

        # print 'Vfrom ', fromlines
        # print 'Vnew ', newlines
        #
        # for item in newlines:
        #     self.connectors.append(item)
        #     dev = dev_id+'x'+str(num_inputs)
        #     num_inputs -= 1
        # if num_inputs > 2:
        #     return poista_input(kommponentit, dev_id, tyyppi, self.connectors, num_inputs)
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
            difference += line.replace("+"," ")

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
    except ValueError: # Tämä tapahtuu kun LED puuttuu
        return -4 ## palautetaan tyhjä totuustaulu, kaaviosta puuttuu sisäänmenoja ja/tai ulostuloja

def onko_virheita(testitaulu, oikeataulu, maksimipisteet, portit = [], hyvportit = [], tulosta = True):
    """
    23.8.2016 Lisätään testi että on käytetty vaadittuja portteja, aiemmin testattiin vain että onko ylimääräisiä
    :param testitaulu: testattava totuustaulu
    :param oikeataulu: oikea totuustaulu
    :param maksimipisteet: tehtävän maksimipisteet
    :param portit: portit jota käytetty kaaviossa
    :param hyvportit: [] jos saa käyttää mitä tahansa portteja, tai esim. ['NAND'] jos saa käyttää pelkkiä NANDeja
    :param tulosta: Tulostetaanko virheelliset rivit (ja pisteet) opiskelijalle (esim. kokeessa ei tulosteta) - ei toteutettu vielä
    :return: tehtävästä saadut pisteet, virheelliset rivit (tai 'oikein' -teksti tai virheteksti)
    """

    # portit voi sisältää simcir:n EOR je ENOR, kun käytän XOR ja XNOR, muutetaan -->
    portit = [w.replace('EOR', 'XOR') for w in portit]
    portit = [w.replace('ENOR', 'XNOR') for w in portit]

    if hyvportit != []:
        # check if portit only contains ports that are listed in hyvportit
        #print(all(any(p == hyvp for hyvp in hyvportit) for p in portit)) Antaa True tai False
        #print([x for x in portit if x not in hyvportit]) # Antaa ylimääräiset portit
        #print([x for x in portit if x in hyvportit])  # Antaa hyväksytyt portit
        ylimportit = [x for x in portit if x not in hyvportit]
        vaaditutportit = [x for x in portit if x in hyvportit]
    else:
        ylimportit = []
        vaaditutportit = []
    oikeatrivit = oikeataulu.splitlines()
    riveja = len(oikeatrivit) - 1  # poistetaan muuttuja/otsikko -rivi
    if isinstance( testitaulu, int ): # jotain vikaa jos testitaulu on (negatiivinen) int
        return tulosta_vai_ei(0.0, testitaulu, tulosta, oikeatrivit[0])

    virheita, virherivit = testaa_totuustaulut(testitaulu, oikeataulu)
    #print('virheitä ' + str(virheita))
    # Tulostetaan vain maxrivit riviä virheitä
    maxrivit = 8
    if virheita > maxrivit:
        virherivit = '\n'.join(virherivit.splitlines()[:maxrivit])
        virherivit += '\n  ...\nlisäksi ' + str(virheita - maxrivit) + ' muuta virheellistä riviä\n'
    # Lisätään sisäänmenot ja ulostulot virheellisten rivien yläpuolelle
    if virheita > 0:
        virheet = 'Seuraavat rivit kytkentääsi vastaavassa totuustaulussa\npoikkeavat tehtävänantoa vastaavan totuustaulun riveistä:\n' + '  ' + oikeatrivit[0] + '\n' + virherivit
    else:
        virheet = ''
    # maksimipisteet kerrotaan oikeiden rivien suhteella kaikkiin riveihin
    pistekertoja = (riveja-virheita)/riveja
    # Muutetaan siten että pyöristetään alaspäin
    # pisteet = round(maksimipisteet*pistekertoja*4)/4 # round to .25 intervals
    # from math import floor
    # pisteet = floor(maksimipisteet*pistekertoja*4)/4 # round down to .25 intervals
    # 0.25 välein pyöristäminen ei toimi, kun tehtävästä saa vain 0.5 pistettä, niin esim. 7/8 oikein antaa 0.25
    # Pyöristetään vain 2 desimaalin tarkkuudella. 
    pisteet = round(maksimipisteet*pistekertoja,2)

    if virheita == 0:
        if hyvportit == []:
            return tulosta_vai_ei(pisteet, "Oikein, täydet pisteet!\n", tulosta)
        elif hyvportit != []:
            if vaaditutportit == []:
                return tulosta_vai_ei(maksimipisteet / 2,
                                      "Totuustaulu oikein, mutta puolet maksimipisteistä,\nkytkennässä ei ole yhtään sallituista porteista\n"
                                      + str(hyvportit) + ', siten että niiden sisäänmenot \nja ulostulot on kytketty.\n', tulosta)
            elif ylimportit == []:
                return tulosta_vai_ei(pisteet, "Oikein, täydet pisteet!\n", tulosta)
            else:
                return tulosta_vai_ei(maksimipisteet/2, "Totuustaulu oikein, mutta puolet maksimipisteistä,\nkoska et käyttänyt vain sallittuja portteja "
                                      + str(hyvportit) + "\nvaan kytkennässä on myös portit " + str(ylimportit) + "\n", tulosta)
        #else: # Ei määritelty erikseen käytettäviä portteja
        #   return tulosta_vai_ei(pisteet, "Oikein, täydet pisteet!\n", tulosta)
    if pisteet < 0:
        return tulosta_vai_ei(0.0, -5 , tulosta, oikeatrivit[0])
    # alla tapaukset, joissa totuustaulu väärin, ei syntaksivirheitä
    if hyvportit != [] and vaaditutportit == []:
        return tulosta_vai_ei(0.0, virheet + "lisäksi kytkennässä ei ole yhtään sallituista porteista\n"
                                      + str(hyvportit) + ', siten että niiden sisäänmenot \nja ulostulot on kytketty.\n', tulosta)
    elif ylimportit == []:
        return tulosta_vai_ei(pisteet, virheet, tulosta)
    else:
        return tulosta_vai_ei(0.0, virheet + "\nlisäksi sallitut portit olivat "
                              + str(hyvportit) + "\nmutta kytkennässäsi on muita portteja " + str(ylimportit) + "\n", tulosta)


def tulosta_vai_ei(pisteet, taulu, tulosta = True, teksti = ''):
    # 0 -> -len, 1 -> -(len-1), ...
    # Tulostetaan virhetekstit aina (myös koetilanteessa)
    virhetekstit = [
                    "Kytkennässä on portti jännitelähteen (DC) ja kytkimen (sisäänmeno) välissä.\nPortti ei voi olla ennen sisäänmenoa, siirrä portti kytkimen toiselle puolelle.\n",
                    "Kytkennässä ei ole johdotettu kaikkia käytettyjen porttien\nsisäänmenoja tai ulostuloja. Tarkista kytkennän johdotukset.\n",
                    "Kytkennässä ei ole johdotettu kaikkia kytkimiä (porttien kautta)\njohonkin ulostuloon. Tarkista kytkennän johdotukset.\n",
                    "Kytkennässä ei ole johdotettu kaikkia kytkimien sisäänmenoja\nja ulostuloja. Tarkista kytkinten johdotukset.\n",
                    "Kytkennässä on jokin komponentti, jota tarkistin ei tue.\nKäytä vain työkaluissa ja piirikaaviossa olevia komponentteja\n",
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
	testattu toimivaksi ao. lausekkeella
    LED = (((NOT (NOT(NOT A1) XOR B1)) AND (NOT (A2 XOR NOT (NOT (B2))))) AND NOT(NOT((NOT (A3 XOR B3)) AND (NOT (NOT(NOT(A4)) XOR B4)))))
    Toiminta
     - ensin käsitellään NOT, joilla parametrina yksi muuttuja
     - sitten käsitellään NOT:ien alla olevat lausekkeet
     - sitten NOT:tien alla olevat lausekkeet yhdistetään NOT:in kanssa samaksi lausekkeeksi
     - sitten käsitellään muut operaattorit
    Tehdään lausekkeesta lista, jossa muotattuja termejä yhdistellään ja niiden paikkoja vaihdellaan
     - esim. listassa [... , 'A', 'XOR', 'B', ...] --> [... , 'XOR(A,B)', ...]
    kunnes listassa on enää yksi termi ja se on uusi lauseke. LED irrotetaan alussa ja liitetään lopussa.
    :param lauseke: 'LED = a AND b' tai vastaava
    :return: 'LED = AND(a,b)' tai vastaava
    """

    def muokkaaLauseke(t):
        # muokkaillaan lauseketta, yhdistetään operaatioihin termit ja muutetaan järjestystä
        while (len(t) > 1):
            j = 0
            #print(len(t))
            while j < len(t):
                # Käsitellään NOT:it ilman sulkeita
                #print(str(j))
                if j+1 < len(t) and t[j] == 'NOT' and t[j + 1] != '(':
                    t[j], t[j + 1] = t[j] + '(', t[j + 1] + ')'
                    t[j:j + 2] = [''.join(t[j:j + 2])]
                    #print('0. j, t[j]: ' + str(j) + ', ' + t[j])
                # yhdistetään ( -sulkeet sitä edeltävään loogiseen NOT operaatioon
                if j+1 < len(t) and t[j] == 'NOT' and t[j+1] == '(':
                    t[j:j+2] = [''.join(t[j:j+2])] # x[3:6] = [''.join(x[3:6])]
                    #print('1. j, t[j]: ' + str(j) + ', ' + t[j])
                    #print(len(t))
                    #print(t)
                # esim. 'NOT(', 'B2', ')' # Yhdistetään NOT sen sisällä olevaan lausekkeeseen
                elif j+2 < len(t) and t[j] == 'NOT(' and t[j+2] == ')':
                    t[j:j+3] = [''.join(t[j:j+3])]
                    #print('2. j, t[j]: ' + str(j) + ', ' + t[j])
                # esim. A AND B tai A XOR B
                elif len(t) == 3 and j == 0 and t[j+1] in komponentit:
                    t[j], t[j+1], t[j+2] = t[j+1]+'(', t[j+0]+',', t[j+2]+')'
                    t[j:j+3] = [''.join(t[j:j+3])]
                    #print('23. j, t[j]: ' + str(j) + ', ' + t[j])
                # esim. 'NOT(', 'A', 'AND', 'B', ')' # Yhdistetään NOT sen sisällä olevaan lausekkeeseen
                # esim. 'NOT(', '...', 'XOR', '...', ')' # Yhdistetään NOT:n sisällä oleva lauseke
                elif j+4 < len(t) and t[j] == 'NOT(' and t[j+4] == ')' and t[j+2] in komponentit:
                    t[j+1], t[j+2], t[j+3] = t[j+2]+'(', t[j+1]+',', t[j+3]+')'
                    t[j:j+5] = [''.join(t[j:j+5])]
                    #print('3. j, t[j]: ' + str(j) + ', ' + t[j])
                # esim. '(', '...', ')', 'AND', '(', '...', ')'
                elif j-3 >= 0 and j+3 < len(t) and t[j] in komponentit and t[j-1] == ')' and t[j+3] == ')' and t[j-3] == '(' and t[j+1] == '(':
                    t[j-3], t[j-2], t[j-1], t[j], t[j+1] = t[j], t[j-3], t[j-2], ',', ''
                    t[j-3:j+4] = [''.join(t[j-3:j+4])]
                    #print('4. j-3, t[j-3]: ' + str(j-3) + ', ' + t[j-3])
                    j = j-3
                # esim. '...', 'AND', '(', '...', ')'
                elif j-1 >= 0 and j+3 < len(t) and t[j] in komponentit  and t[j+3] == ')' and t[j+1] == '(' and t[j-1] != ')':
                    t[j-1], t[j], t[j+1] = t[j], t[j+1], t[j-1] + ','
                    t[j-1:j+4] = [''.join(t[j-1:j+4])]
                    #print('5. t[j-1]: ' + t[j-1])
                    j = j-1
                # esim. '(', '...', 'AND', '...', ')'
                elif j-2 >= 0 and j+2 < len(t) and t[j] in komponentit and t[j-2] == '(' and t[j+2] == ')':
                    t[j-1], t[j] = t[j], t[j-1]
                    t[j] = '(' + t[j]
                    t[j+1] = ',' + t[j+1] + ')'
                    t[j-1:j+2] = [''.join(t[j-1:j+2])]
                    #print('6. t[j-1]: ' + t[j-1])
                    j = j-1
                 # jos yksi muuttuja tms. sulkeiden välissä --> yhdistetään
                elif j+2 < len(t) and t[j].endswith('(') and t[j+2].startswith(')'):
                    t[j:j+3] = [''.join(t[j:j+3])]
                    #print('7. t[j]: ' + t[j])
                else:
                    j += 1

    # NOT käsitellään erikseen, ei testattu DC:n kanssa vielä (opettajan ratkaisun ei 'pitäisi' sisältää DC:tä)
    komponentit = ['AND', 'NAND', 'OR', 'NOR', 'XOR', 'XNOR', 'EOR', 'ENOR']
    osat = lauseke.split('=') # irroitetaan LED
    t = osat[1].lstrip().replace('(',' ( ').replace(')',' ) ') # varmistetaan että sulkujen ympärillä on välejä
    t = ' '.join(t.split()) # poistetaan extra välit
    t = t.split(' ') # taulukoidaan sulut, muuttujat ja operaatiot
    #print(t)
    muokkaaLauseke(t) # muokataan kunnes listassa on enää yksi lopullinen string
    #print(len(t))
    #print(t)
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
    import ast
    # simcirtest.py [teksti - testattava vastaus] [telsti - oikea kytkentä] [float - pisteet] [array - hyväksyttävät portit] [Boolean - tulostetaanko]
    try: 
        if len(sys.argv) == 4:
            p,v = testaa(sys.argv[1], sys.argv[2], float(sys.argv[3]))
        elif len(sys.argv) == 5:
            p,v = testaa(sys.argv[1], sys.argv[2], float(sys.argv[3]), ast.literal_eval(sys.argv[4]))
        elif len(sys.argv) == 6:
            p,v = testaa(sys.argv[1], sys.argv[2], float(sys.argv[3]), ast.literal_eval(sys.argv[4]), ast.literal_eval(sys.argv[5]))
        else:
            sys.stdout.write('Wrong number or arguments, teacher made a mistake, inform teacher')
            exit()
    except ValueError:
        sys.stdout.write('Hyväksyttävät portit listassa ei ole string tyyppejä')
        exit()
    except SyntaxError:
        sys.stdout.write('Hyväksyttävät portit lista pitää olla lainausmerkeissä, jos siinä useampia portteja')
        exit()
    sys.stdout.write('Pisteet: ' + str(p) + '\n')
    sys.stdout.write(v+ '\n')
