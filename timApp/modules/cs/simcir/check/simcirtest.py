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
        return "(" + str(self.root) + " " + str(self.left) + " " + str(self.right) + ")"


def AND(*x):
    if len(x) < 2:
        return False
    else:
        return all(x)


def OR(*x):
    return any(x)


def EOR(*x):
    return OR(*x) and NOT(AND(*x))


def XOR(*x):
    return OR(*x) and NOT(AND(*x))


def ENOR(*x):
    return NOT(XOR(*x))


def XNOR(*x):
    return NOT(XOR(*x))


def NOT(x):
    return not x


def NAND(*x):
    return NOT(AND(*x))


def NOR(*x):
    return NOT(OR(*x))


def DC():
    return True


# funcs = {'AND': AND, 'NOT': NOT, 'OR': OR, 'XOR': XOR, 'XNOR': XNOR, 'EOR': XOR, 'ENOR': XNOR, 'NAND(3in)': NAND}


class Device:
    def __init__(self, dat):
        self.dev_type = dat["type"]
        self.dev_id = dat["id"]  # I hope these two always exist
        self.label = dat.get("label", None)
        self.x = int(dat["x"])
        self.y = int(dat["y"])
        self.num_inputs = int(dat.get("numInputs", -1))

    def tuple_type_label(self):
        return (self.dev_type, self.label)

    def __str__(self):
        if self.num_inputs > -1:
            numInp = ', "numInputs":"%d"' % self.num_inputs
        else:
            numInp = ""
        return '{"type":"%s", "id":"%s", "label":"%s", "x":"%d", "y":"%d"%s}' % (
            self.dev_type,
            self.dev_id,
            self.label,
            self.x,
            self.y,
            numInp,
        )


class Connector:
    def __init__(self, dat):
        self.from_id, self.from_port = dat["from"].split(".")
        self.to_id, self.to_port = dat["to"].split(".")

    def tuple_to_from(self):
        return (self.to_id, self.from_id)

    def __str__(self):
        return '{{"from":"{}.{}","to":"{}.{}"}}'.format(
            self.from_id,
            self.from_port,
            self.to_id,
            self.to_port,
        )

    def __repr__(self):  # listattujen olioiden tulostamiseen (testausta varten)
        return str(self)


class Logik:
    # def __init__(self, data, reverse):
    def __init__(self, data):
        # self.terminals = set()
        self.terminals = []  # use list to catch possible duplicate switches
        # Lista k??ytetyist?? komponenteista, DC mukana lausekkeiden muodostamista varten (jos portti/LED kytketty suoraan DC:hen ohi kytkimen
        self.komponentit = [
            "AND",
            "NAND",
            "OR",
            "NOR",
            "XOR",
            "XNOR",
            "EOR",
            "ENOR",
            "NOT",
            "DC",
        ]
        self.portit = (
            set()
        )  # Store the types of ports used in the schematic (ehk??p?? lista olisi parempi, jos halutaan tallentaa my??hemmin m????r????), vertailussa nyky????n se kuitenkin muutetaan listaksi
        self.kytkematta = (
            []
        )  # Lista porteista joiden ulostulo(t) tai sis????nmeno(t) on kytkem??tt??. Ne joissa kaikki kytkem??tt?? erotellaan erilleen
        self.kelluvat = (
            []
        )  # komponentit, joista ei mit????n kytketty, eli ei vaikuta mill????n tavalla kytkent????n.
        self.inputkytkematta = []  # Lista kytkimist??, joita ei ole kytketty
        self.porttiennenkytkinta = (
            []
        )  # lista porteista, jotka on DC:n ja kytminen v??liss??
        if data is not None:
            self.devices = {}
            self.ulostulot = []
            self.ledit = []
            self.connectors = []  # data['connectors']
            portti = "LED"  ## Mofify to support others some day, make a list of possible output types

            for c in data["connectors"]:
                con = Connector(c)
                self.connectors.append(con)

            for d in data["devices"]:
                dev = Device(d)
                self.devices[dev.dev_id] = dev
                if dev.dev_type == "Toggle":  # lis???? muita input tyyppej??
                    # self.terminals.add(dev.label)
                    if dev.dev_id in [
                        x.from_id for x in self.connectors
                    ] and dev.dev_id in [x.to_id for x in self.connectors]:
                        self.terminals.append(dev.label)
                    else:
                        self.inputkytkematta.append(dev.label)
                        # print(self.inputkytkematta)
                elif dev.dev_type == "LED":  # verrataan t??h??n, onko kytketty
                    self.ledit.append(dev.label)
                elif (
                    dev.dev_type in self.komponentit and dev.dev_type != "DC"
                ):  # portit, joita kytkenn??ss?? k??ytetty
                    # Tarkastetaan ett?? on oikeasti k??ytetty, eli ainakin 1 ulostulo ja sis????nmeno kytketty, ett?? lis??t????n portteihin
                    # print(dev.dev_id in [x.from_id for x in self.connectors])
                    # print(dev.dev_id in [x.to_id for x in self.connectors])
                    # print('dev ' + dev.dev_id)
                    if dev.dev_id in [
                        x.from_id for x in self.connectors
                    ] and dev.dev_id in [x.to_id for x in self.connectors]:
                        self.portit.add(
                            dev.dev_type
                        )  # ainakin 1 input ja 1 output kytketty
                    elif dev.dev_id in [
                        x.from_id for x in self.connectors
                    ] or dev.dev_id in [x.to_id for x in self.connectors]:
                        self.kytkematta.append(
                            dev.dev_type
                        )  # joko input tai output kytketty, mutta ei molempia
                    else:
                        self.kelluvat.append(
                            dev.dev_type
                        )  # ei yht????n input tai output kytketty

            # print('kytketty ' + str(self.portit))
            # print('kytkem??tt?? ' + str(self.kytkematta))

            for c in self.connectors:
                if self.devices[c.from_id].dev_type == portti:
                    # save the device id which is connected to LED (portti)
                    # self.ulostulot.append(con.to_id);
                    self.ulostulot.append(c)
                    # self.komponentit = {} # k??ytet????n t??m??n tilalla self.devices
                    # self.reverse = reverse
                # Jos kytkimen ja DC:n v??liss?? on jokin portti
                if (
                    self.devices[c.from_id].dev_type == "Toggle"
                    and self.devices[c.to_id].dev_type != "DC"
                ):
                    self.porttiennenkytkinta.append(self.devices[c.to_id].dev_type)
                    # print(self.devices[c.to_id].dev_type)
                # print(con.from_id in self.portit)
                # if self.devices[con.from_id].dev_type == portti:
                # save the device id which is connected to LED (portti)
                # self.ulostulot.append(con.to_id);
                # self.ulostulot.append(con)
                # self.komponentit = {} # k??ytet????n t??m??n tilalla self.devices
                # self.reverse = reverse
            ## Lopuksi katsotaan ett?? oliko 3+ input komponentti ja vaihdetaan tilalle 3-input komponentit ja vastaavat kytkenn??t
            # Ei toimi viel??
            # for k, dev in list(self.devices.items()): # list because we modify self.devices
            #     if dev.num_inputs > 2:
            #         # print(dev.num_inputs)
            #        self.poista_input(dev.dev_id, dev.dev_type, dev.num_inputs)
            # print(self.connectors)

            # print(self.devices)

    def lausekkeesta_totuustaulu(self, lausekkeet):
        import itertools

        # Terminals sis??lt???? muuttujat.
        # Luodaan yht?? monta totuusarvoa ja k??yd????n
        # n??ille kaikki kombinaatiot l??pi (product),
        # joka tuottaa k??tev??sti totuustaulun sy??tteen.
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
            if loc == []:
                return -4, 0  # tyhj?? kaavio
            # loc = sorted(locss[0], reverse=self.reverse)
            label_length = max(len(x) for x in loc)
            frmt = (i.center(label_length) for i in loc)
            labels = str(" ".join(frmt))
            return labels, label_length

        def hianosti(d, label_length):
            k = sorted(d)
            # k = sorted(d, reverse=self.reverse)
            l = []
            for i in k:
                l.append(d[i])
            return str(" ".join((str(int(i))).center(label_length) for i in l))
            # return str(' '.join((str(i)[0]).center(label_length) for i in l))

        """
        useampien ulostulojen totuustaulut eri taulukoissa

        tulostettava = ""
        for (nimi,lauseke) in lausekkeet.items():
            # sys.stdout.write(self.komponentit[nimi].label+'\n')
            #tulostettava += self.komponentit[nimi].label+'\n'
            #tulostettava += nimi + '\n' # bring in label directly <-- siirret????n muuttujien kanssa samalle riville
            hd,label_length = header(locss)
            # sys.stdout.write(hd+'\n')
            tulostettava += hd + ' ' + nimi + '\n'
            for locs in locss:
                arvo = eval(lauseke, globals(), locs)
                # sys.stdout.write(hianosti(locs, label_length)+' '+str(arvo)+'\n')
                tulostettava += hianosti(locs, label_length)+' '+str(arvo)+'\n'

            # sys.stdout.write('\n')
            tulostettava += '\n'
        """

        """
        useampien ulostulojen totuustaulut samassa taulukossa
        """
        try:
            tulostettava = ""
            hd, label_length = header(locss)
            for (nimi, lauseke) in sorted(lausekkeet.items()):
                hd += " ".rjust(label_length) + nimi
                # hd += str(' '.join(nimi.center(max(len(nimi),label_length))))
            for locs in locss:
                tulostettava += hianosti(locs, label_length).rstrip()
                for (nimi, lauseke) in sorted(lausekkeet.items()):
                    # jos LED suoraan kytketty DC:hen
                    if lauseke == "x":  # jos LED ei kytketty
                        arvo = "x"
                        tulostettava += " ".center(max(len(nimi), label_length)) + arvo
                    else:
                        if lauseke == "DC":
                            lauseke = "DC()"
                        try:
                            arvo = eval(lauseke, globals(), locs)
                        except:
                            return (
                                -6
                            )  ## jokin komponentti kytkenn??ss?? jota ei voida evaluoida
                        if callable(
                            arvo
                        ):  # Jos on funktio, niin LED:lt?? ei johdotusta kytkimelle asti
                            return -8
                        tulostettava += " ".center(max(len(nimi), label_length)) + str(
                            int(arvo)
                        )  # change true,false to 1,0
                tulostettava += "\n"
            tulostettava = hd + "\n" + tulostettava

            return totuustaulu_muotoilu(tulostettava)
        except TypeError:
            return (
                -4
            )  ## palautetaan tyhj?? totuustaulu, kaaviosta puuttuu sis????nmenoja ja/tai ulostuloja

    def doit(self):
        # pit???? pohtia viel?? muuttujien nimemisen vaihtoa
        # for (i,j) in self.komponentit.items():
        #    if j[1]=='B1': self.komponentit[i]=(j[0], 'B1')
        lausekkeet = {}
        testlist = []
        for c in self.connectors:
            if (
                c.from_id == c.to_id
            ):  # Takaisinkytkent?? (suoraan sis????nmenoon ulostulosta, helppo havaita, ei turhaan odoteta RecursionExce)
                return -3, self.portit

        if (
            self.ulostulot == [] or self.terminals == []
        ):  ## Kaikki ulostulot tai kytkimet on poistettu
            return -4, self.portit
        for out in self.ulostulot:
            testlist.append(self.devices[out.from_id].label)
        if len(testlist) != len(set(testlist)):  # Jos duplikaatti LEDej??
            return -1, self.portit
        if len(self.terminals) != len(set(self.terminals)):  # Jos duplikaatti kytkimi??
            return -2, self.portit
        if self.inputkytkematta != []:  # Jos kytkin kytkem??tt??
            return -7, self.inputkytkematta
        # self.kytkematta <-- jos noita niin rangaistaan
        if (
            self.kytkematta != []
        ):  # Jos jonkin komponentin (kaikki) inputit tai (kaikki) outputit kytkem??tt??
            return -9, self.inputkytkematta
        if self.porttiennenkytkinta != []:  # Jos portti on DC:n ja kytkimen v??liss??
            return -10, self.porttiennenkytkinta
        kytkemattomat = list(set(self.ledit) - set(testlist))
        if kytkemattomat != []:  # LED kytkem??tt??
            for k in kytkemattomat:
                lausekkeet[k] = "x"

        for out in self.ulostulot:
            try:
                r = BinaryTree(out.to_id)
                self.muodostaPuu(self.connectors, r)
                lausekkeet[self.devices[out.from_id].label] = self.yhdista(
                    r
                )  # use label
            except RecursionError:
                return (
                    -3,
                    self.portit,
                )  # Takaisinkytkent?? (useamman komponentin kautta, ei helppo testata, joten napataan poikkeuksella)

        # return self.lausekkeesta_totuustaulu(lausekkeet)
        return self.lausekkeesta_totuustaulu(lausekkeet), self.portit

    def yhdista(self, oksajuuri, lispish=False):
        if lispish:
            alku, par_alku, par_vali = "(", " ", " "
        else:
            alku, par_alku, par_vali = "", "(", ", "

        if not oksajuuri:
            return

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

        return (
            alku
            + oksakomponentti
            + par_alku
            + str(vasenOksa)
            + par_vali
            + str(oikeaOksa)
            + ")"
        )

    def muodostaPuu(self, lines, puu):
        for con in lines:
            if puu.getRootVal() == con.from_id:
                komp = self.devices[con.to_id].dev_type
                if (
                    komp == "DC" and self.devices[puu.getRootVal()].dev_type == "Toggle"
                ):  # Switch connected to DC, continue
                    continue
                if not puu.getLeftChild():
                    puu = puu.insertLeft(con.to_id)
                    self.muodostaPuu(lines, puu.getLeftChild())
                elif not puu.getRightChild():
                    puu = puu.insertRight(con.to_id)
                    self.muodostaPuu(lines, puu.getRightChild())

    def etsi_komponentit(self):
        """
        T??t?? ei t??ll?? hetkell?? kutsuta:
            - terminalit lis??t????n konstruktorissa
            - self.komponentit korvataan self.devices HUOM self.komponentit otettu muuhun k??ytt????n!!!
            - ei tue viel?? 3+ input loogisia portteja, pit???? mietti?? jos senkin tekee kostruktorissa
        """
        """
        for (dev_id, dev) in self.devices.items():
            if dev.dev_type and dev.label:
                if dev.dev_type == 'Toggle':  # lis???? muita input tyyppej??
                    self.terminals.add(dev.label)
                if dev.num_inputs < 4:
                    self.komponentit[dev_id] = dev
                else:
                    self.connectors = self.poista_input(dev_id, tyyppi, dev.num_inputs)
                    dev = None
        return self.connectors
        """

    def poista_input(self, dev_id, tyyppi, num_inputs):
        # raise Error("ei toteutettu kunnolla viel??")
        print("ei toteutettu kunnolla viel??")
        self.devices[
            dev_id
        ].label = tyyppi  # change label to type, because we use basic components (AND) to replace n-input ones e.g. label AND(3in)
        # K??sitell????n negatoidut komponentit erikseen, tehd????n muutos ilman negatointeja, ja lopputulos vasta negatoidaan.
        # Esim LED = NAND(a,b,c) --> LED = NOT(AND(a,AND(b,c)))
        if tyyppi.startswith("n"):
            tyyppi = tyyppi[1:]
        elif tyyppi == "XNOR":
            tyyppi = "XOR"

        print(self.devices[dev_id])
        # Lis??t????n uusia virtuaalisia (x) portteja kompensoimaan n-input portin muutos 2-input porteiksi
        # poistetaan muutettava komponentti my??hemmin...

        # T??ss?? pit??isi luoda uusi Device ja sitten lis??t?? se devices:iin ja sitten lopulta poistaa alkuper??inen n-input dev
        self.devices[dev_id + "x" + str(num_inputs)] = (tyyppi, tyyppi)

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
        ## T??ss?? pit??isi self. connectorista poistaa dev:i?? vastaavat rivit
        ##fromlines = [rivi for rivi in from_llines if not dev_id+'.in' in rivi]
        ## Ja my??hemmin lis??t?? uudet rivit...
        ## TODO: mieti arin.py versiossa oleva rivien k??sittely versus t??ss?? kun k??sitell????n self.connectors luokkaa

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
    """
    Palauttaa totuustaulun simcirjs kaaviosta
    """
    l = Logik(kaavio)
    return l.doit()


def totuustaulu_lausekkeesta(yhtalot):
    """
    yhtalot pitaa olla taulukko stringeist??, esim.: ["LED = AND(A,B)"] tai ["Sininen = AND(a,sel)", "Keltainen = AND(a,NOT(sel))"]
    = -merkin vasemalla puolella on ulostulon nimi (label)
    Palauttaa: lausekkeista muodotetun totuustaulun, esim. kahdelle outputille
     a  sel   Keltainen   Sininen
     0   0        0         0
     0   1        0         0
     1   0        1         1
     1   1        1         0

    """
    l = Logik(None)
    import re

    lausekkeet = {}
    for yhtalo in yhtalot:
        [output, lauseke] = yhtalo.split("=")
        osat = re.findall(r"[\w']+", lauseke)

        for i in osat:
            if i not in l.komponentit:
                # l.terminals.add(i)
                if i not in l.terminals:
                    l.terminals.append(i)
        # l.terminals = set([u'B4', u'A1', u'A3', u'A2', u'A4', u'B2', u'B3', u'B1'])
        lausekkeet[output.strip()] = lauseke
    return l.lausekkeesta_totuustaulu(lausekkeet)


def testaa_totuustaulut(expected, actual):
    """
    Luotetaan siihen ett?? totuustaulut on samaa muotoa, etsit????n poikkeavat totuustaulun rivit
    palauttaa virheiden lukum????r??n ja virheelliset totuustaulun rivit
    """
    import difflib

    expected = expected.splitlines(1)
    actual = actual.splitlines(1)

    virheita = 0
    d = difflib.Differ()
    result = list(d.compare(actual, expected))
    difference = ""
    for line in result:
        if line[0] == "+":
            virheita += 1
            difference += line.replace("+", " ")

    return virheita, difference


def totuustaulu_muotoilu(totuustaulu):
    """
    muotoillaan totuustaulu samaan muotoon kuin lausekkeesta muotoiltu totuustaulu
    aikamoinen viritys... totuustaulujen samankaltaisuus p????tell????n rivien samankaltaisuudesta
    pit???? mietti??, tekisik?? sen joskus arvoilla, kun muotoilu voi menn?? pilalle
    """
    from math import log2, ceil

    # poistetaan mahdolliset tyhj??t rivit ja viivalla alkavat rivit (TIM taulukosta)
    try:
        totuustaulu = [
            line
            for line in totuustaulu.split("\n")
            if line.strip() != "" and not line.strip().startswith("-")
        ]
        muokattu = ""
        # Selvitet????n muuttujien ja ulostulojen m????r?? ja nimen pituudet,
        # muuttujien m????r?? on 2-kantainen logaritmi totuustaulun riveist?? (pl. otsikkorivi)
        # py??ristet????n yl??s, totuustaulussa ei v??ltt??m??tt?? kaikkia rivej??
        count = int(ceil(log2(len(totuustaulu) - 1)))
        nimet = totuustaulu[0].split()
        maxlenmuuttujat = max(len(item) for item in nimet[:count])
        maxlenulostulot = max(len(item) for item in nimet[count:])
        # Muokkauksia tulostusta varten... sotkuista, mutta kaikki totuustaulut
        # k??y t??t?? kautta, joten tulee samanlainen muotoilu
        frmt1 = (i.center(maxlenmuuttujat) for i in nimet[:count])
        frmt2 = (("   " + i).center(maxlenmuuttujat) for i in nimet[count:])
        labels = str(" ".join(frmt1) + "".join(frmt2))
        muokattu = labels + "\n"
        for i in range(1, len(totuustaulu)):
            rivi = totuustaulu[i].split()
            frmt1 = (i.center(maxlenmuuttujat) for i in rivi[:count])
            frmt2 = (
                i.center(max(maxlenmuuttujat, maxlenulostulot)) for i in rivi[count:]
            )
            labels = str(" ".join(frmt1) + "   " + " ".join(frmt2))
            muokattu += labels.rstrip() + "\n"
        return muokattu
    except ValueError:  # T??m?? tapahtuu kun LED puuttuu
        return (
            -4
        )  ## palautetaan tyhj?? totuustaulu, kaaviosta puuttuu sis????nmenoja ja/tai ulostuloja


def onko_virheita(
    testitaulu, oikeataulu, maksimipisteet, portit=[], hyvportit=[], tulosta=True
):
    """
    23.8.2016 Lis??t????n testi ett?? on k??ytetty vaadittuja portteja, aiemmin testattiin vain ett?? onko ylim????r??isi??
    :param testitaulu: testattava totuustaulu
    :param oikeataulu: oikea totuustaulu
    :param maksimipisteet: teht??v??n maksimipisteet
    :param portit: portit jota k??ytetty kaaviossa
    :param hyvportit: [] jos saa k??ytt???? mit?? tahansa portteja, tai esim. ['NAND'] jos saa k??ytt???? pelkki?? NANDeja
    :param tulosta: Tulostetaanko virheelliset rivit (ja pisteet) opiskelijalle (esim. kokeessa ei tulosteta) - ei toteutettu viel??
    :return: teht??v??st?? saadut pisteet, virheelliset rivit (tai 'oikein' -teksti tai virheteksti)
    """

    # portit voi sis??lt???? simcir:n EOR je ENOR, kun k??yt??n XOR ja XNOR, muutetaan -->
    portit = [w.replace("EOR", "XOR") for w in portit]
    portit = [w.replace("ENOR", "XNOR") for w in portit]

    if hyvportit != []:
        # check if portit only contains ports that are listed in hyvportit
        # print(all(any(p == hyvp for hyvp in hyvportit) for p in portit)) Antaa True tai False
        # print([x for x in portit if x not in hyvportit]) # Antaa ylim????r??iset portit
        # print([x for x in portit if x in hyvportit])  # Antaa hyv??ksytyt portit
        ylimportit = [x for x in portit if x not in hyvportit]
        vaaditutportit = [x for x in portit if x in hyvportit]
    else:
        ylimportit = []
        vaaditutportit = []
    oikeatrivit = oikeataulu.splitlines()
    riveja = len(oikeatrivit) - 1  # poistetaan muuttuja/otsikko -rivi
    if isinstance(testitaulu, int):  # jotain vikaa jos testitaulu on (negatiivinen) int
        return tulosta_vai_ei(0.0, testitaulu, tulosta, oikeatrivit[0])

    virheita, virherivit = testaa_totuustaulut(testitaulu, oikeataulu)
    # print('virheit?? ' + str(virheita))
    # Tulostetaan vain maxrivit rivi?? virheit??
    maxrivit = 8
    if virheita > maxrivit:
        virherivit = "\n".join(virherivit.splitlines()[:maxrivit])
        virherivit += (
            "\n  ...\nlis??ksi "
            + str(virheita - maxrivit)
            + " muuta virheellist?? rivi??\n"
        )
    # Lis??t????n sis????nmenot ja ulostulot virheellisten rivien yl??puolelle
    if virheita > 0:
        virheet = (
            "Seuraavat rivit kytkent????si vastaavassa totuustaulussa\npoikkeavat teht??v??nantoa vastaavan totuustaulun riveist??:\n"
            + "  "
            + oikeatrivit[0]
            + "\n"
            + virherivit
        )
    else:
        virheet = ""
    # maksimipisteet kerrotaan oikeiden rivien suhteella kaikkiin riveihin
    pistekertoja = (riveja - virheita) / riveja
    # Muutetaan siten ett?? py??ristet????n alasp??in
    # pisteet = round(maksimipisteet*pistekertoja*4)/4 # round to .25 intervals
    # from math import floor
    # pisteet = floor(maksimipisteet*pistekertoja*4)/4 # round down to .25 intervals
    # 0.25 v??lein py??rist??minen ei toimi, kun teht??v??st?? saa vain 0.5 pistett??, niin esim. 7/8 oikein antaa 0.25
    # Py??ristet????n vain 2 desimaalin tarkkuudella.
    pisteet = round(maksimipisteet * pistekertoja, 2)

    if virheita == 0:
        if hyvportit == []:
            return tulosta_vai_ei(pisteet, "Oikein, t??ydet pisteet!\n", tulosta)
        elif hyvportit != []:
            if vaaditutportit == []:
                return tulosta_vai_ei(
                    maksimipisteet / 2,
                    "Totuustaulu oikein, mutta puolet maksimipisteist??,\nkytkenn??ss?? ei ole yht????n sallituista porteista\n"
                    + str(hyvportit)
                    + ", siten ett?? niiden sis????nmenot \nja ulostulot on kytketty.\n",
                    tulosta,
                )
            elif ylimportit == []:
                return tulosta_vai_ei(pisteet, "Oikein, t??ydet pisteet!\n", tulosta)
            else:
                return tulosta_vai_ei(
                    maksimipisteet / 2,
                    "Totuustaulu oikein, mutta puolet maksimipisteist??,\nkoska et k??ytt??nyt vain sallittuja portteja "
                    + str(hyvportit)
                    + "\nvaan kytkenn??ss?? on my??s portit "
                    + str(ylimportit)
                    + "\n",
                    tulosta,
                )
        # else: # Ei m????ritelty erikseen k??ytett??vi?? portteja
        #   return tulosta_vai_ei(pisteet, "Oikein, t??ydet pisteet!\n", tulosta)
    if pisteet < 0:
        return tulosta_vai_ei(0.0, -5, tulosta, oikeatrivit[0])
    # alla tapaukset, joissa totuustaulu v????rin, ei syntaksivirheit??
    if hyvportit != [] and vaaditutportit == []:
        return tulosta_vai_ei(
            0.0,
            virheet
            + "lis??ksi kytkenn??ss?? ei ole yht????n sallituista porteista\n"
            + str(hyvportit)
            + ", siten ett?? niiden sis????nmenot \nja ulostulot on kytketty.\n",
            tulosta,
        )
    elif ylimportit == []:
        return tulosta_vai_ei(pisteet, virheet, tulosta)
    else:
        return tulosta_vai_ei(
            0.0,
            virheet
            + "\nlis??ksi sallitut portit olivat "
            + str(hyvportit)
            + "\nmutta kytkenn??ss??si on muita portteja "
            + str(ylimportit)
            + "\n",
            tulosta,
        )


def tulosta_vai_ei(pisteet, taulu, tulosta=True, teksti=""):
    # 0 -> -len, 1 -> -(len-1), ...
    # Tulostetaan virhetekstit aina (my??s koetilanteessa)
    virhetekstit = [
        "Kytkenn??ss?? on portti j??nnitel??hteen (DC) ja kytkimen (sis????nmeno) v??liss??.\nPortti ei voi olla ennen sis????nmenoa, siirr?? portti kytkimen toiselle puolelle.\n",
        "Kytkenn??ss?? ei ole johdotettu kaikkia k??ytettyjen porttien\nsis????nmenoja tai ulostuloja. Tarkista kytkenn??n johdotukset.\n",
        "Kytkenn??ss?? ei ole johdotettu kaikkia kytkimi?? (porttien kautta)\njohonkin ulostuloon. Tarkista kytkenn??n johdotukset.\n",
        "Kytkenn??ss?? ei ole johdotettu kaikkia kytkimien sis????nmenoja\nja ulostuloja. Tarkista kytkinten johdotukset.\n",
        "Kytkenn??ss?? on jokin komponentti, jota tarkistin ei tue.\nK??yt?? vain ty??kaluissa ja piirikaaviossa olevia komponentteja\n",
        "Kytkenn??ss?? on liikaa sis????nmenoja tai ulostuloja.\nSis????nmenot ja ulostulot tulee olla: "
        + teksti
        + "\n",
        "Kytkenn??st?? puuttuu sis????nmenoja tai ulostuloja.\nSis????nmenot ja ulostulot tulee olla: "
        + teksti
        + "\n",
        "Kytkenn??ss?? takaisinkytkent?? (portin ulostulo kytketty omaan sis????nmenoon,\njoko suoraan tai muiden porttien kautta). Poista takaisinkytkent??.\n",
        "Kytkenn??ss?? samannimisi?? sis????nmenoja, poista ylim????r??iset.\nSis????nmenot ja ulostulot tulee olla: "
        + teksti
        + "\n",
        "Kytkenn??ss?? samannimisi?? ulostuloja, poista ylim????r??iset.\nSis????nmenot ja ulostulot tulee olla: "
        + teksti
        + "\n",
    ]
    if isinstance(taulu, int):
        return pisteet, virhetekstit[taulu + len(virhetekstit)]
    if tulosta:
        return pisteet, taulu
    else:
        return pisteet, ""


def muotoile_lauseke(lauseke):
    """
    Muuttaa LED = a AND b tyyppiset lausekkeet muotoon LED = AND(a,b)
        testattu toimivaksi ao. lausekkeella
    LED = (((NOT (NOT(NOT A1) XOR B1)) AND (NOT (A2 XOR NOT (NOT (B2))))) AND NOT(NOT((NOT (A3 XOR B3)) AND (NOT (NOT(NOT(A4)) XOR B4)))))
    Toiminta
     - ensin k??sitell????n NOT, joilla parametrina yksi muuttuja
     - sitten k??sitell????n NOT:ien alla olevat lausekkeet
     - sitten NOT:tien alla olevat lausekkeet yhdistet????n NOT:in kanssa samaksi lausekkeeksi
     - sitten k??sitell????n muut operaattorit
    Tehd????n lausekkeesta lista, jossa muotattuja termej?? yhdistell????n ja niiden paikkoja vaihdellaan
     - esim. listassa [... , 'A', 'XOR', 'B', ...] --> [... , 'XOR(A,B)', ...]
    kunnes listassa on en???? yksi termi ja se on uusi lauseke. LED irrotetaan alussa ja liitet????n lopussa.
    :param lauseke: 'LED = a AND b' tai vastaava
    :return: 'LED = AND(a,b)' tai vastaava
    """

    def muokkaaLauseke(t):
        # muokkaillaan lauseketta, yhdistet????n operaatioihin termit ja muutetaan j??rjestyst??
        while len(t) > 1:
            j = 0
            # print(len(t))
            while j < len(t):
                # K??sitell????n NOT:it ilman sulkeita
                # print(str(j))
                if j + 1 < len(t) and t[j] == "NOT" and t[j + 1] != "(":
                    t[j], t[j + 1] = t[j] + "(", t[j + 1] + ")"
                    t[j : j + 2] = ["".join(t[j : j + 2])]
                    # print('0. j, t[j]: ' + str(j) + ', ' + t[j])
                # yhdistet????n ( -sulkeet sit?? edelt??v????n loogiseen NOT operaatioon
                if j + 1 < len(t) and t[j] == "NOT" and t[j + 1] == "(":
                    t[j : j + 2] = ["".join(t[j : j + 2])]  # x[3:6] = [''.join(x[3:6])]
                    # print('1. j, t[j]: ' + str(j) + ', ' + t[j])
                    # print(len(t))
                    # print(t)
                # esim. 'NOT(', 'B2', ')' # Yhdistet????n NOT sen sis??ll?? olevaan lausekkeeseen
                elif j + 2 < len(t) and t[j] == "NOT(" and t[j + 2] == ")":
                    t[j : j + 3] = ["".join(t[j : j + 3])]
                    # print('2. j, t[j]: ' + str(j) + ', ' + t[j])
                # esim. A AND B tai A XOR B
                elif len(t) == 3 and j == 0 and t[j + 1] in komponentit:
                    t[j], t[j + 1], t[j + 2] = (
                        t[j + 1] + "(",
                        t[j + 0] + ",",
                        t[j + 2] + ")",
                    )
                    t[j : j + 3] = ["".join(t[j : j + 3])]
                    # print('23. j, t[j]: ' + str(j) + ', ' + t[j])
                # esim. 'NOT(', 'A', 'AND', 'B', ')' # Yhdistet????n NOT sen sis??ll?? olevaan lausekkeeseen
                # esim. 'NOT(', '...', 'XOR', '...', ')' # Yhdistet????n NOT:n sis??ll?? oleva lauseke
                elif (
                    j + 4 < len(t)
                    and t[j] == "NOT("
                    and t[j + 4] == ")"
                    and t[j + 2] in komponentit
                ):
                    t[j + 1], t[j + 2], t[j + 3] = (
                        t[j + 2] + "(",
                        t[j + 1] + ",",
                        t[j + 3] + ")",
                    )
                    t[j : j + 5] = ["".join(t[j : j + 5])]
                    # print('3. j, t[j]: ' + str(j) + ', ' + t[j])
                # esim. '(', '...', ')', 'AND', '(', '...', ')'
                elif (
                    j - 3 >= 0
                    and j + 3 < len(t)
                    and t[j] in komponentit
                    and t[j - 1] == ")"
                    and t[j + 3] == ")"
                    and t[j - 3] == "("
                    and t[j + 1] == "("
                ):
                    t[j - 3], t[j - 2], t[j - 1], t[j], t[j + 1] = (
                        t[j],
                        t[j - 3],
                        t[j - 2],
                        ",",
                        "",
                    )
                    t[j - 3 : j + 4] = ["".join(t[j - 3 : j + 4])]
                    # print('4. j-3, t[j-3]: ' + str(j-3) + ', ' + t[j-3])
                    j = j - 3
                # esim. '...', 'AND', '(', '...', ')'
                elif (
                    j - 1 >= 0
                    and j + 3 < len(t)
                    and t[j] in komponentit
                    and t[j + 3] == ")"
                    and t[j + 1] == "("
                    and t[j - 1] != ")"
                ):
                    t[j - 1], t[j], t[j + 1] = t[j], t[j + 1], t[j - 1] + ","
                    t[j - 1 : j + 4] = ["".join(t[j - 1 : j + 4])]
                    # print('5. t[j-1]: ' + t[j-1])
                    j = j - 1
                # esim. '(', '...', 'AND', '...', ')'
                elif (
                    j - 2 >= 0
                    and j + 2 < len(t)
                    and t[j] in komponentit
                    and t[j - 2] == "("
                    and t[j + 2] == ")"
                ):
                    t[j - 1], t[j] = t[j], t[j - 1]
                    t[j] = "(" + t[j]
                    t[j + 1] = "," + t[j + 1] + ")"
                    t[j - 1 : j + 2] = ["".join(t[j - 1 : j + 2])]
                    # print('6. t[j-1]: ' + t[j-1])
                    j = j - 1
                # jos yksi muuttuja tms. sulkeiden v??liss?? --> yhdistet????n
                elif j + 2 < len(t) and t[j].endswith("(") and t[j + 2].startswith(")"):
                    t[j : j + 3] = ["".join(t[j : j + 3])]
                    # print('7. t[j]: ' + t[j])
                else:
                    j += 1

    # NOT k??sitell????n erikseen, ei testattu DC:n kanssa viel?? (opettajan ratkaisun ei 'pit??isi' sis??lt???? DC:t??)
    komponentit = ["AND", "NAND", "OR", "NOR", "XOR", "XNOR", "EOR", "ENOR"]
    osat = lauseke.split("=")  # irroitetaan LED
    t = (
        osat[1].lstrip().replace("(", " ( ").replace(")", " ) ")
    )  # varmistetaan ett?? sulkujen ymp??rill?? on v??lej??
    t = " ".join(t.split())  # poistetaan extra v??lit
    t = t.split(" ")  # taulukoidaan sulut, muuttujat ja operaatiot
    # print(t)
    muokkaaLauseke(t)  # muokataan kunnes listassa on en???? yksi lopullinen string
    # print(len(t))
    # print(t)
    return osat[0].rstrip() + " = " + "".join(t)


def testaa_kaaviot(
    testikaavio, oikeakaavio, maksimipisteet, hyvportit=[], tulosta=True
):
    """
    Testaa opettajan antaman kaavion vs. opiskelijan kytkent??kaavio
    :param testikaavio: JSON string opiskelijan simcirjs kaaviosta
    :param oikeakaavio: JSON string opettajan simcirjs kaaviosta
    :param maksimipisteet: Teht??v??st?? saatava maksimipistem????r??
    :param hyvportit: Hyv??ksytyt portit, on [] jos saa k??ytt???? mit?? tahansa portteja, tai esim. ['NAND'] jos saa k??ytt???? pelkki?? NANDeja
    :param tulosta: True/False - tulostetaanko tieto arvostelusta vai ei (esim. kokeessa) palauttaa kuitenkin pisteet aina!
    :return: (double, string) - pisteet doublena, ja string:n, jossa 'oikein'-teksti, virheelliset totuustaulun rivit tai muuta virheinformaatiota
    """
    testitaulu, portit = totuustaulu_kaaviosta(json.loads(testikaavio))
    oikeataulu, malliportit = totuustaulu_kaaviosta(
        json.loads(oikeakaavio)
    )  # opettajan portit, ei k??ytet??, ehk?? my??hemmin voisi antaa infoa opiskelijalle, jos k??ytti enemm??n portteja kuin mallissa, mutta miten sen hoitaisi sitten kun opettaja antaa totuustaulun...

    return onko_virheita(
        testitaulu, oikeataulu, maksimipisteet, list(portit), hyvportit, tulosta
    )


def testaa_lauseke_vs_kaavio(
    testikaavio, oikealauseke, maksimipisteet, hyvportit=[], tulosta=True
):
    """
    Testaa opettajan antaman lausekkeen vs. opiskelijan kytkent??kaavio
    :param testikaavio: JSON string opiskelijan simcirjs kaaviosta
    :param oikealauseke: lausekkeet omilla riveill????n (j??rjestyksell?? ei v??li??)
    Keltainen = AND(a,NOT(sel))
    Sininen = AND(a,sel)
    tai muodossa (saa sis??lt???? sulkuja)
    Sininen = a AND sel
    Keltainen = a AND NOT sel
    niist?? muodostetaan taulukko esim. ["Sininen = AND(a,sel)", "Keltainen = AND(a,NOT(sel))"]
    HUOM: muuttujat eiv??t voi olla pythonin varattuja sanoja esim. 'in'
    :param maksimipisteet: Teht??v??st?? saatava maksimipistem????r??
    :param hyvportit: Hyv??ksytyt portit, on [] jos saa k??ytt???? mit?? tahansa portteja, tai esim. ['NAND'] jos saa k??ytt???? pelkki?? NANDeja
    :param tulosta: True/False - tulostetaanko tieto arvostelusta vai ei (esim. kokeessa) palauttaa kuitenkin pisteet aina!
    :return: (double, string) - pisteet doublena, ja string:n, jossa 'oikein'-teksti, virheelliset totuustaulun rivit tai muuta virheinformaatiota
    """
    if (
        "," not in oikealauseke
    ):  # a AND b muodossa ei ole pilkkua, kun taas muodossa AND(a,b) on
        oikealauseke = muotoile_lauseke(oikealauseke)
    testitaulu, portit = totuustaulu_kaaviosta(json.loads(testikaavio))
    oikeataulu = totuustaulu_lausekkeesta(oikealauseke.splitlines())

    return onko_virheita(
        testitaulu, oikeataulu, maksimipisteet, list(portit), hyvportit, tulosta
    )


def testaa_totuustaulu_vs_kaavio(
    testikaavio, oikeatotuustaulu, maksimipisteet, hyvportit=[], tulosta=True
):
    """
    Testaa opettajan antaman totuustaulun vs. opiskelijan kytkent??kaavio
    Esimerkki totuustaulun rakenteesta, viivojen ja white space -merkkien m????r??ll?? ei merkityst??

    a sel Keltainen Sininen
    - --- ---       -
    0   0  0         0
    0   1  0         0
    1   0  1         0
    1   1  0         1

    :param testikaavio: JSON string opiskelijan simcirjs kaaviosta
    :param oikeatotuustaulu: totuustaulu TIM-muodossa, esim. ks. yll??
    :param maksimipisteet: Teht??v??st?? saatava maksimipistem????r??
    :param hyvportit: Hyv??ksytyt portit, on [] jos saa k??ytt???? mit?? tahansa portteja, tai esim. ['NAND'] jos saa k??ytt???? pelkki?? NANDeja
    :param tulosta: True/False - tulostetaanko tieto arvostelusta vai ei (esim. kokeessa) palauttaa kuitenkin pisteet aina!
    :return: (double, string) - pisteet doublena, ja string:n, jossa 'oikein'-teksti, virheelliset totuustaulun rivit tai muuta virheinformaatiota
    """
    testitaulu, portit = totuustaulu_kaaviosta(json.loads(testikaavio))
    oikeataulu = totuustaulu_muotoilu(oikeatotuustaulu)

    return onko_virheita(
        testitaulu, oikeataulu, maksimipisteet, list(portit), hyvportit, tulosta
    )


def testaa(testidata, oikeadata, maksimipisteet, hyvportit=[], tulosta=True):
    """
    Testaa opiskelijan kaavion vs. opettajan kaavio, lauseke tai totuustaulu. P????ttelee opettajan
    ratkaisun tyypin sen perusteella sis??lt????k?? se merkit '{' tai '=' vai ei kumpaakaan.
    :param testidata: tekstitiedosto, jossa testattava simcir kaavio JSON muodossa
    :param oikeadata: tekstitiedosto, joko kaavio, lauseke tai totuustaulu. Muotoilu
        Kaavio - JSON simcir kaavio
        Lauseke - jokainen lauseke omalla rivill????n muodossa: LED = AND(a,b) - hyv??ksyy my??s muodon LED = a AND b
        Totuustaulu - jokainen totuustaulun rivi omalla rivill????n, saa sis??lt???? rivin joka alkaa tavuviivalla (-), ne j??tet????n huomiotta
    :param maksimipisteet: Teht??v??st?? saatava maksimipistem????r??
    :param hyvportit: Hyv??ksytyt portit, on [] jos saa k??ytt???? mit?? tahansa portteja,
        tai esim. ['NAND'] jos saa k??ytt???? pelkki?? NANDeja.
        TAI hyv??ksytyt portit voi tuoda oikeadata -tiedostossa tyhj??ll?? rivill?? erotettuna
        pilkulla erotettuna esim. muodossa: NAND, NOR ja noista muodostetaan lista
    :param tulosta: True/False - tulostetaanko tieto arvostelusta vai ei (esim. kokeessa) palauttaa kuitenkin pisteet aina!
    :return: (double, string) - pisteet doublena, ja string:n, jossa 'oikein'-teksti, virheelliset totuustaulun rivit tai muuta virheinformaatiota
    """
    testattava = open(testidata).read()
    oikea = open(oikeadata).read()
    if "\n\n" in oikea:
        hyvportit = [item.strip() for item in oikea.split("\n\n")[1].split(",")]
        if hyvportit == [""]:  # l??ytyi useita tyhji?? rivej?? (ei pit??isi olla)
            hyvportit = []
        oikea = oikea.split("\n\n")[0]
    oikeantyyppi = "".join([x for x in ["{", "="] if x in oikea]) or "t"

    return {
        "{": lambda: testaa_kaaviot(
            testattava, oikea, maksimipisteet, hyvportit, tulosta
        ),
        "=": lambda: testaa_lauseke_vs_kaavio(
            testattava, oikea, maksimipisteet, hyvportit, tulosta
        ),
        "t": lambda: testaa_totuustaulu_vs_kaavio(
            testattava, oikea, maksimipisteet, hyvportit, tulosta
        ),
    }[oikeantyyppi]()


if __name__ == "__main__":
    import sys
    import ast

    # simcirtest.py [teksti - testattava vastaus] [telsti - oikea kytkent??] [float - pisteet] [array - hyv??ksytt??v??t portit] [Boolean - tulostetaanko]
    try:
        if len(sys.argv) == 4:
            p, v = testaa(sys.argv[1], sys.argv[2], float(sys.argv[3]))
        elif len(sys.argv) == 5:
            p, v = testaa(
                sys.argv[1],
                sys.argv[2],
                float(sys.argv[3]),
                ast.literal_eval(sys.argv[4]),
            )
        elif len(sys.argv) == 6:
            p, v = testaa(
                sys.argv[1],
                sys.argv[2],
                float(sys.argv[3]),
                ast.literal_eval(sys.argv[4]),
                ast.literal_eval(sys.argv[5]),
            )
        else:
            sys.stdout.write(
                "Wrong number or arguments, teacher made a mistake, inform teacher"
            )
            exit()
    except ValueError:
        sys.stdout.write("Hyv??ksytt??v??t portit listassa ei ole string tyyppej??")
        exit()
    except SyntaxError:
        sys.stdout.write(
            "Hyv??ksytt??v??t portit lista pit???? olla lainausmerkeiss??, jos siin?? useampia portteja"
        )
        exit()
    sys.stdout.write("Pisteet: " + str(p) + "\n")
    sys.stdout.write(v + "\n")
