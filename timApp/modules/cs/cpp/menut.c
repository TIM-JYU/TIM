/****************************************************************************/
/*
**        M E N U T . C
**
** Aliohjelmia menujen tekemiseen.  Ideana on tarjota k„ytt„j„lle
** normaali menupuu ja ohjelmoijalle 1-tasoinen rakenne, jossa
** kukin aliohjelma on kesken„„n samanarvoinen ja tarvittaessa
** kutakin voidaan kutsua my”s useammasta kohdasta menua.
** Toteutuksessa ohjelmoija saa tiedon vain menunpuun lehtisolmuista
** joista kutsutaan h„nen omia aliohjelmiaan.  Kun aliohjelma
** on suorittanut teht„v„ns„, palataan takaisin menusysteemiin ja
** odotetaan uutta k„ytt„j„n valintaa.  Jos k„ytt„j„ menee menuissa
** yl”sp„in, niin ohjelmoija ei t„st„ tied„ (eik„ tarvitse tiet„„).
**
** Aliohjelmat:
**    InitMenuSystem         - alustaa menusysteemin,
**                             tiedostoa etsiot„„n ensin oletushakemistosta
**                             ja sitten kutsussa olleen nimen hakemistosta
**                             jos nimess„ ei tarkenninta tai se on .exe
**                             liitetet„„n tarkentimeksi .mnu.
**    GetInitError           - alustusvirhe selv„kielisen„
**    GetMessage             - k„ytt„„ menusysteemi„
**    DispatchMessage        - toteuttaa aliohjelman
**    PostQuitMessage        - poistaa menuille varatut tilat
**    MenuRun                - alustaa menut ja ajaa menusysteemi„
**    MenuRunArg             - kuten edell„, mutta parametrina argc ja argv
**    EditNameString         - editoi valittu nimetty„ parametria
**    ScrMessage             - parametrit”n viestin tulostus (tai
**                             oikeastaan parametrina kaikki)
**    ScrMessage1            - yksi parametrinen viestin tulostus
**    ScrMessage2            - kaksi parametrinen viestin tulostus
**    ScrMessage3            - kolme parametrinen viestin tulostus
**    ScrMessage4            - nelj„ parametrinen viestin tulostus
**    ScrMessage5            - viisi parametrinen viestin tulostus
**    ScrMessage6            - kuusi parametrinen viestin tulostus
**    SetMessageParam        - asettaa valitun viestin parametrin
**    GetMessageParam        - palauttaa valitun viestin parametrin
**    SetNameParam           - asettaa nimetyn parametrin
**    GetNameParam           - palauttaa nimetyn parametrin
**    SetNameParamInt        - asettaa nimetyn kokonaislukuparametrin
**    GetNameParamInt        - palauttaa nimetyn kokonaislukuparametrin
**    SetNameParamDouble     - asettaa nimetyn reaalilukuparametrin
**    GetNameParamDouble     - palauttaa nimetyn reaalilukuparametrin
**
** Tekij„:         Vesa Lappalainen 25.1.1994
** Muutettu:       14.2.1994/vl
** Mit„ muutettu:  N„yt”nk„sittely omaksi aliohjelmakseen.
**                 Jos rivi jatkuu ] j„lkeen, se on viestirivi
**                 ja sille on hieman omat s„„nn”t.
**                 Mm. ennen viestirivin tulostamista suoritetaan
**                 [Message0] ja ennen seuraavaa rivi„ [Message1]
**                 ja ennen seuraava [Message2] jne.
**                 Jos jokin [Message?] puuttuu, k„ytet„„n l„hint„
**                 numeroa alhaaltap„in.
** Muutettu:       15.2.1994/vl
** Mit„ muutettu:  Tiedoston etsiminen mahdollista k„ynnistyspolusta.
**                 - vaatii kutsun argv[0]:lla.
**                 Viestirivien alusta tyhj„t pois
**                 Uusia @ -komentoja:
**                   Pvm + formaattit pp.kk.vv (ks. FmtPvm)
**                   T nro    - tabulointi sarakkeeseen
**                   C v„ri   - vaihtaa taustan ja merkin v„rin
**                   [menu *] - parametri menukutsuun
**                 Aina teht„v„t hommat:
**                   [MenuBegin] - ennen menun tulostamista
**                   [MenuEnd]   - menun tulostamisen j„lkeen
**                   [MenuExit]  - kun menusta poistutaan
**                                 eli ennen lehtisolmun aliohjelman kutsua
**                   [MessageEnd]
**                   [MessageExit]
** Muutettu:       8.3.1994/vl
** Mit„ muutettu:  Virhek„sittely alustukseen muistin loppumista yms. varten.
**                   InitError ja sen  kutsu MenuRun-aliohjelmaan
**                 Rivi ei ole viestirivi jos [..]=xxx, eli sulun j„lkeen
**                 muuta kuin v„lily”nti.
**                 Nimetyt parametrit [baud]=9600
**                 Uusia @-komentoja:
**                   I param  - lukee nimetyn parametrin arvon
**                   ++param  - kasvattaa parametrin arvoa
**                   R n;m    - toistaa n kertaa merkki„ m, voidaan k„ytt„„
**                              esim @R 100@   => rivi tyhjenee
**                                   @R 100;-@ => viiva
**                 Turvallisuutta lis„tty lis„„m„ll„ const char * jokaiseen
**                 tarpeelliseen paikkaan.
**                 Lis„tty EditNameString
** Muutettu:       24.9.1994/vl
** Mit„ muutettu:  + Ep„suora osoitus, eli
**                   @goto 29,(1)@Terve  toimii siten, ett„ jos %1 = 15
**                   niin tuosta seuraa goto 29,15 (my”s ep„suorat osoitukset
**                   jopa nimettyjen parametrien kautta)
**                 + @=jono;Kissa@
**                 + nimettyyn parametriin maksimipituuden s„„t”
**                    [jono]=Kissa|10 # => jono max. pituus on 10 ja
**                                         oletusarvo "Kissa"
**                 + mahdollisuus ilmoittaa montako tasoa menuissa palataan
**                   eli @Mpop *@ ja @Mpush *@ @Mgoto *@
**                 + @Run ali@ toimimaan
**                 + \n ja \0x6a -tulkinnat.  Samoin mahdollisuus
**                   tulostaa @- yms. merkkej„ (\@)
**                 + @Exit n@
**                 + Mahdollisuus suorittaa komentoja menurivin valintana
**                   Eli  0 = Lopetus|0L|@Exit@
** Muutettu:       30.9.1994/vl
** Mit„ muutettu:  + voi olla [jono]=|10
**                 + voi olla [jono]=   Eka   |20
**                   ja v„lily”nnit s„ilyy
** Muutettu:       9.10.1994/vl
** Mit„ muutettu:  + nyt [Mpop] toimii myos suoraan "menuna"
**                   (eli Paluu|4|@Mpop -1)
**                 + reealilukuparametrit
**                 + @++ i;2@ , eli mahdollisuus valita lis„yksen m„„r„
**                 + p„„ohjelman funktioiden t„ytyy palauttaa int
**                   vanhat (void -p„„ohjelmat) voidaan k„„nt„„ ilman
**                   varoituksia m„„rittelem„ll„ omaan ohjelmaan
**                    #define MENU94_9 ennen "include "menut.h":ta.
**                   Miksi t„m„ muutos?  Siksi ett„ voidaan tehd„ if
**                   tyyppisi„ asioita:
**                    @Run oma@@[homma(ret)]@
**                    [homma0]....
**                    [homma1]....
**                    [homma*]....
**                   Eli kutsutun funktion paluuarvo on nimetyss„
**                   paramterissa ret
**                 + @M[Viesti0]@  eli viestin kutsuminen @-komentona
**                   Hy”ty:  Voidaan tehd„ odota-n„pp„in juttuja.
**                   Valittu arvo (0,1,2..) palautetaan nimetyss„
**                   parametrissa msg
**                 + ep„suora viittaus (ret) saa olla my”s muodossa ([ret])
**                   samoin (1) voi olla muodossa (%1)
**                 + @-komentojen kirjoittaminen itse:
**                       @+ a;(b);5@ miss„
**                       [+*]- @=(0);(1)@@++(0);(2)@
**                 + uusi "p„„ohjelma"  MenuRunArg, jolloin menun nimi
**                   voidaan antaa parametrina
**                 + Formatoitu tulostus PFi, PFd ja PFc
**                      Esim. @PFi ret;%3d@
**                            @PFd val;%5.2lf@
**                            @PFc jono;-20s@
**                 + @RL file;param@  - lukee tiedoston file 1. rivin
**                   parametriin param.
**                      Esim. @RL C:\\SYS\\CURRENT.DIR;dirname@
**                            @RL (file);dirname@
**                   Tarkoitus:  kommunikointi ulkoisten ohjelmien kanssa
**
**
**
** Mit„ vikaa tai mit„ tekem„tt„:
**   - kutsuparametrit pinoon (t„m„ voi olla hankalaa t„ll„ hetkell„?)!
**   - listatut parametrit ja niiden valinta
**   - helpompi dialogien tekeminen
**   - Debuggeri
**   - @-komentojen kirjoittaminen itse: @+a;(b);5@
**     Nyt t„ytyy kutsun olla            @+ a;(b);5@
**     Eli omatekoisissa @-komennoissa pit„„ ennen parametrilistaa
**     olla ainakin yksi tyhj„.
**   - Nimettyjen parametrien talletus .INI-tiedostoon
**   - Menujen poisto homman loputtua
**   - Kello toimimaan, nyt kello antaa aina 23.20
**   - gotojen siivoaminen (silloin kun se todella siivoaa)
**   - aliohjelmien kommentointi ja parametrien dokumentointi
**   - k„ytet„„n viel„ joissakin (monissa) paikoissa GMenu->MenuS, jolloin
**     tietoja EI voi v„litt„„ msg-parametrin avulla kuten oli
**     alunperin ajatus
**   - mahdollisuus k„ytt„„ my”s ilman koko msg-parametria (eli jos
**     se on NULL
**   - numeropohjaiset itemit toteuttamatta (tarvitaan jos
**     p„„ohjelma halutaan switch-toimivaksi)
**   - automaattinen lopetus?
**   - menun k„ytt” ohjelmasta k„sin alkaen jostakin toisesta
**     kohdasta (rinnakkainen k„sittely)
**   - @-komentojen dokumentointi ja niiden lis„„misen dokumentointi
**   - mit„ tapahtuu valinnan j„lkeen?  Menu-kohtaisesti vai
**     item-kohtaisesti? Eli mahdollisuus esim. ruudun tyhjennykseen
**     valinnan j„lkeen, tietyn tekstin tulostamiseen yms?
**     Vaikuttaa tietenkin vain lehtisolmuihin, kun muissa tulostus
**     voidaan hoitaa seuraavan menun alussa.
**   - tarvitaanko (lehti)aliohjelmiin joitakin parametreja ja jos
**     tarvitan niin mitk„ olisivat hyv„t?
**   - komento raamien piirt„miseksi
**
**
**----------------------------------------------------------------------------
** K„ytt”:
**----------------------------------------------------------------------------
**
** Projektiin mukaan:
**   omat ohjelmat
**   mjonot.c
**   menut.c
**   console.c
**
** Kirjoitetaan seuraavanlainen tiedosto (ohjelman_nimi.MNU):
**#---------------------------------------------------------------------------
**[] # t„m„ on p„„menu
**  T„n„„n on @Pvm p.k.vv@
**
**  Valitse seuraavista:
**   0 = Lopetus         |0L|Lopetus
**   1 = Tiedostot       |1T|[Tiedostot]
**   2 = Korjailu        |2K|[Korjailu]
**   3 = Ikkunat         |3I|[Ikkunat]
**   ? = Avustus         |?A|[Avustus]
**   :
**[Tiedostot]
**
**   Kello on @Kello@ Suomen aikaa.
**
**   0 = P„„menu         |0P|
**   1 = Talletus        |1T|Talletus
**   2 = Uusi            |2U|Uusi
**   3 = Sulje           |3S|Sulje
**   ? = Avustus         |?A|ApuaT
**[Korjailu]
**...
**[Ikkunat]
**...
**[Avustus]
**   0 = P„„menu         |0P|
**   1 = Yleist„         |1Y|ApuaY
**   2 = Sis„llys        |2S|ApuaS
**
**[baud]=9600 # T„m„ on nimetty parametri
**[Message0] @Goto 5,21@ # Kaikkien viestien 1. riville t„m„
**[Message1] @Goto 5,-1@ # Viestien seuraaville riveille t„m„
**[Virhe1]   Levyvirhe @%0@ Jatketaanko      |KE|
**[Virhe2]   Muisti loppui, paina jotakin    |*|
**[Virhe*]   Tuntematon virhe! Paina jotakin |*|
**[Odota]    Paina jotakin!                  |*|
**[Ilmoitus]        T„m„ ei odota mit„„n!  # Viestirivin alusta tyhj„t pois!
**
**----------------------------------------------------------------------------
** Huomautuksia tiedoston sis„ll”st„:
**-----------------------------------
**
**  Tiedosto on "case-sensitive" eli isot ja pienet kirjaimet ovat erisuuria
**  Jos menuvalinnaksi halutaan v„lily”nti, on se pistett„v„
**  joukkonsa alkuun (eli vaikkapa | 1Y|).  Jos halutaan pelkk„ v„lily”nti
**  niin pit„„ laittaa "avuksi" jokin merkki, jota "ei ikin„ voida saada"
**  tai valintaa pit„„ antaa heksana |\0x20|
**
**  Rivien tulkinta:
**
**   1 = Tiedostot       |1T|[Tiedostot]
**
** => 1. tulostetaan teksti "    1 = Tiedostot"
**    2. toiminto voidaan valita n„pp„imill„ 1, t ja T
**    3. Kun toiminto on valittu kutsutaan menua [Tiedostot]
**
**[Tiedostot] # menun aihe-nimi (hyppy- tai kutsuosoite)
**   ...
**   2 = Uusi            |2U|Uusi
**
** => 1. tulostetaan teksti "   2 = Uusi"
**    2. toiminto voidaan valita n„pp„imill„ 2, u ja U
**    3. Kun toiminto valitaan, kutsutaan taulukon avulla C-aliohjelmaa
**       "Uusi"
**
**[Virhe1]   Levyvirhe @%0@ Jatketaanko      |KE|
**
** => 1. Viestirivi, joka tulostaa n„ytt””n yo. tekstin korvaten
**       @%0@ parametrilla 0.  Esim. jos kutsutaan
**         ScrMessage1("Virhe",1,"Levy poissa"), niin teksti "Levy poissa"
**       on parametri 0.
**    2. Viesti voidaan kuitata n„pp„imill„ K tai E.  Jos painetaan
**       K palauttaa  ScrMessage arvon 0 ja E:ll„ arvon 1.
**       Viesti voidaan aina kuitata my”s [Ret], [Space] tai [Esc].
**                     paluuarvot:          0       1          2
**
**[Virhe*]   Tuntematon virhe! Paina jotakin |*|
**
** => 1. T„sm„„ kaikkiin tuntemattomiin virheisiin.
**    2. Viesti voidaan kuitata mill„ tahansa n„pp„imell„.
**       Kuitenkin [Ret], [Space] tai  [Esc] painaminen palauttaa
**       arvot       0,     1      tai  2
**
**[Ilmoitus]        T„m„ ei odota mit„„n!
**
** => 1. Viestirivi joka tulostaa ko. tekstin, muttei odota mit„„n
**       n„pp„int„.
**
**-----------------------------------
**  # j„lkeinen osa rivist„ on kommenttia
**-----------------------------------
**  @ komentoja:
**-----------------------------------
**      @Clear@       - pyyhkii ruudun tyhj„ksi
**      @Pvm fmt@     - fmt esim kk.pp.vv => tulostaa nykyisen p„iv„m„„r„n
**      @Goto x,y@    - siirtyy ruudulla kohtaan x,y, jos x tai y -1
**                      niin tarkoittaa +1 vanhaan arvoon.
**      @T sarake@    - "tabuloituu" sarakkeeseen saakka.  T„rke„ jos
**                      k„ytet„„n muita v„rej„ kuin valkoista mustalla
**      @I param@     - Input, lukee nimetyn parametrin param arvon
**                      muoto voi olla my”s param;muutos1;muutos2;muutos3
**                      miss„
**                        JI = jono_isoksi
**                        JP = jono_pieneksi
**                        JA = jono_alku_isoksi
**                        PT = poista_tyhjat
**                        PA = poista_alkutyhjat
**                        PL = poista_lopputyhjat
**                        P2 = poista_2_tyhjat
**                      Esimerkiksi @I nimi;JI@ lukee arvon parametrille
**                      nimi ja muuttaa jonoa koko ajan funktiolla jono_isoksi
**      @++param@     - kasvattaa parametrin arvoa 1:ll„
**      @++param;inc@ - kasvattaa parametrin arvoa m„„r„ll„ inc
**      @=param;arvo@ - sijoitetaan arvo parametrille.  Esimerkiksi kopioidaan
**                      kutsuparametrin %1 arvo parametrille jono:
**                        @=jono=(1)@
**                      Jos paramterissa apu on teksti jono, niin my”s
**                      seuraava ep„suora sijoitus toimisi:
**                        @=(apu)=Kissa@  => jono="Kissa"
**                        @=jono=Kissa(apu)@ => jono="Kissajono"
**      @C v„ri@      - vaihtaa k„ytett„v„n v„rin.  Esimerkkej„ v„reist„:
**                        0x07   - mustalla pohjalla valkoista
**                        0x70   - valkoisella pohjalla mustaa
**                      tavun vasen puoli merkitsee taustan v„ri„ ja oikea
**                      puoli kirjaimen v„ri„.  0 = musta ja 7 valkoinen.
**                      Muut v„rit ovat t„ll„ v„lill„.  Bitti 3 merkitsee
**                      kirkastettua merkki„.
**
**                      Esimerkiksi keltaiset merkit vihre„lle pohjalle,
**                      ei vilkkua saadaan arvolla 0x3E:
**                        0   ÄÄÄÄÄ  ei vilkkuva
**                        0   ÄÄ¿
**                        1     ÃÄÄ  2 = vihre„
**                        0   ÄÄÙ
**                        1   ÄÄÄÄÄ  kirkkaat merkit -> oranssi muuttuu kelt.
**                        1   ÄÄ¿
**                        1     ÃÄÄ  6 = oranssi
**                        0   ÄÄÙ
**
**                        00101110 = $3E
**
**                      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
**                      ³       Oletusv„rit:        ³
**                      ÃÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
**                      ³ 0 ³ 000 ³ musta           ³
**                      ³ 1 ³ 001 ³ sininen         ³
**                      ³ 2 ³ 010 ³ vihre„          ³
**                      ³ 3 ³ 011 ³ vaalean sininen ³
**                      ³ 4 ³ 100 ³ punainen        ³
**                      ³ 5 ³ 101 ³ violetti        ³
**                      ³ 6 ³ 110 ³ oranssi         ³
**                      ³ 7 ³ 111 ³ valkea          ³
**                      ÀÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
**
**      @[menu]       - "menukutsu ilman parametreja
**      @[menu p0;p1] - "menukutsu" (vrt. aliohjelma), suorittaa
**                      tiedostoa kohdasta [menu *] ja palaa sitten
**                      takaisin.  p0, p1 jne. parametreja.
**      @%n@          - tulostaa parametrin n arvon (my”s @(n)@ toimii,
**                      mutta vie enemm„n muistia ja on varattu l„hinn„
**                      ep„suoriin kutsuihin)
**      @Mpop [menu]@ - Palaa menuissa taaksep„in kunnes tulee kohta [menu]
**                      Jos kohtaa ei l”ydy, k„ytet„„n p„„valikkoa
**      @Mpop -n@     - poistaa menupinosta n tasoa (esim. @Mpop -1@ poistaa
**                      yhden tason, eli "palaa" edelliseen menuun
**                      Jos pino menee tyhj„ksi, k„ytet„„n p„„valikkoa
**      @Mpush [menu]@- laittaa menupinoon yhden tason lis„„ (jos [menu]
**                      l”ytyy).
**                      Rinnakkainen siirtyminen voidaan tehd„:
**                      @Mpop -1@@Mpush [ali]@
**      @Exit n@      - Menusysteemin lopetus paluuarvolla n
**                      Jos t„t„ k„ytet„„n, h„vi„v„t nimetyt parametrit,
**                      joten jos niit„ tarvitaan ennen lopetusta, on
**                      varsinaisen C-ohjelman talletettava ne ennen
**                      kuin t„m„ p„„see tapahtumaan.
**      @M[viesti]@   - Kutsutaan viesti„ [viesti]
**      @PFx par;for@ - tulostetaan formatoidusta parametri par.
**                      formaatti on c:n formaatti.
**                       x = tyyppi:
**                            i = integer
**                            d = double
**                            c = char * (merkkijono)
**                      Esim. @PFi ret;%3d@
**      @RL file;par@ - lukee tiedoston file 1. rivin nimettyyn parametriin
**                      par.  Muista ett„ tiedoston nimess„ t„ytyy \ olla \\
**                      Esim. @RL C:\\SYS\\CURRENT.DIR;dirname@
**                            @RL (file);dirname@
**
**
**
**
**  Parametreja tulee joko viesteist„ tai "menu-kutsuista".
**  Jos esim. menun nimi on
**  [Alku *]
**  ja sit„ kutsutaan [Alku pp1;pp2] saa parametri 0 arvon pp1 ja 1 arvon pp2.
**  Jos menukutsua vastaava aihe puuttuu, ei tapahdu mit„„n.
**
**-----------------------------------
** Varatut nimetyt parametrit
**-----------------------------------
**  Seuraavat parametrit t„ytyy esitell„, mutta niille on varattu
**  systeemik„ytt”:
**      ret           - paluuarvo C-funktioista (Menut 94/9 eteenpp„in)
**                      paluuarvo my”s System-kutsusta (0=OK, -1 ei voi
**                      kutsua.)
**      msg           - viestiss„ viimeksi valittun n„pp„imen j„rj. nro
**
**-----------------------------------
** Ep„suora osoitus:
**-----------------------------------
**  @-komentojen sis„lle ja menujen "Goto"-kohtiin voidaan suluilla laittaa
**  ep„suoria osoituksia.  Esimerkiksi:
**    [ypaikka]=15
**    @Goto 10,(ypaikka)@     # => Goto 10,15
**    [Tulosta *]
**    @Goto (0),(1)@@%2@
**    @[Tulosta 10;15;Kissa]@ # => tulostetaan Kissa paikkaan (10,15)
**
**  Jopa useampikertainen ep„suora osoitus on mahdollinen:
**    [Apu]=15
**    [A1]=A
**    [B2]=pu
**    @Goto 10,((A1)(B2))@    # => Goto 10,(Apu) => Goto 10,15
**
**  Ep„suora osoitus toimii my”s menukutsuissa:
**   [apu]=Tiedostot
**   ...
**   1 = Tiedostot       |1T|[(apu)]
**
**  Parametrin 0 talletus apumerkkijonoon:
**   @=jono;(0)@
**
**  Huom!  Esimerkiksi "muuttujan" ypaikka arvo kannattaa tulostaa
**     @[ypaikka]@  tai @PFi ypaikka;%d@
**
**-----------------------------------
** Menut ja viestit:
**-----------------------------------
**  Tiedoston aiheet, joissa otsikon (eli [...] ) j„lkeen on
**  v„lily”nti tai + ovat viestirivej„.
**  Jos ] j„lkeen on:
**          = - merkki, on kyseess„ nimetty parametri
**          - - merkki, pyyhit„„n alkutyhj„t kunkin rivin alusta.
**          : - merkki, ei pyyhit„ alkutyhji„ pois
**
**  Esimerkiksi
**    [menu1]           # t„m„ ei ole viestirivi, tyhji„ ei pois
**    [viesti1]  Viesti # t„m„ on viestirivi, tyhj„t pois.
**    [viesti1]+ Viesti # t„m„ on viestirivi, jonka alusta EI pyyhit„
**                        tyhji„ pois
**    [lyhyt]=Lyhyt     # t„m„ ei ole viestirivi vaan nimetty parametri
**                        parametrin pisin sallittu arvo t„m„n j„lkeen on
**                        5 kirjainta = pituus("Lyhyt")
**                        Muuten nimetty parametri toimii kuten muutkin
**                        "aliohjelmat"
**    [Pitka]=lyhyt|100 # Alkuarvo "lyhyt", mutta mahtuu 100 merkki„
**    [pyyhi]-  @Clear@ # Tavallinen "aliohjelma", josta kuitenkin
**                        rivien alkuv„lily”nnit pois (ei siis viesti)
**    [pyyhi]:  @Clear@ # Tavallinen "aliohjelma", josta ei
**                        rivien alkuv„lily”ntej„ pois (ei siis viesti)
**                        Itse asiassa mik„ tahansa muu kuin +,= ja -
**                        tekev„t ei-viestin, josta ei v„lily”ntej„ pois
**
**  Lis„ksi [Message -alkuiset rivit eiv„t ole viestirivej„.
**  Viestirivit eroavat
**  normaaleista "menu"-riviest„ siin„, ett„ niiden edest„ poistetaan
**  tarpeettomat v„lily”nnit ja ennen viestirivin rivin N (N=0,1,...)
**  tulostamista suoritetaan kohta [MessageN]. Jos ei l”ydy kohtaa
**  [MessageN], niin etsit„„n l„hin pienempi [MessageM], miss„ M<N ja
**  suoritetaan se.  Mik„li ei ole yht„„n [MessageN]-rivi„, mit„„n
**  erikoista ei suoriteta.
**
**-----------------------------------
** Rivinvaihto:
**-----------------------------------
**  Jos tiedoston rivi loppuu "parilliseen" @-merkkiin, ei vaihdeta
**  uudelle riville!  Tarvittaessa @-komennon lopettava @ voidaan
**  j„tt„„ pois JoS halutaan rivinvaihto (tai sitten t„ytyy laittaa
**  tyhj„ rivi).
**  My”s aiheen viimeisen rivin j„lkeen ei tule rivinvaihtoa!
**
**-----------------------------------
** Valmiit aihenimet:
**-----------------------------------
**  Tiedostossa on muutamia ennalta kiinnitettyj„ aiheiden nimi„:
**
**     []            - p„„menu
**     [MenuBegin]   - ennen menun tulostamista
**     [MenuEnd]     - menun tulostamisen j„lkeen
**     [MenuExit]    - kun menusta poistutaan
**                     eli ennen lehtisolmun aliohjelman kutsua
**     [Message0]
**     [Message1]
**       ...         - ennen viestirivi„ n teht„v„ "aliohjelma"
**     [MessageEnd]  - viestin lopuksi teht„v„ "aliohjelma"
**     [MessageExit] - viestist„ poistumisen yhteydess„ teht„v„t hommat
**                     tehd„„n kun viestirivi tai "menurivi" ja
**                     kutsutaan ScrMessage
**                     Ei kuitenkaan tehd„ jos rivi alkaa [nimi]x...
**                     miss„ x muuta kuin v„lily”nti.
**
**
**
**----------------------------------------------------------------------------
** Ohjelmaan kirjoitetaan:
**-------------------------
** #include "menut.h"
**
** int talletus(void)
** {
**   ...
**   if ( ScrMessage1("Virhe",1,"luukku auki.") == 0 ) ...
**   ...
** }
** ...
**
** int Lopetus(void)
** {
**   return PostQuitMessage(0);
** }
**
** tMenuTable menu_table[] = {
**    {"Talletus",0,talletus},
**    {"Uusi"    ,0,uusi    },
**    {"Sulje"   ,0,sulje   },
**    {"ApuaT"   ,0,ApuaT   },
**    ...
**    {"ApuaS"   ,0,ApuaS   },
**    {"Lopetus" ,0,Lopetus },
**    {NULL      ,0,NULL    }
** }
**
** int main(int argv,char *argc[])
** {
**   return MenuRunArg(argc,argv,menu_table);
** }
**
**----------------------------------------------------------------------------
** Jos halutaan parempi kontrolli, kirjoitetaan "viestisilmukka"
** itse:
**
** ...
** int main(int argv,char *argc[])
** {
**   MSG msg;
**   if ( InitMenuSystem(argv[0],&msg,menu_table) ) return 1;
**   alustukset();
**   while ( GetMessage(&msg) ) {
**     DispatchMessage(&msg);
**   }
**   vapautukset();
**   return msg.wParam;
** }
**
**
**
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <time.h>
#include "mjonot.h"
#include "menut.h"
#include "console.h"

/****************************************************************************/
/* Tarvittavia tietorakenteita                                              */
/****************************************************************************/

/*

Esimerkiksi tiedostosta:

#----------------------------------------------------
[] # t„m„ on p„„menu
  T„n„„n on @Pvm p.k.vv@
   0 = Lopetus         |0L|Lopetus
   1 = Avustus         |1A|[Avustus]
[Virhe1]   Levyvirhe @%0@ Jatketaanko      |KE|
[Avustus]
   0 = P„„menu         |0P|
   1 = Yleist„         |1Y|ApuaY
   2 = Sis„llys        |2S|ApuaS
#-----------------------------------------------------

tulisi tietorakenne joka n„ytt„isi seuraavalta kun
p„„menusta on valittu Avustus:


tMenuSystem:              ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
             ÚÄÄÄÄÄÄ¿     ³                                   ³
  menu       ³   oÄÄÅÄÄÄÄÄÅÄÄÄ¿   ÚÄÄÄ¿                       ³
  CurrentMenu³   oÄÄÅÄÄÄÄÄÙ   ÀÄ>0³ oÄÅÄÄÄÄ¿                  ³    tMenu:
  nmenu      ³   3  ³            1³ oÄÅÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿   ÀÄÄÄÄ>ÚÄÄÄÄ¿
  MenuTable  ³   o  ³            2³ oÄÅÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄ>³[Avu³
  sTop       ³   1  ³            3³   ³    ³              ³         ³ 3  ³
             ³ ÚÄÄ¿ ³            4³   ³    ³              ³         ³ 3  ³
  Stack  n   ³ ³??³ ³          ...³   ³    ³              ³         ³ 0  ³
         ..  ³ ³..³ ³         MAX_³   ³    ³              ³         ³ÚÄÄ¿³
         1   ³ ³??³ ³             ³   ³    ³              ³         ³ÃÄÄ´³
         0   ³ ³oÄÅÄÅÄÄÄÄÄÄÄ¿     ÀÄÄÄÙ    ³              ³         ÀÁÄÄÁÙ
             ³ ÀÄÄÙ ³       ÀÄÄÄÄÄÄÄÄÄÄ¿   v              v
  Main       ³   oÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄ>ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿  ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
             ÀÄÄÄÄÄÄÙ     MenuName  ³ []           ³  ³ [Virhe1]     ³
                   tMenu: nItems    ³ 2            ³  ³ 1            ³
                          rows      ³ 3            ³  ³ 1            ³
                          message   ³ 0            ³  ³ 1            ³
         tMenuItem:                 ³ÚÄÄÄÄÄÄÄÄÄÄÄÄ¿³  ³ÚÄÄÄÄÄÄÄÄÄÄÄÄ¿³
                 Items[0].ItemText  ³³T„n„„n on...³³  ³³Levyvirhe @%³³
                          Choose    ³³            ³³  ³³KE          ³³
                          sitem     ³³            ³³  ³³            ³³
                          item      ³³            ³³  ³³            ³³
                                    ³ÃÄÄÄÄÄÄÄÄÄÄÄÄ´³  ³ÀÄÄÄÄÄÄÄÄÄÄÄÄÙ³
                 Items[1].ItemText  ³³0 = Lopetus ³³  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
                          Choose    ³³0L          ³³
                          sitem     ³³Lopetus     ³³
                          item      ³³            ³³
                                    ³ÃÄÄÄÄÄÄÄÄÄÄÄÄ´³
                 Items[2].ItemText  ³³1 = Avustus ³³
                          Choose    ³³1A          ³³
                          sitem     ³³Avustus     ³³
                          item      ³³            ³³
                                    ³ÀÄÄÄÄÄÄÄÄÄÄÄÄÙ³
                                    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ


*/

/* Menun yksi rivi valintamahdollisuuksineen */
typedef struct {
  char *ItemText;
  char *Choose;
  char *sitem;
  int  item;
} tMenuItem;


#define MAX_ITEMS     25
#define MAX_MENUS     300
#define MAX_MENUSTACK 10

/*--------------------------------------------------------------------------*/
/* Erilaisten "menujen ominaisuudet:                                        */
/*                                                                          */
#define MENU_MENU     0
#define MENU_MSG_NCB  1
#define MENU_MSG      2
#define MENU_PARAM    3
#define MENU_NOMSG    4
#define MENU_CLEARBEG 5

static int SET_Message[]    = {MENU_MSG_NCB,MENU_MSG,-1};
static int SET_ClearBeg[]   = {MENU_MSG,MENU_CLEARBEG,-1};
static int SET_NoClearEnd[] = {MENU_PARAM,-1};
static int SET_Param[]      = {MENU_PARAM,-1};
static int SET_MsgEnd[]     = {MENU_MENU,MENU_MSG_NCB,MENU_MSG,-1};

int IN(const int *set,int n)
{
  int i;
  for (i=0; set[i] != -1; i++)
    if ( set[i] == n ) return 1;
  return 0;
}

int EQ(int a,int b)
{
  return (char)a == (char)b;
}


/*--------------------------------------------------------------------------*/
/* Muistissaoloaikaiset kirjainmuutokset:                                   */
/*                                                                          */
#define COMMENT    248  /* # */
#define MENUSEP    249  /* | */
#define LEFT_B     250  /* ( */
#define RIGHT_B    251  /* ) */
#define LEFT_SB    252  /* [ */
#define RIGHT_SB   253  /* ] */
#define MIAU       254  /* @ */
#define SEP        255  /* ; */

static char MessageStr[] = {LEFT_SB,'M','e','s','s','a','g','e',0};
static char MenuStr[]    = {LEFT_SB,'*',RIGHT_SB,0};
static char MsgStr[]     = {'M',LEFT_SB,'*',RIGHT_SB,0};

typedef struct {
  char from;
  char to;
} tMenuTrans;


static tMenuTrans MenuTrans[] = {
  { '#'  , COMMENT  },
  { '|'  , MENUSEP  },
  { '('  , LEFT_B   },
  { ')'  , RIGHT_B  },
  { '['  , LEFT_SB  },
  { ']'  , RIGHT_SB },
  { '@'  , MIAU     },
  { ';'  , SEP      },
  { 0    , 0        }
};


/*--------------------------------------------------------------------------*/
int Mprintscr(const char *s)
/* Palautetaan tulostusta varten takaisin (,[ yms. merkit. */
{
  char *p,*st = tee_jono(s);
  int i;
  if ( st == NULL ) { printscr(s); return 1; }

  for (p=st; *p; p++)
    for (i=0; MenuTrans[i].from; i++)
      if ( *p == MenuTrans[i].to ) { *p = MenuTrans[i].from; break; }
  printscr(st);
  free(st);
  return 0;
}


/*--------------------------------------------------------------------------*/
/* Yhden menun vaatima tietorakenne                                         */
typedef struct {
  char *MenuName;
  int  nItems;                          /* Valintojen lkm                   */
  int  rows;                            /* Rivien lkm                       */
  int  message;                         /* Onko viesti vai menu             */
  tMenuItem Items[MAX_ITEMS];           /* jokaisen menun rivit             */
} tMenu;

/*--------------------------------------------------------------------------*/
/* Tietorakenne menusysteemin hallintaa varten                              */
typedef struct {
  tMenu      **menu;                    /* osoitin yksit. menujen taulukkoon*/
  tMenu      *CurrentMenu;              /* osoitin t„m„nhetkiseen menuun    */
  int        nmenu;                     /* tauluun ladattujen menujen lkm   */
  tMenuTable *MenuTable;                /* osoitin menu-funktio taulukkoon  */
  int        sTop;                      /* menupinon pinta (0=tyhj„ pino)   */
  tMenu      *Stack[MAX_MENUSTACK];     /* menupino                         */
  tMenu      *Main;                     /* osoitin p„„menuun                */
} tMenuSystem;

/*--------------------------------------------------------------------------*/
/* Koko menusysteemin tallettava tietorakenne:                              */
typedef struct {
  int         QuitMsg;
  int         QuitMenu;
  tMenuSystem MenuSystem;
} tMenuStatic;

typedef const int (*tMenuFun)(tMenu *);

/****************************************************************************/
/* "Globaalit muuttujet:                                                    */
/*--------------------------------------------------------------------------*/
static tMenuStatic GMenu = {-1,0};
static char EmptySt[1] = "";


/****************************************************************************/
/* Rekursioiden takia esitell„„n:                                           */
/*--------------------------------------------------------------------------*/
static tMenuItem *FindParam(const char *name); /* Rekursio DoSubstitute     */
static int MenuPrintf(char *s);
static int PrintMenu(tMenu *menu);             /* Rekursio MenuPrintf       */
static const char *TulostaTeksti(const char *s);
/****************************************************************************/


/****************************************************************************/
/* Menupinon k„sittely:                                                     */
/****************************************************************************/

/****************************************************************************/
static int PushMenu(tMenuSystem *MenuS, tMenu *menu)
{
  if ( menu == NULL ) return 0;
  if ( MenuS->sTop >= MAX_MENUSTACK ) return 1;
  MenuS->Stack[MenuS->sTop++] = menu;
  return 0;
}

/****************************************************************************/
static tMenu *PopMenu(tMenuSystem *MenuS)
/*
** Ottaa pinosta p„„llimm„isen.
----------------------------------------------------------------------------*/
{
  if ( MenuS->sTop <= 0 ) return NULL;
  return MenuS->Stack[--MenuS->sTop];
}

/****************************************************************************/
int RunFunc(tMenuTable *MenuTable,const char *sitem)
/* Etsii valitun funktion ja ajaa sen.                                      */
{
  int i;

  for (i=0;MenuTable[i].sitem;i++) {
    if ( strcmp(MenuTable[i].sitem, sitem) == 0 ) {
      int ret = MenuTable[i].func();
      SetNameParamInt("ret",ret);
    }
  }
  return 0;
}

/****************************************************************************/
int SetNameParam(const char *name,const char *s)
{
  tMenuItem *param = FindParam(name);
  if ( !param ) return 1;

  param->ItemText[0] = 0;
  if ( s ) kopioi_jono(param->ItemText,param->item,s);

  return 0;
}

/****************************************************************************/
const char *GetNameParam(const char *name)
{
  tMenuItem *param = FindParam(name);
  if ( !param ) return NULL;

  return param->ItemText;

}

/****************************************************************************/
char *ctoa(char c)
{
  static char s[2];
  s[0]=c; s[1]=0;
  return s;
}

/****************************************************************************/
static char *make_name(char *s,int s_max,const char *topic,const char *ex)
/*
** => s = "["+topic+ex+"]"
** Jos sulut on valmiina, ei niit„ lis„t„.
** Jos kuitenkin topic = "[oma]", ex="2"  => s="[oma2]"
----------------------------------------------------------------------------*/
{
  int i;
  if ( !EQ(topic[0],LEFT_SB) ) kopioi_jono(s,s_max,ctoa(LEFT_SB));
  liita_jono(s,s_max,topic);
  i = strlen(s);
  if ( i && EQ(s[i-1],RIGHT_SB) ) s[i-1] = 0;
  if ( ex ) liita_jono(s,s_max,ex);
  liita_jono(s,s_max,ctoa(RIGHT_SB));
  return s;
}


/****************************************************************************/
static const char *GetReplaceParam(const char *beg, const char *end)
/*
** Etsit„„n v„lilt„ beg end korvaava parametri.
** Esimerkiksi:
**   paikka = "15"     "@Goto 10,(paikka)@
**                             beg^    ^end
**                  => "15"
** Jos parametri on numero, k„ytet„„n %1,%2 jne arvo, muuten nimettyj„
** parametreja.
** Samaistukset (%1) => (1),  ([paikka]) => (paikka)
----------------------------------------------------------------------------*/
{
  tMenuItem *item;
  char name[80];
  int pit = end-beg+1;
  if ( pit <= 0 ) return "";
  if ( isdigit(*beg) ) return GetMessageParam(atoi(beg));
  if ( *beg == '%' )   return GetMessageParam(atoi(beg+1));
  if ( pit >= sizeof(name)-1 ) return "";
  strncpy(name,beg,pit); name[pit] = 0;
  item = FindParam(name);
  if ( item == NULL ) return "";
  return item->ItemText;
}

/****************************************************************************/
static const char *FindPair(const char *s,char e)
/* Etsii kohdasta s eteenp„in sulun lopettavaa sulkua
** Esim s="( (1+2) ) - ( 5 + 4 )" ja e =')'
**  palautus       ^
----------------------------------------------------------------------------*/
{
  const char *p;
  char b = *s;
  int count = 0;
  for ( p=s; *p; p++) {
    if ( *p == b ) count++;
    if ( *p == e ) { count--; if ( count == 0 ) return p; }
  }
  return p;
}

/****************************************************************************/
static char *DoSubstitute(const char *s)
/*
** Sijoittaa jonossa olevien (n) tilalle vastaavat parametrit
** Palauttaa ihka uuden merkkijonon joka pit„„ tuhota free-kutsulla k„yt”n
** j„lkeen. Korvauksia tehd„„n, kunnes yht„„n ()-lausetta ei en„„
** l”ydy.  Rekursion ansiosta my”s sis„kk„iset sulut toimivat
** Esim  %1 = "15" ja s ="Goto 10,(1)" => "Goto 10,15"
**       [APU]="15"
**       [A1]="A"
**       [B2]="PU"
**       s = "Goto 10,((A1)(B2))" => (APU) => Goto 10,15
----------------------------------------------------------------------------*/
{
#define MAXSPE 1000
  const char *p,*pe;
  const char *param;
  int i=0,vaihtoja;
  char *from=tee_jono(s),*spe;
  if ( !from ) return NULL;

  do {
    vaihtoja = 0;
    spe = calloc(MAXSPE,1);
    if ( !spe ) return from;

    for (i=0,p=from; *p && i < MAXSPE-1; p++) {
      if ( !EQ(*p,LEFT_B) ) { spe[i++] = *p; continue; }
      pe = FindPair(p,RIGHT_B);
      if ( *pe == 0 ) break;
      vaihtoja++;
      param = GetReplaceParam(p+1,pe-1);
      p = pe;
      liita_jono(spe,MAXSPE,param);
      i = strlen(spe);
    }
    spe[i] = 0;
    free(from);

    from = realloc(spe,strlen(spe)+1);
    if ( from == NULL ) from = spe;
  } while ( vaihtoja );
  return from;
}


/****************************************************************************/
/* Parametrien k„sittely:                                                   */
/****************************************************************************/


/****************************************************************************/
#define MAX_PARAMS 10
static char Params[MAX_PARAMS][50];
/****************************************************************************/
int SetMessageParam(int n,const char *s)
{
  if ( s == NULL ) return 0;
  if ( n < 0 || MAX_PARAMS <= n ) return 1;
  kopioi_jono(N_S(Params[n]),s);
  return 0;
}


/****************************************************************************/
const char *GetMessageParam(int n)
{
  if ( n < 0 || MAX_PARAMS <= n ) return EmptySt;
  return Params[n];
}


/****************************************************************************/
static int SetParams(const char *s)
/*
** Funktiolla asetetaan parametrit merkkijonon s mukaan.
** Jonon 1. sana unohdetaan ja kukin seuraava ;
** erotettu osa laitetaan parametriksi 0,1,2,3
**
----------------------------------------------------------------------------*/
{
  char st[80],*p;
  char sep[3]={SEP,RIGHT_SB,0};
  int i=0,j;
  kopioi_jono(N_S(st),s);
  p = palanen(st," ",&j);
  while ( j > 0 ) {
    p = palanen(NULL,sep,&j);
    SetMessageParam(i,p);
    i++;
  }
  return i;
}

/****************************************************************************/
static tMenu *FindMenu(tMenu **menus,const char *s)
/*
** Etsii merkkijonoa s vastaavan menun tai viestin.
----------------------------------------------------------------------------*/
{
  int i;
  tMenu *ret = NULL;
  char *spec;
  if ( s == NULL ) return NULL;

  if ( ( spec = DoSubstitute(s) ) == NULL ) return NULL;

  for (i=0; menus[i]; i++) {
    if ( wildmat(spec,menus[i]->MenuName) == 0 ) {
      SetParams(spec);
      ret = menus[i];
      break;
    }
  }
  free(spec);
  return ret;
}

/****************************************************************************/
static const char *TulostaTekstiFun(const char *s,tMenuFun PrintMenu)
/*
** Tulostaa merkkijonoa s vastaavan menun tai viestin.
** Voidaan kutsua joko [Oma] tai LEFT_SB Oma RIGHT_SB
----------------------------------------------------------------------------*/
{
  char st[80];
  tMenu *menu;
  int l = strlen(s);
  kopioi_jono(N_S(st),s);
  if ( st[0] == '[' ) st[0] = LEFT_SB;
  if ( l>0 && st[l-1] == ']' ) st[l-1] = RIGHT_SB;
  menu = FindMenu(GMenu.MenuSystem.menu,st);
  if ( !menu ) return NULL;
  PrintMenu(menu);
  return EmptySt;
}

/****************************************************************************/
/* N„pp„inten odotukset GetMessage ja ScrMessage                            */
/****************************************************************************/


/****************************************************************************/
static int WaitMenuKey(tMenu *menu,const char *ex,int *j)
/*
** Funktiolla odotetaan kunnes on painettu jokin n„pp„in joka l”ytyy
** menusta tai taulukosta ex.  Funktiolla palautetaan sen menun
** indeksi, josta n„pp„in l”ytyy.  Muuttujassa ja palautetaan
** monesko valittu n„pp„in on kyseisess„ menussa tai taulukossa ex.
** Jos menun sallituissa merkeiss„ on *, kelpaa vastinkohtaan
** mik„ tahansa n„pp„in.
** Tutkimisj„rjestys: menu, ex, *
----------------------------------------------------------------------------*/
{
  char c;
  int i,dummy;
  if (j == NULL ) j = &dummy;

  while ( 1 ) {
    c = isoksi(ReadChar());

    for (i=0; i<menu->rows; i++ )
      if ( ( *j = paikka(menu->Items[i].Choose,c) ) >= 0 ) return i;

    if ( ( *j = paikka(ex,c) ) >= 0 ) return -1;

    for (i=0; i<menu->rows; i++ )
      if ( ( *j = paikka(menu->Items[i].Choose,'*') ) >= 0 ) return i;
  }
}

/****************************************************************************/
int DoScrMessage(tMenu *message)
{
  int i;
  PrintMenu(message);
  if ( IN(SET_MsgEnd,message->message) ) TulostaTeksti("[MessageEnd]");

  SetNameParamInt("msg",0);
  if ( message->nItems == 0 ) return 0;

  WaitMenuKey(message,"\r \x1b",&i);
  if ( IN(SET_MsgEnd,message->message) ) TulostaTeksti("[MessageExit]");

  SetNameParamInt("msg",i);
  return i;
}

/****************************************************************************/
static tMenuItem *FindParam(const char *name)
/*
** Etsit„„m parametri name.  Jos nimest„ puuttuu [] ymp„rilt„,
** laitetaan ne.
----------------------------------------------------------------------------*/
{
  char s[80];
  tMenu *menu;
  menu = FindMenu(GMenu.MenuSystem.menu,make_name(N_S(s),name,NULL));

  if ( !menu ) return NULL;

  if ( menu->rows == 0 ) return NULL;
  if ( !IN(SET_Param,menu->message) ) return NULL;
  if ( menu->Items[0].ItemText == EmptySt ) return NULL;
  return &menu->Items[0];
}

/****************************************************************************/
/* P„iv„yksen k„sittely:                                                    */
/****************************************************************************/

/****************************************************************************/
static int SubstDay(const char **p,int d, char *s)
/*
** Funktio sijoittaa merkkijonoon s kokonaisluvun d formaatin *p mukaisesti.
** Palautetaan s:n pituus.
** Osoitinta p siirret„„n samojen fmt merkkien ohi
**
** Jos esim. *p ="pp..."    ja d=2  => s="02"   ja *p = "p..."
**           *p ="p..."     ja d=2  => s="2"    ja *p = "p..."
**           *p ="p..."     ja d=20 => s="20"   ja *p = "p..."
**           *p ="vvvv..."  ja d=94 => s="1994" ja *p = "v..."
**           *p ="vvvv..."  ja d=04 => s="2004" ja *p = "v..."
** Tosin funktio ei ota kantaa siihen mik„ merkki on paikassa **p
** vaan toimii aivan samalla tavalla oli siin„ k,v,p tai jopa joku muu.
** Oletetaan ett„ kutsupuolella on tarkistettu **p:n olevan j„rkev„.
**
----------------------------------------------------------------------------*/
{
  int len = 1;
  while ( *(*p+1) == **p ) { (*p)++; len++; }
  if ( len == 4 ) if ( d < 50 ) d+=2000; else d+=1900;
  if ( len == 3 || len > 4 ) len = 2;
  sprintf(s,"%0*d",len,d);
  return  strlen(s);
}


/****************************************************************************/
static const char *FmtPvm(const char *fmt)
/*
** Funktiolla palautetaan nykyinen p„iv„m„„r„ oletuksena muodossa
**   pp.kk.19vv
** Jos halutaan muu muoto, pit„„ kutsua s merkkijonon arvolla
** jossa on p k ja v halutulla tavalla muotoiltuna
** Esim "p.k"        seuraa  2.1      tai 10.12
**      "pp.kk.vv"   seuraa  02.01.94 tai 10.12.94
**      "vvvvkkpp"   seuraa  19940201 tai 19941210
**
----------------------------------------------------------------------------*/
{
  time_t t     = time(NULL);
  struct tm *d = localtime(&t);
  static char st[50];
  int i;
  const char *p;

  if ( fmt == NULL || *fmt == 0 ) {
    sprintf(st,"%02d.%02d.19%02d",d->tm_mday,d->tm_mon+1,d->tm_year);
    return st;
  }


  for (i=0; i<sizeof(st); i++) st[i]=0;

  p = fmt; i=0;
  while ( *p && i < sizeof(st)-6 ) {
    switch ( *p ) {
      case 'p': i += SubstDay(&p,d->tm_mday,st+i);  break;
      case 'k': i += SubstDay(&p,d->tm_mon+1,st+i); break;
      case 'v': i += SubstDay(&p,d->tm_year,st+i);  break;
      default : st[i++] = *p;
    }

    p++;
  }
  return st;
}

/****************************************************************************/
/* @-kielen funktiot:                                                       */
/****************************************************************************/

/****************************************************************************/
static const char *Pvm(const char *s)
/*
** Funktiolla palautetaan nykyinen p„iv„m„„r„ oletuksena muodossa
**   pp.kk.19vv
** Jos halutaan muu muoto, pit„„ kutsua s merkkijonon arvolla
**  "Pvm fmt", miss„ fmt on FmtPvm-aliohjelman mukainen formaatti
----------------------------------------------------------------------------*/
{
  if ( strlen(s) <= 4 ) return FmtPvm(NULL);
  return FmtPvm(s+4);
}


/****************************************************************************/
static const char *Kello(const char *s)
{
  if ( s ) s++;
  return "23:20";
}


/****************************************************************************/
static const char *DoColor(const char *s)
{
  int c;
  if ( strlen(s) < 2 ) return 0;
  sscanf(s+2,"%i",&c);
  ScrColor(c);
  return EmptySt;
}


/****************************************************************************/
static const char *DoTab(const char *s)
{
  int mx,x,t,i;
  static char st[80];
  if ( strlen(s) < 2 ) return 0;
  sscanf(s+2,"%i",&t);
  x = GetScreenx();
  mx = GetMaxx(); if ( t > mx ) t = mx;
  for (i=0;x<t && i<sizeof(st)-2;x++) st[i++] = ' ';
  st[i]=0;
  return st;
}

/****************************************************************************/
static const char *DoRepeat(const char *s)
{
  int y,x,n,i,mx=GetMaxx();
  char st[200],m=' ';
  const char *p;
  if ( strlen(s) < 2 ) return 0;
  sscanf(s+2,"%i",&n);
  p = strchr(s+2,SEP); if ( p ) m = p[1];
  x = GetScreenx();
  y = GetScreeny();
  for (i=0;i<n && i<sizeof(st)-2 && i+x < mx;i++) st[i] = m;
  st[i]=0;
  Mprintscr(st);
  ScrMove(x,y);
  return EmptySt;
}


/****************************************************************************/
typedef struct {
  char *name;
  pEditFunc f;
} tStringChange;

static tStringChange StringChange[] = {
  {"JI",jono_isoksi        },
  {"JP",jono_pieneksi      },
  {"JA",jono_alku_isoksi   },
  {"PT",poista_tyhjat      },
  {"PA",poista_alkutyhjat  },
  {"PL",poista_lopputyhjat },
  {"P2",poista_2_tyhjat    },
  {NULL,NULL               }
};

static char *pStringChange;

/****************************************************************************/
static char *DoStringChange(char *s)
{
  int i;
  if ( pStringChange == NULL ) return s;
  for (i=0; StringChange[i].name; i++)
    if (strstr(pStringChange,StringChange[i].name) != NULL )
      StringChange[i].f(s);
  return s;
}

/****************************************************************************/
const char *EditNameString(const char *s,pEditFunc f)
{
  tMenuItem *param;
  param = FindParam(s);
  if ( param == NULL ) return NULL;
  EditString(param->ItemText,param->item,f);
  return param->ItemText;
}

/****************************************************************************/
static const char *DoEdit(const char *s)
{
  char st[80];
  if ( strlen(s) < 2 ) return 0;
  kopioi_jono(N_S(st),s+2);
  pStringChange = strchr(st,SEP);
  if ( pStringChange ) *pStringChange++=0;

  EditNameString(st,DoStringChange);
  return 0;
}

/****************************************************************************/
static const char *DoInc(const char *s)
{
  double d,dx = 1;
  char *p;
  p = strchr(s+1,SEP);
  if ( p ) {
    if ( *p ) dx = atof(p+1);
    *p = 0;
  }
  d = GetNameParamDouble(s+2);
  SetNameParamDouble(s+2,d+dx);
  if ( p ) *p = SEP;
  return 0;
}

/****************************************************************************/
static const char *DoSet(const char *s)
{
  char *p;
  p = strchr(s+1,SEP);
  if ( !p ) return 0;
  *p = 0;
  SetNameParam(s+1,p+1);
  *p = SEP;
  return 0;
}

/****************************************************************************/
static const char *Parameter(const char *s)
{
  return GetMessageParam(atoi(s+1));
}


/****************************************************************************/
static const char *DoGoto(const char *s)
{
  int x=0,y=-1;
  sscanf(s,"Goto %d,%d",&x,&y);
  if ( x < 0 ) x = GetScreenx();
  if ( y < 0 ) y = GetScreeny();
  ScrMove(x,y);
  return EmptySt;
}

/****************************************************************************/
static const char *DoRun(const char *s)
{
  RunFunc(GMenu.MenuSystem.MenuTable,s+4);
  return 0;
}

/****************************************************************************/
static const char *DoPushMenu(const char *s)
{
  tMenu *menu;
  const char *menu_to_push = s+6;
  menu = FindMenu(GMenu.MenuSystem.menu,menu_to_push);
  if ( menu == NULL ) return 0;

  PushMenu(&GMenu.MenuSystem,GMenu.MenuSystem.CurrentMenu);

  GMenu.MenuSystem.CurrentMenu = menu;
  return 0;
}

/****************************************************************************/
static const char *DoPopMenuFind(const char *s)
{
  tMenu *menu;
  const char *menu_to_find = s+5;

  if ( menu_to_find[0] == '-' ) {
    int i,n = atoi(menu_to_find+1);
    for (i=0; i<n; i++)
      menu = PopMenu(&GMenu.MenuSystem);
    GMenu.MenuSystem.CurrentMenu = menu;
    return 0;
  }

  while (1) {
    menu = PopMenu(&GMenu.MenuSystem);
    if ( menu == NULL || strcmp(menu->MenuName,menu_to_find ) == 0) {
      GMenu.MenuSystem.CurrentMenu = menu;
      return 0;
    }
  }
}

/****************************************************************************/
static const char *DoExit(const char *s)
{
  int i = 0;
  sscanf(s,"Exit%d",&i);
  PostQuitMessage(i);
  return 0;
}

/****************************************************************************/
static const char *DoSystem(const char *s)
{
  int ret,i = strlen("System ");
  ret = system(s+i);
  SetNameParamInt("ret",ret);
  return 0;
}

/****************************************************************************/
static const char *DoMessage(const char *s)
{
  TulostaTekstiFun(s+1,DoScrMessage);
  return 0;
}


/****************************************************************************/
static const char *DoPrintF(const char *s)
/*
**  PFi nimi;format
**  01234
----------------------------------------------------------------------------*/
{
  double d;
  int    i;
  char *st,*format;
  const char *name = s+4;
  char   type = s[2];

  if ( s+3 == 0 ) return 0;
  format = strchr(name+1,SEP);  if ( !format ) return 0;
  st = malloc(2000);  if ( !st ) return 0; st[0] = 0;

  *format = 0;


  switch ( type ){
    case 'i':
      i = GetNameParamInt(name);
      sprintf(st,format+1,i);
      break;
    case 'd':
      d = GetNameParamDouble(name);
      sprintf(st,format+1,d);
      break;
    case 'c':
      sprintf(st,format+1,GetNameParam(name));
      break;
  }


  MenuPrintf(st);
  free(st);
  *format = SEP;
  return 0;
}

/****************************************************************************/
static const char *DoReadLn(const char *s)
/*
**  RL file;name
**  01234
----------------------------------------------------------------------------*/
{
  FILE *f;
  char st[80],*name;
  const char *file = s+3;

  name = strchr(file,SEP);  if ( !name ) return 0;

  *name = 0;
  f = fopen(file,"rt");

  if ( f ) {
    f_lue_jono(f,N_S(st));
    SetNameParam(name+1,st);
    fclose(f);
  }


  *name = SEP;
  return 0;
}

/****************************************************************************/
/* @-kielen hyppytaulukko:                                                  */
/****************************************************************************/

typedef const char * (*pSpecFunc)(const char *);

typedef struct {
  char      *command;
  pSpecFunc func;
} tSpecialTable;

tSpecialTable SpecialTable[] = {
  {"Clear"   ,ClearConsole      }, /* Pyyhkii ruudun                        */
  {"Run *"   ,DoRun             }, /* Ajetaan aliohjelma                    */
  {MenuStr   ,TulostaTeksti     }, /* Suorittaa menutulostuksen [*]         */
  {"Kello"   ,Kello             }, /* Kellonaika                            */
  {"Pvm*"    ,Pvm               }, /* Pvm pp.kk.vv                          */
  {"%*"      ,Parameter         }, /* Parametri nro *                       */
  {"Goto *"  ,DoGoto            }, /* Goto x,y                              */
  {"C *"     ,DoColor           }, /* C vari, esim 0x07 normaali 0x70 k„„nt.*/
  {"T *"     ,DoTab             }, /* T tab, tabulointikohta                */
  {"R *"     ,DoRepeat          }, /* R n;m toistaan n kertaa merkki„ m     */
  {"I *"     ,DoEdit            }, /* I param, editoidaan nimetty„ parametri*/
  {"++*"     ,DoInc             }, /* ++param;lisa kasvattaa param arvoa    */
  {"=*"      ,DoSet             }, /* =param;arvo, sijoitetan parm. arvo    */
  {"Mpop *"  ,DoPopMenuFind     }, /* Mpop ottaa menupinosta kunnes nimi    */
  {"Mpush *" ,DoPushMenu        }, /* Mpush laittaa menupinoon menun        */
  {"Exit*"   ,DoExit            }, /* Lopettaa menusysteemin                */
  {"System *",DoSystem          }, /* Suorittaa systeemikutsun              */
  {MsgStr    ,DoMessage         }, /* M[*], suoritetaan viesti *            */
  {"PF*"     ,DoPrintF          }, /* PFx muuttuja;format  x=i,d,c          */
  {"RL *"    ,DoReadLn          }, /* RL file;muuttuja                      */
  {NULL      ,NULL              }  /*                                       */
};

/****************************************************************************/
/* Yleisi„ tulostamisen apufunktioita:                                      */
/****************************************************************************/


/****************************************************************************/
int DoSpecialTable(const char *spe)
/*
** Etsit„„n ensin l”ytyyk” @-taulukosta.  Jos ei l”ydy, niin laitetaan
** [] ymp„rille ja etsit„„n tavallisista komennoista.
----------------------------------------------------------------------------*/
{
  int i;
  const char *ps;
  char s[80];
  tMenu *menu;

  for (i=0; SpecialTable[i].command; i++)
    if ( wildmat(spe,SpecialTable[i].command) == 0 ) {
      ps = SpecialTable[i].func(spe);
      if ( ps && ps[0] ) Mprintscr(ps);
      return 0;
    }

  menu = FindMenu(GMenu.MenuSystem.menu,make_name(N_S(s),spe,NULL));
  if ( !menu ) return 1;
  PrintMenu(menu);
  return 0;
}


/****************************************************************************/
static int MenuPrintf(char *s)
/*
** Tulostetaan merkkijono siten, ett„ suoritetaan siin„ mahdollisesti
** olevat @-komennot. Eli vaihtaa @-merkein erotetut tekstin osat
** SpecialTablen sis„lt„mien funktioiden antamiin tuloksiin. SpecialTable
** on vakiona koodissa.
** Palutetaan 0 jos rivi ei p„„ty @ ja 1 jos p„„ttyy.
**
** Saattaa johtaa rekursioon! (Joka tietysti hallittuna ei haittaa.)
** TulostaTeksti->PrintMenu (->TulostaTeksti) -> MenuPrintf
**  -> SpecialTable:n kautta mahdollisesti TulostaTeksti
----------------------------------------------------------------------------*/
{
  char *pa,*pl=NULL;
  char *spe;

  do {

    if ( ( pa = strchr(s,MIAU) ) == NULL ) {
      Mprintscr(s);
      return ( pl && s[0]==0 );
    }

    *pa = 0; pa++;
    Mprintscr(s);

    if ( ( pl = strchr(pa,MIAU) ) != NULL ) {
      *pl = 0; pl++;
    }

    spe = DoSubstitute(pa);
    if ( spe == NULL ) return 0;

    DoSpecialTable(spe);

    free(spe);
    *(pa-1) = MIAU; if ( pl ) *(pl-1) = MIAU;

    s = pl;
  } while ( s != NULL );

  return 0;
}


/****************************************************************************/
static int PrintMenu(tMenu *menu)
/*
** Tulostaa menun kaikki rivit.
** Viestirivien tapauksessa etsit„„n aina ensin vastaava [MessageN], miss„
** N tulostettavan viestirivin numero.
** Jos tulostettava rivi ei lopu @, vaihdetaan uudelle riville, muuten
** j„„d„„n samalle riville.
----------------------------------------------------------------------------*/
{
  int i,m;
  char *p;
  for (i=0; i < menu->rows; i++) {

    /* Viesteille suorit. [Message?] rivi */
    if ( IN(SET_Message,menu->message) ) {
      char s[30];
      int j;
      for (j=i; j>=0; j--) { /* Etsit„„n suurin [Message?] */
        sprintf(s,"%s%d%c",MessageStr,j,RIGHT_SB);
        if ( TulostaTeksti(s) ) break;
      }
    }

    p = menu->Items[i].ItemText;
    m = MenuPrintf(p);
    if ( IN(SET_Param,menu->message) ) break; /* Parametreist„ vain 1. rivi */
    if ( m == 0 && i < menu->rows-1 ) Mprintscr("\n");
  }
  return 0;
}


/****************************************************************************/
static const char *TulostaTeksti(const char *s)
/*
** Tulostaa merkkijonoa s vastaavan menun tai viestin.
** Voidaan kutsua joko [Oma] tai LEFT_SB Oma RIGHT_SB
----------------------------------------------------------------------------*/
{
  return TulostaTekstiFun(s,PrintMenu);
}

/****************************************************************************/
/* Viestisilmukan keskeytyksen kutsu ja menunsysteemin alustukset           */
/****************************************************************************/

/****************************************************************************/
int PostQuitMessage(int n)
{
  GMenu.QuitMsg  = n;
  GMenu.QuitMenu = 1;
  return 0;
}

/****************************************************************************/
FILE *PathOpen(const char *path,const char *ext,
               const char *mode, char **name)
/*
** Funktiolla yritet„„n avata tiedostoa.
** path-nimest„ otetaan ensin mahdollinen .exe pois ja sitten
** liitet„„n tilalle ext.
** Mahdollinen polku otetaan pois tiedoston nimest„ ja
** yritet„n avausta. Jollei onnistu, yritet„„n avausta polun kanssa.
** Polun kirjaimiksi tulkitaan : \ ja /.
** Esimerkki:
**  path              ext           yritys 1    yritys 2
**  c:\oma\koe.exe    .mnu          koe.mnu     c:\oma\koe.mnu
**  c:\oma\koe.txt    .mnu          koe.txt     c:\oma\koe.txt
**  koe.mnu           ""                        koe.mnu
**
** Palautetaan avattu tiedosto ja name-parametrissa (jos != NULL) nimi
** l”ytyneeseen tiedostoon (staattinen muuttuja, joka on
** voimassa funktion seuraavaan kutsuun saakka).
----------------------------------------------------------------------------*/
{
  FILE *f = NULL;
  static char s[100];
  char *p;
  int i;

  if ( name ) *name = NULL;
  if ( path == NULL ) return NULL;
  kopioi_jono(N_S(s),path);

  if ( ( p = strstr(s,".exe") ) != NULL ) *p = 0;
  if ( ( p = strstr(s,".EXE") ) != NULL ) *p = 0;
  if ( ext != NULL && strstr(s,".") == NULL ) liita_jono(N_S(s),ext);
  for (i=strlen(s)-1; i>=0; i--) /* Oletushakemistossa oleva nimi   */
    if ( strchr(":\\/",s[i]) ) { i++; break; }
  if ( i < 0 ) i = 0;

  if ( i > 0 )   /* Nimest„ saatiin hakemisto pois, yritet„„n t„t„.         */
    f = fopen(s+i,mode);


  if ( !f ) f = fopen(s,mode);
  if ( f && name ) *name = s+i;

  return f;
}


/****************************************************************************/
/* Testej„ varten                                                           */
#if 0
#include <alloc.h>
static int printmem(void)
{
  long l = coreleft();
  printf("%10ld",l);
}
#if 1
int iso[20000];
#endif
static int pausemem(void)
{
  lue_merkki();
}
#else
# define printmem()
# define pausemem()
#endif

/****************************************************************************/
int DoMenuCharTranslate(char *s)
{
  char *p = s;
  char *st = malloc(strlen(s)+1);
  int j,i;

  for (i=0;*p;p++) {
    if ( *p == '\\' ) { st[i++] = CPrefix(&p); continue; }
    for (j=0; MenuTrans[j].from; j++)
      if ( *p == MenuTrans[j].from ) {
        *p = MenuTrans[j].to;
        break;
      }
    st[i++] = *p;
  }
  st[i] = 0;
  strcpy(s,st);
  free(st);
  return 0;
}


/****************************************************************************/
int InitMenuSystem(const char *name,MSG *msg,tMenuTable *MenuTable)
/*
** Lataa menurivit menut.mnu-tiedostosta taulukkoon ja varaa muistitilat
** osoittimelle, joka osoittaa t„m„nhetkiseen menuun. Rakentaa pino-
** rakenteen menujen osoittimille, jotta paluu syv„st„kin valikkoraken-
** teesta toimii sujuvasti.
**
** Palautetaan:  0 = onnistui
**               1 = tiedostoa ei l”ydy
**               2 = ylitettiin menujen maksimim„„r„ MAX_MENUS
**               3 = muisti loppui
**
** Virhe saadaan selv„kielisen„ funktiolla InitError(err)
----------------------------------------------------------------------------*/
{
#define NEXTE(s) ( p = palanen(s,ctoa(MENUSEP),&j),  \
                   p = IN(SET_NoClearEnd,menu->message) ? \
                       p : poista_lopputyhjat(p), \
                   p[0] == 0 ? EmptySt : tee_jono(p) )
#define NEXT(s) ( p = poista_lopputyhjat(palanen(s,ctoa(MENUSEP),&j)),\
                p[0] == 0 ? EmptySt : tee_jono(p) )
  tMenu *menu;
  FILE *f;
  char s[300];
  char *p,c;
  int j;
  int items;
  int ret = 3;
  tMenuSystem *MenuS = &(GMenu.MenuSystem);

  f = PathOpen(name,".mnu","rt",NULL);
  if ( f == NULL ) return 1;

  MenuS->nmenu = 0;
  msg->MenuTable = MenuTable;
  MenuS->MenuTable = MenuTable;
  MenuS->CurrentMenu = NULL;
  MenuS->menu = calloc(sizeof(tMenu *),MAX_MENUS);
  if ( ! MenuS->menu ) goto closeMenuFile;
  ret = 0;

  while ( 1 ) {
    if ( f_lue_jono(f,N_S(s)) < 0 ) break;
    DoMenuCharTranslate(s);
newMenu:

    if ( MenuS->nmenu > 0 ) { /* Poistetaan turha koko edellisest„ menusta */
      menu = MenuS->menu[MenuS->nmenu-1];
      MenuS->menu[MenuS->nmenu-1] =
       realloc(menu,sizeof(tMenu) - (MAX_ITEMS-menu->rows)*sizeof(tMenuItem));
      printmem();
    }

    if ( MenuS->nmenu >= MAX_MENUS ) { ret = 2; break; }
    if ( !EQ(s[0],LEFT_SB) ) continue;
    p = palanen(s,ctoa(COMMENT),&j);
    menu = calloc(sizeof(tMenu),1);
    if ( menu == NULL ) { ret = 3; break; }

    MenuS->menu[MenuS->nmenu++] = menu;

    p = strchr(s,RIGHT_SB);
    if ( p != NULL ) { c = *(++p); *p = 0; }

    menu->MenuName = tee_jono(poista_lopputyhjat(s));
    items = 0;
    menu->nItems = 0;
    menu->rows   = 0;
    menu->message= MENU_MENU;

    if ( p ) { /* Onko samalla rivill„ muuta => viestimuoto!       */
      *p = c;
      if ( c != '=' ) poista_lopputyhjat(p);
      if ( p[0] ) {
        p++;  /* Alkuv„lily”nti pois                               */
        memmove(s,p,strlen(p)+1);
        if ( strstr(menu->MenuName,MessageStr) == NULL )
                             menu->message = MENU_MSG;
        if      ( c == '=' ) menu->message = MENU_PARAM;
        else if ( c == '-' ) menu->message = MENU_CLEARBEG;
        else if ( c == '+' ) menu->message = MENU_MSG_NCB;
        else if ( c != ' ' ) menu->message = MENU_NOMSG;
        goto itemText;
      }
    }

    while ( 1 ) {
      if ( f_lue_jono(f,N_S(s)) < 0 ) goto closeMenuFile;
      DoMenuCharTranslate(s);
itemText:
      if ( EQ(s[0],COMMENT) ) continue;
      if ( EQ(s[0],LEFT_SB) ) goto newMenu;
      if ( items >= MAX_ITEMS ) continue;

      p = palanen(s,ctoa(COMMENT),&j);

      menu->Items[items].ItemText = NEXTE(s);
      if ( IN(SET_ClearBeg,menu->message) )
        poista_alkutyhjat(menu->Items[items].ItemText);
      menu->Items[items].Choose   = NEXT(NULL);
      menu->Items[items].sitem    = NEXT(NULL);

      if ( IN(SET_Param,menu->message) ) { /* Nimetty parametri */
        int m = 0 , len = strlen(menu->Items[items].ItemText)+1;
        char *p;
        menu->Items[items].item = len;
        m = atoi(menu->Items[items].Choose)+1;
        if ( m > len ) { /* Onko pituusparametri annettu ja isontaako se? */
          if ( menu->Items[items].ItemText == EmptySt )
            p = calloc(m,1);
          else
            p = realloc(menu->Items[items].ItemText,m);
          if ( p ) {
            menu->Items[items].ItemText = p;
            menu->Items[items].item = m;
          }
        }
      } /* Nimetty parametri */
      else if ( menu->Items[items].Choose != EmptySt ) menu->nItems++;
      items++;
      menu->rows = items;
    }

  }
closeMenuFile:

/*  msg->MenuSystem = MenuS; */
  fclose(f);
  pausemem();
  return ret;
#undef NEXT
#undef NEXTE
}

/****************************************************************************/
static char *ErrorText(int err)
{
  switch ( err ) {
    case 0: return "";
    case 1: return "Tiedostoa .mnu ei l”ydy";
    case 2: return "Ylitettiin menujen maksimim„„r„";
    case 3: return "Muisti loppui";
  }
  return "Tuntematon virhe!";
}

/****************************************************************************/
char *GetInitError(int err)
{
  static char errtext[80]="";
  if ( err == 0 ) return "";
# define GM GMenu.MenuSystem
  if ( GM.nmenu ) {
    liita_jono(N_S(errtext),GM.menu[GM.nmenu-1]->MenuName);
    liita_jono(N_S(errtext),": ");
  }
# undef GM
  liita_jono(N_S(errtext),ErrorText(err));
  return errtext;
}

/****************************************************************************/
int ClearMenus(MSG *msg)
{
  /* T„ss„ t„ytyisi vapauttaa kaikki tietorakenteet!!!! */
  /*  msg->MenuSystem = NULL; */
  msg->wParam = 0;
  return 0;
}


/****************************************************************************/
int GetMessage(MSG *msg)
/*
** Hakee p„„valikon mik„li ohjelma on alussa tai nykyinen menu on kadonnut
** Lehtitason suorittavien ohjelman osien kutsujen j„lkeen palauttaa kutsua
** edelt„neen menun n„yt”lle. Hoitaa pinoa hyv„ksik„ytt„en paluun menupolussa
** Lukee ja tarkistaa annetun valinnan. Kun saatu ko.menun sallittu valinta
** funktio tarkistaa onko kyseess„ lehtitason valinta. Mik„li on funktio
** asettaa MSG-tietorakenteeseen toivotun funktion nimen. Ellei ole funktio
** painaa t„m„nhetkisen menun menupinon p„„llimm„iseksi alkioksi ja hakee
** FindMenu-aliohjelmalla valitun alivalikon.
----------------------------------------------------------------------------*/
{
  tMenuSystem *MenuS = &GMenu.MenuSystem; /* msg->MenuSystem; */
  tMenu       *menu;
  tMenu       *submenu;
  char MainMenu[] = {LEFT_SB,RIGHT_SB,0};
  int i;


  while (1) {

    if ( GMenu.QuitMenu != 0 ) return ClearMenus(msg);

    menu = MenuS->CurrentMenu;
    if ( menu == NULL ) menu = FindMenu(MenuS->menu,MainMenu);
    if ( menu == NULL ) return 0;
    MenuS->CurrentMenu = menu;

    if ( menu->nItems == 0 ) {
      PrintMenu(menu);
      return 0;
    }

    TulostaTeksti("[MenuBegin]");
    PrintMenu(menu);
    TulostaTeksti("[MenuEnd]");

    i = WaitMenuKey(menu,"",NULL);

    msg->sitem = menu->Items[i].sitem;

    if ( msg->sitem == EmptySt ) {
      MenuS->CurrentMenu = PopMenu(MenuS);
      continue;
    }

    if ( EQ(msg->sitem[0],MIAU) ) {
      MenuPrintf(msg->sitem);
      continue;
    }
    if ( !EQ(msg->sitem[0],LEFT_SB) ) break;

    submenu = FindMenu(MenuS->menu,msg->sitem);
    if ( submenu == NULL      ) continue;
    if ( submenu->nItems == 0 ) { PrintMenu(submenu); continue; }
    PushMenu(MenuS,menu);
    MenuS->CurrentMenu = submenu;

  }

  TulostaTeksti("[MenuExit]");
  msg->wParam = 0;
  return 1;
}

/****************************************************************************/
int ScrMessage(const char *topic,int nr)
/*
** Funktiolla n„ytet„„n viesti n„ytt””n.
** Parametrit @%n@ korvautuvat parametreill„
** Viesti on ollut muotoa:
** [DOS1] Dos virhe @%0@. Jatketaanko (K/e) |Ke|
**
** Funktio palauttaa:
**   -1 jos ei l”ydy viesti„ [topic+nr] eik„ [topic*]
**   0  jos painetaan 1. valittu kirjain tai return
**   1  jos painetaan 2. kirjain tai space
**   2  jos painetaan 3. kirjain tai ESC
**  >2  jos painetaan muita valittavissa olevia kirjaimia
----------------------------------------------------------------------------*/
{
  char s[80],sn[15];
  tMenu *message;
  sn[0]=0;
  if ( nr >= 0 ) sprintf(sn,"%d",nr);
  message = FindMenu(GMenu.MenuSystem.menu,make_name(N_S(s),topic,sn));
  if ( !message )
    message = FindMenu(GMenu.MenuSystem.menu,make_name(N_S(s),topic,"*"));
  if ( !message ) return -1;

  return DoScrMessage(message);
}


/****************************************************************************/
int ScrMessage6(Cchar *topic,int nr,Cchar *p1,Cchar *p2,Cchar *p3,Cchar *p4,
                Cchar *p5,Cchar *p6)
{
  SetMessageParam(0,p1);
  SetMessageParam(1,p2);
  SetMessageParam(2,p3);
  SetMessageParam(3,p4);
  SetMessageParam(4,p5);
  SetMessageParam(5,p6);
  return ScrMessage(topic,nr);
}

/****************************************************************************/
int ScrMessage1(Cchar *topic,int nr,Cchar *p1)
{
  return ScrMessage6(topic,nr,p1,NULL,NULL,NULL,NULL,NULL);
}

/****************************************************************************/
int ScrMessage2(Cchar *topic,int nr,Cchar *p1,Cchar *p2)
{
  return ScrMessage6(topic,nr,p1,p2,NULL,NULL,NULL,NULL);
}

/****************************************************************************/
int ScrMessage3(Cchar *topic,int nr,Cchar *p1,Cchar *p2,Cchar *p3)
{
  return ScrMessage6(topic,nr,p1,p2,p3,NULL,NULL,NULL);
}

/****************************************************************************/
int ScrMessage4(Cchar *topic,int nr,Cchar *p1,Cchar *p2,Cchar *p3,Cchar *p4)
{
  return ScrMessage6(topic,nr,p1,p2,p3,p4,NULL,NULL);
}

/****************************************************************************/
int ScrMessage5(Cchar *topic,int nr,Cchar *p1,Cchar *p2,Cchar *p3,Cchar *p4,
                Cchar *p5)
{
  return ScrMessage6(topic,nr,p1,p2,p3,p4,p5,NULL);
}


/****************************************************************************/
int SetNameParamInt(const char *name,int i)
{
  char s[10];
  sprintf(s,"%d",i);
  return SetNameParam(name,s);
}

/****************************************************************************/
int GetNameParamInt(const char *name)
{
  const char *s=GetNameParam(name);
  if ( !s ) return 0;

  return atoi(s);
}

/****************************************************************************/
int SetNameParamDouble(const char *name,double i)
{
  char s[20];
  sprintf(s,"%g",i);
  return SetNameParam(name,s);
}

/****************************************************************************/
double GetNameParamDouble(const char *name)
{
  const char *s=GetNameParam(name);
  if ( !s ) return 0;

  return atof(s);
}


/****************************************************************************/
/* Viestin tulkinta ja menusysteemin py”ritys:                              */
/****************************************************************************/

/****************************************************************************/
int DispatchMessage(MSG *msg)
/*
** Funktiolla tulkitaan viesti ja kutustaan tarvittavaa aliohjelmaa.
----------------------------------------------------------------------------*/
{
  RunFunc(msg->MenuTable,msg->sitem);
  return 0;
}


/****************************************************************************/
int MenuRun(Cchar *menuname,tMenuTable *MenuTable)
/*
** Funktiolla alustetaan menusysteemi ja toteutetaan viestisilmukka.
** Yksinkertaisia p„„ohjelmia varten.
----------------------------------------------------------------------------*/
{
  MSG msg;
  int err;

  if ( ( err = InitMenuSystem(menuname,&msg,MenuTable) ) != 0 ) {
    printf("%s\n",GetInitError(err));
    return err;
  }

  InitConsole();

  while ( GetMessage(&msg) ) {
    DispatchMessage(&msg);
  }

  ReleaseConsole();

  return msg.wParam;
}


/****************************************************************************/
int MenuRunArg(int argc,char *argv[],tMenuTable *MenuTable)
/*
** Funktiolla alustetaan menusysteemi ja toteutetaan viestisilmukka.
** Yksinkertaisia p„„ohjelmia varten.
----------------------------------------------------------------------------*/
{
  char *menuname = argv[0];

  if ( argc > 1 ) menuname = argv[1];

  return MenuRun(menuname,MenuTable);
}
