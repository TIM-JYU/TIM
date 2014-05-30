<h1>Esipuhe</h1>
<p>Tämä moniste on luentomoniste kurssille Ohjelmointi 1. Luentomoniste
tarkoittaa sitä, että sen ei ole tarkoitus korvata kunnon kirjaa, vaan
esittää asiat samassa järjestyksessä ja samassa valossa kuin ne
esitetään luennolla. Jotta moniste ei paisuisi kohtuuttomasti, ei
asioita käsitellä missään nimessä täydellisesti. Siksi opiskelun tueksi
tarvitaan jokin hyvä aihetta käsittelevä kirja, sekä rutkasti
ennakkoluulotonta asennetta ottaa asioista selvää. Tuorein tieto löytyy
tietenkin netistä. On myös huomattava, että useimmat saatavilla olevat
kirjat keskittyvät hyvin paljon tiettyyn ohjelmointikieleen—erityisesti
aloittelijoille tarkoitetut. Osin tämä on luonnollista, koska ihmisetkin
tarvitsevat jonkin yhteisen kielen kommunikoidakseen toisen kanssa.
Siksi ohjelmoinnin aloittaminen ilman, että ensin opetellaan jonkun
kielen perusteet, on aika haastavaa.</p>
<p>Jäsentämisen selkeyden takia kirjoissa käsitellään yleensä yksi aihe
perusteellisesti alusta loppuun. Aloittaessaan puhumaan lapsi ei
kuitenkaan ole kykeneväinen omaksumaan kaikkea tietyn lauserakenteen
kieliopista. Vastaavasti ohjelmoinnin alkeita kahlattaessa
vastaanottokyky ei vielä riitä kaikkien kikkojen käsittämiseen. Tässä
monisteessa ja luennolla asioiden käsittelyjärjestys on sellainen, että
asioista annetaan ensin esimerkkejä tai johdatellaan niiden tarpeeseen,
ja sitten jonkin verran selitetään mistä oli kyse. Tästä syystä tästä
monisteesta saa yhden näkemyksen mukaisen pintaraapaisun asioille ja
kirjoista ja nettilähteistä asiaa on syvennettävä.</p>
<p>Tässä monisteessa käytetään esimerkkikielenä <em>C#</em>-kieltä. Kuitenkin
nimenomaan esimerkkinä, koska monisteen rakenne ja esimerkit voisivat
olla aivan samanlaisia mille tahansa muullekin ohjelmointikielelle.
Tärkeintä on nimenomaan ohjelmoinnin ajattelutavan oppiminen. Kielen
vaihtaminen toiseen samansukuiseen kieleen on ennemmin verrattavissa
Savon murteen vaihtamisen Turun murteeseen, kuin suomen kielen
vaihtamisen ruotsin kieleen. Toisin sanoen, jos yhdellä kielellä on
oppinut ohjelmoimaan, kykenee jo lukemaan toisella kielellä
kirjoitettuja ohjelmia pienen harjoittelun jälkeen. Toisella kielellä
kirjoittaminen on hieman haastavampaa, mutta samat rakenteet sielläkin
toistuvat. Ohjelmointikielet tulevat ja menevät. Tätäkin vastaavaa
kurssia on pidetty Jyväskylän yliopistossa seuraavilla kielillä:
Fortran, Pascal, C, C++, Java ja nyt C#. Joissakin yliopistoissa
aloituskielenä on Python.</p>
<p>Ohjelmointia on täysin mahdotonta oppia pelkästään kirjoja lukemalla.
Siksi kurssi sisältää luentojen ohella myös viikoittaisten
harjoitustehtävien (demojen) tekemistä, ohjattua pääteharjoittelua
tietokoneluokassa sekä harjoitustyön tekemisen. Näistä lisätietoa,
samoin kuin kurssilla käytettävien työkalujen hankkimisesta ja
asentamisesta löytyy kurssin kotisivuilta:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
https://trac.cc.jyu.fi/projects/ohj1</code></p>
<p>Tämä moniste perustuu Martti Hyvösen ja Vesa Lappalaisen syksyllä 2009
kirjoittamaan <em>Ohjelmointi 1</em> -monisteeseen, joka osaltaan sai muotonsa
monen eri kirjoittajan työn tuloksena aina 80-luvulta alkaen. Suurimman
panoksen monisteeseen ovat antaneet Timo Männikkö ja Vesa Lappalainen.</p>
<p>Jyväskylässä 2.1.2013</p>
<p>Martti Hyvönen, Vesa Lappalainen, Antti-Jussi Lakanen</p>
<h1>1. Mitä ohjelmointi on?</h1>
<p>Ohjelmointi on yksinkertaisimmillaan toimintaohjeiden antamista ennalta
määrätyn toimenpiteen suorittamista varten. Ohjelmoinnin kaltaista
toimintaa esiintyy jokaisen ihmisen arkielämässä lähes päivittäin.
Algoritmista esimerkkinä voisi olla se, että annamme jollekulle
puhelimessa ajo-ohjeet, joiden avulla hänen tulee päästä perille
ennestään vieraaseen paikkaan. Tällöin luomme sarjan ohjeita ja
komentoja, jotka ohjaavat toimenpiteen suoritusta. Alkeellista
ohjelmointia on tavallaan myös mikroaaltouunin käyttäminen, sillä
tällöin uunille annetaan ohjeet siitä, kuinka kauan ja kuinka suurella
teholla sen tulee toimia.</p>
<p>Kaikissa edellisissä esimerkeissä oli siis kyse yksikäsitteisten
ohjeiden antamisesta. Kuitenkin esimerkit käsittelivät hyvinkin
erilaisia viestintätilanteita. Ihmisten välinen kommunikaatio,
mikroaaltouunin kytkimien kiertäminen tai nappien painaminen, samoin
kuin videon ajastimen säätö laserkynällä ovat ohjelmoinnin kannalta
toisiinsa rinnastettavissa, mutta ne tapahtuvat eri työvälineitä
käyttäen. Ohjelmoinnissa työvälineiden valinta riippuu asetetun tehtävän
ratkaisuun käytettävissä olevista välineistä. Ihmisten välinen
kommunikaatio voi tapahtua puhumalla, kirjoittamalla tai näiden
yhdistelmänä. Samoin ohjelmoinnissa voidaan usein valita erilaisia
toteutustapoja tehtävän luonteesta riippuen.</p>
<p>Ohjelmoinnissa on olemassa eri tasoja riippuen siitä, minkälaista
työvälinettä tehtävän ratkaisuun käytetään. Pitkälle kehitetyt korkean
tason työvälineet mahdollistavat työskentelyn käsitteillä ja
ilmaisuilla, jotka parhaimmillaan muistuttavat luonnollisen kielen
käyttämiä käsitteitä ja ilmaisuja, kun taas matalan tason työvälineillä
työskennellään hyvin yksinkertaisilla ja alkeellisilla käsitteillä ja
ilmaisuilla.</p>
<p>Eräänä esimerkkinä ohjelmoinnista voidaan pitää sokerikakun
valmistukseen kirjoitettua ohjetta:</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
Sokerikakku</p>
<p>6       munaa
1,5 dl  sokeria
1,5 dl  jauhoja
1,5 tl  leivinjauhetta</p>
<ol>
<li>Vatkaa sokeri ja munat vaahdoksi.</li>
<li>Sekoita jauhot ja leivinjauhe.</li>
<li>Sekoita muna-sokerivaahto ja jauhoseos.</li>
<li>Paista 45 min 175°C lämpötilassa.
```</li>
</ol>
<p>Valmistusohje on ilmiselvästi kirjoitettu ihmistä varten, vieläpä
sellaista ihmistä, joka tietää leipomisesta melko paljon. Jos sama ohje
kirjoitettaisiin ihmiselle, joka ei eläessään ole leiponut mitään, ei
edellä esitetty ohje olisi alkuunkaan riittävä, vaan siinä täytyisi
huomioida useita leipomiseen liittyviä niksejä: uunin ennakkoon
lämmittäminen, vaahdon vatkauksen salat, yms.</p>
<p>Koneelle kirjoitettavat ohjeet poikkeavat merkittävästi ihmisille
kirjoitetuista ohjeista. Kone ei osaa automaattisesti kysyä neuvoa
törmätessään uuteen ja ennalta arvaamattomaan tilanteeseen. Se toimii
täsmälleen niiden ohjeiden mukaan, jotka sille on annettu, olivatpa ne
vallitsevassa tilanteessa mielekkäitä tai eivät. Kone toistaa saamiaan
toimintaohjeita uskollisesti sortumatta ihmisille tyypilliseen
luovuuteen. Näin ollen tämän päivän ohjelmointikielillä koneelle
tarkoitetut ohjeet on esitettävä hyvin tarkoin määritellyssä muodossa ja
niissä on pyrittävä ottamaan huomioon kaikki mahdollisesti esille
tulevat tilanteet. [MÄN]</p>
<h1>2. Ensimmäinen C#-ohjelma</h1>
<h2>2.1 Ohjelman kirjoittaminen</h2>
<p>C#-ohjelmia (lausutaan <em>c sharp</em>) voi kirjoittaa millä tahansa
tekstieditorilla. Tekstieditoreja on kymmeniä, ellei satoja, joten yhden
nimeäminen on vaikeaa. Osa on kuitenkin suunniteltu varta vasten
ohjelmointia ajatellen. Tällaiset tekstieditorit osaavat muotoilla
ohjelmoijan kirjoittamaa lähdekoodia (tai lyhyesti koodia)
automaattisesti siten, että lukeminen on helpompaa ja siten ymmärtäminen
ja muokkaaminen nopeampaa. Ohjelmoijien suosimia ovat mm. <em>Vim,</em> <em>Emacs,
ConTEXT</em> <em>ja</em> <em>NotePad++</em>, mutta monet muutkin ovat varmasti hyviä.
Monisteen alun esimerkkien kirjoittamiseen soveltuu hyvin mikä tahansa
tekstieditori.</p>
<p><em>Koodi, lähdekoodi =</em> Ohjelmoijan tuottama tiedosto, josta varsinainen
ohjelma muutetaan tietokoneen ymmärtämäksi konekieleksi.</p>
<p>Kirjoitetaan tekstieditorilla alla olevan mukainen C#-ohjelma ja
tallennetaan se vaikka nimellä HelloWorld.cs. Tiedoston tarkenteeksi
(eli niin sanottu tiedostopääte) on sovittu juuri tuo .cs, mikä tulee
käytetyn ohjelmointikielen nimestä, joten tälläkin kurssilla käytämme
sitä. Kannattaa olla tarkkana tiedostoa tallennettaessa, sillä jotkut
tekstieditorit yrittävät oletuksena tallentaa kaikki tiedostot
tarkenteella .txt ja tällöin tiedoston nimi voi helposti tulla muotoon
HelloWorld.cs.txt.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
public class HelloWorld 
{
   public static void Main() 
   {
      System.Console.WriteLine("Hello World!");
   }
}</code></p>
<p>Tämän ohjelman pitäisi tulostaa näytölle teksti</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Hello World!</code></p>
<p>Voidaksemme kokeilla ohjelmaa käytännössä, täytyy se ensiksi kääntää
tietokoneen ymmärtämään muotoon.</p>
<p><em>Kääntäminen =</em> Kirjoitetun lähdekoodin muuntamista suoritettavaksi
ohjelmaksi.</p>
<p>Esimerkkejä muilla ohjelmointikielillä kirjoitetusta HelloWorld
-ohjelmasta löydät vaikkapa:</p>
<p><a href="http://www2.latech.edu/~acm/HelloWorld.html">http://www2.latech.edu/\~acm/HelloWorld.html</a>.</p>
<h2>2.2 Ohjelman kääntäminen ja ajaminen</h2>
<p>Jotta ohjelman kääntäminen ja suorittaminen onnistuu, täytyy koneelle
olla asennettuna joku C#-sovelluskehitin. Mikäli käytät Windowsia, niin
aluksi riittää hyvin Microsoft .NET SDK (Software Development Kit, suom.
kehitystyökalut). Muiden käyttöjärjestelmien tapauksessa
sovelluskehittimeksi käy esimerkiksi <em>Novell Mono</em>. Hyvin monet tämän
kurssin harjoituksista on tehtävissä Mono-sovelluskehittimellä, mutta
tämän monisteen ohjeet ja esimerkit tullaan käsittelemään
Windows-ympäristössä. Edelleen, Jypeli-kirjaston käyttäminen on
mahdollista vain Windows-ympäristössä.</p>
<p>Lisätietoa .NET-kehitystyökaluista ja asentamisesta löytyy kurssin
kotisivuilta:
<a href="https://trac.cc.jyu.fi/projects/ohj1/wiki/dotnet-tyokalut">https://trac.cc.jyu.fi/projects/ohj1/wiki/dotnet-tyokalut</a>.
Kun sovelluskehitin on asennettu, käynnistetään komentorivi (Command
Prompt, lyhyemmin cmd) ja siirrytään siihen hakemistoon, johon
HelloWorld.cs tiedosto on tallennettu. Ohjelma käännetään nyt
komennolla:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
csc HelloWorld.cs</code></p>
<p>Komento ”csc” tulee sanoista <em>C Sharp Compiler</em> (compiler = kääntäjä).
Kääntämisen jälkeen hakemistoon ilmestyy HelloWorld.exe-niminen
tiedosto, joka voidaan ajaa kuten minkä tahansa ohjelman syöttämällä
ohjelman nimi:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
HelloWorld</code></p>
<p>Ohjelman tulisi nyt tulostaa näyttöön teksti Hello World!, kuten alla
olevassa kuvassa.</p>
<p><img alt="\
 Kuva 1: Ohjelman kääntäminen ja ajaminen Windowsin
komentorivillä." src="../src/luentomonistecsUusin_htm_m16b63f32.png" /></p>
<p>\
 Huomaa, että käännettäessä kirjoitetaan koko tiedoston nimi
.cs-tarkentimen kanssa.</p>
<p>Jos saat virheilmoituksen</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
'csc' is not recognized as an internal or external command, operable program or batch file.</code></p>
<p>niin kääntäjäohjelmaa csc.exe ei silloin löydy niin sanotusta
<em>hakupolusta</em>. Ohjelman lisääminen hakupolkuun onnistuu komennolla:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
set PATH=%WINDIR%\Microsoft.NET\Framework\v4.0.30319;%path%</code></p>
<p>Jotta kääntäjää ei tarvitsisi joka kerta lisätä hakupolkuun, voi sen
lisätä siihen pysyvästi. Esimerkiksi Windows 7:ssä tämä tapahtuu
seuraavasti.</p>
<p>Klikkaa Oma tietokone -kuvaketta hiiren oikealla painikkeella ja valitse
<strong>Ominaisuudet</strong> (Properties). Valitse sitten vasemmalta Advanced system
settings ja Advanced-välilehdeltä Environment variables. Ylemmästä
laatikosta valitse muuttuja PATH ja paina <strong>Muokkaa</strong>. Siirry rivin
Variable value loppuun, kirjoita puolipiste (;) ja heti perään polku</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
%WINDIR%\Microsoft.NET\Framework\v4.0.30319</code></p>
<p>XP:ssä vastaavasti Oma tietokone → Ominaisuudet → Lisäasetukset →
Ympäristömuuttujat.</p>
<h2>2.3 Ohjelman rakenne</h2>
<p>Ensimmäinen kirjoittamamme ohjelma HelloWorld.cs on oikeastaan
yksinkertaisin mahdollinen C#-ohjelma. Alla yksinkertaisimman ohjelman
kaksi ensimmäistä riviä.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
public class HelloWorld
{</code></p>
<p>Ensimmäisellä rivillä määritellään <em>luokka</em> (class) jonka nimi on
HelloWorld. Tässä vaiheessa riittää ajatella luokkaa ”kotina”
<em>aliohjelmille</em>. Aliohjelmista puhutaan lisää hieman myöhemmin.
Toisaalta luokkaa voidaan verrata ”piparkakkumuottiin”-se on
rakennusohje olioiden (eli ”piparkakkujen”) luomista varten. Ohjelman
ajamisen aikana olioita syntyy luokkaan kirjoitetun koodin avulla.
Olioita voidaan myös tuhota. Yhdellä luokalla voidaan siis tehdä monta
samanlaista oliota, aivan kuten yhdellä piparkakkumuotilla voidaan tehdä
monta samanlaista (samannäköistä) piparia.</p>
<p>Jokaisessa C#-ohjelmassa on vähintään yksi luokka, mutta luokkia voi
olla enemmänkin. Luokan, jonka sisään ohjelma kirjoitetaan, on hyvä olla
samanniminen kuin tiedoston nimi. Jos tiedoston nimi on HelloWorld.cs,
on suositeltavaa, että luokan nimi on myös HelloWorld, kuten meidän
esimerkissämme. Tässä vaiheessa ei kuitenkaan vielä kannata liikaa
vaivata päätänsä sillä, mikä luokka oikeastaan on, se selviää tarkemmin
myöhemmin.</p>
<p>Huomaa! C#:ssa <em>ei</em> samasteta isoja ja pieniä kirjaimia. Ole siis
tarkkana kirjoittaessasi luokkien nimiä.</p>
<p>Huomaa! Vahva suositus (ja tämän kurssin tapa) on, että luokka alkaa
isolla alkukirjaimella, ja ettei skandeja käytetä luokan nimessä.</p>
<p>Luokan edessä oleva public-sana on eräs <em>saantimääre</em> (eng. <em>access
modifier</em>). Saantimääreen avulla luokka voidaan asettaa rajoituksetta
tai osittain muiden (luokkien) saataville, tai piilottaa kokonaan. Sana
public tarkoittaa, että luokka on muiden luokkien näkökulmasta
<em>julkinen</em>, kuten luokat useimmiten ovat. Muita saantimääreitä ovat
protected, internal ja private.</p>
<p>Määreen voi myös jättää kirjoittamatta luokan eteen, jolloin luokan
määreeksi tulee automaattisesti internal. Puhumme aliohjelmista
myöhemmin, mutta mainittakoon, että vastaavasti, jos aliohjelmasta
jättää määreen kirjoittamatta, tulee siitä private. Tällä kurssilla
kuitenkin harjoitellaan kirjoittamaan julkisia luokkia (ja aliohjelmia),
jolloin public-sana kirjoitetaan aina luokan ja aliohjelman eteen.</p>
<p>Toisella rivillä on oikealle auki oleva <em>aaltosulku</em>. Useissa
ohjelmointikielissä yhteen liittyvät asiat ryhmitellään tai kootaan
aaltosulkeiden sisälle. Oikealle auki olevaa aaltosulkua sanotaan
aloittavaksi aaltosuluksi ja tässä tapauksessa se kertoo kääntäjälle,
että tästä alkaa HelloWorld-luokkaan liittyvät asiat. Jokaista
aloittavaa aaltosulkua kohti täytyy olla vasemmalle auki oleva lopettava
aaltosulku. HelloWorld-luokan lopettava aaltosulku on rivillä viisi,
joka on samalla ohjelman viimeinen rivi. Aaltosulkeiden rajoittamaa
aluetta kutsutaan <em>lohkoksi</em> (block).</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
public static void Main()
{</code></p>
<p>Rivillä kolme määritellään (tai oikeammin <em>esitellään</em>) uusi aliohjelma
nimeltä Main. Nimensä ansiosta se on tämän luokan pääohjelma. Sanat
static ja void kuuluvat aina Main-aliohjelman esittelyyn. Paneudumme
niihin tarkemmin hieman myöhemmin, mutta sanottakoon tässä kohtaa, että
static tarkoittaa, että aliohjelma on <em>luokkakohtainen</em> (vastakohtana
<em>oliokohtainen</em>, jolloin static-sanaa ei kirjoiteta). Vastaavasti void
merkitsee, ettei aliohjelma palauta mitään tietoa.</p>
<p>Samoin kuin luokan, niin myös pääohjelman sisältö kirjoitetaan
aaltosulkeiden sisään. C#:ssa ohjelmoijan kirjoittaman koodin
suorittaminen alkaa aina käynnistettävän luokan pääohjelmasta. Toki
sisäisesti ehtii tapahtua paljon asioita jo ennen tätä.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
System.Console.WriteLine("Hello World!");</code></p>
<p>Rivillä neljä tulostetaan näytölle Hello World!. C#:ssa tämä tapahtuu
pyytämällä .NET-ympäristön mukana tulevan luokkakirjaston
System-luokkakirjaston Console-luokkaa tulostamaan
WriteLine()-<em>metodilla</em> (method).</p>
<p>Huomaa! Viitattaessa aliohjelmiin on kirjallisuudessa usein tapana
kirjoittaa aliohjelman nimen perään sulut. Kirjoitustyyli korostaa, että
kyseessä on aliohjelma, mutta asiayhteydestä riippuen sulut voi myös
jättää kirjoittamatta. Tässä monisteessa käytetään pääsääntöisesti
jälkimmäistä tapaa, tilanteesta riippuen.</p>
<p>Kirjastoista, olioista ja metodeista puhutaan lisää kohdassa 4.1 ja
luvussa 8. Tulostettava merkkijono kirjoitetaan sulkeiden sisälle
lainausmerkkeihin (Shift + 2). Tämä rivi on myös tämän ohjelman ainoa
<em>lause</em> (statement). Lauseiden voidaan ajatella olevan yksittäisiä
toimenpiteitä, joista ohjelma koostuu. Jokainen lause päättyy C#:ssa
puolipisteeseen. Koska lauseen loppuminen ilmoitetaan puolipisteellä, ei
C#:n syntaksissa (syntax) ”tyhjillä merkeillä” (white space), kuten
rivinvaihdoilla ja välilyönneillä ole merkitystä ohjelman toiminnan
kannalta. Ohjelmakoodin luettavuuden kannalta niillä on kuitenkin suuri
merkitys. Huomaa, että puolipisteen unohtaminen on yksi yleisimmistä
ohjelmointivirheistä ja tarkemmin sanottuna <em>syntaksivirheistä.</em></p>
<p><em>Syntaksi</em> = Tietyn ohjelmointikielen (esimerkiksi C#:n)
kielioppisäännöstö.</p>
<h3>2.3.1 Virhetyypit</h3>
<p>Ohjelmointivirheet voidaan jakaa karkeasti <em>syntaksivirheisiin</em> ja
<em>loogisiin virheisiin</em>.</p>
<p>Syntaksivirhe estää ohjelman kääntymisen vaikka merkitys eli
<em>semantiikka</em> olisikin oikein. Siksi ne huomataankin aina viimeistään
ohjelmaa käännettäessä. Syntaksivirhe voi olla esimerkiksi joku
kirjoitusvirhe tai puolipisteen unohtaminen lauseen lopusta.</p>
<p>Loogisissa virheissä semantiikka, eli merkitys, on väärin. Ne on
vaikeampi huomata, sillä ohjelma kääntyy semanttisista virheistä
huolimatta. Ohjelma voi jopa näyttää toimivan täysin oikein. Jos
looginen virhe ei löydy <em>testauksessakaan</em> (testing), voivat seuraukset
ohjelmistosta riippuen olla tuhoisia. Tässä yksi tunnettu esimerkki
loogisesta virheestä:</p>
<p><a href="http://money.cnn.com/magazines/fortune/fortune_archive/2000/02/07/272831/index.htm">http://money.cnn.com/magazines/fortune/fortune_archive/2000/02/07/272831/index.htm</a>.</p>
<h3>2.3.2 Kääntäjän virheilmoitusten tulkinta</h3>
<p>Alla on esimerkki syntaksivirheestä HelloWorld-ohjelmassa.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
public class HelloWorld
{
  public static void Main()
  {
       System.Console.Writeline("Hello World!");
  }
}</code></p>
<p>Ohjelmassa on pieni kirjoitusvirhe, joka on (ilman apuvälineitä) melko
hankala huomata. Tutkitaan csc-kääntäjän antamaa virheilmoitusta.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
HelloWorld.cs(5,17): error CS0117: 'System.Console' does not contain a
        definition for 'Writeline'</code></p>
<p>Kääntäjä kertoo, että tiedostossa HelloWorld.cs rivillä 5 ja sarakkeessa
17 on seuraava virhe: System.Console-luokka ei tunne Writeline-komentoa.
Tämä onkin aivan totta, sillä WriteLine kirjoitetaan isolla L:llä.
Korjattuamme tuon ohjelma toimii jälleen.</p>
<p>Valitettavasti virheilmoituksen sisältö ei aina kuvaa ongelmaa kovinkaan
hyvin. Alla olevassa esimerkissä on erehdytty laittamaan puolipisteen
väärään paikkaan.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
public class HelloWorld
{
  public static void Main();
  {
       System.Console.Writeline("Hello World!");
  }
}</code></p>
<p>Virheilmoitus, tai oikeastaan virheilmoitukset, näyttävät seuraavalta.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
HelloWorld.cs(4,3): error CS1519: Invalid token '{' in class, struct, or
        interface member declaration
HelloWorld.cs(5,26): error CS1519: Invalid token '(' in class, struct, or
        interface member declaration
HelloWorld.cs(7,1): error CS1022: Type or namespace definition, or end-of-file
        expected</code></p>
<p>Ensimmäinen virheilmoitus osoittaa riville 4, vaikka todellisuudessa
ongelma on rivillä 3. Toisin sanoen, näistä virheilmoituksista ei ole
meille tässä tilanteessa lainkaan apua, päinvastoin, ne kehottavat
tekemään jotain, mitä emme halua.</p>
<h3>2.3.3 Tyhjät merkit (White spaces)</h3>
<p>Esimerkkinämme ollut HelloWorld-ohjelma voitaisiin, ilman että sen
toiminta muuttuisi, vaihtoehtoisesti kirjoittaa myös seuraavassa
muodossa.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
public class HelloWorld
                          {</p>
<pre><code> public static void Main()
</code></pre>
<p>{
System.Console.WriteLine("Hello World!");
    }</p>
<p>}
```</p>
<p>\
 \</p>
<p>Edelleen, koodi voitaisiin kirjoittaa myös seuraavasti.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
public class HelloWorld { public static void Main() {
    System.Console.WriteLine("Hello World!"); } }</code></p>
<p>Vaikka molemmat yllä olevista esimerkeistä ovat syntaksiltaan oikein,
eli ne noudattavat C#:n kielioppisääntöjä, on niiden luettavuus
huomattavasti heikompi kuin alkuperäisen ohjelmamme. C#:ssa on
yhteisesti sovittuja koodauskäytänteet (code conventions), jotka
määrittelevät, miten ohjelmakoodia tulisi kirjoittaa. Kun kaikki
kirjoittavat samalla tavalla, on muiden koodin lukeminen helpompaa.
Tämän monisteen esimerkit on pyritty kirjoittamaan näiden käytänteiden
mukaisesti. Linkkejä koodauskäytänteisiin löytyy kurssin wiki-sivulta
osoitteesta</p>
<p><a href="https://trac.cc.jyu.fi/projects/ohj1/wiki/CsKoodausKaytanteet">https://trac.cc.jyu.fi/projects/ohj1/wiki/CsKoodausKaytanteet</a><a href="https://trac.cc.jyu.fi/projects/ohj1/wiki/CsKoodausKaytanteet">.</a></p>
<p>Merkkijonoja käsiteltäessä välilyönneillä, tabulaattoreilla ja
rivinvaihdoilla on kuitenkin merkitystä. Vertaa alla olevia tulostuksia.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
System.Console.WriteLine("Hello World!");</code></p>
<p>Yllä oleva rivi tulostaa: Hello World!, kun taas alla oleva rivi
tulostaa: H e l l o W o r l d !</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
System.Console.WriteLine("H e l l o    W o r l d !");</code></p>
<h2>2.4 Kommentointi</h2>
<blockquote>
<p>“Good programmers use their brains, but good guidelines save us having
to think out every case.” -Francis Glassborow</p>
</blockquote>
<p>Lähdekoodia on usein vaikea ymmärtää pelkkää ohjelmointikieltä
lukemalla. Tämän takia koodin sekaan voi ja pitää lisätä selosteita eli
<em>kommentteja</em>. Kommentit ovat sekä koodin kirjoittajaa itseään varten
että tulevia ohjelman lukijoita ja ylläpitäjiä varten. Monet asiat
voivat kirjoitettaessa tuntua ilmeisiltä, mutta jo viikon päästä saakin
pähkäillä, että miksihän tuonkin tuohon kirjoitin.</p>
<p>Kääntäjä jättää kommentit huomioimatta, joten ne eivät vaikuta ohjelman
toimintaan. C#:ssa on kolmenlaisia kommentteja.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
// Yhden rivin kommentti</code></p>
<p>Yhden rivin kommentti alkaa kahdella vinoviivalla (//). Sen vaikutus
kestää koko rivin loppuun.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
/* Tämä   kommentti
   on usean
   rivin
   pituinen */</code></p>
<p>Vinoviivalla ja asteriskilla alkava (/*) kommentti kestää niin kauan
kunnes vastaan tulee asteriski ja vinoviiva ( */). Huomaa, ettei
asteriskin ja vinoviivan väliin tule välilyöntiä.</p>
<h3>2.4.1 Dokumentointi</h3>
<p>Kolmas kommenttityyppi on <em>dokumentaatiokommentti</em>.
Dokumentaatiokommenteissa on tietty syntaksi, ja tätä noudattamalla
voidaan dokumentaatiokommentit muuttaa sellaiseen muotoon, että
kommentteihin perustuvaa yhteenvetoa on mahdollista tarkastella
nettiselaimen avulla.</p>
<p>Dokumentaatiokommentti olisi syytä kirjoittaa ennen jokaista luokkaa,
pääohjelmaa, aliohjelmaa ja metodia (aliohjelmista ja metodeista
puhutaan myöhemmin). Lisäksi jokainen C#-tiedosto alkaa aina
dokumentaatiokommentilla, josta selviää tiedoston tarkoitus, tekijä ja
versio.</p>
<p>Dokumentaatiokommentit kirjoitetaan siten, että rivin alussa on aina
aina kolme vinoviivaa (Shift + 7). Jokainen seuraava
dokumentaatiokommenttirivi aloitetaan siis myöskin kolmella
vinoviivalla.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
/// Tämä 
/// on 
/// dokumentaatiokommentti</code></p>
<p>Dokumentoiminen tapahtuu <em>tagien</em> avulla. Jos olet joskus kirjoittanut
HTML-sivuja, on merkintätapa sinulle tuttu. Dokumentaatiokommentit
alkavat aloitustagilla, muotoa \&lt;esimerkki>, jonka perään tulee
kommentin asiasisältö. Kommentti loppuu lopetustagiin, muotoa
\&lt;/esimerkki>, siis muuten sama kuin aloitustagi, mutta ensimmäisen
kulmasulun jälkeen on yksi vinoviiva.</p>
<p>C#-tageja ovat esimerkiksi \&lt;summary>, jolla ilmoitetaan pieni
yhteenveto kommenttia seuraavasta koodilohkosta (esimerkiksi pääohjelma
tai metodi). Yhteenveto päättyy \&lt;/summary> -lopetustagiin.</p>
<p>Ohjelman kääntämisen yhteydessä dokumentaatiotagit voidaan kirjoittaa
erilliseen <em>XML</em>-tiedostoon, josta ne voidaan edelleen muuntaa helposti
selattaviksi HTML-sivuiksi. Tageja voi keksiä itsekin lisää, mutta tämän
kurssin tarpeisiin riittää hyvin suositeltujen tagien luettelo. Tiedot
suositelluista tageista löytyvät C#:n dokumentaatiosta:</p>
<p><a href="http://msdn.microsoft.com/en-us/library/5ast78ax.aspx">http://msdn.microsoft.com/en-us/library/5ast78ax.aspx</a><a href="http://msdn.microsoft.com/en-us/library/5ast78ax.aspx">.</a></p>
<p>Voisimme kirjoittaa nyt C#-kommentit HelloWorld-ohjelman alkuun
seuraavasti:</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
/// @author  Antti-Jussi Lakanen
/// @version 28.8.2012
///
/// <summary>
/// Esimerkkiohjelma, joka tulostaa tekstin "Hello World!"
/// </summary></p>
<p>public class HelloWorld 
{
  /// <summary>
  /// Pääohjelma, joka hoitaa varsinaisen tulostamisen.
  /// </summary>
  /// <param name="args">Ei käytössä</param>
  public static void Main(string[] args) 
  { // Suoritus alkaa siis tästä, ohjelman "entry point"
    System.Console.WriteLine("Hello World!"); // Tämä lause tulostaa ruudulle
  } // Ohjelman suoritus päättyy tähän
}
```</p>
<p>Ohjelman alussa kerrotaan kohteen tekijän nimi. Tämän jälkeen tulee
ensimmäinen dokumentaatiokommentti (huomaa kolme vinoviivaa), joka on
lyhyt ja ytimekäs kuvaus tästä luokasta. Huomaa, että jossain
dokumentaation tiivistelmissä näytetään vain tuo ensimmäinen virke.
[DOC] [HYV]</p>
<p><em>Dokumentointi on erittäin keskeinen osa ohjelmistotyötä</em>. Luokkien ja
koodirivien määrän kasvaessa dokumentointi helpottaa niin omaa
työskentelyä kuin tulevien käyttäjien ja ylläpitäjien tehtävää.
Dokumentoinnin tärkeys näkyy muun muassa siinä, että jopa 40-60%
ylläpitäjien ajasta kuluu muokattavan ohjelman ymmärtämiseen.
[KOSK][KOS]</p>
<h1>3. Algoritmit</h1>
<blockquote>
<p>“First, solve the problem. Then, write the code.” - John Johnson</p>
</blockquote>
<h2>3.1 Mikä on algoritmi?</h2>
<p>Pyrittäessä kirjoittamaan koneelle kelpaavia ohjeita joudutaan
suoritettavana oleva toimenpide kirjaamaan sarjana yksinkertaisia
toimenpiteitä. Toimenpidesarjan tulee olla yksikäsitteinen, eli sen
tulee joka tilanteessa tarjota yksi ja vain yksi tapa toimia, eikä siinä
saa esiintyä ristiriitaisuuksia. Yksikäsitteistä kuvausta tehtävän
ratkaisuun tarvittavista toimenpiteistä kutsutaan algoritmiksi.</p>
<p>Ohjelman kirjoittaminen voidaan aloittaa hahmottelemalla tarvittavat
algoritmit eli kirjaamalla lista niistä toimenpiteistä, joita tehtävän
suoritukseen tarvitaan:</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
Kahvin keittäminen:</p>
<ol>
<li>Täytä pannu vedellä.</li>
<li>Keitä vesi.</li>
<li>Lisää kahvijauhot.</li>
<li>Anna tasaantua.</li>
<li>Tarjoile kahvi.
```</li>
</ol>
<p>Algoritmi on yleisesti ottaen mahdollisimman pitkälle tarkennettu
toimenpidesarja, jossa askel askeleelta esitetään yksikäsitteisessä
muodossa ne toimenpiteet, joita asetetun ongelman ratkaisuun tarvitaan.</p>
<h2>3.2 Tarkentaminen</h2>
<p>Kun tarkastellaan lähes mitä tahansa tehtävänantoa, huomataan, että
tehtävän suoritus koostuu selkeästi toisistaan eroavista osatehtävistä.
Se, miten yksittäinen osatehtävä ratkaistaan, ei vaikuta muiden
osatehtävien suorittamiseen. Vain sillä, että kukin osasuoritus tehdään,
on merkitystä. Esimerkiksi pannukahvinkeitossa jokainen osatehtävä
voidaan jakaa edelleen osasiin:</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
Kahvinkeitto:</p>
<ol>
<li>Täytä pannu vedellä:
  1.1.  Pistä pannu hanan alle.
  1.2.  Avaa hana.
  1.3.  Anna veden valua, kunnes vettä on riittävästi.</li>
<li>Keitä vesi:
  2.1.  Aseta pannu hellalle.
  2.2.  Kytke virta keittolevyyn.
  2.3.  Anna lämmetä, kunnes vesi kiehuu.</li>
<li>Lisää kahvinporot:
  3.1.  Mittaa kahvinporot.
  3.2.  Sekoita kahvinporot kiehuvaan veteen.</li>
<li>Anna tasaantua:
  4.1.  Odota, kunnes suurin osa valmiista kahvista on vajonnut
        pannun pohjalle.</li>
<li>Tarjoile kahvi:
  5.1.  Tämä sitten onkin jo oma tarinansa...
```</li>
</ol>
<p>Edellä esitetyn kahvinkeitto-ongelman ratkaisu esitettiin jakamalla
ratkaisu viiteen osavaiheeseen. Ratkaisun algoritmi sisältää viisi
toteutettavaa lausetta. Kun näitä viittä lausetta tarkastellaan
lähemmin, osoittautuu, että niistä kukin on edelleen jaettavissa
osavaiheisiin, eli ratkaisun pääalgoritmi voidaan jakaa edelleen
alialgoritmeiksi, joissa askel askeleelta esitetään, kuinka kukin
osatehtävä ratkaistaan.</p>
<p>Algoritmien kirjoittaminen osoittautuu hierarkkiseksi prosessiksi, jossa
aluksi tehtävä jaetaan osatehtäviin, joita edelleen tarkennetaan, kunnes
kukin osatehtävä on niin yksinkertainen, ettei sen suorittamisessa enää
ole mitään moniselitteistä.</p>
<h2>3.3 Yleistäminen</h2>
<p>Eräs tärkeä algoritmien kirjoittamisen vaihe on yleistäminen. Tällöin
valmiiksi tehdystä algoritmista pyritään paikantamaan kaikki alunperin
annetusta tehtävästä riippuvat tekijät, ja pohditaan voitaisiinko ne
kenties kokonaan poistaa tai korvata joillakin yleisemmillä tekijöillä.</p>
<h2>3.4 Harjoitus</h2>
<p>Tarkastele edellä esitettyä algoritmia kahvin keittämiseksi ja luo
vastaava algoritmi teen keittämiseksi. Vertaile algoritmeja: mitä samaa
ja mitä eroa niissä on? Onko mahdollista luoda algoritmi, joka
yksiselitteisesti selviäisi sekä kahvin että teen keitosta? Onko
mahdollista luoda algoritmi, joka saman tien selviytyisi maitokaakosta
ja rommitotista?</p>
<h2>3.5 Peräkkäisyys</h2>
<p>Kuten luvussa 1 olevassa reseptissä ja muissakin ihmisille
kirjoitetuissa ohjeissa, niin myös tietokoneelle esitetyt ohjeet luetaan
ylhäältä alaspäin, ellei muuta ilmoiteta. Esimerkiksi ohjeen lumiukon
piirtämisestä voisi esittää yksinkertaistettuna alla olevalla tavalla.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Piirrä säteeltään 20cm kokoinen ympyrä koordinaatiston pisteeseen (20, 80)
Piirrä säteeltään 15cm kokoinen ympyrä edellisen ympyrän päälle
Piirrä säteeltään 10cm kokoinen ympyrä edellisen ympyrän päälle</code></p>
<p>Yllä oleva koodi ei ole vielä mitään ohjelmointikieltä, mutta se
sisältää jo ajatuksen siitä kuinka lumiukko voitaisiin tietokoneella
piirtää. Piirrämme lumiukon C#-ohjelmointikielellä seuraavassa luvussa.</p>
<h1>4. Yksinkertainen graafinen C#-ohjelma</h1>
<p>Seuraavissa esimerkeissä käytetään Jyväskylän yliopistossa
kehitettyä<em>Jypeli-ohjelmointikirjastoa.</em> Kirjaston voit ladata koneelle
osoitteesta</p>
<p><a href="https://trac.cc.jyu.fi/projects/npo/wiki/LataaJypeli">https://trac.cc.jyu.fi/projects/npo/wiki/LataaJypeli</a>,</p>
<p>josta löytyy myös ohjeet kirjaston asennukseen ja käyttöön. Huomaa, että
tietokoneellasi tulee olla asennettuna .NET Framework 4 sekä XNA Game
Studio 4, jotta graafinen ohjelma voidaan kääntää. .NET-frameworkin
asennusohjeet löytyvät osoitteesta</p>
<p><a href="https://trac.cc.jyu.fi/projects/ohj1/wiki/dotnet-tyokalut">https://trac.cc.jyu.fi/projects/ohj1/wiki/dotnet-tyokalut</a>.</p>
<p>Vaikka tässä kohtaa emme vielä Visual Studio 2010 -kehitysympäristöä
tarvitsekaan, on sen asentaminen tässä kohtaa myös viisasta, sillä
Visual Studio tulisi olla asennettuna ennen XNA:n ja Jypelin
asentamista.</p>
<h2>4.1 Mikä on kirjasto?</h2>
<p>C#-ohjelmat koostuvat luokista. Luokat taas sisältävät metodeja (ja
aliohjelmia), jotka suorittavat tehtäviä ja mahdollisesti palauttavat
arvoja suoritettuaan näitä tehtäviä. Metodi voisi esimerkiksi laskea
kahden luvun summan ja palauttaa tuloksen tai piirtää ohjelmoijan
haluaman kokoisen ympyrän. Samaan asiaan liittyviä metodeja kootaan
luokkaan ja luokkia kootaan edelleen kirjastoiksi. Idea kirjastoissa on,
ettei kannata tehdä uudelleen sitä minkä joku on jo tehnyt. Toisin
sanoen, pyörää ei kannata keksiä uudelleen.</p>
<p>C#-ohjelmoijan kannalta oleellisin kirjasto on .NET Framework
luokkakirjasto. Luokkakirjaston dokumentaatioon (documentation)
kannattaa tutustua, sillä sieltä löytyy monia todella hyödyllisiä
metodeja. Dokumentaatio löytyy Microsoftin sivuilta osoitteesta</p>
<p><a href="http://msdn.microsoft.com/en-us/library/ms229335.aspx">http://msdn.microsoft.com/en-us/library/ms229335.aspx</a>.</p>
<p><em>Dokumentaatio =</em> Sisältää tiedot kaikista kirjaston luokista ja niiden
metodeista (ja aliohjelmista). Löytyy useimmiten ainakin WWW-muodossa.</p>
<h2>4.2 Jypeli-kirjasto</h2>
<p>Jypeli-kirjaston kehittäminen aloitettiin Jyväskylän yliopistossa
keväällä 2009. Tämän monisteen esimerkeissä käytetään versiota 4.
Jypeli-kirjastoon on kirjoitettu valmiita luokkia ja metodeja siten,
että esimerkiksi fysiikan ja matematiikan ilmiöiden, sekä pelihahmojen
ja liikkeiden ohjelmointi lopulliseen ohjelmaan on helpompaa.</p>
<h2>4.3 Esimerkki: Lumiukko</h2>
<p>Piirretään lumiukko käyttämällä Jypeli-kirjastoa. Mallikoodin vasemmassa
reunassa juoksee myös rivinumerointi, joka ei kuulu koodiin, mutta
helpottaa lukemista.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
01 // Otetaan käyttöön Jyväskylän yliopiston Jypeli-kirjasto
02 using Jypeli;
03 
04 /// @author  Vesa Lappalainen, Antti-Jussi Lakanen 
05 /// @version 22.12.2011
06 ///
07 ///
08 /// &lt;summary&gt;
09 /// Luokka, jossa harjoitellaan piirtämistä lisäämällä ympyröitä ruudulle
10 /// &lt;/summary&gt;
11 public class Lumiukko : PhysicsGame
12 {
13   /// &lt;summary&gt;
14   /// Pääohjelmassa laitetaan "peli" käyntiin Jypelille tyypilliseen tapaan    
15   /// &lt;/summary&gt;
16   /// &lt;param name="args"&gt;Ei käytössä&lt;/param&gt;
17   public static void Main(string[] args)
18   {
19     using (Lumiukko peli = new Lumiukko())
20     {
21       peli.Run();
22     }
23   }
24   
25   /// &lt;summary&gt;
26   /// Piirretään oliot ja zoomataan kamera niin että kenttä näkyy kokonaan.
27   /// &lt;/summary&gt;
28   public override void Begin()
29   {
30     Camera.ZoomToLevel();
31     Level.Background.Color = Color.Black;
32     
33     PhysicsObject p1 = new PhysicsObject(2*100.0, 2*100.0, Shape.Circle);
34     p1.Y = Level.Bottom + 200.0;
35     Add(p1);
36     
37     PhysicsObject p2 = new PhysicsObject(2 * 50.0, 2 * 50.0, Shape.Circle);
38     p2.Y = p1.Y + 100 + 50;
39     Add(p2);
40     
41     PhysicsObject p3 = new PhysicsObject(2 * 30.0, 2 * 30.0, Shape.Circle);
42     p3.Y = p2.Y + 50 + 30;
43     Add(p3);
44   }
45 }</code></p>
<p>Ajettaessa ohjelman tulisi piirtää yksinkertainen lumiukko keskelle
ruutua, kuten alla olevassa kuvassa. 4.3.1 Ohjelman suoritus</p>
<p><img alt="\
 Kuva 2: Lumiukko Jypeli-kirjaston avulla
piirrettynä" src="../src/luentomonistecsUusin_htm_m793342fb.png" /></p>
<p>\</p>
<h3>4.3.1 Ohjelman suoritus</h3>
<p>Ohjelman suoritus aloitetaan aina pääohjelmasta ja sitten edetään rivi
riviltä ylhäältä alaspäin ohjelman loppuun niin kauan kuin lauseita
riittää. Ohjelmassa voi olla myös rakenteita, joissa toistetaan tiettyjä
rivejä useampaan kertaan vain muuttamalla jotain arvoa tai arvoja.
Pääohjelmassa voi olla myös aliohjelmakutsuja jolloin hypätään
pääohjelmasta suorittamaan aliohjelmaa ja palataan sitten takaisin
pääohjelman suoritukseen. Aliohjelmista puhutaan enemmän luvussa 6.</p>
<h3>4.3.2 Ohjelman oleellisemmat kohdat</h3>
<p>Tarkastellaan ohjelman oleellisempia kohtia.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
02 using Jypeli;</code></p>
<p>Aluksi meidän täytyy kertoa kääntäjälle, että haluamme ottaa käyttöön
koko Jypeli-kirjaston. Nyt Jypeli-kirjaston kaikki luokat (ja niiden
metodit) ovat käytettävissämme.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
08 /// &lt;summary&gt;
09 /// Luokka, jossa harjoitellaan piirtämistä lisäämällä ympyröitä ruudulle
10 /// &lt;/summary&gt;
11 public class Lumiukko : PhysicsGame
12 {</code></p>
<p>Rivit 8-10 ovat dokumentaatiokommentteja. Rivillä 11 luodaan
Lumiukko-luokka, joka hieman poikkeaa HelloWorld-esimerkin tavasta luoda
uusi luokka. Tässä kohtaa käytämme ensimmäisen kerran Jypeli-kirjastoa,
ja koodissa kerrommekin, että Lumiukko-luokka, jota juuri olemme
tekemässä, ”perustuu” Jypeli-kirjastossa olevaan PhysicsGame-luokkaan.
Täsmällisemmin sanottuna Lumiukko-luokka peritään PhysicsGame-luokasta.
Tuon PhysicsGame-luokan avulla objektien piirtäminen, myöhemmin
liikuttelu ruudulla ja fysiikan lakien hyödyntäminen on vaivatonta.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
13   /// &lt;summary&gt;
14   /// Pääohjelmassa laitetaan "peli" käyntiin Jypelille tyypilliseen tapaan.    
15   /// &lt;/summary&gt;
16   /// &lt;param name="args"&gt;Ei käytössä&lt;/param&gt;
17   public static void Main(String[] args)
18   {
19     using (Lumiukko peli = new Lumiukko())
20     {
21       peli.Run();
22     }
23   }</code></p>
<p>Myös Main-metodi, eli pääohjelma, on Jypeli-peleissä käytännössä aina
tällainen vakiomuotoinen, joten jatkossa siihen ei tarvitse juurikaan
koskea. Ohitamme tässä vaiheessa pääohjelman sisällön mainitsemalla
vain, että pääohjelmassa Lumiukko-luokasta tehdään uusi olio (eli uusi
”peli”), joka sitten laitetaan käyntiin peli.Run()-kohdassa.
Jypeli-kirjaston rakenteesta johtuen kaikki varsinainen peliin liittyvä
koodi kirjoitetaan omaan aliohjelmaansa, Begin-aliohjelmaan, jota
käsittelemme seuraavaksi.</p>
<p>Tarkasti ottaen Begin alkaa riviltä 29. Ensimmäinen lause on kirjoitettu
riville 30.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
30     Camera.ZoomToLevel();
31     Level.Background.Color = Color.Black;</code></p>
<p>Näistä kahdesta rivistä ensimmäisellä kutsutaan Camera-luokan
ZoomToLevel-aliohjelmaa, joka pitää huolen siitä, että ”kamera” on
kohdistettuna ja zoomattuna oikeaan kohtaan. Aliohjelma ei ota vastaan
parametreja, joten sulkujen sisältö jää tyhjäksi. Toisella rivillä
muutetaan taustan väri.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
33     PhysicsObject p1 = new PhysicsObject(2*100.0, 2*100.0, Shape.Circle);
34     p1.Y = Level.Bottom + 200.0;
35     Add(p1);</code></p>
<p>Näiden kolmen rivin aikana luomme uuden fysiikkaolio-ympyrän, annamme
sille säteen, y-koordinaatin, sekä lisäämme sen ”pelikentälle”, eli
näkyvälle alueelle valmiissa ohjelmassa.</p>
<p>Tarkemmin sanottuna luomme uuden PhysicsObject-<em>olion</em> eli
PhysicsObject-luokan <em>ilmentymän</em>, jonka nimeksi annamme p1.
PhysicsObject-oliot ovat pelialueella liikkuvia olioita, jotka
noudattavat fysiikan lakeja. Sulkujen sisään laitamme tiedon siitä,
millaisen objektin haluamme luoda - tässä tapauksessa leveys ja korkeus
(Jypeli-mitoissa, ei pikseleissä), sekä olion muoto. Teemme siis
ympyrän, jonka säde on 100 (leveys 2 * 100 ja korkeus 2 * 100). Muita
Shape-kokoelmasta löytyviä muotoja ovat muiden muassa kolmio, neliö,
sydän jne. Olioista puhutaan lisää luvussa 8.</p>
<p>Kokeile itse muuttaa olion muotoa!</p>
<p>Seuraavalla rivillä asetetaan olion paikka Y-arvon avulla. Huomaa että Y
kirjoitetaan isolla kirjaimella. Tämä on p1-olion ominaisuus,
attribuutti. X-koordinaattia meidän ei tarvitse tässä erikseen asettaa,
se on oletusarvoisesti 0 ja se kelpaa meille. Saadaksemme ympyrät
piirrettyä oikeille paikoilleen, täytyy meidän laskea koordinaattien
paikat. Oletuksena ikkunan keskipiste on koordinaatiston origo eli piste
(0, 0). x-koordinaatin arvot kasvavat oikealle ja y:n arvot ylöspäin,
samoin kuin ”normaalissa” koulusta tutussa koordinaatistossa.</p>
<p>Peliolio täytyy aina lisätä kentälle, ennen kuin se saadaan näkyviin.
Tämä tapahtuu Add-metodin avulla, joka ottaa parametrina kentälle
lisättävän olion nimen (tässä p1).</p>
<p>Metodeille annettavia tietoja sanotaan <em>parametreiksi</em> (parameter).
ZoomToLevel-metodi ei ota vastaan yhtään parametria, mutta Add-metodi
sen sijaan ottaa yhden parametrin: PhysicsObject-tyyppisen olion.
Add-metodille voidaan antaa toinenkin parametri: <em>taso</em>, jolle olio
lisätään. Tasojen avulla voidaan hallita, mitkä oliot lisätään
päällimmäiseksi. Tasoparametri voidaan kuitenkin jättää antamatta,
jolloin ohjelma itse päättää tasojen parhaan järjestyksen. Parametrit
kirjoitetaan metodin nimen perään sulkeisiin ja ne erotetaan toisistaan
pilkuilla.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
MetodinNimi(parametri1, parametri2,..., parametriX);</code></p>
<p>Seuraavien rivien aikana luomme vielä kaksi ympyrää vastaavalla tavalla,
mutta vaihtaen sädettä ja ympyrän koordinaatteja.</p>
<p>Esimerkissä koordinaattien laskemiseen on käytetty C#:n <em>aritmeettisia
operaatioita</em>. Voisimme tietenkin laskea koordinaattien pisteet myös
itse, mutta miksi tehdä niin jos tietokone voi laskea pisteet
puolestamme? C#:n aritmeettiset perusoperaatiot ovat summa (+),
vähennys (-), kerto (*), jako (/) ja jakojäännös (%). Aritmeettisista
operaatioista puhutaan lisää muuttujien yhteydessä kohdassa 7.7.1.</p>
<p>Keskimmäinen ympyrä tulee alimman ympyrän yläpuolelle niin, että ympyrät
sivuavat toisiaan. Keskimmäisen ympyrän keskipiste sijoittuu siis siten,
että sen x-koordinaatti on 0 ja y-koordinaatti on <em>alimman ympyrän
paikka</em> +<em>alimman ympyrän säde + keskimmäisen ympyrän säde</em>. Kun
haluamme, että keskimmäisen ympyrän säde on 50, niin silloin
keskimmäisen ympyrän keskipiste tulee kohtaan (0, p1.Y + 100 + 50) ja se
piirretään lauseella:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
PhysicsObject p2 = new PhysicsObject(2 * 50.0, 2 * 50.0, Shape.Circle);
p2.Y = p1.Y + 100 + 50;
Add(p2);</code></p>
<p>Huomaa, että fysiikkaolion Y-ominaisuuden asettamisen (<em>set</em>) lisäksi
voimme myös lukea tai pyytää (<em>get</em>) kyseisen ominaisuuden arvon. Yllä
teemme sen kirjoittamalla yksinkertaisesti sijoitusoperaattorin oikealle
puolelle p1.Y.</p>
<p>Seuraava kuva havainnollistaa ensimmäisen ja toisen pallon asettelua.</p>
<p><img alt="\
 Kuva 3: Lumiukon kaksi ensimmäistä palloa asemoituina
paikoilleen." src="../src/luentomonistecsUusin_htm_m58b94a29.png" /></p>
<p>\
 \
 \</p>
<p>Ylin ympyrä sivuaa sitten taas keskimmäistä ympyrää. Harjoitustehtäväksi
jätetään laskea ylimmän ympyrän koordinaatit, kun ympyrän säde on 30.</p>
<p>Kaikki tiedot luokista, luokkien metodeista sekä siitä mitä parametreja
metodeille tulee antaa löydät käyttämäsi kirjaston dokumentaatiosta.
Jypelin luokkadokumentaatio löytyy osoitteesta:
<a href="http://kurssit.it.jyu.fi/npo/material/latest/documentation/html/">http://kurssit.it.jyu.fi/npo/material/latest/documentation/html/</a>.</p>
<h2>4.4 Harjoitus</h2>
<p>Etsi Jypeli-kirjaston dokumentaatiosta RandomGen-luokka. Mitä tietoa
löydät NextInt(int min, int max)-metodista? Mitä muita metodeja luokassa
on?</p>
<h2>4.5 Kääntäminen ja luokkakirjastoihin viittaaminen</h2>
<p>Jotta Lumiukko-esimerkkiohjelma voitaisiin nyt kääntää C#-kääntäjällä,
tulee Jypeli-kirjasto olla tallennettuna tietokoneelle. Jypeli käyttää
XNA-kirjaston lisäksi vapaan lähdekoodin fysiikka- ja
matematiikkakirjastoja. Fysiikka- ja matematiikkakirjastot on
sisäänrakennettuina Jypeli-kirjastoon.</p>
<p>Ennen kääntämistä kopioi seuraavat tiedostot kurssin kotisivuilta
(<a href="https://trac.cc.jyu.fi/projects/ohj1/wiki/csharpCommandLine">https://trac.cc.jyu.fi/projects/ohj1/wiki/csharpCommandLine</a>)
samaan kansioon Lumiukko.cs-tiedoston kanssa.</p>
<ul>
<li>Jypeli.dll</li>
</ul>
<p>Meidän täytyy vielä välittää kääntäjälle tieto siitä, että
Jypeli-kirjastoa tarvitaan Lumiukko-koodin kääntämiseen. Lisäksi
annetaan kääntäjälle tieto siitä, että ohjelma tehdään <em>32-bittisille</em>
järjestelmille (x86). Tämä tehdään csc-ohjelman
<code>/reference</code>{.terminaali-western}-parametrin avulla. Lisäksi tarvitaan
referenssi Jypelin käyttämään XNA-kirjastoon. Kirjoita nyt
komentoriville(kaikki rivit samalle riville niin, että /-viivan edessä
on yksi välilyönti)</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
csc Lumiukko.cs /reference:Jypeli.dll;"%XNAGSv4%\References\Windows\x86\Microsoft.Xna.Framework.Game.dll" /platform:x86</code></p>
<p>Jos käyttöjärjestelmäsi ei tunnista csc-komentoa, niin kertaa luvussa 2
olevat ohjeet komennon asettamisesta
<code>PATH</code>{.terminaali-western}-ympäristömuuttujan poluksi.</p>
<p>Vinkki! Yllä esitelty kääntämiskomento on varsin pitkä. Asioiden
helpottamiseksi voit kirjoittaa tiedoston csk.bat, joka sisältää
seuraavan tekstin (komento on yksi pitkä rivi):\
 \
@"%WINDIR%\Microsoft.NET\Framework\v4.0.30319\csc" %*
/reference:Jypeli.dll;"%XNAGSv4%References\Windows\x86\Microsoft.Xna.Framework.Game.dll";"%XNAGSv4%References\Windows\x86\Microsoft.Xna.Framework.dll"
/platform:x86 /define:WINDOWS\
 \
Tämä asettaa puolestasi reference ja platform -parametrit. Varmista,
että tekemäsi csk.bat-tiedosto on ”polussa”. Tämän jälkeen kääntäminen
onnistuu yksinkertaisemmin: csk OhjelmanNimi.cs</p>
<h1>5. Lähdekoodista prosessorille</h1>
<h2>5.1 Kääntäminen</h2>
<p>Tarkastellaan nyt tarkemmin sitä kuinka C#-lähdekoodi muuttuu lopulta
prosessorin ymmärtämään muotoon. Kun ohjelmoija luo ohjelman
lähdekoodin, joka käyttää <em>.NET Framework</em> -ympäristöä, tapahtuu
kääntäminen sisäisesti kahdessa vaiheessa. Ohjelma käännetään ensin
eräänlaiselle välikielelle, <em>MSIL</em>:lle (Microsoft Intermediate
Language), joka ei ole vielä suoritettavissa millään
käyttöjärjestelmällä. Tästä välivaiheen koodista käännetään ajon aikana
valmis ohjelma halutulle käyttöjärjestelmälle, kuten Mac Os X:lle tai
Linuxille, niin sanotulla <em>JIT-kääntäjällä</em> (Just-In-Time). JIT-kääntäjä
muuntaa välivaiheen koodin juuri halutulle käyttöjärjestelmälle
sopivaksi koodiksi nimenomaan ohjelmaa ajettaessa - tästä tulee nimi
”just-in-time”.</p>
<p>Ennen ensimmäistä kääntämistä kääntäjä tarkastaa, että koodi on
syntaksiltaan oikein. [VES][KOS]</p>
<p>Kääntäminen tehtiin Windowsissa komentorivillä (Command Prompt)
käyttämällä komentoa</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
csc Tiedostonnimi.cs</code></p>
<p>tai hyödyntämällä edellisessä luvussa esiteltyä komentojonoa</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
csk Tiedostonnimi.cs</code></p>
<h2>5.2 Suorittaminen</h2>
<p>C#:n tuottaa siis lähdekoodista suoritettavan (tai ”ajettavan”)
tiedoston. Tämä tiedosto on käyttöjärjestelmäriippuvainen, ja
suoritettavissa vain sillä alustalla, johon käännös on tehty. Toisin
sanoen, Windows-ympäristössä käännetyt ohjelmat eivät ole ajettavissa OS
X -käyttöjärjestelmässä, ja toisin päin.</p>
<p>Toisin kuin C#, eräät toiset ohjelmointikielet tuottavat
käyttöjärjestelmäriippumatonta koodia. Esimerkiksi <em>Java</em>-kielessä
kääntäjän tuottama tiedosto on niin sanottua <em>tavukoodia</em>, joka on
käyttöjärjestelmäriippumatonta koodia. Tavukoodin suorittamiseen
tarvitaan Java-virtuaalikone (Java Virtual Machine). Java-virtuaalikone
on oikeaa tietokonetta matkiva ohjelma, joka tulkkaa tavukoodia ja
suorittaa sitä sitten kohdekoneen prosessorilla. Tässä on merkittävä ero
perinteisiin käännettäviin kieliin (esimerkiksi C ja C++), joissa
käännös on tehtävä erikseen jokaiselle eri laitealustalle. [VES][KOS]</p>
<p>\
 \</p>
<h1>6. Aliohjelmat</h1>
<blockquote>
<p>“Copy and paste is a design error.” - David Parnas</p>
</blockquote>
<p>Pääohjelman lisäksi ohjelma voi sisältää muitakin aliohjelmia.
Aliohjelmaa <em>kutsutaan</em> pääohjelmasta, metodista tai toisesta
aliohjelmasta suorittamaan tiettyä tehtävää. Aliohjelmat voivat saada
parametreja ja palauttaa arvon, kuten metoditkin. Pohditaan seuraavaksi
mihin aliohjelmia tarvitaan.</p>
<p>Jos tehtävänämme olisi piirtää useampi lumiukko, niin tämänhetkisellä
tietämyksellämme tekisimme todennäköisesti jonkin alla olevan kaltaisen
ratkaisun.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
01 using Jypeli;
02 
03 /// &lt;summary&gt;
04 /// Piirretään lumiukko.
05 /// &lt;/summary&gt;
06 public class Lumiukko : PhysicsGame
07 {
08   /// &lt;summary&gt;
09   /// Pääohjelmassa peli käyntiin.
10   /// &lt;/summary&gt;
11   /// &lt;param name="args"&gt;Ei käytössä.&lt;/param&gt;
12   public static void Main(String[] args)
13   {
14     using (Lumiukko game = new Lumiukko())
15     {
16       game.Run();
17     }
18   }
19   
20   /// &lt;summary&gt;
21   /// Aliohjelma, jossa
22   /// piirretään ympyrät.
23   /// &lt;/summary&gt;
24   public override void Begin()
25   {
26     Camera.ZoomToLevel();
27     Level.Background.Color = Color.Black;
28     
29     PhysicsObject p1, p2, p3;
30     
31     // Eka ukko
32     p1 = new PhysicsObject(2 * 100.0, 2 * 100.0, Shape.Circle);
33     p1.Y = Level.Bottom + 200.0;
34     Add(p1);
35     
36     p2 = new PhysicsObject(2 * 50.0, 2 * 50.0, Shape.Circle);
37     p2.Y = p1.Y + 100 + 50;
38     Add(p2);
39     
40     p3 = new PhysicsObject(2 * 30.0, 2 * 30.0, Shape.Circle);
41     p3.Y = p2.Y + 50 + 30;
42     Add(p3);
43     
44     // Toinen ukko
45     p1 = new PhysicsObject(2 * 100.0, 2 * 100.0, Shape.Circle);
46     p1.X = 200;
47     p1.Y = Level.Bottom + 300.0;
48     Add(p1);
49     
50     p2 = new PhysicsObject(2 * 50.0, 2 * 50.0, Shape.Circle);
51     p2.X = 200;
52     p2.Y = p1.Y + 100 + 50;
53     Add(p2);
54     
55     p3 = new PhysicsObject(2 * 30.0, 2 * 30.0, Shape.Circle);
56     p3.X = 200;
57     p3.Y = p2.Y + 50 + 30;
58     Add(p3);
59   }
60 }</code></p>
<p>Huomataan, että ensimmäisen ja toisen lumiukon piirtäminen tapahtuu
lähes samanlaisilla koodinpätkillä. Itse asiassa ainoa ero on, että
jälkimmäisen lumiukon pallot saavat ensimmäisestä lumiukosta eroavat
koordinaatit. Toisaalta voisimme kirjoittaa koodin myös niin, että
lumiukon alimman pallon keskipiste tallennetaan <em>muuttujiin</em> x ja y.
Näiden pisteiden avulla voimme sitten laskea muiden pallojen paikat.
Määritellään heti alussa myös p1, p2 ja p3 PhysicsObject-olioiksi.
Rivinumerointi on tässä jätetty pois selvyyden vuoksi. Luvun lopussa
korjattu ohjelma esitellään kokonaisuudessaan rivinumeroinnin kanssa.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
double x, y;
PhysicsObject p1, p2, p3;</p>
<p>// Tehdään ensimmäinen lumiukko
x = 0; y = Level.Bottom + 200.0;
p1 = new PhysicsObject(2<em>100.0, 2</em>100.0, Shape.Circle);
p1.X = x;
p1.Y = y;
Add(p1);</p>
<p>p2 = new PhysicsObject(2 * 50.0, 2 * 50.0, Shape.Circle);
p2.X = x;
p2.Y = y + 100 + 50; // y + 1. pallon säde + 2. pallon säde
Add(p2);</p>
<p>p3 = new PhysicsObject(2 * 30.0, 2 * 30.0, Shape.Circle);
p3.X = x;
p3.Y = y + 100 + 2 * 50 + 30; // y + 1. pallon säde + 2. halk. + 3. säde
Add(p3);
```</p>
<p>Vastaavasti toiselle lumiukolle: asetetaan vain x:n ja y:n arvot
oikeiksi.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
// Tehdään toinen lumiukko
x = 200; y = Level.Bottom + 300.0;
p1 = new PhysicsObject(2 * 100.0, 2 * 100.0, Shape.Circle);
p1.X = x;
p1.Y = y;
Add(p1);</p>
<p>p2 = new PhysicsObject(2 * 50.0, 2 * 50.0, Shape.Circle);
p2.X = x;
p2.Y = y + 100 + 50;
Add(p2);</p>
<p>p3 = new PhysicsObject(2 * 30.0, 2 * 30.0, Shape.Circle);
p3.X = x;
p3.Y = y + 100 + 2*50 + 30;
Add(p3);
```</p>
<p>Tarkastellaan nyt muutoksia hieman tarkemmin.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
double x, y;</code></p>
<p>Yllä olevalla rivillä esitellään kaksi liukuluku<em>tyyppistä</em> <em>muuttujaa</em>.
Liukuluku on eräs tapa esittää <em>reaalilukuja</em> tietokoneissa. C#:ssa
jokaisella muuttujalla on oltava tyyppi ja eräs liukulukutyyppi C#:ssa
on double. Muuttujista ja niiden tyypeistä puhutaan lisää luvussa 7.</p>
<p><em>Liukuluku</em> (floating point) = Tietokoneissa käytettävä esitysmuoto
reaaliluvuille. Tarkempaa tietoa liukuluvuista löytyy luvusta 26.</p>
<p>\</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
x = 0; y = Level.Bottom + 200.0;</code></p>
<p>Yllä olevalla rivillä on kaksi lausetta. Ensimmäisellä asetetaan
muuttujaan x arvo 0 ja toisella muuttujaan y arvo 50 (jos Level.Bottom
sattuu olemaan vaikka -150). Nyt voimme käyttää lumiukon pallojen
laskentaan näitä muuttujia.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
x = 300; y = Level.Bottom + 300.0;</code></p>
<p>Vastaavasti yllä olevalla rivillä asetetaan nyt muuttujiin uudet arvot,
joita käytetään seuraavan lumiukon pallojen paikkojen laskemiseen.
Huomaa, että y-koordinaatti saa negatiivisen arvon, jolloin lumiukon
alimman pallon keskipiste painuu kuvaruudun keskitason alapuolelle.</p>
<p>Nyt alimman pallon x-koordinaatiksi sijoitetaankin <em>muuttuja</em> x, ja
vastaavasti y-koordinaatin arvoksi asetetaan <em>muuttuja</em> y, ja muiden
pallojen sijainnit lasketaan ensimmäisen pallon koordinaattien
perusteella.</p>
<p><img alt="\
 Kuva 4: Kaksi
lumiukkoa." src="../src/luentomonistecsUusin_htm_m12339dd1.png" /></p>
<p>\
 Näiden muutosten jälkeen molempien lumiukkojen varsinainen piirtäminen
tapahtuu nyt <strong>täysin samalla koodilla</strong>.</p>
<p>Uusien lumiukkojen piirtäminen olisi nyt jonkin verran helpompaa, sillä
meidän ei tarvitse kuin ilmoittaa ennen piirtämistä uuden lumiukon
paikka ja varsinaisen lumiukkojen piirtäminen onnistuisi kopioimilla ja
liittämällä koodia (copy-paste). Kuitenkin, jos koodia kirjoittaessa
joutuu tekemään suoraa kopiointia, pitäisi pysähtyä miettimään, että
onko tässä mitään järkeä.</p>
<p>Kahden lumiukon tapauksessa tämä vielä onnistuu ilman, että koodin määrä
kasvaa kohtuuttomasti, mutta entä jos meidän pitäisi piirtää 10 tai 100
lumiukkoa? Kuinka monta riviä ohjelmaan tulisi silloin? Kun lähes
samanlainen koodinpätkä tulee useampaan kuin yhteen paikkaan, on
useimmiten syytä muodostaa siitä oma <em>aliohjelma</em>. Koodin monistaminen
moneen paikkaan lisäisi vain koodirivien määrää, tekisi ohjelman
ymmärtämisestä vaikeampaa ja vaikeuttaisi testaamista.</p>
<p>\
 \</p>
<p>Lisäksi jos monistetussa koodissa olisi vikaa, jouduttaisiin korjaukset
tekemään myös useampaan paikkaan. Hyvän ohjelman yksi mitta (kriteeri)
onkin, että jos jotain pitää muuttaa, niin kohdistuvatko muutokset
kohdistuvat vain yhteen paikkaan (hyvä) vai joudutaanko muutoksia
tekemään useaan paikkaan (huono).</p>
<h2>6.1 Aliohjelman kutsuminen</h2>
<p>Haluamme siis aliohjelman, joka piirtää meille lumiukon tiettyyn
pisteeseen. Kuten metodeille, myös aliohjelmalle viedään parametrien
avulla sen tarvitsemaa tietoa. Parametreina tulisi viedä vain
minimaaliset tiedot, joilla aliohjelman tehtävä saadaan suoritettua.</p>
<p>Sovitaan, että aliohjelmamme piirtää aina samankokoisen lumiukon
haluamaamme pisteeseen. Mitkä ovat ne välttämättömät tiedot, jotka
aliohjelma tarvitsee piirtääkseen lumiukon?</p>
<p>Aliohjelma tarvitsee tiedon <em>mihin</em> pisteeseen lumiukko piirretään.
Viedään siis parametrina lumiukon alimman pallon keskipiste. Muiden
pallojen paikat voidaan laskea tämän pisteen avulla. Lisäksi tarvitaan
yksi Game-tyyppinen parametri, jotta aliohjelmaamme voisi kutsua myös
toisesta ohjelmasta. Nämä parametrit riittävät lumiukon piirtämiseen.</p>
<p>Kun aliohjelmaa käytetään ohjelmassa, sanotaan, että aliohjelmaa
<em>kutsutaan</em>. Kutsu tapahtuu kirjoittamalla aliohjelman nimi ja antamalla
sille parametrit. Aliohjelmakutsun erottaa metodikutsusta vain se, että
metodi liittyy aina tiettyyn olioon. Esimerkiksi pallo-olio p1
voitaisiin poistaa pelikentällä kutsumalla metodia Destroy(), eli
kirjoittaisimme:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
p1.Destroy();</code></p>
<p>Toisin sanoen metodeja kutsuttaessa täytyy ensin kirjoittaa sen olion
nimi, jonka metodia kutsutaan, ja sen jälkeen pisteellä erotettuna
kirjoittaa haluttu metodin nimi. Sulkujen sisään tulee luonnollisesti
tarvittavat parametrit. Yllä olevan esimerkin Destroy-metodi ei ota
vastaan yhtään parametria.</p>
<p>Päätetään, että aliohjelman nimi on PiirraLumiukko. Sovitaan myös, että
aliohjelman ensimmäinen parametri on tämä peli, johon lumiukko ilmestyy
(kirjoitetaan this). Toinen parametri on lumiukon alimman pallon
keskipisteen x-koordinaatti ja kolmas parametri lumiukon alimman pallon
keskipisteen y-koordinaatti. Tällöin kentälle voitaisiin piirtää
lumiukko, jonka alimman pallon keskipiste on (0, Level.Bottom + 200.0),
seuraavalla kutsulla:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
PiirraLumiukko(this, 0, Level.Bottom + 200.0);</code></p>
<p>Kutsussa voisi myös ensiksi mainita sen luokan nimen mistä aliohjelma
löytyy. Tällä kutsulla aliohjelmaa voisi kutsua myös muista luokista,
koska määrittelimme Lumiukot-luokan julkiseksi (public).</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Lumiukot.PiirraLumiukko(this, 0, Level.Bottom + 200.0);</code></p>
<p>Vaikka tämä muoto muistuttaa jo melko paljon metodin kutsua on ero
kuitenkin selvä. Metodia kutsuttaessa toimenpide tehdään aina <em>tietylle
oliolle</em>, kuten p1.Destroy() tuhoaa juuri sen pallon, johon p1-olio
viittaa. Pallojahan voi tietenkin olla myös muita erinimisiä (kuten
esimerkissämme onkin). Alla olevassa aliohjelmakutsussa kuitenkin
käytetään vain luokasta Lumiukot löytyvää PiirraLumiukko-aliohjelmaa.</p>
<p>Jos olisimme toteuttaneet jo varsinaisen aliohjelman, piirtäisi Begin
meille nyt kaksi lumiukkoa.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
/// <summary>
/// Kutsutaan PiirraLumiukko-aliohjelmaa
/// sopivilla parametreilla.
/// </summary>
public override void Begin()
{
  Camera.ZoomToLevel();
  Level.Background.Color = Color.Black;</p>
<p>PiirraLumiukko(this, 0, Level.Bottom + 200.0);
  PiirraLumiukko(this, 200.0, Level.Bottom + 300.0);
}
```</p>
<p>Koska PiirraLumiukko-aliohjelmaa ei luonnollisesti vielä ole olemassa,
ei ohjelmamme vielä toimi. Seuraavaksi meidän täytyy toteuttaa itse
aliohjelma, jotta kutsut alkavat toimimaan.</p>
<p>Usein ohjelman toteutuksessa on viisasta edetä juuri tässä
järjestyksessä: suunnitellaan aliohjelmakutsu ensiksi, kirjoitetaan
kutsu sille kuuluvalle paikalle, ja vasta sitten toteutetaan varsinainen
aliohjelman kirjoittaminen.</p>
<p>Lisätietoja aliohjelmien kutsumisesta löydät kurssin wiki-sivulta:
<a href="https://trac.cc.jyu.fi/projects/ohj1/wiki/aliohjelmienKutsuminen">https://trac.cc.jyu.fi/projects/ohj1/wiki/aliohjelmienKutsuminen</a>.</p>
<h2>6.2 Aliohjelman kirjoittaminen</h2>
<p>Ennen varsinaista aliohjelman toiminnallisuuden kirjoittamista täytyy
aliohjelmalle tehdä määrittely (kutsutaan myös esittelyksi,
declaration). Kirjoitetaan määrittely aliohjelmalle, jonka kutsun jo
teimme edellisessä alaluvussa.</p>
<p>Lisätään ohjelmaamme aliohjelman runko. Dokumentoidaan aliohjelma myös
saman tien.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
/// <summary>
/// Kutsutaan PiirraLumiukko-aliohjelmaa
/// sopivilla parametreilla.
/// </summary>
public override void Begin()
{
  Camera.ZoomToLevel();
  Level.Background.Color = Color.Black;</p>
<p>PiirraLumiukko(this, 0, Level.Bottom + 200.0);
  PiirraLumiukko(this, 200.0, Level.Bottom + 300.0);
}</p>
<p>/// <summary>
/// Aliohjelma piirtää lumiukon
/// annettuun paikkaan.
/// </summary>
/// <param name="peli">Peli, johon lumiukko tehdään.</param>
/// <param name="x">Lumiukon alimman pallon x-koordinaatti.</param>
/// <param name="y">Lumiukon alimman pallon y-koordinaatti.</param>
public static void PiirraLumiukko(Game peli, double x, double y)
{
}
```</p>
<p>Alla oleva kuva selvittää aliohjelmakutsun ja aliohjelman määrittelyn
sekä vastinparametrien yhteyttä.</p>
<p><img alt="\
 Kuva 5: Aliohjelmakutsu ja aliohjelman
vastinparametrit." src="../src/luentomonistecsUusin_htm_m48da2e91.png" /></p>
<p>\
 Aliohjelman toteutuksen ensimmäistä riviä</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
public static void PiirraLumiukko(Game peli, double x, double y)</code></p>
<p>sanotaan aliohjelman <em>otsikoksi</em> (header) tai <em>esittelyriviksi</em>. Otsikon
alussa määritellään aliohjelman <em>näkyvyys</em> julkiseksi (public). Kun
näkyvyys on julkinen, niin aliohjelmaa voidaan kutsua eli käyttää myös
muissa luokissa. Aliohjelma määritellään myös staattiseksi (static),
sillä jos emme määrittelisi aliohjelmaa staattiseksi, olisi se
oikeastaan metodi, eli olion toiminto (ks. luku 8.5). Statttinen
aliohjelma pystyy tekemään kaikki toimensa parametreinä tuodun tiedon
perusteella.</p>
<p>Aliohjelmalle on annettu myös palautusarvoksi void, joka tarkoittaa
sitä, että aliohjelma ei palauta mitään arvoa. Aliohjelma voisi
nimittäin myös lopettaessaan palauttaa jonkun arvon, jota tarvitsisimme
ohjelmassamme. Tällaisista aliohjelmista puhutaan luvussa 9.
void-määrityksen jälkeen aliohjelmalle on annettu nimeksi
PiirraLumiukko.</p>
<p>Huomaa! C#:ssa aliohjelmat kirjoitetaan tyypillisesti isolla
alkukirjaimella.</p>
<p>\</p>
<p>Huomaa! Aliohjelmien (ja metodien) nimien tulisi olla verbejä tai
tekemistä ilmaisevia lauseita, esimerkiksi LuoPallo, Siirry,
TormattiinEsteeseen.</p>
<p>Aliohjelman nimen jälkeen ilmoitetaan sulkeiden sisässä aliohjelman
parametrit. Jokaista parametria ennen on ilmoitettava myös parametrin
<em>tietotyyppi</em>. Parametrinä annettiin lumiukon alimman pallon x- ja
y-koordinaatit. Molempien tietotyyppi on double, joten myös
vastinparametrien tyyppien tulee olla double. Annetaan myös nimet
kuvaavasti x ja y.</p>
<p>Tietotyypeistä voit lukea lisää kohdasta 7.2 ja luvusta 8.</p>
<p>Huomaa! Aliohjelman parametrien nimien ei tarvitse olla samoja kuin
kutsussa. Niiden nimet kannattaa kuitenkin olla mahdollisimman kuvaavia.</p>
<p>\</p>
<p>Huomaa! Parametrien tyyppien ei tarvitse olla keskenään samoja, kunhan
kukin parametri on yhteensopiva kutsussa olevan vastinparametrin kanssa.
Esimerkkejä funktioista löydät kurssin wiki-sivulta:
<a href="https://trac.cc.jyu.fi/projects/ohj1/wiki/aliohjelmienKirjoittaminen">https://trac.cc.jyu.fi/projects/ohj1/wiki/aliohjelmienKirjoittaminen</a>.</p>
<p>\</p>
<p>Aliohjelmakutsulla ja aliohjelman määrittelyllä on siis hyvin vahva
yhteys keskenään. Aliohjelmakutsussa annetut tiedot ”sijoitetaan”
kullakin kutsukerralla aliohjelman määrittelyrivillä esitellyille
vastinparametreille. Toisin sanoen, aliohjelmakutsun yhteydessä tapahtuu
väljästi sanottuna seuraavaa.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
aliohjelman Game = this;
aliohjelman x = 200.0;
aliohjelman y = Level.Bottom + 300;</code></p>
<p>Voimme nyt kokeilla ajaa ohjelmaamme. Se toimii (lähtee käyntiin), mutta
ei tietenkään vielä piirrä lumiukkoja, eikä pitäisikään, sillä luomamme
aliohjelma on ”tyhjä”. Lisätään aaltosulkujen väliin varsinainen koodi,
joka pallojen piirtämiseen tarvitaan.</p>
<p>Pieni muutos aikaisempaan versioon kuitenkin tarvitaan. Rivit, joilla
pallot lisätään kentälle, muutetaan muotoon</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
peli.Add(...);</code></p>
<p>missä pisteiden paikalle tulee pallo-olion muuttujan nimi.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
/// <summary>
/// Kutsutaan PiirraLumiukko-aliohjelmaa
/// sopivilla parametreilla.
/// </summary>
public override void Begin()
{
  Camera.ZoomToLevel();
  Level.Background.Color = Color.Black;</p>
<p>PiirraLumiukko(this, 0, Level.Bottom + 200.0);
  PiirraLumiukko(this, 200.0, Level.Bottom + 300.0);
}</p>
<p>/// <summary>
/// Aliohjelma piirtää lumiukon
/// annettuun paikkaan.
/// </summary>
/// <param name="g">Peli, johon lumiukko tehdään.</param>
/// <param name="x">Lumiukon alimman pallon x-koordinaatti.</param>
/// <param name="y">Lumiukon alimman pallon y-koordinaatti.</param>
public static void PiirraLumiukko(Game peli, double x, double y)
{
  PhysicsObject p1, p2, p3;
  p1 = new PhysicsObject(2 * 100.0, 2 * 100.0, Shape.Circle);
  p1.X = x;
  p1.Y = y;
  peli.Add(p1);</p>
<p>p2 = new PhysicsObject(2 * 50.0, 2 * 50.0, Shape.Circle);
  p2.X = x;
  p2.Y = p1.Y + 100 + 50;
  peli.Add(p2);</p>
<p>p3 = new PhysicsObject(2 * 30.0, 2 * 30.0, Shape.Circle);
  p3.X = x;
  p3.Y = p2.Y + 50 + 30;
  peli.Add(p3);
}
```</p>
<p>Varsinaista aliohjelman toiminnallisuutta kirjoittaessa käytämme nyt
parametreille antamiamme nimiä. Alimman ympyrän keskipisteen
koordinaatit saamme nyt suoraan parametreista x ja y, mutta muiden
ympyröiden keskipisteet meidän täytyy laskea alimman ympyrän
koordinaateista. Tämä tapahtuu täysin samalla tavalla kuin edellisessä
esimerkissä. Itse asiassa, jos vertaa aliohjelman sisältöä edellisen
esimerkin koodiin, on se täysin sama.</p>
<p>C#:ssa on tapana aloittaa aliohjelmien ja metodien nimet isolla
kirjaimella ja nimessä esiintyvä jokainen uusi sana alkamaan isolla
kirjaimella. Kirjoitustavasta käytetään termiä PascalCasing. Muuttujat
kirjoitetaan pienellä alkukirjaimella, ja jokainen seuraava sana isolla
alkukirjaimella: esimerkiksi double autonNopeus. Tästä käytetään nimeä
camelCasing. Lisää C#:n nimeämiskäytännöistä voit lukea sivulta</p>
<p><a href="http://msdn.microsoft.com/en-us/library/ms229043.aspx">http://msdn.microsoft.com/en-us/library/ms229043.aspx</a>.</p>
<p>Tarkastellaan seuraavaksi mitä aliohjelmakutsussa tapahtuu.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
PiirraLumiukko(this, 0, Level.Bottom + 200.0);</code></p>
<p>Yllä olevalla kutsulla aliohjelman peli-nimiseen muuttujaan sijoitetaan
this, eli kyseessä oleva peli, x-nimiseen muuttujaan sijoitetaan arvo 0
(liukulukuun voi sijoittaa kokonaislukuarvon) ja aliohjelman muuttujaan
y arvo Level.Bottom + 200.0. Voisimme sijoittaa tietenkin minkä tahansa
muunkin liukuluvun.</p>
<p>Aliohjelmakutsun suorituksessa lasketaan siis ensiksi jokaisen kutsussa
olevan lausekkeen arvo ja sitten lasketut arvot sijoitetaan kutsussa
olevassa järjestyksessä aliohjelman vastinparametreille. Siksi
vastinparametrien pitää olla sijoitusyhteensopivia kutsun lausekkeiden
kanssa. Esimerkin kutsussa lausekkeet ovat yksinkertaisia: muuttujan
nimi (this), kokonaislukuarvo (0) ja reaalilukuarvo ( Level.Bottom +
200.0). Ne voisivat kuitenkin olla kuinka monimutkaisia lausekkeita
tahansa, esimerkiksi näin:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
PiirraLumiukko(this, 22.7+sin(2.4), 80.1-Math.PI);</code></p>
<p>\
 \</p>
<p><em>Lause</em> (statement) ja <em>lauseke</em> (expression) ovat eri asia. Lauseke on
arvojen, aritmeettisten operaatioiden ja aliohjelmien (tai metodien
yhdistelmä), joka evaluoituu tietyksi arvoksi. Lauseke on siis lauseen
osa. Seuraava kuva selventää eroa.</p>
<p><img alt="\
 Kuva 6: Lauseen ja lausekkeen
ero" src="../src/luentomonistecsUusin_htm_m5dfaebb8.png" /></p>
<p>\
 Koska määrittelimme koordinaattien parametrien tyypiksi double,
voisimme yhtä hyvin antaa parametreiksi mitä tahansa muitakin
desimaalilukuja. Täytyy muistaa, että C#:ssa desimaalilukuvakioissa
käytetään pistettä erottamaan kokonaisosa desimaaliosasta.</p>
<p>Kokonaisuudessaan ohjelma näyttää nyt seuraavalta:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
01 using Jypeli;
02 
03 
04 /// @author  Antti-Jussi Lakanen, Vesa Lappalainen
05 /// @version 22.8.2012
06 ///
07 /// &lt;summary&gt;
08 /// Piirretään lumiukkoja ja harjoitellaan aliohjelman käyttöä.
09 /// &lt;/summary&gt;
10 public class Lumiukot : PhysicsGame
11 {
12  /// &lt;summary&gt;
13  /// Pääohjelmassa laitetaan "peli" käyntiin.
14  /// &lt;/summary&gt;
15  /// &lt;param name="args"&gt;Ei käytössä&lt;/param&gt;
16  public static void Main(string[] args)
17  {
18    using (Lumiukot peli = new Lumiukot())
19    {
20     peli.Run();
21    }
22  }
23  
24  /// &lt;summary&gt;
25  /// Kutsutaan PiirraLumiukko-aliohjelmaa
26  /// sopivilla parametreilla.
27  /// &lt;/summary&gt;
28  public override void Begin()
29  {
30   Camera.ZoomToLevel();
31   Level.Background.Color = Color.Black;
32   
33   PiirraLumiukko(this, 0, Level.Bottom + 200.0);
34   PiirraLumiukko(this, 200.0, Level.Bottom + 300.0);
35  }
36
37  /// &lt;summary&gt;
38  /// Aliohjelma piirtää lumiukon
39  /// annettuun paikkaan.
40  /// &lt;/summary&gt;
41  /// &lt;param name="peli"&gt;Peliluokka&lt;/param&gt; 
42  /// &lt;param name="x"&gt;Lumiukon alimman pallon x-koordinaatti.&lt;/param&gt;
43  /// &lt;param name="y"&gt;Lumiukon alimman pallon y-koordinaatti.&lt;/param&gt;
44  public static void PiirraLumiukko(Game peli, double x, double y)
45  {
46    PhysicsObject p1, p2, p3;
47    p1 = new PhysicsObject(2 * 100.0, 2 * 100.0, Shape.Circle);
48    p1.X = x;
49    p1.Y = y;
50    peli.Add(p1);
51    
52    p2 = new PhysicsObject(2 * 50.0, 2 * 50.0, Shape.Circle);
53    p2.X = x;
54    p2.Y = p1.Y + 100 + 50;
55    peli.Add(p2);
56    
57    p3 = new PhysicsObject(2 * 30.0, 2 * 30.0, Shape.Circle);
58    p3.X = x;
59    p3.Y = p2.Y + 50 + 30;
60    peli.Add(p3);
61  }
62 }</code></p>
<p>\
 \</p>
<p>Kutsuttaessa aliohjelmaa hyppää ohjelman suoritus välittömästi
parametrien sijoitusten jälkeen kutsuttavan aliohjelman ensimmäiselle
riville ja alkaa suorittamaan aliohjelmaa kutsussa määritellyillä
parametreilla. Kun päästään aliohjelman koodin loppuun palataan
jatkamaan kutsun jälkeisestä seuraavasta lausekkeesta. Esimerkissämme
kun ensimmäinen lumiukko on piirretty, palataan tavallaan ensimmäisen
kutsun puolipisteeseen ja sitten pääohjelma jatkuu kutsumalla toista
lumiukon piirtämistä.</p>
<p>Jos nyt haluaisimme piirtää lisää lumiukkoja, lisäisi jokainen uusi
lumiukko koodia vain yhden rivin.</p>
<p>Huomaa! Aliohjelmien käyttö selkeyttää ohjelmaa ja aliohjelmia kannattaa
kirjoittaa, vaikka niitä kutsuttaisiin vain yhden kerran. Hyvää
aliohjelmaa voidaan kutsua muustakin käyttöyhteydestä.</p>
<h2>6.3 Aliohjelmien dokumentointi</h2>
<p>Jokaisen aliohjelman tulisi sisältää dokumentaatiokommentti. Aliohjelman
dokumentaatiokommentin tulee sisältää ainakin seuraavat asiat: Lyhyt
kuvaus aliohjelman toiminnasta, selitys kaikista parametreista sekä
selitys mahdollisesta paluuarvosta. Nämä asiat kuvataan tägien avulla
seuraavasti:</p>
<ul>
<li>
<p>Dokumentaatiokommentin alkuun laitetaan \&lt;summary>-tagien väliin
    lyhyt ja selkeä kuvaus aliohjelman toiminnasta.</p>
</li>
<li>
<p>Jokainen parametri selitetään omien \&lt;param>-tagien väliin ja</p>
</li>
<li>
<p>paluuarvo \&lt;returns>-tagien väliin.</p>
</li>
</ul>
<p>PiirraLumiukko-aliohjelman dokumentaatiokommentit on edellisessä
esimerkissämme riveillä 36-41.</p>
<p>\</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
36 /// &lt;summary&gt;
37 /// Aliohjelma piirtää lumiukon
38 /// annettuun paikkaan.
39 /// &lt;/summary&gt;
40 /// &lt;param name="g"&gt;Peliluokka&lt;/param&gt;
41 /// &lt;param name="x"&gt;Lumiukon alimman pallon x-koordinaatti.&lt;/param&gt;
42 /// &lt;param name="y"&gt;Lumiukon alimman pallon y-koordinaatti.&lt;/param&gt;</code></p>
<p><em>Doxygen</em>-työkalun (ks.
<a href="http://en.wikipedia.org/wiki/Doxygen">http://en.wikipedia.org/wiki/Doxygen</a>)
tuottama HTML-sivu tästä luokasta näyttäisi nyt seuraavalta:</p>
<p><img alt="\
 Kuva 7: Osa Lumiukot-luokan
dokumentaatiosta" src="../src/luentomonistecsUusin_htm_5219b95e.png" /></p>
<p>\
 Dokumentaatiossa näkyy kaikki luokan aliohjelmat ja metodit. Huomaa,
että Doxygen nimittää sekä aliohjelmia että metodeja jäsenfunktioiksi
(member functions). Kuten sanottu, nimitykset vaihtelevat
kirjallisuudessa, ja tässä kohtaa käytössä on hieman harvinaisempi
nimeämistapa. Kysymys on kuitenkin samasta asiasta, josta me tällä
kurssilla käytämme nimeä aliohjelmat ja metodit.</p>
<p>Jokaisesta aliohjelmasta ja metodista löytyy lisäksi tarkemmat tiedot
Detailed Description -kohdasta. Aliohjelman PiirraLumiukko dokumentaatio
parametreineen näkyy kuvan alaosassa.</p>
<h3>6.3.1 Huomautus</h3>
<p>Kaikki PiirraLumiukko-aliohjelmassa tarvittava tieto välitettiin
parametrien avulla, eikä aliohjelman suorituksen aikana tarvittu
aliohjelman ulkopuolisia tietoja. Tämä on tyypillistä aliohjelmille, ja
usein lisäksi toivottava ominaisuus.</p>
<h2>6.4 Aliohjelmat, metodit ja funktiot</h2>
<p>Kuten ehkä huomasit, aliohjelmilla ja metodeilla on paljon yhteistä.
Monissa kirjoissa nimitetään myös aliohjelmia metodeiksi. Tällöin
aliohjelmat erotetaan olioiden metodeista nimittämällä niitä
staattisiksi metodeiksi. Tässä monisteessa metodeista puhutaan kuitenkin
vain silloin, kun tarkoitetaan olioiden toimintoja. Jypelin
dokumentaatiosta tutkit RandomGen-luokan staattisia metodeja, millä
voidaan luoda esimerkiksi satunnaisia lukuja. Yksittäinen pallo
poistettiin metodilla Destroy, joka on olion toiminto.</p>
<p>Aliohjelmista puhutaan tällä kurssilla, koska sitä termiä käytetään
monissa muissa ohjelmointikielissä. Tämä kurssi onkin ensisijaisesti
ohjelmoinnin kurssi, jossa käytetään C#-kieltä. Päätavoitteena on siis
oppia ohjelmoimaan ja työkaluna meillä sen opettelussa on C#-kieli.</p>
<p>Aliohjelmamme PiirraLumiukko ei palauttanut mitään arvoa. Aliohjelmaa
(tai metodia) joka palauttaa jonkun arvon voidaan kutsua myös tarkemmin
<em>funktioksi</em> (function).</p>
<p>Aliohjelmia ja metodeja nimitetään eri tavoin eri kielissä. Esimerkiksi
C++-kielessä sekä aliohjelmia että metodeja sanotaan funktioiksi.
Metodeita nimitetään C++-kielessä tarkemmin vielä jäsenfunktioiksi,
kuten Doxygen teki myös C#:n tapauksessa.</p>
<p>Kerrataan vielä lyhyesti aliohjelman, funktion ja metodin erot.</p>
<p><strong>Aliohjelma</strong>\
 Osaohjelma, joka voi tehdä kaikki toimensa käyttäen hyväkseen vain
parametrilistassa tuotua tietoa. Paluuarvon tyyppinä on void, jolloin
aliohjelma ei palauta mitään arvoa.</p>
<p><strong>Funktio\
</strong>Aliohjelma, joka palauttaa jonkin tuloksen, esimerkiksi kahden luvun
keskiarvon. Tämän määritelmän mukaan funktiossa on aina return-lause
jonka perässä on lauseke, esimerkiksi return (a+b)/2.0; Puhtaassa
aliohjelmassakin (ks. edellinen kohta) voi olla return-lause, mutta sen
perässä ei ole lauseketta.</p>
<p><strong>Metodi\
</strong>Aliohjelma, joka tarvitsee tehtävän suorittamiseksi kohteena olevan
olion omia tietoja. Näitä käytetään tällä kurssilla, mutta ei tehdä
alkupään esimerkeissä itse. Lopullisessa pelissä voi tulla tehtäväksi
myös metodeja, jotka tarvitsevat peliluokan-olion tietoja toimintansa
toteuttamiseen. Joku voi myös mahdollisesti tehdä vaikka uuden
aseluokan, jolle kirjoitetaan omia metodeja.</p>
<p>Metodi voi myös funktion tapaan palauttaa arvon tai aliohjelman tapaan
olla palauttamatta. Emme erottele tätä enää eri nimillä.</p>
<h1>7. Muuttujat</h1>
<p>Muuttujat (variable) toimivat ohjelmassa tietovarastoina erilaisille
asioille. Muuttuja on kuin pieni laatikko, johon voidaan varastoida
asioita, esimerkiksi lukuja, sanoja, tietoa ohjelman käyttäjästä ja
paljon muuta. Ilman muuttujia järkevä tiedon käsittely olisi oikeastaan
mahdotonta. Olemme jo ohimennen käyttäneetkin muuttujia, esimerkiksi
Lumiukko-esimerkissä teimme PhysicsObject-tyyppisiä muuttujia p1, p2 ja
p3. Vastaavasti Lumiukko-aliohjelman parametrit (Game peli, double x,
double y) ovat myös muuttujia: Game-tyyppinen oliomuuttuja peli, sekä
double-alkeistietotyyppiset muuttujat x ja y.</p>
<p>Termi <em>muuttuja</em> on lainattu ohjelmointiin matematiikasta, mutta niitä
ei tule kuitenkaan sekoittaa keskenään - muuttuja matematiikassa ja
muuttuja ohjelmoinnissa tarkoittavat hieman eri asioita. Tulet
huomaamaan tämän seuraavien kappaleiden aikana.</p>
<p>Muuttujien arvot tallennetaan keskusmuistiin tai rekistereihin, mutta
ohjelmointikielissä voimme antaa kullekin muuttujalle nimen
(identifier), jotta muuttujan arvon käsittely olisi helpompaa. Muuttujan
nimi onkin ohjelmointikielten helpotus, sillä näin ohjelmoijan ei
tarvitse tietää tarvitsemansa tiedon keskusmuisti- tai
rekisteriosoitetta, vaan riittää muistaa itse nimeämänsä muuttujan nimi.
[VES]</p>
<h2>7.1 Muuttujan määrittely</h2>
<p>Kun matemaatikko sanoo, että ”n on yhtäsuuri kuin 1”, tarkoittaa se,
että tuo termi (eli muuttuja) n on jollain käsittämättömällä tavalla
sama kuin luku 1. Matematiikassa muuttujia voidaan esitellä tällä
tavalla ”häthätää”.</p>
<p>Ohjelmoijan on kuitenkin tehtävä vastaava asia hieman tarkemmin.
C#-kielessä tämä tapahtuisi kirjoittamalla seuraavasti:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
int n;
n = 1;</code></p>
<p>Ensimmäinen rivi tarkoittaa väljästi sanottuna, että ”lohkaise pieni
pala - johon mahtuu int-kokoinen arvo - säilytystilaa tietokoneen
muistista, ja käytä siitä jatkossa nimeä n. Toisella rivillä
julistetaan, että ”talleta arvo 1 muuttujaan, jonka nimi on n, siten
korvaten sen, mitä kyseisessä säilytystilassa mahdollisesti jo on”.</p>
<p>Mikä sitten on tuo edellisen esimerkin int?</p>
<p>C#:ssa jokaisella muuttujalla täytyy olla <em>tietotyyppi</em> (usein myös
lyhyesti <em>tyyppi</em>). Tietotyyppi on määriteltävä, jotta ohjelma tietäisi,
millaista tietoa muuttujaan tullaan tallentamaan. Toisaalta tietotyyppi
on määriteltävä siksi, että ohjelma osaa varata muistista sopivan
kokoisen lohkareen muuttujan sisältämää tietoa varten. Esimerkiksi
int-tyypin tapauksessa tilantarve olisi 32 bittiä (4 tavua), byte-tyypin
tapauksessa 8 bittiä (1 tavu) ja double-tyypin 64 bittiä (8 tavua).
Muuttuja määritellään (declare) kirjoittamalla ensiksi tietotyyppi ja
sen perään muuttujan nimi. Muuttujan nimet aloitetaan C#ssa pienellä
kirjaimella, jonka jälkeen jokainen uusi sana alkaa aina isolla
kirjaimella. Kuten aiemmin mainittiin, tämä nimeämistapa on nimeltään
camelCasing.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
muuttujanTietotyyppi muuttujanNimi;</code></p>
<p>Tuo mainitsemamme int on siis tietotyyppi, ja int-tyyppiseen muuttujaan
voi tallentaa kokonaislukuja. Muuttujaan n voimme laittaa lukuja 1, 2,
3, samoin 0, -1, -2, ja niin edelleen, mutta emme lukua 0.1 tai sanaa
”Moi”.</p>
<p>Henkilön iän voisimme tallentaa seuraavaan muuttujaan:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
int henkilonIka;</code></p>
<p>Huomaa, että tässä emme aseta muuttujalle mitään arvoa, vain
määrittelemme muuttujan int-tyyppiseksi ja annamme sille nimen.</p>
<p>Samantyyppisiä muuttujia voidaan määritellä kerralla useampia
erottamalla muuttujien nimet pilkulla. Tietotyyppiä double käytetään,
kun halutaan tallentaa desimaalilukuja.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
double paino, pituus;</code></p>
<p>Määrittely onnistuu toki myös erikseen.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
double paino;
double pituus;</code></p>
<h2>7.2 Alkeistietotyypit</h2>
<p>C#:n tietotyypit voidaan jakaa alkeistietotyyppeihin (primitive types)
ja oliotietotyyppeihin (reference types). Oliotietotyyppeihin kuuluu
muun muassa käyttämämme PhysicsObject-tyyppi, jota pallot p1 jne.
olivat, sekä merkkijonojen tallennukseen tarkoitettu String-olio.
Oliotyyppejä käsitellään myöhemmin luvussa 8.</p>
<p>Eri tietotyypit vaativat eri määrän kapasiteettia tietokoneen muistista.
Vaikka nykyajan koneissa on paljon muistia, on hyvin tärkeää valita
oikean tyyppinen muuttuja kuhunkin tilanteeseen. Suurissa ohjelmissa
ongelma korostuu hyvin nopeasti käytettäessä muuttujia, jotka kuluttavat
tilanteeseen nähden kohtuuttoman paljon muistikapasiteettia. C#:n
alkeistietotyypit on lueteltu alla.</p>
<p>Taulukko 1: C#:n alkeistietotyypit koon mukaan järjestettynä.</p>
<p>C#-merkki</p>
<p>Koko</p>
<p>Selitys</p>
<p>Arvoalue</p>
<p>bool</p>
<p>1 bitti</p>
<p>kaksiarvoinen tietotyyppi</p>
<p>true tai false</p>
<p>sbyte</p>
<p>8 bittiä</p>
<p>yksi tavu</p>
<p>-128..127</p>
<p>byte</p>
<p>8 bittiä</p>
<p>yksi tavu (etumerkitön)</p>
<p>0..255</p>
<p>char</p>
<p>16 bittiä</p>
<p>yksi merkki</p>
<p>kaikki merkit</p>
<p>short</p>
<p>16 bittiä</p>
<p>pieni kokonaisluku</p>
<p>-32,768..32,767</p>
<p>ushort</p>
<p>16 bittiä</p>
<p>pieni kokonaisluku (etumerkitön)</p>
<p>0..65,535</p>
<p>int</p>
<p>32 bittiä</p>
<p>kokonaisluku</p>
<p>-2,147,483,648..\
 2,147,483,647</p>
<p>uint</p>
<p>32 bittiä</p>
<p>kokonaisluku (etumerkitön)</p>
<p>0..4,294,967,295</p>
<p>float</p>
<p>32 bittiä</p>
<p>liukuluku</p>
<p>noin 7 desimaalin tarkkuus,</p>
<p>±1.5 × 10^-45^.. ±3.4 × 10^38^</p>
<p>long</p>
<p>64 bittiä</p>
<p>iso kokonaisluku</p>
<p>-2^63^..2^63^-1</p>
<p>ulong</p>
<p>64 bittiä</p>
<p>iso kokonaisluku (etumerkitön)</p>
<p>0..\
 18,446,744,073,709,615</p>
<p>double</p>
<p>64 bittiä</p>
<p>tarkka liukuluku</p>
<p>noin 15 desimaalin tarkkuus,</p>
<p>±5.0 × 10^-324^.. ±1.7 × 10^308^</p>
<p>decimal</p>
<p>128 bittiä</p>
<p>erittäin tarkka liukuluku</p>
<p>Noin 28 luvun tarkkuus</p>
<p>\</p>
<p>\</p>
<p>\</p>
<p>\</p>
<p>Tässä monisteessa suositellaan, että desimaalilukujen talletukseen
käytettäväksi aina double-tietotyyppiä (jossain tapauksissa jopa
decimal-tyyppiä), vaikka monessa paikassa float-tietotyyppiä
käytetäänkin. Tämä johtuu siitä, että liukuluvut, joina desimaaliluvut
tietokoneessa käsitellään, ovat harvoin tarkkoja arvoja tietokoneessa.
Itse asiassa ne ovat tarkkoja vain kun ne esittävät jotakin kahden
potenssin kombinaatiota, kuten esimerkiksi 2.0, 7.0, 0.5 tai 0.375.</p>
<p>Useimmiten liukuluvut ovat pelkkiä approksimaatioita oikeasta
reaaliluvusta. Esimerkiksi lukua 0.1 ei pystytä tietokoneessa esittämään
biteillä tarkasti. Tällöin laskujen määrän kasvaessa lukujen epätarkkuus
vain lisääntyy. Tämän takia onkin turvallisempaa käyttää aina
double-tietotyyppiä, koska se suuremman bittimääränsä takia pystyy
tallettamaan enemmän merkitseviä desimaaleja.</p>
<p>Tietyissä sovelluksissa, joissa mahdollisimman suuri tarkkuus on
välttämätön (kuten pankki- tai nanotason fysiikkasovellukset), on
suositeltavaa käyttää korkeimpaa mahdollista tarkkuutta tarjoavaa
decimal-tyyppiä. Reaalilukujen esityksestä tietokoneessa puhutaan lisää
kohdassa <a href="#o246Liukuluku_floatingpoint">26.6</a>. [VES][KOS]</p>
<h2>7.3 Arvon asettaminen muuttujaan</h2>
<p>Muuttujaan asetetaan arvo sijoitusoperaattorilla (assignment operator)
"=". Lauseita, joilla asetetaan muuttujille arvoja sanotaan
sijoituslauseiksi (assignment statement). On tärkeää huomata, että
sijoitus tapahtuu aina oikealta vasemmalle: sijoitettava on
yhtäsuuruusmerkin oikealla puolella ja kohde merkin vasemmalla puolella.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
x = 20.0;
henkilonIka = 23;
paino = 80.5;
pituus = 183.5;</code></p>
<p>Muuttujaan voi asettaa arvon myös jo määrittelyn yhteydessä.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
bool onkoKalastaja = true;
char merkki = 't';
int kalojenLkm = 0;
double luku1 = 0, luku2 = 0, luku3 = 0;</code></p>
<p>Muuttuja täytyy olla määritelty tietyn tyyppiseksi ennen kuin siihen voi
asettaa arvoa. Muuttujaan voi asettaa vain määrittelyssä annetun
tietotyypin mukaisia arvoja tai sen kanssa <em>sijoitusyhteensopivia</em>
arvoja. Esimerkiksi liukulukutyyppeihin (float ja double) voi sijoittaa
myös kokonaislukutyyppisiä arvoja, sillä kokonaisluvut on reaalilukujen
osajoukko. Alla sijoitamme arvon 4 muuttujaan nimeltä luku2, ja
kolmannella rivillä luku2-muuttujan sisältämän arvon (4) muuttujaan,
jonka nimi on luku1.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
double luku1;
int luku2 = 4;
luku1 = luku2;</code></p>
<p>Toisinpäin tämä ei onnistu: double-tyyppistä arvoa ei voi sijoittaa
int-tyyppiseen muuttujaan. Alla oleva koodi ei kääntyisi:</p>
<p><code>{.huonoesimerkki-western lang="zxx" xml:lang="zxx"}
//TÄMÄ KOODI EI KÄÄNNY!
int luku1;
double luku2 = 4.0;
luku1 = luku2;</code></p>
<p>Kun decimal-tyyppinen muuttuja alustetaan jollain luvulla, tulee luvun
perään (ennen puolipistettä) laittaa m (tai M)-merkki. Samoin
float-tyyppisten muuttujien alustuksessa perään laitetaan f (tai
F)-merkki.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
decimal tilinSaldo = 3498.98m;
float lampotila = -4.8f;</code></p>
<p>Huomaa, että char-tyyppiseen muuttujaan sijoitetaan arvo laittamalla
merkki heittomerkkien väliin, esimerkiksi näin.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
char ekaKirjain = 'k';</code></p>
<p>Näin sen erottaa myöhemmin käsiteltävästä String-tyyppiseen muuttujaan
sijoittamisesta, jossa sijoitettava merkkijono laitetaan lainausmerkkien
väliin, esimerkiksi seuraavasti.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
String omaNimi = "Antti-Jussi";</code></p>
<p>Sijoituslause voi sisältää myös monimutkaisiakin lausekkeita,
esimerkiksi aritmeettisia operaatioita:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
double numeroidenKeskiarvo = (2 + 4 + 1 + 5 + 3 + 2) / 6.0;</code></p>
<p>Sijoituslause voi sisältää myös muuttujia.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
double huoneenPituus = 540.0;
double huoneenLeveys = huoneenPituus;
double huoneenAla = huoneenPituus * huoneenLeveys;</code></p>
<p>Eli sijoitettava voi olla mikä tahansa lauseke, joka tuottaa muuttujalle
kelpaavan arvon.</p>
<p>C#:ssa täytyy aina asettaa joku arvo muuttujaan <em>ennen</em> sen
käyttämistä. Kääntäjä ei käännä koodia, jossa käytetään muuttujaa jolle
ei ole asetettu arvoa. Alla oleva ohjelma ei siis kääntyisi.</p>
<p><code>{.huonoesimerkki-western lang="zxx" xml:lang="zxx"}
// TÄMÄ OHJELMA EI KÄÄNNY
public class Esimerkki 
{
  public static void Main(string[] args) 
  {
    int ika;
    System.Console.WriteLine(ika);
  }
}</code></p>
<p>Virheilmoitus näyttää tältä:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Esimerkki.cs(4,40): error CS0165: Use of unassigned local variable
        'ika'</code></p>
<p>Kääntäjä kertoo, että ika-nimistä muuttuja yritetään käyttää, vaikka
sille ei ole osoitettu mitään arvoa. Tämä ei ole sallittua, joten
ohjelman kääntämisyritys päättyy tähän.</p>
<h2>7.4 Muuttujan nimeäminen</h2>
<p>Muuttujan nimen täytyy olla siihen tallennettavaa tietoa kuvaava.
Yleensä pelkkä yksi kirjain on huono nimi muuttujalle, sillä se harvoin
kuvaa kovin hyvin muuttujaa. Kuvaava muuttujan nimi selkeyttää koodia ja
vähentää kommentoimisen tarvetta. Lyhyt muuttujan nimi ei ole
itseisarvo. Vielä parikymmentä vuotta sitten se saattoi olla sitä, koska
se nopeutti koodin kirjoittamista. Nykyaikaisia kehitysympäristöjä
käytettäessä tämä ei enää pidä paikkaansa, sillä editorit osaavat
täydentää muuttujan nimen samalla kun koodia kirjoitetaan, joten niitä
ei käytännössä koskaan tarvitse kirjoittaa kokonaan, paitsi tietysti
ensimmäisen kerran.</p>
<p>Yksikirjaimisia muuttujien nimiäkin voi perustellusti käyttää, jos
niillä on esimerkiksi jo matematiikasta tai fysiikasta ennestään tuttu
merkitys. Nimet x ja y ovat hyviä kuvaamaan koordinaatteja. Nimi l (eng.
length) viittaa pituuteen ja r (eng. radius) säteeseen. Fysikaalisessa
ohjelmassa s voi hyvin kuvata matkaa.</p>
<p>Huomaa! Muuttujan nimi ei voi C#:ssa alkaa numerolla.</p>
<p>C#:n koodauskäytänteiden mukaan muuttujan nimi alkaa pienellä
kirjaimella ja jos muuttujan nimi koostuu useammasta sanasta aloitetaan
uusi sana aina isolla kirjaimella kuten alla.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
int polkupyoranRenkaanKoko;</code></p>
<p>C#:ssa muuttujan nimi voi sisältää ääkkösiä, mutta niiden käyttö ei
suositella, koska siirtyminen koodistosta toiseen aiheuttaa usein
ylimääräisiä ongelmia.</p>
<p><em>Koodisto =</em> Määrittelee jokaiselle <em>merkistön</em> merkille yksikäsitteisen
koodinumeron. Merkin numeerinen esitys on usein välttämätön
tietokoneissa. Merkistö määrittelee joukon merkkejä ja niille nimen,
numeron ja jonkinnäköisen muodon kuvauksen. Merkistöllä ja koodistolla
tarkoitetaan usein samaa asiaa, kuitenkin esimerkiksi Unicode-merkistö
sisältää useita eri koodaus tapoja (UTF-8, UTF-16, UTF-32). Koodisto on
siis se merkistön osa, joka määrittelee merkille numeerisen koodiarvon.
Koodistoissa syntyy ongelmia yleensä silloin, kun siirrytään jostain
skandimerkkejä (ä,ö,å, ...) sisältävästä koodistosta seitsemänbittiseen
ASCII-koodistoon, joka ei tue skandeja. ASCII-koodistosta puhutaan lisää
luvussa <a href="#o25_ASCIIkoodi">27</a>.</p>
<h3>7.4.1 C#:n varatut sanat</h3>
<p>Muuttujan nimi ei saa olla mikään ohjelmointikielen varatuista sanoista,
eli sanoista joilla on C#:ssa joku muu merkitys.</p>
<p>Taulukko 2: C#:n varatut sanat.</p>
<hr />
<p>abstract   do         in          protected    true
  as         double     int         public       try
  base       else       interface   readonly     typeof
  bool       enum       internal    ref          uint
  break      event      is          return       ulong
  byte       explicit   lock        sbyte        unchecked
  case       extern     long        sealed       unsafe
  catch      false      namespace   short        ushort
  char       finally    new         sizeof       using
  checked    fixed      null        stackalloc   virtual
  class      float      object      static       void
  const      for        operator    string       volatile
  continue   foreach    out         struct       while
  decimal    goto       override    switch     <br />
  default    if         params      this       <br />
  delegate   implicit   private     throw        </p>
<hr />
<p>\
 \</p>
<p>\
 \</p>
<h2>7.5 Muuttujien näkyvyys</h2>
<p>Muuttujaa voi käyttää (lukea ja asettaa arvoja) vain siinä lohkossa,
missä se on määritelty. Muuttujan määrittelyn täytyy aina olla ennen
(koodissa ylempänä), kun sitä ensimmäisen kerran käytetään. Jos muuttuja
on käytettävissä sanotaan, että muuttuja <em>näkyy</em>. Aliohjelman sisällä
määritelty muuttuja ei siis näy muissa aliohjelmissa.</p>
<p>Luokan sisällä muuttuja voidaan määritellä myös niin, että se näkyy
kaikkialla, siis kaikille aliohjelmille. Kun muuttuja on näkyvissä
kaikille ohjelman osille, sanotaan sitä <em>globaaliksi muuttujaksi</em>
(global variable). Globaaleja muuttujia tulee välttää aina kun
mahdollista.</p>
<p>Lisätietoa muuttujien näkyvyydestä löydät wikistä osoitteessa</p>
<p><a href="https://trac.cc.jyu.fi/projects/ohj1/wiki/MuuttujienNakyvyys">https://trac.cc.jyu.fi/projects/ohj1/wiki/MuuttujienNakyvyys</a>.</p>
<h2>7.6 Vakiot</h2>
<blockquote>
<p>One man's constant is another man's variable. -Alan Perlis</p>
</blockquote>
<p>Muuttujien lisäksi ohjelmointikielissä voidaan määritellä vakioita
(constant). Vakioiden arvoa ei voi muuttaa määrittelyn jälkeen. C#:ssa
vakio määritellään muuten kuten muuttuja, mutta muuttujan tyypin eteen
kirjoitetaan lisämääre const.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
const int KUUKAUSIEN_LKM = 12;</code></p>
<p>Tällä kurssilla vakiot kirjoitetaan suuraakkosin siten, että sanat
erotetaan alaviivalla (_). Näin ne erottaa helposti muuttujien nimistä,
jotka alkavat pienellä kirjaimella. Muitakin kirjoitustapoja on,
esimerkiksi Pascal Casing on toinen yleisesti käytetty vakioiden
kirjoitusohje.</p>
<h2>7.7 Operaattorit</h2>
<p>Usein meidän täytyy tallentaa muuttujiin erilaisten laskutoimitusten
tuloksia. C#:ssa laskutoimituksia voidaan tehdä aritmeettisilla
operaatioilla (arithmetic operation), joista mainittiin jo kun teimme
lumiukkoesimerkkiä. Ohjelmassa olevia aritmeettisia laskutoimituksia
sanotaan aritmeettisiksi lausekkeiksi (arithmetic expression).</p>
<p>C#:ssa on myös vertailuoperaattoreita (comparison operators), loogisia
operaattoreita, bittikohtaisia operaattoreita (bitwise operators),
arvonmuunto-operaattoreita (shortcut operators), sijoitusoperaattori =,
is-operaattori sekä ehto-operaattori ?. Tässä luvussa käsitellään näistä
tärkeimmät.</p>
<h3>7.7.1 Aritmeettiset operaatiot</h3>
<p>C#:ssa peruslaskutoimituksia suoritetaan aritmeettisilla operaatiolla,
joista + ja - tulivatkin esille aikaisemmissa esimerkeissä.
Aritmeettisia operaattoreita on viisi.</p>
<p>Taulukko 3: Aritmeettiset operaatiot.</p>
<p>Operaat-\
 tori</p>
<p>Toiminto</p>
<p>Esimerkki</p>
<p>+</p>
<p>yhteenlasku</p>
<p>System.Console.WriteLine(1+2); // tulostaa 3</p>
<p>-</p>
<p>vähennyslasku</p>
<p>System.Console.WriteLine(1-2); // tulostaa -1</p>
<p>*</p>
<p>kertolasku</p>
<p>System.Console.WriteLine(2*3); // tulostaa 6</p>
<p>/</p>
<p>jakolasku</p>
<p>System.Console.WriteLine(6 / 2); // tulostaa 3</p>
<p>System.Console.WriteLine(7 / 2); // Huom! 3</p>
<p>System.Console.WriteLine(7 / 2.0); // 3.5</p>
<p>System.Console.WriteLine(7.0 / 2); // 3.5</p>
<p>%</p>
<p>jakojäännös (modulo)</p>
<p>System.Console.WriteLine(18 % 7); // tulostaa 4</p>
<h3>7.7.2 Vertailuoperaattorit</h3>
<p>Vertailuoperaattoreiden avulla verrataan muuttujien arvoja keskenään.
Vertailuoperaattorit palauttavat totuusarvon (true tai false).
Vertailuoperaattoreita on kuusi. Lisää vertailuoperaattoreista luvussa
13.</p>
<h3>7.7.3 Arvonmuunto-operaattorit</h3>
<p>Arvonmuunto-operaattoreiden avulla laskutoimitukset voidaan esittää
tiiviimmässä muodossa: esimerkiksi x++; (4 merkkiä) tarkoittaa samaa
asiaa kuin x = x+1; (6 merkkiä). Niiden avulla voidaan myös alustaa
muuttujia.</p>
<p>\
 \</p>
<p>Taulukko 4: Arvonmuunto-operaattorit.</p>
<p>Ope-raattori</p>
<p>Toiminto</p>
<p>Esimerkki</p>
<p>++</p>
<p>Lisäysoperaattori. Lisää muuttujan arvoa yhdellä.</p>
<p>int luku = 0;</p>
<p>\</p>
<p>System.Console.WriteLine(luku++); //tulostaa 0</p>
<p>System.Console.WriteLine(luku++); //tulostaa 1</p>
<p>System.Console.WriteLine(luku); //tulostaa 2</p>
<p>System.Console.WriteLine(++luku); //tulostaa 3</p>
<p>--</p>
<p>Vähennysoperaattori. Vähentää muuttujan arvoa yhdellä.</p>
<p>int luku = 5;</p>
<p>System.Console.WriteLine(luku--); //tulostaa 5</p>
<p>System.Console.WriteLine(luku--); //tulostaa 4</p>
<p>System.Console.WriteLine(luku); //tulostaa 3</p>
<p>System.Console.WriteLine(--luku); //tulostaa 2</p>
<p>System.Console.WriteLine(luku); //tulostaa 2</p>
<p>+=</p>
<p>Lisäys-operaatio.</p>
<p>int luku = 0;</p>
<p>luku += 2; //luku muuttujan arvo on 2</p>
<p>luku += 3; //luku muuttujan arvo on 5</p>
<p>luku += -1; //luku muuttujan arvo on 4</p>
<p>-=</p>
<p>Vähennys-operaatio</p>
<p>int luku = 0;</p>
<p>luku -= 2; // luku muuttujan arvo on -2</p>
<p>luku -= 1 // luku muuttujan arvon -3</p>
<p>*=</p>
<p>Kertolasku-operaatio</p>
<p>int luku = 1;</p>
<p>luku *= 3; // luku-muuttujan arvo on 3</p>
<p>luku *= 2; //luku-muuttujan arvo on 6</p>
<p>/=</p>
<p>Jakolasku-operaatio</p>
<p>double luku = 27;</p>
<p>luku /= 3; //luku-muuttujan arvo on 9</p>
<p>luku /= 2.0; //luku-muuttujan arvo on 4.5</p>
<p>%=</p>
<p>Jakojäännös-operaatio</p>
<p>int luku = 9;</p>
<p>luku %= 5; //luku-muuttujan arvo on 4</p>
<p>luku %=2; //luku-muuttujan arvo on 0</p>
<p>\</p>
<p>\</p>
<p>\</p>
<p>Lisäysoperaattoria (++) ja vähennysoperaattoria (--) voidaan käyttää,
ennen tai jälkeen muuttujan. Käytettäessä ennen muuttujaa, arvoa
muutetaan ensin ja mahdollinen toiminto esimerkiksi tulostus, tehdään
vasta sen jälkeen. Jos operaattori sen sijaan on muuttujan perässä,
toiminto tehdään ensiksi ja arvoa muutetaan vasta sen jälkeen.</p>
<p>Huomaa! Arvonmuunto-operaattorit ovat ns. sivuvaikutuksellisia
operaattoreita. Toisin sanoen, operaatio muuttaa muuttujan arvoa toisin
kuin esimerkiksi aritmeettiset operaatiot. Seuraava esimerkki
havainnollistaa asiaa.</p>
<p>\
 \</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
int luku1 = 5;
int luku2 = 5;
System.Console.WriteLine(++luku1); // tulostaa 6;
System.Console.WriteLine(luku2 + 1 ); // tulostaa 6;
System.Console.WriteLine(luku1); // 6
System.Console.WriteLine(luku2); // 5</code></p>
<h3>7.7.4 Aritmeettisten operaatioiden suoritusjärjestys</h3>
<p>Aritmeettisten operaatioiden suoritusjärjestys on vastaava kuin
matematiikan laskujärjestys. Kerto- ja jakolaskut suoritetaan ennen
yhteen- ja vähennyslaskua. Lisäksi sulkeiden sisällä olevat lausekkeet
suoritetaan ensin.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
System.Console.WriteLine(5 + 3 * 4 - 2);  //tulostaa 15
System.Console.WriteLine((5 + 3) * (4 - 2));  //tulostaa 16</code></p>
<h2>7.8 Huomautuksia</h2>
<h3>7.8.1 Kokonaisluvun tallentaminen liukulukumuuttujaan</h3>
<p>Kun yritetään tallentaa kokonaislukujen jakolaskun tulosta
liukulukutyyppiseen (float tai double) muuttujaan, voi tulos tallettua
kokonaislukuna, jos jakaja ja jaettava ovat molemmat ilmoitettu ilman
desimaaliosaa.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
double laskunTulos = 5 / 2;
System.Console.WriteLine(laskunTulos); // tulostaa 2</code></p>
<p>Jos kuitenkin vähintään yksi jakolaskun luvuista on desimaalimuodossa,
niin laskun tulos tallentuu muuttujaan oikein.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
double laskunTulos = 5 / 2.0;
System.Console.WriteLine(laskunTulos); // tulostaa 2.5</code></p>
<p>Liukuluvuilla laskettaessa kannattaa pitää desimaalimuodossa myös luvut,
joilla ei ole desimaaliosaa, eli ilmoittaa esimerkiksi luku 5 muodossa
5.0.</p>
<p>Kokonaisluvuilla laskettaessa kannattaa huomioida seuraava:</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
int laskunTulos = 5 / 4;
System.Console.WriteLine(laskunTulos); // tulostaa 1</p>
<p>laskunTulos = 5 / 6;
System.Console.WriteLine(laskunTulos); // tulostaa 0</p>
<p>laskunTulos = 7 / 3;
System.Console.WriteLine(laskunTulos); // tulostaa 2
```</p>
<p>Kokonaisluvuilla laskettaessa lukuja ei siis pyöristetä lähimpään
kokonaislukuun, vaan desimaaliosa menee C#:n jakolaskuissa ikään kuin
"hukkaan".</p>
<h3>7.8.2 Lisäys- ja vähennysoperaattoreista</h3>
<p>On neljä tapaa kasvattaa luvun arvoa yhdellä.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
++a;
a++; // idiomi
a += 1;
a = a + 1; // huonoin muutettavuuden ja kirjoittamisen kannalta</code></p>
<p>Ohjelmoinnissa <em>idiomilla</em> tarkoitetaan tapaa jolla asia yleensä
kannattaa tehdä. Näistä a++ on ohjelmoinnissa vakiintunut tapa ja
(yleensä) suositeltavin, siis idiomi. Kuitenkin, jos lukua a pitäisikin
kasvattaa (tai vähentää) kahdella tai kolmella, ei tämä tapa enää
toimisi. Seuraavassa esimerkissä tarkastellaan eri tapoja kahdella
vähentämiseksi. Siihen on kolme vaihtoehtoista tapaa.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
a -= 2;
a += -2;  // lisättävä luku voisi olla muuttuja. Luku voi olla myös negatiivinen
a = a - 2;</code></p>
<p>Tässä tapauksessa += -operaattorin käyttö olisi suositeltavinta, sillä
lisättävä luku voi olla positiivinen tai negatiivinen (tai nolla), joten
+= -operaattori ei tässä rajoita sitä, millaisia lukuja a-muuttujaan
voidaan lisätä.</p>
<h2>7.9 Esimerkki: Painoindeksi</h2>
<p>Tehdään ohjelma joka laskee painoindeksin. Painoindeksi lasketaan
jakamalla paino (kg) pituuden (m) neliöllä, eli kaavalla</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
paino / (pituus * pituus)</code></p>
<p>C#:lla painoindeksi saadaan siis laskettua seuraavasti.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
/// @author  Antti-Jussi Lakanen
/// @version 22.8.2012
///
/// &lt;summary&gt;
/// Ohjelma, joka laskee painoindeksin
/// pituuden (m) ja painon (kg) perusteella. 
/// &lt;/summary&gt;
public class Painoindeksi
{
    /// &lt;summary&gt;
    /// Pääohjelma, jossa painoindeksi tulostetaan ruudulle.
    /// &lt;/summary&gt;
    public static void Main() 
    {
      double pituus = 1.83;
      double paino = 75.0;
      double painoindeksi = paino / (pituus*pituus);
      System.Console.WriteLine(painoindeksi);
   }
}</code></p>
<p>\
 \</p>
<h1>8. Oliotietotyypit</h1>
<p>C#:n alkeistietotyypit antavat melko rajoittuneet puitteet
ohjelmointiin. Niillä pystytään tallentamaan ainoastaan lukuja ja
yksittäisiä kirjaimia. Vähänkin monimutkaisemmissa ohjelmissa tarvitaan
kehittyneempiä rakenteita tiedon tallennukseen. C#:ssa, Javassa ja
muissa oliokielissä tällaisen rakenteen tarjoavat oliot. C#:ssa jo
merkkijonokin (String) toteutetaan oliona.</p>
<h2>8.1 Mitä oliot ovat?</h2>
<p>Olio (object) on ”tietorakenne”, jolla pyritään ohjelmoinnissa kuvaamaan
reaalimaailman ilmiöitä. Luokkapohjaisissa kielissä (kuten C#, Java ja
C++) olion rakenteen ja käyttäytymisen määrittelee luokka, joka kuvaa
olion attribuutit ja metodit. Attribuutit ovat olion ominaisuuksia ja
metodit olion toimintoja. Olion sanotaan olevan luokan <em>ilmentymä</em>.
Yhdestä luokasta voi siis luoda useita olioita, joilla on samat metodit
ja attribuutit. Attribuutit voivat saada samasta luokasta luoduilla
olioilla eri arvoja. Attribuuttien arvot muodostavat olion tilan. Huomaa
kuitenkin, että vaikka oliolla olisi sama tila, sen <em>identiteetti</em> on
eri. Esimerkiksi, kaksi täsmälleen samannäköistä palloa voi olla samassa
paikassa (näyttää yhdeltä pallolta), mutta todellisuudessa ne ovat kaksi
eri palloa.</p>
<p>Olioita voi joko tehdä itse tai käyttää jostain kirjastosta löytyviä
valmiita olioita. Omien olioluokkien tekeminen ei kuulu vielä
Ohjelmointi 1 -kurssin asioihin, mutta käyttäminen kyllä. Tarkastellaan
seuraavaksi luokan ja olion suhdetta, sekä kuinka oliota käytetään.</p>
<p>Luokan ja olion suhdetta voisi kuvata seuraavalla esimerkillä. Olkoon
luentosalissa useita ihmisiä. Kaikki luentosalissa olijat ovat ihmisiä.
Heillä on tietyt samat ominaisuudet, jotka ovat kaikilla ihmisillä,
kuten pää, kaksi silmää ja muitakin ruumiinosia. Kuitenkin jokainen
salissa olija on erilainen ihmisen ilmentymä, eli jokaisella oliolla on
oma identiteetti - eiväthän he ole yksi ja sama vaan heitä on useita.
Eri ihmisillä voi olla erilainen tukka ja eriväriset silmät ja oma
puhetyyli. Lisäksi ihmiset voivat olla eri pituisia, painoisia jne.
Luentosalissa olevat identtiset kaksosetkin olisivat eri ilmentymiä
ihmisestä. Jos Ihminen olisi luokka, niin kaikki luentosalissa olijat
olisivat Ihminen-luokan ilmentymiä eli Ihminen-olioita. Tukka, silmät,
pituus ja paino olisivat sitten olion ominaisuuksia eli attribuutteja.
Ihmisellä voisi olla lisäksi joitain toimintoja eli metodeja kuten Syo,
MeneToihin, Opiskele jne. Tarkastellaan seuraavaksi hieman todellisempaa
esimerkkiä olioista.</p>
<p>Oletetaan, että suunnittelisimme yritykselle palkanmaksujärjestelmää.
Siihen tarvittaisiin muun muassa Tyontekija-luokka. Tyontekija-luokalla
täytyisi olla ainakin seuraavat attribuutit: nimi, tehtava, osasto,
palkka. Luokalla täytyisi olla myös ainakin seuraavat metodit:
MaksaPalkka, MuutaTehtava, MuutaOsasto, MuutaPalkka. Jokainen työntekijä
olisi nyt omanlaisensa Tyontekija-luokan ilmentymä eli olio.</p>
<h2>8.2 Olion luominen</h2>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Tyontekija teppo = new Tyontekija("Teppo Tunari", "Projektipäällikkö", "Tutkimusosasto", 5000);</code></p>
<p>Olioviite määritellään kirjoittamalla ensiksi sen luokan nimi, josta
olio luodaan. Seuraavaksi kirjoitetaan nimi, jonka haluamme oliolle
antaa. Nimen jälkeen tulee yhtäsuuruusmerkki, jonka jälkeen oliota
luotaessa kirjoitetaan sana new ilmoittamaan, että luodaan uusi olio.
Tämä new-operaattori varaa tilan tietokoneen muistista oliota varten.</p>
<p>Seuraavaksi kirjoitetaan luokan nimi uudelleen, jonka perään
kirjoitetaan sulkuihin mahdolliset olion luontiin liittyvät parametrit.
Parametrit riippuvat siitä kuinka luokan <em>konstruktori</em> (constructor,
muodostaja) on toteutettu. Konstruktori on metodi, joka suoritetaan aina
kun uusi olio luodaan. Valmiita luokkia käyttääkseen ei tarvitse
kuitenkaan tietää konstruktorin toteutuksesta, vaan tarvittavat
parametrit selviävät aina luokan dokumentaatiosta. Yleisessä muodossa
uusi olio luodaan alla olevalla tavalla.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Luokka olionNimi = new Luokka(parametri1, parametri2,..., parametriN);</code></p>
<p>Jos olio ei vaadi luomisen yhteydessä parametreja, kirjoitetaan silloin
tyhjä sulkupari.</p>
<p>Ennen kuin oliolle on varattu tila tietokoneen muistista
new-operaattorilla, ei sitä voi käyttää. Ennen new-operaattorin käyttöä
olion arvo on null, joka tarkoittaa, että olio on käyttökelvoton,
”mitätön”. Tällaisen olion käyttö aiheuttaa virheilmoituksen. Olio
voidaan myös tarkoituksellisesti merkitä käyttökelvottomaksi asettamalla
olionNimi = null.</p>
<p>Uusi Tyontekija-olio voitaisiin luoda esimerkiksi seuraavasti.
Parametrit riippuisivat nyt siitä kuinka olemme toteuttaneet
Tyontekija-luokan konstruktorin. Tässä tapauksessa annamme nyt
parametrina oliolle kaikki attribuutit.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Tyontekija akuAnkka = new Tyontekija("Aku Ankka", "Johtaja", "Osasto3", 3000);</code></p>
<p>Monisteen alussa loimme lumiukkoja piirrettäessä PhysicsObject-luokan
olion seuraavasti.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
PhysicsObject p1 = new PhysicsObject(2 * 100.0, 2 * 100.0, Shape.Circle);</code></p>
<p>Itse asiassa oliomuuttuja on C#:ssa ainoastaan <em>viite</em> varsinaiseen
olioon. Siksi niitä kutsutaankin usein myös <em>viitemuuttujiksi.</em>
Viitemuuttujat eroavat oleellisesti alkeistietotyyppisistä muuttujista.</p>
<h2>8.3 Oliotietotyyppien ja alkeistietotyyppien ero</h2>
<p>C#:ssa on kahden mallisia rakenteita, joihin tietoja voidaan tallentaa.
Tapauksesta riippuen tiedot tallennetaan joko <em>alkeistietotyyppeihin</em>
tai <em>oliotietotyyppeihin</em>. Oliotietotyypit eroavat alkeistietotyypeistä
siinä, että ne ovat <em>viitteitä</em> tiettyyn olioon, ja siksi niitä
kutsutaan myös nimellä viitetyypit tai viitemuuttujat.</p>
<ul>
<li>
<p>Alkeistietotyypit säilyttävät sisältämänsä tiedon yhdessä paikassa
    tietokoneen muistissa (nimeltään <em>pino</em>).</p>
</li>
<li>
<p>Viitetyypit sisältävät viitteen johonkin toiseen paikkaan muistissa
    (kutsutaan nimellä <em>keko</em>), missä varsinainen data sijaitsee.
    Viittaus olioon sijaitsee kuitenkin pinossa.</p>
</li>
</ul>
<p>Yleensä meidän ei tarvitse olla kovin huolissamme siitä käytämmekö
alkeistietotyyppiä (kuten int, double tai char) vai oliotyyppiä (kuten
String). Yleisesti ottaen tärkein ero on siinä, että alkeistietotyyppien
tulee (tiettyjä poikkeuksia lukuun ottamatta) aina sisältää jokin arvo,
mutta oliotietotyypit voivat olla null-arvoisia (eli ”ei-minkään”
arvoisia). Jäljempänä esimerkkejä alkeistietotyyppien ja viitetyyppien
eroista.</p>
<p>Samaan olioon voi viitata useampi muuttuja. Vertaa alla olevia
koodinpätkiä.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
int luku1 = 10;
int luku2 = luku1;
luku1 = 0;
Console.WriteLine(luku2); //tulostaa 10</code></p>
<p>Yllä oleva tulostaa ”10” niin kuin pitääkin. Muuttujan luku2 arvo ei
siis muutu, vaikka asetamme kolmannella rivillä muuttujaan luku1 arvon
0. Tämä johtuu siitä, että toisella rivillä asetamme muuttujaan luku2
muuttujan luku1 arvon, emmekä viitettä muuttujaan luku1.
Oliotietotyyppisten muuttujien kanssa asia on toinen. Vertaa yllä olevaa
esimerkkiä seuraavaan:</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
PhysicsObject p1 = new PhysicsObject(2<em>100.0, 2</em>100.0, Shape.Circle);
Add(p1);
p1.X = -50;</p>
<p>PhysicsObject p2 = p1;
p2.X = 100;
```</p>
<p>Yllä oleva koodi piirtää seuraavan kuvan:</p>
<p><img alt="\
 Kuva 8: Molemmat muuttujat, p1 ja p2, liikuttelevat samaa ympyrää.
Lopputuloksena ympyrä seisoo pisteessä
x=100." src="../src/luentomonistecsUusin_htm_m3838408a.png" /></p>
<p>\
 Nopeasti voisi olettaa, että ikkunassamme näkyisi nyt vain kaksi
samanlaista ympyrää eri paikoissa. Näin ei kuitenkaan ole, vaan molemmat
PhysicsObject-oliot viittaavat samaan ympyrään, jonka säde on 50. Tämä
johtuu siitä, että muuttujat p1 ja p2 ovat olioviitteitä, jotka
viittaavat (ts. osoittavat) samaan olioon.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
PhysicsObject p2 = p1;</code></p>
<p>Toisin sanoen yllä olevalla rivillä ei luoda uutta PhysicsObject-oliota,
vaan ainoastaan uusi olioviite, joka viittaa nyt samaan olioon kuin p1.</p>
<p><img alt="\
 Kuva 9: Sekä p1 että p2 viittaavat samaan
olioon." src="../src/luentomonistecsUusin_htm_75ff594f.png" /></p>
<p>\
 \</p>
<p><em>Oliomuuttuja =</em> Viite todelliseen olioon. Samaan olioon voi olla
useitakin viitteitä.</p>
<p>Viitteitä käsitellään tarkemmin luvussa 14.</p>
<h2>8.4 Metodin kutsuminen</h2>
<p>Jokaisella tietystä luokasta luodulla oliolla on käytössä kaikki tämän
luokan julkiset metodit. Metodikutsussa käsketään oliota tekemään
jotain. Voisimme esimerkiksi käskeä PhysicsObject-oliota liikkumaan, tai
Tyontekija-oliota muuttamaan palkkaansa.</p>
<p>Olion metodeita kutsutaan kirjoittamalla ensiksi olion nimi, piste ja
kutsuttavan metodin nimi. Metodin mahdolliset parametrit laitetaan
sulkeiden sisään ja erotetaan toisistaan pilkulla. Jos metodi ei vaadi
parametreja, täytyy sulut silti kirjoittaa, niiden sisälle ei vaan tule
mitään. Yleisessä muodossa metodikutsu on seuraava:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
olionNimi.MetodinNimi(parametri1,parametri2,...parametriN);</code></p>
<p>Voisimme nyt esimerkiksi muuttaa akuAnkka-olion palkkaa alla olevalla
tavalla.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
akuAnkka.MuutaPalkka(3500);</code></p>
<p>Tai laittaa p1-olion (oletetaan että p1 on PhysicsObject-olio)
liikkeelle käyttäen Hit-metodia.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
p1.Hit(new Vector(1000.0, 500.0));</code></p>
<p>String-luokasta löytyy esimerkiksi Contains-metodi, joka palauttaa arvon
True tai False. Parametrina Contains-metodille annetaan merkkijono, ja
metodi etsii oliosta antamaamme merkkijonoa vastaavia ilmentymiä. Jos
olio sisältää merkkijonon (yhden tai useamman kerran) palautetaan True.
Muutoin palautetaan False. Alla esimerkki.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
String lause = "Pekka meni kauppaan";
Console.WriteLine(lause.Contains("eni")); // Tulostaa True</code></p>
<h2>8.5 Metodin ja aliohjelman ero</h2>
<p>Aliohjelma esitellään static-tyyppiseksi, mikäli aliohjelma ei käytä
mitään muita tietoja kuin parametreinä tuodut tiedot. Esimerkiksi
luvussa 20.4.2 on seuraava aliohjelma.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
private void KuunteleLiiketta(AnalogState hiirenTila)
{
   pallo.X = Mouse.PositionOnWorld.X;
   pallo.Y = Mouse.PositionOnWorld.Y;</p>
<p>Vector hiirenLiike = hiirenTila.MouseMovement;
}
```</p>
<p>Tässä tarvitaan hiiren tilan lisäksi pelioliossa esitellyn pallon
tietoja, joten enää ei ole kyse stattisesta aliohjelmasta ja siksi
static-sana jätetään pois. Metodi sen sijaan pystyy käyttämään <em>olion</em>
omia "ominaisuuksia", attribuutteja, metodeja ja ns. ominaisuus-kenttiä
(property fields).</p>
<h2>8.6 Olion tuhoaminen ja roskienkeruu</h2>
<p>Kun olioon ei enää viittaa yhtään muuttujaa (olioviitettä), täytyy olion
käyttämät muistipaikat vapauttaa muuhun käyttöön. Oliot poistetaan
muistista puhdistusoperaation avulla. Tästä huolehtii C#:n
automaattinen roskienkeruu (garbage collection). Kun olioon ei ole enää
viitteitä, se merkitään poistettavaksi ja aina tietyin väliajoin
<em>puhdistusoperaatio</em> (kutsutaan usein myös nimellä roskienkerääjä,
garbage collector) vapauttaa merkittyjen olioiden muistipaikat.</p>
<p>Kaikissa ohjelmointikielissä näin ei ole (esim. alkuperäinen C++), vaan
muistin vapauttamisesta ja olioiden tuhoamisesta tulee useimmiten
huolehtia itse. Näissä kielissä on yleensä destruktori (destructor =
hajottaja), joka suoritetaan aina kun olio tuhotaan. Itse
kirjoitettavasta destruktorista on tapana kutsua olion elinaikanaan
luomien olioiden tuhoamista tai muiden resurssien vapauttamista. Vertaa
konstruktoriin, joka suoritettiin kun olio luodaan. Haastavaksi näiden
kielien yhteydessä tuleekin se, että joissakin tapauksissa olioiden
elinkaari on automaattista ja joissakin ei. Siksi muistivuodot ovat
erittäin yleisiä aloittelevilla C++ -ohjelmoijilla.</p>
<p>Yleensä C#-ohjelmoijan ei tarvitse huolehtia muistin vapauttamisesta,
mutta on tiettyjä tilanteita, joissa voidaan itse joutua poistamaan
oliot. Yksi esimerkki tällaisesta tilanteesta on tiedostojen käsittely:
Jos olio on avannut tiedoston, olisi viimeistään ennen olion tuhoamista
järkevää sulkea tiedosto. Tällöin samassa yhteydessä olion tuhottavaksi
merkitsemisen kanssa suoritettaisiin myös tiedoston sulkeminen. Tämä
tehdään esittelemällä <em>hajotin</em> (destructor), joka on luokan metodi ja
jonka tehtävänä on tyhjentää olio kaikesta sen sisältämästä tiedosta
sekä vapauttaa sen sisältämät rakenteet, kuten kytkökset avoinna oleviin
resursseihin (esim tiedostoon, tosin yleensä tiedostoa ei ole hyvä pitää
avoinna niin kauan aikaa kuin jonkin olion elinkaari voi olla).</p>
<h2>8.7 Olioluokkien dokumentaatio</h2>
<p>Luokan dokumentaatio sisältää tiedot luokasta, luokan konstruktoreista
ja metodeista. Luokkien dokumentaatioissa on yleensä linkkejä
esimerkkeihin, kuten myös String-luokan tapauksessa. Tutustutaan nyt
tarkemmin String-luokan dokumentaatioon. String-luokan dokumentaatio
löytyy sivulta
<a href="http://msdn.microsoft.com/en-us/library/system.string.aspx">http://msdn.microsoft.com/en-us/library/system.string.aspx</a>,
ja lista <em>jäsenistä</em> eli käytössä olevista konstruktoreista,
attribuuteista (fields), ominaisuuksista (property) ja metodeista
sivulta
<a href="http://msdn.microsoft.com/en-us/library/system.string_members.aspx">http://msdn.microsoft.com/en-us/library/system.string_members.aspx</a>.</p>
<p>Olemme kiinnostuneita tässä vaiheessa kohdista String Constructor ja
String Methods (sivun vasemmassa osassa hierarkiapuussa). Klikkaa
kohdasta String Constructor saadaksesi lisätietoa luokan
konstruktoreista tai String Methods saadaksesi tietoja käytössä olevista
metodeista.</p>
<h3>8.7.1 Konstruktorit</h3>
<p>Avaa luokan String sivu String Constructor. Tämä kohta sisältää tiedot
kaikista luokan konstruktoreista. Konstruktoreita voi olla useita,
kunhan niiden parametrit eroavat toisistaan. Jokaisella konstruktorilla
on oma sivu, ja sivulla kunkin ohjelmointikielen kohdalla oma versionsa,
sillä .NET Framework käsittää useita ohjelmointikieliä. Me olemme
luonnollisesti tässä vaiheessa kiinnostuneita vain C#-kielisistä
versioista.</p>
<p>Kunkin konstruktorin kohdalla on lyhyesti kerrottu mitä se tekee, ja sen
jälkeen minkä tyyppisiä ja montako parametria konstruktori ottaa
vastaan. Kaikista konstruktoreista saa lisätietoa klikkaamalla
konstruktorin esittelyriviä. Esimerkiksi linkki</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
[C#] public String(char[]);</code></p>
<p>vie <a href="http://msdn.microsoft.com/en-us/library/ttyxaek9.aspx">sivulle</a>
(<a href="http://msdn.microsoft.com/en-us/library/ttyxaek9.aspx">http://msdn.microsoft.com/en-us/library/ttyxaek9.aspx</a>)
jossa konstruktorista public String(char[]) kerrotaan lisätietoja ja
annetaan käyttöesimerkkejä.</p>
<p><img alt="\
 Kuva 10: Tiedot luokan konstruktoreista löytyvät MSDN-dokumentaatioissa
Constructor-kohdasta." src="../src/luentomonistecsUusin_htm_1b70689e.png" /></p>
<p>\
 \
 \</p>
<p>Huomaa, että monet String-luokan konstruktoreista on merkitty
unsafe-merkinnällä, jolloin niitä ei tulisi käyttää omassa koodissa.
Tällaiset konstruktorit on tarkoitettu ainoastaan järjestelmien
keskinäiseen viestintään.</p>
<p>Tässä vaiheessa voi olla vielä hankalaa ymmärtää kaikkien konstruktorien
merkitystä, sillä ne sisältävät tietotyyppejä joita emme ole vielä
käsitelleet. Esimerkiksi tietotyypin perässä olevat hakasulkeet (esim.
int[]) tarkoittavat että kyseessä on <em>taulukko</em>. Taulukoita käsitellään
lisää luvussa 15.</p>
<p>String-luokan olio on C#:n ehkä yleisin olio, ja on itse asiassa
kokoelma (taulukko) perättäisiä yksittäisiä char-tyyppisiä merkkejä. Se
voidaan luoda seuraavasti.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
String nimi = new string(new char [] {'J', 'a', 'n', 'n', 'e'});
System.Console.WriteLine(nimi); // Tulostaa Janne</code></p>
<p>Näin kirjoittaminen on tietenkin usein melko vaivalloista. String-luokan
olio voidaan kuitenkin poikkeuksellisesti luoda myös
alkeistietotyyppisten muuttujien määrittelyä muistuttavalla tavalla.
Alla oleva oleva lause on vastaava kuin edellisessä kohdassa, mutta
lyhyempi kirjoittaa.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
String nimi = "Janne";</code></p>
<p>Huomaa, että merkkijonon ympärille tulee lainausmerkit. Näppäimistöltä
lainausmerkit saadaan näppäinyhdistelmällä
<code>Shift+2</code>{.terminaali-western}. Vastaavasti merkkijono voitaisiin
kuitenkin alustaa myös muilla String-luokan konstruktoreilla, joita on
pitkä lista.</p>
<p>Jos taas tutkimme PhysicsObject-luokan dokumentaatiota (löytyy
osoitteesta
<a href="http://kurssit.it.jyu.fi/npo/material/latest/documentation/html/">http://kurssit.it.jyu.fi/npo/material/latest/documentation/html/</a>
→ Luokat → PhysicsObject), löydämme useita eri konstruktoria (ks. kohta
<em>Julkiset jäsenfunktiot</em>, jotka alkavat sanalla PhysicsObject).
Konstruktoreista järjestyksessä toinen saa parametrina kaksi lukua ja
muodon. Tätä konstruktoria käytimme jo lumiukkoesimerkissä.</p>
<p><img alt="\
 Kuva 11: Jypeli-kirjaston luokan konstruktorit löytyvät Julkiset
jäsenfunktiot -otsikon
alta." src="../src/luentomonistecsUusin_htm_74f89f25.png" /></p>
<p>\
 Voisimme kuitenkin olla antamatta muotoa (ensimmäinen konstruktori) ja
määritellä muodon vasta myöhemmin fysiikkaolionShape-ominaisuuden
avulla.</p>
<h3>8.7.2 Harjoitus</h3>
<p>Tutki muita konstruktoreja. Mitä niistä selviää dokumentaation
perusteella? Mikä on oletusmuoto?</p>
<h3>8.7.3 Metodit</h3>
<p>Kohta
<a href="http://msdn.microsoft.com/en-us/library/system.string_methods.aspx">Methods</a>
(<a href="http://msdn.microsoft.com/en-us/library/system.string_methods.aspx">http://msdn.microsoft.com/en-us/library/system.string_methods.aspx</a>)
sisältää tiedot kaikista luokan metodeista. Jokaisella metodilla on
taulukossa oma rivi, ja rivillä lyhyt kuvaus, mitä metodi tekee.
Klikattuasi jotain metodia saat siitä tarkemmat tiedot. Tällä sivulla
kerrotaan mm. minkä tyyppisen parametrin metodi ottaa, ja minkä
tyyppisen arvon metodi palauttaa. Esimerkiksi String-luokassa
käyttämämme ToUpper-metodi, joka siis palauttaa String-tyyppisen arvon.</p>
<h3>8.7.4 Huomautus: Luokkien dokumentaatioiden googlettaminen</h3>
<p>Huomaa, että kun haet luokkien dokumentaatioita hakukoneilla, saattavat
tulokset viitata .NET Frameworkin vanhempiin versioihin (esimerkiksi 1.0
tai 2.0). Kirjoitushetkellä uusin .NET Framework-versio on 4, ja onkin
syytä varmistua että löytämäsi dokumentaatio koskee juuri oikeaa
versiota. Voit esimerkiksi käyttää hakutermissä versionumeroa tähän
tapaan: ”c# string documentation .net 4”. Versionumeron näkee otsikon
alapuolella. Voit halutessasi vaihtaa johonkin toiseen versioon
klikkaamalla Other Versions -pudotusvalikkoa.</p>
<h2>8.8 Tyyppimuunnokset</h2>
<p>C#:ssa yhteen muuttujaan voi tallentaa vain yhtä tyyppiä. Tämän takia
meidän täytyy joskus muuttaa esimerkiksi String-tyyppinen muuttuja
int-tyyppiseksi tai double-tyyppinen muuttuja int-tyyppiseksi ja niin
edelleen. Kun muuttujan tyyppi vaihdetaan toiseksi, sanotaan sitä
tyyppimuunnokseksi (cast, taitype cast).</p>
<p>Kaikilla alkeistietotyypeillä sekä C#:n oliotyypeillä on
ToString-metodi, jolla olio voidaan muuttaa merkkijonoksi. Alla
esimerkki int-luvun muuttamisesta merkkijonoksi.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
// kokonaisluku merkkijonoksi
int kokonaisluku = 24;
String intMerkkijonona = kokonaisluku.ToString();</p>
<p>// liukuluku merkkijonoksi
double liukuluku = 0.562;
String doubleMerkkijonona = liukuluku.ToString();
```</p>
<p>Merkkijonon muuttaminen alkeistietotyypiksi onnistuu sen sijaan
jokaiselle alkeistietotyypille tehdystä luokasta löytyvällä metodilla.
Alkeistietotyypithän eivät ole olioita, joten niillä ei ole metodeita.
C#:sta löytyy kuitenkin jokaista alkeistietotyyppiä vastaava <em>rakenne</em>
(struct), joista löytyy alkeistietotyyppien käsittelyyn hyödyllisiä
metodeita. Rakenteet sijaitsevat System-nimiavaruudessa, ja tästä syystä
ohjelman alussa tarvitaan lause</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
using System;</code></p>
<p>Alkeistietotyyppejä vastaavat rakenteet löytyy seuraavasta taulukosta.</p>
<p>Taulukko 5: Alkeistietotyypit ja niitä vastaavat rakenteet.</p>
<p>Alkeistieto-tyyppi</p>
<p>Rakenne</p>
<p>bool</p>
<p><a href="http://msdn.microsoft.com/en-us/library/system.boolean.aspx">Boolean</a></p>
<p>byte</p>
<p><a href="http://msdn.microsoft.com/en-us/library/system.byte.aspx">Byte</a></p>
<p>char</p>
<p><a href="http://msdn.microsoft.com/en-us/library/system.char.aspx">Char</a></p>
<p>short</p>
<p><a href="http://msdn.microsoft.com/en-us/library/system.int16.aspx">Int16</a></p>
<p>int</p>
<p><a href="http://msdn.microsoft.com/en-us/library/system.int32.aspx">Int32</a></p>
<p>long</p>
<p><a href="http://msdn.microsoft.com/en-us/library/system.int64.aspx">Int64</a></p>
<p>ulong</p>
<p><a href="http://msdn.microsoft.com/en-us/library/system.uint64.aspx">UInt64</a></p>
<p>float</p>
<p><a href="http://msdn.microsoft.com/en-us/library/system.single.aspx">Single</a></p>
<p>double</p>
<p><a href="http://msdn.microsoft.com/en-us/library/system.double.aspx">Double</a></p>
<p>\</p>
<p>\</p>
<p>\
 Huomaa, että rakenteen ja alkeistietotyypin nimet ovat C#:ssa
synonyymejä. Seuraavat rivit tuottavat saman lopputuloksen (mikäli
System-nimiavaruus on otettu käyttöön using-lauseella).</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
int luku1 = 5;
Int32 luku2 = 5;</code></p>
<p>Vastaavasti kaikki rakenteiden metodit ovat käytössä, kirjoittipa
alkeistietotyypin tai rakenteen nimen. Tästä esimerkki seuraavaksi.</p>
<p>Merkkijonon (String) muuttaminen int-tyypiksi onnistuu C#:n
int.Parse-funktiolla seuraavasti.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
String luku1 = "24";
int luku2 = int.Parse(luku1);</code></p>
<p>Tarkasti sanottuna Parse-funktio luo parametrina saamansa merkkijonon
perusteella uuden int-tyyppisen tiedon, joka talletetaan muuttujaan
luku2.</p>
<p>Jos luvun parsiminen (jäsentäminen, muuttaminen) ei onnistu, aiheuttaa
se niin sanotun <em>poikkeuksen</em>. double-luvun parsiminen onnistuu
vastaavasti Double-rakenteesta (iso D-kirjain) löytyvällä
Parse-funktiolla.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
String luku3 = "2.45";
double luku4 = Double.Parse(luku3);</code></p>
<p>\
 \</p>
<h1>9. Aliohjelman paluuarvo</h1>
<p>Aliohjelmat-luvussa tekemämme Lumiukko-aliohjelma ei palauttanut mitään
arvoa. Usein on kuitenkin hyödyllistä, että lopettaessaan aliohjelma
palauttaa jotain tietoa ohjelman suorituksesta. Mitä hyötyä olisi
esimerkiksi aliohjelmasta, joka laskee kahden luvun keskiarvon, jos emme
koskaan saisi tietää mikä niiden lukujen keskiarvo on? Voisimmehan me
tietenkin tulostaa luvun keskiarvon suoraan aliohjelmassa, mutta lähes
aina on järkevämpää palauttaa tulos ”kysyjälle” paluuarvona. Tällöin
aliohjelmaa voidaan käyttää myös tilanteessa, jossa keskiarvoa ei haluta
tulostaa, vaan sitä tarvitaan johonkin muuhun laskentaan. Paluuarvon
palauttaminen tapahtuu return-lauseella, ja return-lause lopettaa aina
aliohjelman suorittamisen (eli palataan takaisin kutsuvaan ohjelman
osaan).</p>
<p>Toteutetaan nyt aliohjelma, joka laskee kahden kokonaisluvun keskiarvon
ja palauttaa tuloksen paluuarvona.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
public static double Keskiarvo(int a, int b) 
{
   double keskiarvo;
   keskiarvo = (a + b) / 2.0; // Huom 2.0 auttaa, että tulos on reaaliluku 
   return keskiarvo;
}</code></p>
<p>Ensimmäisellä rivillä määritellään jälleen julkinen ja staattinen
aliohjelma. Lumiukko-esimerkissä static-sanan jälkeen luki void, joka
tarkoitti, että aliohjelma ei palauttanut mitään arvoa. Koska nyt
haluamme, että aliohjelma palauttaa parametrina saamiensa
kokonaislukujen keskiarvon, niin meidän täytyy kirjoittaa paluuarvon
tyyppi void-sanan tilalle static-sanan jälkeen. Koska kahden
kokonaisluvun keskiarvo voi olla myös desimaaliluku, niin paluuarvon
tyyppi on double. Sulkujen sisällä ilmoitetaan jälleen parametrit. Nyt
parametreina on kaksi kokonaislukua a ja b. Toisella rivillä
määritellään reaalilukumuuttuja keskiarvo. Kolmannella rivillä lasketaan
parametrien a ja b summa ja jaetaan se kahdella muuttujaan keskiarvo.
Neljännellä rivillä palautetaan keskiarvo-muuttujan arvo.</p>
<p>Aliohjelmaa voitaisiin nyt käyttää pääohjelmassa esimerkiksi alla
olevalla tavalla.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
double keskiarvo;
keskiarvo = Keskiarvo(3, 4);
Console.WriteLine("Keskiarvo = " + keskiarvo);</code></p>
<p>Tai lyhyemmin kirjoitettuna:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Console.WriteLine("Keskiarvo = " + Keskiarvo(3, 4));</code></p>
<p>Koska Keskiarvo-aliohjelma palauttaa aina double-tyyppisen liukuluvun,
voidaan kutsua käyttää kuten mitä tahansa double-tyyppistä arvoa. Se
voidaan esimerkiksi tulostaa tai tallentaa muuttujaan.</p>
<p>Itse asiassa koko Keskiarvo-aliohjelman voisi kirjoittaa lyhyemmin
muodossa:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
public static double Keskiarvo(int a, int b) 
{
    double keskiarvo = (a + b) / 2.0; 
    return keskiarvo;
}</code></p>
<p>Yksinkertaisimmillaan Keskiarvo-aliohjelman voisi kirjoittaa jopa alla
olevalla tavalla.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
public static double Keskiarvo(int a, int b) 
{
    return (a + b) / 2.0;
}</code></p>
<p>Kaikki yllä olevat tavat ovat oikein, eikä voi sanoa mikä tapa on paras.
Joskus "välivaiheiden" kirjoittaminen selkeyttää koodia, mutta
Keskiarvo-aliohjelman tapauksessa viimeisin tapa on selkein ja lyhin.</p>
<p>Aliohjelmassa voi olla myös useita return-lauseita. Tästä esimerkki
kohdassa: 13.5.1.</p>
<p>Aliohjelma ei kuitenkaan voi palauttaa kerralla suoranaisesti useita
arvoja. Toki voidaan palauttaa esimerkiksi taulukko, jossa sitten on
monia arvoja. Toinen keino olisi tehdä olio, joka sisältäisi useita
arvoja ja palautettaisiin. C#:ssa on olemassa kolmaskin keino on
olemassa, jota ei käsitellä tällä kurssilla: ref- ja out-parametrit,
katso</p>
<p><a href="http://msdn.microsoft.com/en-us/library/t3c3bfhx(v=vs.80).aspx">http://msdn.microsoft.com/en-us/library/t3c3bfhx(v=vs.80).aspx</a>.</p>
<p>Metodeita ja aliohjelmia, jotka ottavat vastaan parametreja palauttavat
arvon sanotaan joskus myös <em>funktioiksi</em>. Nimitys ei ole hullumpi, jos
vertaa Keskiarvo-aliohjelmaa vaikkapa matematiikan funktioon f(x, y) =
(x + y) / 2. Funktiolla ei lisäksi saisi olla sivuvaikutuksia, kuten
esimerkiksi tulostamista tai globaalien muuttujien muuttamista.</p>
<p>Mitä eroa on tämän</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
double tulos = Keskiarvo(5, 2); // funktio Keskiarvo laskisi kahden luvun keskiarvon
Console.WriteLine(tulos); //tulostaa 3.5
Console.WriteLine(tulos); //tulostaa 3.5</code></p>
<p>ja tämän</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Console.WriteLine(Keskiarvo(5, 2)); //tämäkin tulostaa 3.5
Console.WriteLine(Keskiarvo(5, 2)); //tämäkin tulostaa 3.5</code></p>
<p>koodin suorituksessa?</p>
<p>Ensimmäisessä lukujen 5 ja 2 keskiarvo lasketaan vain kertaalleen, jonka
jälkeen tulos tallennetaan muuttujaan. Tulostuksessa käytetään sitten
tallessa olevaa laskun tulosta.</p>
<p>Jälkimmäisessä versiossa lukujen 5 ja 2 keskiarvo lasketaan tulostuksen
yhteydessä. Keskiarvo lasketaan siis kahteen kertaan. Vaikka alemmassa
tavassa säästetään yksi koodirivi, kulutetaan siinä turhaan tietokoneen
resursseja laskemalla sama lasku kahteen kertaan. Tässä tapauksessa
tällä ei ole juurikaan merkitystä, sillä Keskarvo-aliohjelman suoritus
ei juurikaan rasita nykyaikaisia tietokoneita. Kannattaa kuitenkin
opetella tapa, ettei ohjelmassa tehtäisi <em>mitään</em> turhia suorituksia.</p>
<h2>9.1 Harjoitus</h2>
<p>Muuttujat-luvun lopussa tehtiin ohjelma, joka laski painoindeksin. Tee
ohjelmasta uusi versio, jossa painoindeksin laskeminen tehdään
aliohjelmassa. Aliohjelma saa siis parametreina pituuden ja painon ja
palauttaa painoindeksin.</p>
<h1>10. Visual Studio 2010</h1>
<p>Vaikka ohjelmakoodia voi kirjoittaa pelkällä editorillakin, ohjelmien
koon kasvaessa alkaa kaipaamaan työvälineiltä hieman enemmän
ominaisuuksia. Peruseditoreja enemmän ominaisuuksia tarjoavat
sovelluskehittimet eli IDE:t (Integrated Development Environment).
C#:lle tehtyjä ilmaisia sovelluskehittimiä ovat muun muassa</p>
<ul>
<li>
<p>Visual Studio 2010 Express -tuoteperhe,</p>
</li>
<li>
<p>Mono ja</p>
</li>
<li>
<p>SnippetCompiler.</p>
</li>
</ul>
<p>\</p>
<p>Tässä monisteessa tutustumme tarkemmin <em>Visual Studio 2010 Express</em>
-tuoteperheeseen kuuluvaan <em>Visual C# 2010 Express</em> -kehittimeen. Muita
perheeseen kuuluvia ilmaisia C#-kehittimiä ovat mm. <em>Visual Studio 2010
Express for Windows Phone</em> sekä <em>Visual Web Developer 2010 Express</em></p>
<p>Kaikki ohjeet on testattu toimiviksi Visual C# 2010 Express sekä Visual
Studio 2010 Ultimate -versioilla, luonnollisesti Windows-ympäristössä.</p>
<h2>10.1 Asennus</h2>
<p>Visual Studion (VS) maksullinen - ja Jyväskylän yliopiston
opiskelijoille maksuton - Ultimate-versio, on ladattavissa Microsoftin
yliopistoyhteistyöpalvelusta, kirjoitushetkellä (elokuu 2012) nimeltään
DreamSpark Premium. Edellä mainittu Express-versio on saatavilla
ilmaiseksi Microsoftin verkkosivuilta. Express-versio riittää muilta
osin tämän kurssin tarpeisiin, mutta siitä puuttuvat testausominaisuudet
(ks. luku 11). Muistathan, että VS on saatavissa vain
Windows-käyttöjärjestelmille. Linux- ja Mac OS X -ympäristöihin on
saatavilla ainakin Mono-sovelluskehitin, mutta tällä kurssilla
käytettävä Jypeli-ohjelmointikirjasto vaatii toimiakseen
Windows-käyttöjärjestelmän.</p>
<h2>10.2 Käyttö</h2>
<h3>10.2.1 Ensimmäinen käyttökerta</h3>
<p>Kun käynnistät VS:n ensimmäisen kerran, kysyy VS millä oletusasetuksilla
haluat valikoita ja toimintoja käyttää. Valitse Visual C# Development
Settings.</p>
<p>Aluksi rivinumerot eivät ole käytössä. Koodin seuraamisen
helpottamiseksi otetaan ne käyttöön kohdasta Tools → Options → Text
Editor → C# → Line Numbers. Jos Error list-ikkuna ei ole jo näkyvissä
ruudun alareunassa, kannattaa se ottaa käyttöön: View → Error List.</p>
<h3>10.2.2 Projektit ja solutionit</h3>
<p>Visual Studiossa on <em>solutioneja</em> ja <em>projekteja</em>. Yhdessä solutionissa
voi olla monta projektia ja jokaisen projektin täytyy kuulua johonkin
solutioniin. Uuden ohjelman kirjoittaminen alkaa aina projektin
perustamisella johonkin solutioniin, jolloin myös solution luodaan
samalla, ellei sitä ole jo tehty.</p>
<p>Hierarkia ei ole kiveen hakattu, mutta solutioneja ja projekteja voi
hahmottaa esimerkiksi seuraavasti:</p>
<ul>
<li>
<p>Jokainen demokerta (tai luento, ohjauskerta tms) on uusi solution,
    eli kun aloitat tekemään <em>ensimmäistä</em> demotehtävää (ohjaustehtävää)
    niin toimitaan seuraavasti</p>
<ul>
<li>
<p>Klikkaa File → New project (Ctrl+Shift+N)</p>
</li>
<li>
<p>Valitse projektin tyyppi: Jos teet konsoliohjelmaa niin valitse
    Visual C# -kohdasta Console Application. Jos teet Jypeli-peliä,
    valitse Visual C# → Jypeli-kohdasta haluamasi projektimalli
    (esimerkiksi FysiikkaPeli).</p>
</li>
<li>
<p>Anna projektille (eli tehtävälle) nimi kohtaan Name, esimerkiksi
    Lumiukko tai HelloWorld. Tarkista demotehtävien nimeämiskäytäntö
    opettajalta.</p>
</li>
<li>
<p>Laita Location-kohtaan demotehtäviesi juuripolku, eli vaikkapa
    C:\MyTemp\\&lt;omatunnus>\ohj1\demot. Laita omatunnus kohtaan
    luonnollisesti yliopiston mikroverkon käyttäjätunnuksesi. Älä
    käytä ääkkösiä tai välilyöntejä kansioiden eikä tiedostojen
    nimissä, tulemme yleensä toimeen myös ilman niitä, mutta
    ongelmia ääkköset ja erikoismerkit aiheuttavat usein.</p>
</li>
</ul>
</li>
</ul>
<p>Huomaa! Tarkista luennoitsijalta demotehtävien tallennuspaikka
yliopiston mikroverkossa.</p>
<ul>
<li>
<ul>
<li>
<p>Valitse Solution-valikosta Create new Solution ja paina Create
        directory for solution (tärkeä)</p>
</li>
<li>
<p>Laita Solution Name kohtaan demoN (N = demokerran numero),
    esimerkiksi demo3</p>
</li>
<li>
<p>Nyt sinulle on luotu uusi solution ja yksi projekti solutionin
    sisään.</p>
</li>
</ul>
</li>
<li>
<p>Kun haluat <em>lisätä</em> demotehtäviä (projekteja) tiettyyn demokertaan
    (solutioniin) toimi näin</p>
<ul>
<li>
<p>Avaa sen demokerran solution (.sln-tiedosto) johon haluat
    tehtävän lisätä (ellei se ole jo auki Visual Studiossa).</p>
</li>
<li>
<p>File → New project <em>tai</em> Solution Explorerissa klikkaa hiiren
    oikealla solutionin päälle, ja klikkaa Add → New project</p>
</li>
<li>
<p>Anna projektille nimi, jätä Location ennalleen, ja valitse
    Solution-valikosta Add to Solution. Solution Name -kohta
    himmenee ja Location-kohtaan pitäisi tulla automaattisesti
    lisäteksti demoN.</p>
</li>
</ul>
</li>
</ul>
<p>Näin tehtynä kaikki yhden demokerran tehtävät löytyvät "saman katon
alta" eli yhden solutionin kaikki projektit menevät samaan kansioon.
Resurssienhallinnassa hakemistopuusi voisi näyttää esimerkiksi tältä.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
ohj1
 |
 +--demot 
 |  +--demo1
 |  |  +--HelloWorld
 |  |  +--Lumiukko
 |  |  '-demo1.sln
 |  '--demo2
 |     +--Lumiukko2
 |     '--LukujenLaskemista
 '--ohjaukset
    +--ohjaus1
    |  +--HelloWorld
    |  '--Lumiukko
    '--ohjaus2</code></p>
<p>Yksittäisten projektien kansioihin (tässä HelloWorld, Lumiukko,
Lumiukko2, ja niin edelleen) syntyy myös läjä erilaisia tiedostoja.
Tutkitaan vielä, millainen rakenne yksittäisen projektin kansioon on
syntynyt. Esimerkkinä vaikkapa HelloWorld-kansio.</p>
<p>\</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
HelloWorld
 |
 +--bin
 |  +--Debug
 |     +-HelloWorld.vshost.exe
 |     +-HelloWorld.vshost.exe.manifest
 +--obj
 |  +--x86
 |     +-(...)
 +--Properties
 |  +-AssemblyInfo.cs
 +-HelloWorld.csproj
 '-Program.cs</code></p>
<p>Lähdekoodi sijaitsee Program.cs-tiedostossa. Kuten huomaat, projektiin
kuuluu kuitenkin monia muitakin tiedostoja ja kansioita. bin- ja
obj-kansiot sisältävät ohjelman käännökseen liittyviä
väliaikaistiedostoja. Properties-kansio taas sisältää julkaisuun
liittyvää tietoa esimerkiksi tekijänoikeuksista ja merkistöstä. Huomaa,
että varsinainen projektitiedosto on .csproj-päätteinen.</p>
<p>\</p>
<p>Solution toimii tavallaan liimana näiden eri projektitiedostojen välille
yhdistäen ne Visual Studiossa. Tämän johdosta monen eri projektin
käsittely yhtä aikaa on kätevää.</p>
<p>\</p>
<p>Viimeisenä tutkitaan Jypeli-projektimallista luotua projektia, sillä sen
kansiorakenne eroaa hieman konsoliohjelmaan syntyvästä
kansiorakenteesta, esimerkkinä Lumiukko-projekti.</p>
<p>\</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Lumiukko
 |
 +--bin
 |  +-(...)
 +--obj
 |  +-(...)
 +--Properties
 |  +-AssemblyInfo.cs
 +-Game.ico
 +-GameThumbnail.png
 +-Lumiukko.csproj
 +-Ohjelma.cs
 '-Peli.cs
LumiukkoContent
 |
 +--bin
 |  +-(...)
 +--obj
 |  +-(...)
 '-LumiukkoContent.contentproj</code></p>
<p>Projekti sisältää yhden kansion sijaan kaksi kansiota (Lumiukko ja
LumiukkoContent), joista jälkimmäinen on varattu sisällön, kuten äänien
ja kuvien, tuomiselle peliin. Lumiukko-kansiosta löytyy edelleen kaksi
kooditiedostoa, Ohjelma.cs ja Peli.cs, joista ensimmäinen on varattu
Main-metodille, ja jälkimmäinen sisältää varsinaisen pelin lähdekoodin.
Ohjelma.cs-tiedostoa ei käytännössä juurikaan tarvitse muokata.</p>
<p>Peli.cs sisältää lähdekoodin, ja se on yleensä se tiedosto, joka
pyydetään palautettavaksi demotehtävissä. Muita tiedostoja ei tarvitse
palauttaa, ellei niitä erikseen pyydetä. Huomaa, että tiedoston nimen
voi ja tulee muuttaa vastaamaan luokan nimeä.</p>
<h3>10.2.3 Visual Studion perusnäkymä</h3>
<p>Kun olet luonut ensimmäisen projektisi, pitäisi edessäsi olla VS:n
perusnäkymä. VS voi vaikuttaa aluksi melko monimutkaiselta, mutta sen
peruskäytön oppii nopeasti. Jos jotakin työkalua ei tarvi ja se vie
mielestäsi tilaa ruudulla, sen voi yleensä piilottaa. Kannattaakin
opetella käyttämään ruututilaa tehokkaasti ja poistaa turhat visuaaliset
elementit käytöstä kun niitä ei tarvita.</p>
<p>Oikealla on Solution Explorer, jossa näkyy kaikki projektit, sekä
projektien sisältämät tiedostot ja viitteet muihin kooditiedostoihin.</p>
<p>Alhaalla on Error List (mikäli aktivoit sen päälle aikaisemmin), joka
näyttää koodin kirjoittamisen aikana havaittuja virheitä, sekä
käännöksessä tapahtuvia virheitä. Error List on yksi ohjelmoijan
tärkeimmistä työkaluista ja Visual Studion ehdoton vahvuus -
syntaksivirheet saadaan kiinni ilman, että ohjelmoijan tarvitsee
erikseen edes kääntää tekemäänsä ohjelmaa. Voit tuplaklikata Error
Listissä virhettä, ja VS vie kursorin suoraan siihen pisteeseen, jossa
olettaa virheen olevan.</p>
<p>Keskellä näkyy koodin kirjoitusikkuna. Eri kooditiedostot aukeavat
välilehtiin, joita voi selata näppäinyhdistelmällä Ctrl+Tab (ja
Shift+Ctrl+Tab).</p>
<h3>10.2.4 Ohjelman kirjoittaminen</h3>
<p>Jokainen C#-ohjelma kirjoitetaan luokan sisään. Luotaessa uusi
konsoliohjelma, tekee Visual Studio meille valmiin kooditiedoston, jossa
on pieni pätkä valmista ohjelmakoodia. Oletusarvoisesti konsoliohjelmaan
luodaan yksi kooditiedosto nimeltä Program.cs, jossa on valmiina muutama
using-lause, nimiavauus eli namespace (projektia vastaavalla nimellä),
luokka sekä pääohjelma eli Main-metodi.</p>
<p>Aivan aluksi kannattaa muuttaa kooditiedoston nimi johonkin hieman
kuvaavampaan klikkaamalla Solution Explorerissa tiedoston päällä hiiren
oikealla napilla ja valitsemalla Rename. Kun tiedoston nimi on muutettu,
kysyy Visual Studio, haluatko vyöryttää tekemäsi muutokset myös muualle
ohjelmaan. Vastaa tähän kyllä.</p>
<p>Nyt meillä on edessä uusi luokan raakile Main-metodeineen ja editori,
jolla ohjelma voidaan kirjoittaa.</p>
<h3>10.2.5 Ohjelman kääntäminen ja ajaminen</h3>
<p>Kun ohjelma on kirjoitettu, sen ajaminen onnistuu ylhäältä Debug-napista
(vihreä kolmio) tai klikkaamalla F5 (debug). Nappia painamalla VS
kääntää ohjelman automaattisesti ja suorittaa heti sen jälkeen ns.
debuggaustilassa. Ohjelma voidaan ajaa myös nopeammin ilman debuggausta
- mikä ei yleensä ole suositeltavaa - painamalla Ctrl+F5 (start without
debugging). Jos haluamme lopettaa ohjelman suorituksen jostain syystä
kesken kaiken, onnistuu se painamalla Shift+F5.</p>
<h3>10.2.6 Referenssien asettaminen</h3>
<p>Oman tai jonkun muun tekemän luokkakirjaston (.dll-tiedoston),
esimerkiksi kurssilla käytettävän Jypelin, voi lisätä VS:oon
seuraavasti. Valitse Solution Explorerista References ja klikkaa hiiren
oikealla napilla Add reference. Mene Browse-välilehdelle ja hae
haluamasi kirjasto, esimerkiksi Jypeli.dll. Nyt voit kirjoittaa
kooditiedoston alkuun lauseen</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
using Jypeli;</code></p>
<p>jolloin Jypeli-kirjasto on käytössäsi.</p>
<h2>10.3 Debuggaus</h2>
<p>Termi debug johtaa yhden legendan mukaan aikaan, jolloin
tietokoneohjelmissa ongelmia aiheuttivat releiden väliin lämmittelemään
päässeet luteet. Ohjelmien korjaaminen oli siis kirjaimellisesti
hyönteisten (bugs) poistoa. Katso lisätietoja Wikipediasta:</p>
<p><a href="http://en.wikipedia.org/wiki/Software_bug#Etymology">http://en.wikipedia.org/wiki/Software_bug#Etymology</a>.</p>
<p>Virheet ohjelmakoodissa ovat valitettava tosiasia. Ne tulevat aina
olemaan mukanasi, vaikka olisit kuinka hyvä ohjelmoija tahansa. Hyvä
ohjelmoija kuitenkin tiedostaa tämän asian, ja valmistautuu siihen
hankkimalla työkalut virheiden jäljittämistä ja korjaamista varten. On
olemassa pieniä virheitä, jotka eivät vaikuta ohjelman toimintaan
millään tavalla (kuten kirjoitusvirhe painonapissa), mutta on olemassa
myös virheitä, jotka ovat erittäin vakavia. Tällaiset virheet kaatavat
ohjelman. <em>Syntaksivirheet</em> voivat olla pieniä, mutta estävät ohjelmaa
kääntymästä. <em>Loogiset</em> <em>virheet</em> eivät jää kääntäjän kouriin, mutta
aiheuttavat ongelmia ohjelman ajon aikana.</p>
<p>Ehkäpä ohjelmasi ei onnistu lisäämään oikeaa tietoa tietokantaan, koska
tarvittava kenttä puuttuu, tai lisää väärän tiedon jossain olosuhteissa.
Tällaiset virheet, jossa sovelluksen logiikka on jollain tavalla
pielessä, ovat semanttisia virheitä tai loogisia virheitä.</p>
<p>Varsinkin monimutkaisemmista ohjelmista loogisen virheen löytäminen on
välillä vaikeaa, koska ohjelma ei kenties millään tavalla ilmoita
virheestä - huomaat vain lopputuloksesta virheen kuitenkin tapahtuneen.</p>
<p>Tähän erinomaisena apuna on VS:n virheidenjäljitystoiminto eli
<em>debuggaus</em>. Siinä ohjelman suoritusta voi seurata rivi riviltä, samoin
kuin muuttujien arvojen muuttumista. Tämä auttaa huomattavasti virheen
tai epätoivotun toiminnan syyn selvittämisessä. Vanha tapa tehdä samaa
asiaa on lisätä ohjelmaan tulostuslauseita, mutta sitten nämä ohjelman
muutokset jäävät helposti ohjelmaan ja saattavat toisinaan myös muuttaa
ohjelman toimintaa.</p>
<p>VS:ssa debuggaus aloitetaan asettamalla ensin johonkin kohtaan koodia
keskeytyskohta (breakpoint). Keskeytyskohta on kohta, johon haluamme
testauksen aikana ohjelman suorituksen väliaikaisesti pysähtyvän.
Ohjelman pysähdyttyä voidaan sitä sitten alkaa suorittamaan rivi
riviltä. Keskeytyskohta tulee siis asettaa koodiin ennen oletettua
virhekohtaa. Jos haluamme debugata koko ohjelman, asetamme vain
keskeytyskohdan ohjelman alkuun. Aseta keskeytyskohta kursorin kohdalle
painamalla F9 tai klikkaamalla koodi-ikkunassa rivinumeroiden vasemmalle
puolelle (harmaalle alueelle). Keskeytyskohta pitäisi näkyä punaisena
pallona ja rivillä oleva lause värjättynä punaisella.</p>
<p>Kun keskeytyskohta on asetettu, klikataan ylhäältä Debug-nappia tai
painetaan F5. Visual Studiossa ohjelman ”käynnistäminen” ja debuggauksen
käynnistäminen ovat sama asia. Toki ohjelma voidaan käynnistää ilman
debug-tilaakin, mutta silloin mahdolliset ohjelman virheet eivät tule
Visual Studion tietoon, ja esimerkiksi nollalla jakaminen kaataa ajetun
ohjelman.</p>
<p>Ohjelman suoritus on nyt pysähtynyt siihen kohtaan mihin asetimme
keskeytyskohdan. Avaa Locals-välilehti alhaalta, ellei se ole jo auki.
Debuggaus-näkymässä Locals-paneelissa näkyy kaikki tällä hetkellä
näkyvillä olevat muuttujat (paikalliset, eli lokaalit muuttujat) ja
niiden arvot. Keskellä näkyy ohjelman koodi, ja keltaisella se rivi
missä kohtaa ohjelmaa ollaan suorittamassa. Vasemalla näkyy myös
keltainen nuoli joka osoittaa sen hetkisen rivinumeron.</p>
<p>Ohjelman suoritukseen rivi riviltä on nyt kaksi eri komentoa: Step Into
(F11) ja Step Over (F10). Napit toimivat muuten samalla tavalla, mutta
jos kyseessä on aliohjelmakutsu, niin Step Into -komennolla mennään
aliohjelman sisälle, kun Step Over -komento suorittaa rivin kuin se
olisi yksi lause. Kaikki tällä hetkellä näkyvyysalueella olevat
muuttujat ja niiden arvot nähdään oikealla olevalla
Variables-välilehdellä.</p>
<p>Kun emme enää halua suorittaa ohjelmaa rivi riviltä, voimme joko
suorittaa ohjelman loppuun Debug → Continue (F5)-napilla tai keskeyttää
ohjelman suorituksen Terminate (Shift+F5)-napilla.</p>
<h2>10.4 Hyödyllisiä ominaisuuksia</h2>
<h3>10.4.1 Syntaksivirheiden etsintä</h3>
<p>Visual Studio huomaa osan syntaksivirheistä, joten osa virheistä voidaan
korjata jo ennen kääntämistä - tarkemmin sanottuna VS kääntää jatkuvasti
koodia havaitakseen ja ilmoittaakseen mahdolliset virheet. Kun VS löytää
virheen, ilmestyy Error List -paneeliin virheilmoitus ja tieto siitä
rivistä, jolla virheellinen koodi sijaitsee. Lisäksi jos virhe
paikallistuu alleviivaa VS virheellisen koodinpätkän punaisella
aaltoviivalla. Viemällä hiiri punaisen rastin päälle, VS kertoo
tarkemmin mikä kyseisessä kohdassa on vikana. Huomaa, että VS ei
välttämättä paikallista virhettä täysin oikein. Usein virhe voi olla
myös edellisellä tai seuraavalla rivillä.</p>
<h3>10.4.2 Kooditäydennys, IntelliSense</h3>
<p>IntelliSense on yksi VS:n parhaista ominaisuuksista. IntelliSense on
monipuolinen automaattinen koodin täydentäjä, sekä dokumentaatiotulkki.</p>
<p>Yksi dokumentaatioon perustuva IntelliSensen ominaisuus on
parametrilistojen selaus kuormitetuissa aliohjelmissa. Kirjoitetaan
esimerkiksi</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
String nimi = "Kalle";</code></p>
<p>Kun tämän jälkeen kirjoitetaan nimi ja piste ”.”, ilmestyy lista niistä
funktioaliohjelmista ja metodeista, jotka kyseisellä oliolla ovat
käytössä. Aliohjelman valinnan jälkeen klikkaa kaarisulku auki, jolloin
pienten nuolten avulla voi selata kyseessä olevan aliohjelman eri
”versioita”, eli samannimisiä aliohjelmia eri parametrimäärillä
varustettuna. Lisäksi saadaan lyhyt kuvaus metodin toiminnasta ja jopa
esimerkkejä käytöstä.</p>
<p>IntelliSense auttaa myös kirjoittamaan nopeammin ja erityisesti
ehkäisemään kirjoitusvirheiden syntymistä. Jos ei ole konekirjoituksen
Kimi Räikkönen, voi koodia kirjoittaessa helpottaa elämää painamalla
Ctrl+Space. Tällöin VS yrittää arvata (perustuen kirjoittamaasi tekstiin
sekä aiemmin kirjoittamaasi koodiin), mitä haluat kirjoittaa. Jos
mahdollisia vaihtoehtoja on monta, näyttää VS vaihtoehdot listana.</p>
<h3>10.4.3 Uudelleenmuotoilu</h3>
<p>Visual Studio pyrkii muotoilemaan koodia "kauniiksi" kirjoittamisen
yhteydessä. Käyttäjä voi kuitenkin tarkoituksellisesti tai vahingossa
rikkoa VS:n sisäisiä muotoilusäännöksiä. Tällöin koodi voidaan palauttaa
vastaamaan säännöksiä painamalla Edit → Advanced → Format Document tai
näppäinyhdistelmällä Ctrl+E, D (pidä Ctrl pohjassa, klikkaa ensin E ja
sitten D).</p>
<p>Jos välttämättä haluat, voit muokata VS:n koodimuotoilun oletussääntöjä
valikosta:</p>
<p>Tools → Options → Text editor → C# → Formatting.</p>
<h2>10.5 Lisätietoja Visual Studion käytöstä</h2>
<p>Kurssin Wikistä löytyy lisää vinkkejä ja neuvoja Visual Studion
käyttöön:</p>
<p><a href="https://trac.cc.jyu.fi/projects/ohj1">https://trac.cc.jyu.fi/projects/ohj1</a>.</p>
<p>\</p>
<h1>11. ComTest</h1>
<blockquote>
<p>“Program testing can be a very effective way to show the presence of
bugs, but is hopelessly inadequate for showing their absence.” -
Edsger W. Dijkstra</p>
</blockquote>
<p>Jo melko yksinkertaisten ohjelmien testaaminen ruudulle tulostelemalla
veisi paljon aikaa. Tulostukset pitäisi tehdä uudestaan jokaisen
muutoksen jälkeen, sillä emme voisi mitenkään tietää, että ennen
muutosta tekemämme testit toimisivat vielä muutoksen jälkeen.
Yksikkötestauksen idea on, että jokaiselle aliohjelmalle ja metodille
kirjoitetaan oma testinsä erilliseen tiedostoon, jotka voidaan sitten
kaikki ajaa kerralla. Näin voimme suorittaa kerran kirjoitetut testit
jokaisen pienenkin muutoksen jälkeen todella helposti.</p>
<p>Visual Studion Pro-, Premium- ja Ultimate-versioissa on olemassa
monipuoliset testausominaisuudet, mikä mahdollistaa ns. yksikkötestien
(unit tests) kirjoittamisen. Ongelmana on, että näiden VS:n tukemien
testitiedostojen kirjoittaminen on melko työlästä. Tähän on apuna
Jyväskylän yliopiston tietotekniikan laitoksella kehitetty
ComTest-työkalu, joka hyödyntää Visual Studion sisäänrakennettua
testausjärjestelmää, mutta madaltaa huomattavasti kynnystä kirjoittaa
ohjelmaansa yksikkötestejä. ComTest-testaustyökalun idea on, että testit
voidaan kirjoittaa yksinkertaisella syntaksilla aliohjelmien ja metodien
dokumentaatiokommentteihin suoraan ohjelman kooditiedostoon, joista
sitten luodaan varsinaiset Visual Studio -testiprojektit ja -tiedostot.
Samalla kirjoitetut testit toimivat dokumentaatiossa esimerkkinä
aliohjelman tai metodin toiminnasta. ComTest:n asennusohjeet löytyy
sivulta:</p>
<p><a href="https://trac.cc.jyu.fi/projects/comtest/wiki/ComTestCsharp">https://trac.cc.jyu.fi/projects/comtest/wiki/ComTestCsharp</a>.</p>
<p>Koska ComTest on vielä kehitysvaiheessa, löytyy sivuilta myös
ajankohtaisimmat tiedot ComTestin käytöstä.</p>
<h2>11.1 Käyttö</h2>
<p>ComTestistä johtuen luokan, jossa aliohjelmia halutaan testata, on
oltava julkisuusmääreellä public, muutoin testaaminen ei onnistu. Samoin
jokaisen testattavan aliohjelman on oltava public-aliohjelma.</p>
<p>Kirjoita aliohjelmaan dokumentaatiokommentit ja kommentoi aliohjelma.
Siirry dokumentaatiokommentin alaosaan, laita yksi tyhjä rivi (ilman
kauttaviivoja) ja kirjoita ”comt” ja painaa Tab+Tab (kaksi kertaa
sarkan-näppäintä). Tällöin Visual Studio luo valmiiksi paikan, johon
testit kirjoitetaan. Dokumentaatiokommentteihin pitäisi ilmestyä
seuraavat rivi.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
/// &lt;example&gt;
/// &lt;pre name="test"&gt;
/// 
/// &lt;/pre&gt;
/// &lt;/example&gt;</code></p>
<p>Testit kirjoitetaan pre-tagien sisälle. Ylläoleva syntaksi on
Doxygen-tyokalua (ja muita automaattisia dokumentointityökaluja) varten.</p>
<p>Aliohjelmat ja metodit testataan yksinkertaisesti antamalla niille
parametreja ja kirjoittamalla mitä niiden odotetaan palauttavan
annetuilla parametreilla. ComTest-testeissä käytetään erityistä
vertailuoperaattoria, jossa on kolme yhtä suuri kuin -merkkiä (===).
Tämä tarkoittaa, että arvon pitää olla sekä samaa tyyppiä, että
samansisältöinen. Kirjoitetaan Yhdista-aliohjelma, joka yhdistää kahden
annetun ei-negatiivisen luvun numerot toisiinsa. Tehdään aliohjelmalle
samalla testit. Aliohjelman toteutuksessa on (naiivisti) oletettu, että
parametrina annettavat luvut täyttävät varmasti annetun ehdon
(ei-negatiivinen). Myöhemmin opimme käsittelemään myös sellaiset
tilanteet, joissa tästä ehdosta ollaan poikettu.</p>
<p>Huomaa, että ComTest-testeihin kirjoitetuissa aliohjelmakutsuissa luokan
nimi täytyy antaa ennen aliohjelman nimeä. Tässä luokan nimeksi on
laitettu Laskuja.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
/// &lt;summary&gt;
/// Yhdistää kahden annetun ei-negatiivisen 
/// luvun numerot toisiinsa.
/// &lt;/summary&gt;
/// &lt;param name="a"&gt;Ensimmäinen luku&lt;/param&gt;
/// &lt;param name="b"&gt;Toinen luku&lt;/param&gt;
/// &lt;returns&gt;Yhdistetty luku&lt;/returns&gt;
/// &lt;example&gt;
/// &lt;pre name="test"&gt;
/// Laskuja.Yhdista(0, 0) === 0;
/// Laskuja.Yhdista(1, 0) === 10;
/// Laskuja.Yhdista(0, 1) === 1;
/// Laskuja.Yhdista(1, 1) === 11;
/// Laskuja.Yhdista(13, 2) === 132;
/// Laskuja.Yhdista(10, 0) === 100;
/// Laskuja.Yhdista(10, 87) === 1087;
/// Laskuja.Yhdista(10, 07) === 107;
/// &lt;/pre&gt;
/// &lt;/example&gt;
public static int Yhdista(int a, int b)
{
    String ab = a.ToString() + b; // Ei tehokkain toteutus, mutta yksinkertainen.
    int tulos = int.Parse(ab);    // Testien ansiosta toteutusta voidaan vaihtaa
    return tulos;                 // ja silti varmistaa että saadaan sama tulos. 
}</code></p>
<p>Tarkastellaan testejä nyt hieman tarkemmin.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Laskuja.Yhdista(0, 0) === 0;</code></p>
<p>Yllä olevalla rivillä testataan, että jos Yhdista-aliohjelma saa
parametrikseen arvot 0 ja 0, niin myös sen palauttavan arvon tulisi olla
0.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Laskuja.Yhdista(1, 0) === 10;</code></p>
<p>Seuraavaksi testataan, että jos parametreistä ensimmäinen on luku 1 ja
toinen luku 0, niin näiden yhdistelmä on 10, joten aliohjelman tulee
palauttaa luku 10. Nollan ja ykkösen yhdistelmä antaisi 01, mutta sitä
vastaava luku on tietenkin 1, joten se on luku jota palautuksena
odotamme, ja näin jatketaan.</p>
<p>Varsinaisen VS-testin voi nyt luoda ja ajaa painamalla Ctrl+Shift+Q tai
valikosta Tools → ComTest. Jos <em>Test Results</em> -välilehti (oletuksena
näytön alareunassa, ilmestyy ajettaessa ComTest) näyttää vihreää ja
lukee <em>Passed</em>, testit menivät oikein. Punaisen ympyrän tapauksessa
testit menivät joko väärin, tai sitten testitiedostossa on virheitä.</p>
<p>Myös testit täytyy testata. Voihan olla, että kirjoittamissamme
testeissä on myös virheitä. Tämä onkin syytä testata kirjoittamalla
testeihin virhe tarkoituksella. Tällöin Test Results -välilehdellä
pitäisi tietenkin näkyä punainen ympyrä. Jos näin ei ole, on joku
testeistä väärin, tai aliohjelmassa on virhe. Hyvien testien
kirjoittaminen on myös oma taitonsa. Kaikkia mahdollisia tilanteitahan
ei millään voi testata, joten joudumme valitsemaan, mille parametreille
testit tehdään. Täytyisi ainakin testata todennäköiset virhepaikat.
Näitä ovat yleensä ainakin kaikenlaiset "ääritilanteet".</p>
<p>Esimerkkinä olevassa Yhdista-aliohjelmassa ääritilanteita ovat lähinnä
nollat, kummallakin puolella erikseen ja yhdessä. Muutoin testiarvot on
valittu melko sattumanvaraisesti.</p>
<h2>11.2 Liukulukujen testaaminen</h2>
<p>Liukulukuja (double ja float) testataan ComTest:n
vertailuoperaattorilla, jossa on kolme aaltoviivaa (\~\~\~). Tämä johtuu
siitä, että kaikkia reaalilukuja ei pystytä esittämään tietokoneella
tarkasti, joten täytyy sallia pieni virhemarginaali. Tehdään
Keskiarvo-aliohjelma, joka osaa laskea kahden double-tyyppisen luvun
keskiarvon ja kirjoitetaan sille samalla dokumentaatiokommentit ja
ComTest-testit.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
/// &lt;summary&gt;
/// Aliohjelma laskee parametrinaan saamiensa kahden 
/// double-tyyppisen luvun keskiarvon.
/// &lt;/summary&gt;
/// &lt;param name="a"&gt;Ensimmäinen luku&lt;/param&gt;
/// &lt;param name="b"&gt;Toinen luku&lt;/param&gt;
/// &lt;returns&gt;Lukujen keskiarvo&lt;/returns&gt;
/// &lt;example&gt;
/// &lt;pre name="test"&gt;
/// Laskuja.Keskiarvo(0.0, 0.0)   ~~~  0.0;
/// Laskuja.Keskiarvo(1.2, 0.0)   ~~~  0.6;
/// Laskuja.Keskiarvo(0.8, 0.2)   ~~~  0.5;
/// Laskuja.Keskiarvo(-0.1, 0.1)  ~~~  0.0;
/// Laskuja.Keskiarvo(-1.5, -2.5) ~~~ -2.0;
/// &lt;/pre&gt;
/// &lt;/example&gt;
public static double Keskiarvo(double a, double b)
{
    return (a + b) / 2.0;
}</code></p>
<p>\</p>
<p>Liukulukuja testattaessa täytyy parametrit antaa desimaaliosan kanssa.
Esimerkiksi jos yllä olevassa esimerkissä ensimmäinen testi olisi muotoa
Keskiarvo(0, 0) \~\~\~ 0.0, niin tällöin ajettaisiin aliohjelma
Keskiarvo(int x, int y), <em>jos</em> sellainen olisi olemassa.</p>
<h1>12. Merkkijonot</h1>
<p>Tässä luvussa tutustumme tarkemmin merkkijonoihin. Merkkijonot voidaan
jakaa <em>muuttumattomiin</em> ja <em>muokattaviin</em>. C#:n muuttumaton merkkijono
on tyypiltään String, johon olemmekin jo hieman tutustuneet olioiden
yhteydessä. Muuttumatonta merkkijonoa ei voi muuttaa luomisen jälkeen.
Muokattavan merkkijonon, StringBuilder-olion, käsittely on joissakin
tilanteessa mielekkäämpää. Vaikka String-olioita ei voikaan muuttaa,
pärjäämme sillä monissa tilanteissa.</p>
<p>Huomaa, että string (pienellä alkukirjaimella kirjoitettuna) on
System.String-luokan alias, joten string ja String voidaan samaistaa
muuttujien tyyppimäärittelyssä, vaikka tarkasti ottaen toinen on alias
ja toinen luokan nimi. Yksinkertaisuuden vuoksi jatkossa puhutaan
pääsääntöisesti vain String-tyypistä sillä oletuksella, että
System-nimiavaruus on otettu käyttöön lauseella using System;</p>
<h2>12.1 Alustaminen</h2>
<p>Merkkijono on <em>kokoelma peräkkäisiä merkkejä</em>. Tarkalleen ottaen
merkkijono toteutetaan C#:ssa sisäisesti taulukkona, joka sisältää
merkkejä (char). Taulukoista on tässä monisteessa oma lukunsa myöhemmin.</p>
<p>Olioiden yhteydessä tutustuimme jo hieman String-tyyppiin. Sen voi siis
alustaa kahdella tavalla:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
String henkilo1 = new string(new char[] {'J', 'a', 'n', 'n', 'e'});
String henkilo2 = "Kalle Korhonen";</code></p>
<p>Jälkimmäinen tapa muistuttaa enemmän alkeistietotyyppien alustamista,
mutta merkkijonot ovat C#:ssa siis aina <em>olioita</em>.</p>
<h2>12.2 Hyödyllisiä metodeja ja ominaisuuksia</h2>
<p>String-luokassa on paljon hyödyllisiä metodeja, joista käsitellään nyt
muutama. Kaikki metodit näet C#:n
<a href="http://msdn.microsoft.com/en-us/library/system.string.aspx">MSDN-dokumentaatiosta</a>.</p>
<ul>
<li><a href="http://msdn.microsoft.com/en-us/library/858x0yyx.aspx">Equals(String)</a>
    Palauttaa tosi jos kaksi merkkijonoa ovat sisällöltään samat
    merkkikoko huomioon ottaen. Muutoin palauttaa epätosi.</li>
</ul>
<p><code>{.esimerkkisisennetty-western lang="zxx" xml:lang="zxx"}
if (etunimi.Equals("Aku")) Console.WriteLine("Löytyi!");</code></p>
<ul>
<li><a href="../../../Users/anlakane/Downloads/Length%20Palauttaa%20merkkijonon%20pituuden.String%20henkilo%20=%20%22Ville%22%3BConsole.WriteLine(henkilo.Length)%3B%20//tulostaa%205">Compare(String, String,
    Boolean)</a>
    Vertaa merkkijonon aakkosjärjestystä toiseen merkkijonoon. Palauttaa
    arvon 0 jos merkkijonot ovat samat, nollaa pienemmän arvon jos
    ensimmäinen merkkijono on aakkosjärjestykseltään ennen kuin toinen
    ja nollaa suuremman arvon jos jälkimmäinen merkkijono on aakkosissa
    ennen ensimmäistä. Kirjainkoko saadaan merkitseväksi asettamalla
    kolmanneksi parametriksi false, tai jos halutaan unohtaa
    kirjainkoko, laitetaan kolmanneksi true.</li>
</ul>
<p><code>{.esimerkkisisennetty-western lang="zxx" xml:lang="zxx"}
String s1 = "jAnNe"; String s2 = "JANNE";
if (Compare(s1, s2, true) == 0) Console.WriteLine("Samat tai melkein samat!"); // samat tai melkein samat</code></p>
<ul>
<li><a href="http://msdn.microsoft.com/en-us/library/dy85x1sa.aspx">Contains(String)</a>
    Palauttaa totuusarvon sen perusteella esiintyykö parametrin
    sisältämän merkkijono tutkittavana olevassa merkkijonossa.</li>
</ul>
<p><code>{.esimerkkisisennetty-western lang="zxx" xml:lang="zxx"}
String henkilo = "Ville Virtanen";
String haettava = "irta";
if (henkilo.Contains(haettava)) Console.WriteLine(haettava + " löytyi!");</code></p>
<ul>
<li><a href="http://msdn.microsoft.com/en-us/library/hxthx5h6.aspx">Substring(Int32)</a>
    Palauttaa osan merkkijonosta alkaen parametrinaan saamastaan
    indeksistä.</li>
</ul>
<p><code>{.esimerkkisisennetty-western lang="zxx" xml:lang="zxx"}
String henkilo = "Ville Virtanen";
String sukunimi = henkilo.Substring(6);
Console.WriteLine(sukunimi); // Virtanen</code></p>
<ul>
<li><a href="http://msdn.microsoft.com/en-us/library/aka44szs.aspx">Substring(Int32,
    Int32)</a>
    Palauttaa osan merkkijonosta parametrinaan saamiensa indeksien
    välistä. Ensimmäinen parametri on palautettavan merkkijonon
    ensimmäisen merkin indeksi ja toinen parametri palautettavien
    merkkien määrä.</li>
</ul>
<p><code>{.esimerkkisisennetty-western lang="zxx" xml:lang="zxx"}
String henkilo = "Ville Virtanen";
String etunimi = henkilo.Substring(0, 5); 
Console.WriteLine(etunimi); // Ville</code></p>
<ul>
<li><a href="http://msdn.microsoft.com/en-us/library/system.string.tolower.aspx">ToLower()</a>
    Palauttaa merkkijonon niin, että kaikki kirjaimet on muutettu
    pieniksi kirjaimiksi.</li>
</ul>
<p><code>{.esimerkkisisennetty-western lang="zxx" xml:lang="zxx"}
String henkilo = "Ville Virtanen";
Console.WriteLine(henkilo.ToLower()); //tulostaa "ville virtanen"</code></p>
<ul>
<li><a href="http://msdn.microsoft.com/en-us/library/system.string.toupper.aspx">ToUpper()</a>
    Palauttaa merkkijonon niin, että kaikki kirjaimet on muutettu
    isoiksi kirjaimiksi.</li>
</ul>
<p><code>{.esimerkkisisennetty-western lang="zxx" xml:lang="zxx"}
String henkilo = "Ville Virtanen";
Console.WriteLine(henkilo.ToUpper()); //tulostaa "VILLE VIRTANEN"</code></p>
<ul>
<li><a href="http://msdn.microsoft.com/en-us/library/czx8s9ts.aspx">Replace(Char,
    Char)</a> Korvaa
    merkkijonon kaikki tietyt merkit toisilla merkeillä. Ensimmäisenä
    parametrina korvattava merkki ja toisena korvaaja. Huomaa, että
    parametrit laitetaan char-muuttujille tyypilliseen tapaan
    yksinkertaisten lainausmerkkien sisään.</li>
</ul>
<p><code>{.esimerkkisisennetty-western lang="zxx" xml:lang="zxx"}
String sana = "katti";
sana = sana.Replace('t', 's');
Console.WriteLine(sana);  //tulostaa "kassi"</code></p>
<ul>
<li><a href="http://msdn.microsoft.com/en-us/library/fk49wtc1.aspx">Replace(String,
    String)</a>
    Korvaa merkkijonon kaikki merkkijonoesiintymät toisella
    merkkijonolla. Ensimmäisenä parametrina korvattava merkkijono ja
    toisena korvaaja.</li>
</ul>
<p><code>{.esimerkkisisennetty-western lang="zxx" xml:lang="zxx"}
String sana = "katti kattinen";
sana = sana.Replace("atti", "issa");
Console.WriteLine(sana);  //tulostaa "kissa kissanen"</code></p>
<p>Lisäksi</p>
<ul>
<li><a href="http://msdn.microsoft.com/en-us/library/system.string.length.aspx">Length</a>
    Palauttaa merkkijonon pituuden kokonaislukuna. Huomaa, että tämä EI
    ole aliohjelma / metodi, vaan merkkijono-olion <em>ominaisuus</em>.</li>
</ul>
<p><code>{.esimerkkisisennetty-western lang="zxx" xml:lang="zxx"}
String henkilo = "Ville";
Console.WriteLine(henkilo.Length); //tulostaa 5</code></p>
<p>Koska merkkijono on kokoelma yksittäisiä char-merkkejä, saadaan
merkkijonon kukin merkki char-tyyppisenä laittamalla halutun merkin
paikkaindeksi merkkijono-olion perään hakasulkeiden sisään, esimerkiksi:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
String henkilo1 = "Seppo Sirkuttaja";
char kolmasKirjain;
int i = 2;
kolmasKirjain = henkilo1[i];  // indeksit menevät 0,1,2,3 jne...
Console.WriteLine(henkilo1 + " -nimen paikassa " + i + " oleva merkki on " + kolmasKirjain);</code></p>
<p>\</p>
<p>Merkkijonojen indeksointi alkaa nollasta! Merkkijonon ensimmäinen merkki
on siis indeksissä 0.</p>
<h2>12.3 Muokattavat merkkijonot: StringBuilder</h2>
<p>Niin sanottujen muuttumattomien merkkijonojen, eli String-tyypin,
lisäksi C#:ssa on muuttuvia merkkijonoja. Muuttuvien merkkijonojen idea
on, että voimme lisätä ja poistaa siitä merkkejä luomisen jälkeen.
String-tyyppisen merkkijonon muuttaminen ei onnistu sen luomisen
jälkeen. Käytännössä, jos haluamme muuttaa String-merkkijonoa, tehdään
uusi olio. Jos merkkijonoon tehdään paljon muutoksia (esimerkiksi jonoon
lisätään useaan kertaan merkkejä), käy käsittely lopulta hitaaksi - ja
tämä hitaus alkaa näkyä melko nopeasti.</p>
<p>C#-kielessä muokattava merkkijonoluokka on StringBuilder, joka
sijaitsee System.Text-nimiavaruudessa. Voit ottaa tuon nimiavaruuden
käyttöön kirjoittamalla ohjelman alkuun</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
using System.Text;</code></p>
<p>Merkkijonon perään lisääminen onnistuu Append-metodilla.
Append-metodilla voi lisätä merkkijonon perään muun muassa kaikkia C#:n
alkeistietotyyppejä sekä String-olioita. Myös kaikkien C#:n valmiina
löytyvien olioiden lisääminen onnistuu Append-metodilla, sillä ne
sisältävät ToString-metodin, jolla oliot voidaan muuttaa merkkijonoksi.
Alla oleva koodinpätkä esittelee Append-metodia.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
double a = 3.5;
int b = 6;
double c = 9.5;</p>
<p>StringBuilder yhtalo = new StringBuilder(); // "" (tyhjä)
yhtalo.Append("f(x): "); // "f(x): "
yhtalo.Append(a);        // "f(x): 3.5"
yhtalo.Append(" + ");    // "f(x): 3.5 + "
yhtalo.Append(b);        // "f(x): 3.5 + 6"
yhtalo.Append('x');      // "f(x): 3.5 + 6x"
yhtalo.Append(" = ");    // "f(x): 3.5 + 6x = "
yhtalo.Append(c);        // "f(x): 3.5 + 6x = 9.5"
```</p>
<p>Tiettyyn paikkaan voidaan lisätä merkkejä ja merkkijonoja
Insert-metodilla, joka saa parametrikseen indeksin, eli kohdan, johon
merkki (tai merkit) lisätään, sekä lisättävän merkin (tai merkit).
Indeksointi alkaa jälleen nollasta. Insert-metodilla voi lisätä myös
kaikkia samoja tietotyyppejä kuin Append-metodillakin. Voisimme
esimerkiksi lisätä edelliseen esimerkkiin luvun 3.5 perään vielä
muuttujan x. Sitä ennen tarkastellaan merkkien järjestystä ja
indeksointia ja kirjoitetaan kunkin tulostuvan merkin yläpuolelle sen
paikkaindeksi.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
012345678901234567890
|----+----|----+----|
f(x): 3,5 + 6x = 9,5</code></p>
<p>Tästä huomaamme, että indeksi, johon haluamme muuttujan x lisätä, on 9.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
yhtalo.Insert(9,'x'); //yhtalo: "f(x): 3.5x + 6x = 9.5"</code></p>
<p>Huomaa, että Insert-metodi ei korvaa indeksissä 9 olevaa merkkiä, vaan
lisää merkkijonoon kokonaan uuden merkin, jolloin merkkijonon pituus
kasvaa siis yhdellä. Korvaamiseen on olemassa oma metodi, Replace. Tai
yksittäisen kirjaimen voi vaihtaa suoraam myös yhtalo[2] = 'y';</p>
<h3>12.3.1 Muita StringBuilder-luokan hyödyllisiä metodeja</h3>
<ul>
<li>
<p><a href="http://msdn.microsoft.com/en-us/library/system.text.stringbuilder.remove.aspx">Remove(Int32,
    Int32)</a>.
    Poistaa merkkijonosta merkkejä siten, että ensimmäinen parametri on
    aloitusindeksi, ja toinen parametri on poistettavien merkkien määrä.</p>
</li>
<li>
<p><a href="http://msdn.microsoft.com/en-us/library/4b1063zt.aspx">ToString()</a>
    ja <a href="http://msdn.microsoft.com/en-us/library/fyx7s61s.aspx">ToString(Int32,
    Int32)</a>.
    Palauttaa StringBuilder-olion sisällön ”tavallisena”
    String-merkkijonona. ToString-metodille voi antaa myös kaksi
    int-lukua parametreina, jolloin palautetaan osa merkkijonosta (ks.
    <a href="http://msdn.microsoft.com/en-us/library/aka44szs.aspx">Substring</a>).</p>
</li>
</ul>
<p>Muut metodit löytyvät StringBuilder-luokan MSDN-dokumentaatiosta:</p>
<p><a href="http://msdn.microsoft.com/en-us/library/k5314fdf.aspx">http://msdn.microsoft.com/en-us/library/k5314fdf.aspx</a>.</p>
<h2>12.4 Huomautus: aritmeettinen + vs. merkkijonoja yhdistelevä +</h2>
<p>Merkkijonoihin voidaan ”+”-merkkiä käyttämällä yhdistellä myös myös
numeeristen muuttujien arvoja. Tällöin ero siinä, että toimiiko
”+”-merkki aritmeettisena operaattorina vai merkkijonoja yhdistelevänä
operaattorina on todella pieni. Tutki alla olevaa esimerkkiä.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
public class PlusMerkki 
{<br />
   public static void Main() 
   {
      int luku1 = 2;
      int luku2 = 5;</p>
<pre><code>  //tässä "+"-merkki toimii aritmeettisena operaattorina
  Console.WriteLine(luku1 + luku2); //tulostaa 7

  //tässä "+"-merkki toimii merkkijonoja yhdistelevänä operaattorina
  Console.WriteLine(luku1 + "" + luku2);  //tulostaa 25

  //Tässä ensimmäinen "+"-merkki toimii aritmeettisena operaattorina
  //ja toinen "+"-merkki merkkijonoja yhdistelevänä operaattorina
  Console.WriteLine(luku1 + luku2 + "" + luku1); //tulostaa 72
</code></pre>
<p>}
}
```</p>
<p>Merkkijonojen yhdistäminen luo aina uuden olion ja siksi sitä on
käytettävä harkiten, silmukoissa jopa kokonaan StringBuilderillä ja
Append-metodilla korvaten.</p>
<h2>12.5 Vinkki: näppärä tyyppimuunnos String-tyypiksi</h2>
<p>Itse asiassa lisäämällä muuttujaan ”+”-merkillä merkkijono, tekee C#
automaattisesti tyyppimuunnoksen ja muuttaa muuttujasta ja siihen
lisätystä merkkijonosta String-tyyppisen. Tämän takia voidaan
alkeistietotyyppiset muuttujat muuttaa näppärästi String-tyyppisiksi
lisäämällä muuttujan eteen tyhjä merkkijono.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
int luku = 23;
bool totuusarvo = false;</p>
<p>String merkkijono1 = "" + luku;
String merkkijono2 = "" + totuusarvo;
```</p>
<p>Ilman tuota tyhjän merkkijonon lisäämistä tämä ei onnistuisi, sillä
String-tyyppiseen muuttujaan ei tietenkään voi tallentaa int- tai
bool-tyyppistä muuttujaa.</p>
<p>Tämä ei kuitenkaan mahdollista reaaliluvun muuttamista String-tyypiksi
tietyllä tarkkuudella. Tähän on apuna String-luokan Format-metodi.</p>
<h2>12.6 Reaalilukujen muotoilu String.Format-metodilla</h2>
<p>String-luokan Format-metodi tarjoaa monipuoliset muotoilumahdollisuudet
useille tietotyypeille, mutta katsotaan tässä kuinka sillä voi muotoilla
reaalilukuja. Math-luokasta saa luvun pii 20 desimaalin tarkkuudella
kirjoittamalla Math.PI. Huomaa, että PI ei ole metodi, joten perään ei
tule sulkuja. PI on Math-luokan julkinen staattinen vakio (public const
double). Jos haluaisimme muuttaa piin String-tyypiksi vain kahden
desimaalin tarkkuudella, onnistuisi se seuraavasti:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
String pii = String.Format("{0:#.##}", Math.PI); // pii = "3,14"</code></p>
<p>Tässä Format-metodi saa kaksi parametria. Ensimmäistä parametria
sanotaan muotoilumerkkijonoksi (format string). Toisena parametrina on
sitten muotoiltava arvo. Muotoilumahdollisuuksia on hyvin paljon. Alla
muutamia esimerkkejä erilaisten lukujen muotoilusta. Lisää löydät
<a href="http://msdn.microsoft.com/en-us/library/26etazsy.aspx">MSDN-dokumentaatiosta kohdasta Formatting
types</a>.</p>
<p><img alt="\
 Kuva 12: Muotoilujonoilla voidaan muotoilla lukuja monipuolisesti.
Tämän esimerkin lähdekoodi löytyy osoitteesta
https://trac.cc.jyu.fi/projects/ohj1/browser/luentomonistecs/esimerkit/StringFormat.cs" src="../src/luentomonistecsUusin_htm_356ed562.png" /></p>
<p>\
 Esimerkkisarakkeista kolmas, {0,11:##0.0} (otsikkoriviltä puuttuu tuo
,11 joka määrää kentän viemän minimitilan), on esimerkki siitä, miten
erilaisia lukuja saadaan järjestettyä siististi myös päällekkäin
desimaalipisteen (tai -pilkun) kohdalta. Risuaita tarkoittaa, että jos
luvussa ei ole sen merkin kohdalla numeroa, se jätetään pois paitsi
E-muotoilussa. Sarakkeissa 3 ja 4 pisteen etupuolella olevalla
#-merkillä ei ole vaikutusta tulokseen. Nolla sen sijaan ”pakottaa”
numeron sen merkin paikalle, vaikka syötteessä ei sen merkin kohdalla
olisikaan mitään: esimerkiksi syötteet 17 ja 0 muuttuvat 17.0:ksi ja
0.0:ksi. Esimerkiksi rahaan liittyvissä sovelluksissa oletuksena
desimaalipisteen jälkeen olisi mielekästä olla kaksi nollaa, jolloin
”nollasentit” näytetään joka tapauksessa, myös rahamäärän ollessa
tasasumma.</p>
<p>Huomaa, että ylläolevissa esimerkeissä oletetaan, että järjestelmän
desimaalierottimena on pilkku. Tämä on järjestelmäkohtaista ja
muutettavissa esimerkiksi Windows 7:ssa Control panel → Region and
language → Formats → Additional settings → Decimal symbol.</p>
<p>Muotoilumerkkijono laitetaan lainausmerkkeihin, ja aaltosulkujen sisään.
Muotoilujonoja voi olla myös useita, samoin muotoiltavia merkkijonoja.
Tästä esimerkki alla.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
String luvut = String.Format("{0} {2} {1} {0}", 1, 2, 3);
Console.WriteLine(luvut); // Tulostaa: 1 3 2 1</code></p>
<p>Muotoillun jonon (ja sen määrittelyn) voi antaa myös suoraan esimerkiksi
WriteLine-metodille. Alla edellinen esimerkki lyhyemmin kirjoitettuna.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Console.WriteLine("{0} {2} {1} {0}", 1, 2, 3);</code></p>
<p>Lisätietoja merkkijonojen muotoilusta löytyy MSDN-dokumentaatiosta:</p>
<p><a href="http://msdn.microsoft.com/en-us/library/0c899ak8.aspx">http://msdn.microsoft.com/en-us/library/0c899ak8.aspx.</a></p>
<h1>13. Ehtolauseet (Valintalauseet)</h1>
<blockquote>
<p>“Älä turhaan käytä iffiä, useimmiten pärjäät ilmankin” -Vesa
Lappalainen</p>
</blockquote>
<h2>13.1 Mihin ehtolauseita tarvitaan?</h2>
<p>Tehtävä: Suunnittele aliohjelma, joka saa parametrina kokonaisluvun.
Aliohjelman tulee palauttaa true (tosi), jos luku on parillinen ja false
(epätosi), jos luku on pariton.</p>
<p>Tämänhetkisellä tietämyksellä yllä olevan kaltainen aliohjelma olisi
lähes mahdoton toteuttaa. Pystyisimme kyllä selvittämään onko luku
parillinen, mutta meillä ei ole keinoa muuttaa paluuarvoa sen mukaan,
onko luku parillinen vai ei. Kun ohjelmassa haluamme tehdä eri asioita
riippuen esimerkiksi käyttäjän syötteestä tai aliohjelmien
parametreista, tarvitsemme ehtolauseita.</p>
<h2>13.2 if-rakenne: ”Jos aurinko paistaa, mene ulos.”</h2>
<p>Tavallinen ehtolause sisältää aina sanan ”jos”, ehdon sekä toimenpiteet
mitä tehdään, jos ehto on tosi. Arkielämän naiivi ehtolause voitaisiin
ilmaista vaikka seuraavasti:</p>
<blockquote>
<p>Jos aurinko paistaa, mene ulos.</p>
</blockquote>
<p>Hieman monimutkaisempi ehtolause voisi sisältää myös ohjeen, mitä
tehdään, jos ehto ei pädekään:</p>
<blockquote>
<p>Jos aurinko paistaa, mene ulos, muuten koodaa sisällä.</p>
</blockquote>
<p>Molemmille rakenteille löytyy C#:sta vastineet. Tutustutaan ensiksi
ensimmäiseen eli if-rakenteeseen.</p>
<p>Yleisessä muodossa C#:n if-rakenne on alla olevan kaltainen:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
if (ehto) lause;</code></p>
<p>Esimerkki ehtolause: ”Jos aurinko paistaa, mene ulos” voidaan nyt
esittää C#:n syntaksin mukaan seuraavasti.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
if (aurinkoPaistaa) MeneUlos();</code></p>
<p>Jos ehdon ollessa totta täytyy suorittaa useampia lauseita, tulee ehdon
jälkeen muodostaa oma lohko.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
if (ehto) 
{
  lause1;
  lause2;
  ...
  lauseN;
}</code></p>
<p>Ehto on aina joku <em>looginen lauseke</em>. Looginen lauseke voi saada vain
kaksi arvoa: tosi (true) tai epätosi (false). Jos looginen lauseke saa
arvon tosi, perässä oleva lause tai lauseet suoritetaan, muuten ei tehdä
mitään ja jatketaan ohjelman suoritusta. Looginen lauseke voi sisältää
muun muassa lukuarvoja, joiden suuruuksia voidaan vertailla
vertailuoperaattoreilla.</p>
<p>Vuokaaviolla if-rakennetta voisi kuvata seuraavasti:</p>
<p><img alt="\
 Kuva 13: if-rakenne
vuokaaviona" src="../src/luentomonistecsUusin_htm_194ffc3.png" /></p>
<p>\
 \</p>
<p><em>Vuokaavio =</em> Kaavio, jolla mallinnetaan <em>algoritmia</em> tai prosessia.</p>
<p>Ennen kuin if-rakenteesta voidaan antaa esimerkkiä, tarvitsemme hieman
tietoa vertailuoperaattoreista.</p>
<h2>13.3 Vertailuoperaattorit</h2>
<p>Vertailuoperaattoreilla voidaan vertailla aritmeettisia arvoja.</p>
<p>Taulukko 6: C#:n vertailuoperaattorit.</p>
<p>Operaat-tori</p>
<p>Nimi</p>
<p>Toiminta</p>
<p>==</p>
<p>yhtä suuri kuin</p>
<p>Palauttaa tosi, jos vertailtavat arvot yhtä suuret.</p>
<p>!=</p>
<p>eri suuri kuin</p>
<p>Palauttaa tosi, jos vertailtavat arvot erisuuret.</p>
<p>></p>
<p>suurempi kuin</p>
<p>Palauttaa tosi, jos vasemmalla puolella oleva luku on suurempi.</p>
<p>>=</p>
<p>suurempi tai yhtä suuri kuin</p>
<p>Palauttaa tosi, jos vasemmalla puolella oleva luku on suurempi tai yhtä
suuri</p>
<p>\&lt;</p>
<p>pienempi kuin</p>
<p>Palauttaa tosi, jos vasemmalla puolella oleva luku on pienempi.</p>
<p>\&lt;=</p>
<p>pienempi tai yhtä suuri kuin</p>
<p>Palauttaa tosi, jos vasemmalla puolella oleva luku on pienempi tai yhtä
suuri.</p>
<p>\</p>
<p>\</p>
<p>\</p>
<h3>13.3.1 Huomautus: sijoitusoperaattori (=) ja vertailuoperaattori (==)</h3>
<p>Muistathan, ettei sijoitusoperaattoria (=) voi käyttää vertailuun. Tämä
on yksi yleisimmistä ohjelmointivirheistä. Vertailuun aina kaksi <strong>=</strong>
-merkkiä ja sijoitukseen yksi. Tästä seuraava esimerkki.</p>
<h2>13.4 Esimerkki: yksinkertaisia if-lauseita</h2>
<p>Yhtäsuuruuden vertailuoperaattorissa on kaksi yhtä suuri kuin -merkkiä.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
if (henkilonIka == 20) Console.WriteLine(”Onneksi olkoon!”);</code></p>
<p>Alla oleva aiheuttaa virheilmoituksen.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
if (henkilonIka = 20) Console.WriteLine(”Onneksi olkoon!”); // Virhe!</code></p>
<p>Seuraava esimerkki havainnollistaa toisen vertailuoperaattorin käyttöä.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
if (luku &lt; 0) Console.WriteLine("Luku on negatiivinen");</code></p>
<p>Yllä oleva lauseke tulostaa "Luku on negatiivinen", jos muuttuja luku on
pienempi kuin nolla. Ehtona on siis looginen lauseke luku \&lt; 0, joka saa
arvon "tosi", aina kun muuttuja luku on nollaa pienempi. Tällöin perässä
oleva lause tai lohko suoritetaan.</p>
<h2>13.5 if-else -rakenne</h2>
<p>if-else -rakenne sisältää myös kohdan mitä tehdään jos ehto ei olekaan
tosi.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Jos aurinko paistaa mene ulos, muuten koodaa sisällä.</code></p>
<p>Yllä oleva lause sisältää ohjelmoinnin if-else -rakenteen idean. Siinä
on ehto ja ohje mitä tehdään jos ehto on tosi sekä ohje mitä tehdään jos
ehto on epätosi. Lauseen voisi kirjoittaa myös:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
jos (aurinko paistaa) mene ulos
muuten koodaa sisällä</code></p>
<p>Yllä oleva muoto on jo useimpien ohjelmointikielten syntaksin mukainen.
Siinä ehto on erotettu sulkeiden sisään, ja perässä on ohje, mitä
tehdään, jos ehto on tosi. Toisella rivillä sen sijaan on ohje mitä
tehdään, jos ehto on epätosi. C#:n syntaksin mukaiseksi ohjelma
saadaan, kun ohjelmointikieleen kuuluvat sanat muutetaan englanniksi.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
if (aurinko paistaa) mene ulos;
else koodaa sisällä;</code></p>
<p>if-else -rakenteen yleinen muoto:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
if (ehto) lause1;
else lause2;</code></p>
<p>\
 \</p>
<p>Kuten pelkässä if-rakenteessa myös if-else -rakenteessa lauseiden
tilalla voi olla myös lohko.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
if (ehto) 
{
  lause1;
  lause2;
  lause3;
} 
else 
{
  lause4;
  lause5;
}</code></p>
<p>if-else -rakennetta voisi sen sijaan kuvata seuraavalla vuokaaviolla:</p>
<p><img alt="\
 Kuva 14: if-else-rakenne
vuokaaviona." src="../src/luentomonistecsUusin_htm_m19490db5.gif" /></p>
<p>\
 \</p>
<h3>13.5.1 Esimerkki: Pariton vai parillinen</h3>
<p>Tehdään aliohjelma joka palauttaa true jos luku on parillinen ja false
jos luku on pariton.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
public static bool OnkoLukuParillinen(int luku) 
{
   if ((luku % 2) == 0) return true;
   else return false;
}</code></p>
<p>Aliohjelma saa parametrina kokonaisluvun ja palauttaa siis true, jos
kokonaisluku oli parillinen ja false, jos kokonaisluku oli pariton.
Toisella rivillä otetaan muuttujan luku ja luvun 2 jakolaskun
jakojäännös. Jos jakojäännös on 0, niin silloin luku on parillinen, eli
palautetaan true. Jos jakojäännös ei mennyt tasan, niin silloin luvun on
pakko olla pariton eli palautetaan false.</p>
<p>Itse asiassa, koska aliohjelman suoritus päättyy return-lauseeseen,
voitaisiin else-sana jättää kokonaan pois, sillä else-lauseeseen mennään
ohjelmassa nyt vain siinä tapauksessa, että if-ehto <em>ei</em> ollut tosi.
Voisimmekin kirjoittaa aliohjelman hieman lyhyemmin seuraavasti:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
public static bool OnkoLukuParillinen(int luku) 
{
   if ((luku % 2) == 0) return true;
   return false;  // Huom! Ei tarvita else
}</code></p>
<p>Usein if-lauseita käytetään aivan liikaa. Tämänkin esimerkin voisi yhtä
hyvin kirjoittaa vieläkin lyhyemmin (ei aina selkeämmin kaikkien
mielestä) seuraavasti:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
public static bool OnkoLukuParillinen(int luku) 
{
   return ((luku % 2) == 0);
}</code></p>
<p>Tämä johtuu siitä, että lauseke ((luku % 2) == 0), on true jos luku on
parillinen ja muuten false. Saman tien voimme siis palauttaa suoraan
tuon lausekkeen arvon, ja aliohjelma toimii kuten aiemminkin.</p>
<p>Loogisia arvoja ei ole koskaan tyylikästä testata muodossa</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
if ( ( a &lt; 5 ) == true ) ...    // vaan if ( a &lt; 5 ) ...
if ( ( a &lt; 5 ) == false ) ...   // vaan if ( a &gt;= 5 ) ... tai if ( 5 &lt;= a ) ...
if ( OnkoLukuParillinen(3) == false ) ... // vaan if ( !OnkoLukuParillinen(3) ) ...</code></p>
<h2>13.6 Loogiset operaattorit</h2>
<p>Loogisia lausekkeita voidaan myös yhdistellä <em>loogisilla
operaattoreilla</em>.</p>
<p>Taulukko 7: Loogiset operaattorit.</p>
<p>C#-koodi</p>
<p>Operaattori</p>
<p>Toiminta</p>
<p>!</p>
<p>looginen ei</p>
<p>Tosi, jos lauseke epätosi.</p>
<p>&amp;</p>
<p>looginen ja</p>
<p>Tosi, jos molemmat lausekkeet tosia.</p>
<p>&amp;&amp;</p>
<p>looginen ehdollinen ja</p>
<p>Tosi, jos molemmat lausekkeet tosia. Eroaa edellisestä siinä, että jos
lausekkeen totuusarvo on jo saatu selville, niin loppua ei enää
tarkisteta. Toisin sanoen jos ensimmäinen lauseke oli jo epätosi, niin
toista lauseketta ei enää suoriteta.</p>
<p>|</p>
<p>looginen tai</p>
<p>Tosi, jos toinen lausekkeista on tosi.</p>
<p>||</p>
<p>looginen ehdollinen tai</p>
<p>Tosi, jos toinen lausekkeista on tosi. Vastaavasti jos lausekkeen arvo
selviää jo aikaisemmin, niin loppua ei enää tarkisteta. Toisin sanoen,
jos ensimmäinen lauseke saa arvon tosi, niin koko lauseke saa arvon tosi
ja jälkimmäistä ei tarvitse enää tarkastaa.</p>
<p>\^</p>
<p>eksklusiivinen tai (XOR)</p>
<p>Tosi, <em>jos</em> toinen, <em>mutta</em> eivät molemmat, on tosi.</p>
<p>\</p>
<p>\</p>
<p>\</p>
<p>\
 \</p>
<p>\
 \</p>
<p>\
 \</p>
<p>\
 \</p>
<h3>13.6.1 Operaattoreiden totuustaulut</h3>
<p>Taulukko 8: Seuraavassa 0=epätosi, 1=tosi. Totuustaulu eri
operaattoreille.</p>
<p>p</p>
<p>q</p>
<p>p &amp; q</p>
<p>p | q</p>
<p>p \^ q</p>
<p>!p</p>
<p>0</p>
<p>0</p>
<p>0</p>
<p>0</p>
<p>0</p>
<p>1</p>
<p>0</p>
<p>1</p>
<p>0</p>
<p>1</p>
<p>1</p>
<p>1</p>
<p>1</p>
<p>0</p>
<p>0</p>
<p>1</p>
<p>1</p>
<p>0</p>
<p>1</p>
<p>1</p>
<p>1</p>
<p>1</p>
<p>0</p>
<p>0</p>
<h3>13.6.2 Operaattoreiden käyttö</h3>
<p>Ei-operaattori kääntää loogisen lausekkeen päinvastaiseksi.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
if (!(luku &lt;= 0)) Console.WriteLine("Luku on suurempi kuin nolla");</code></p>
<p>Ei-operaattori siis palauttaa vastakkaisen bool-arvon: todesta tulee
epätosi ja epätodesta tosi. Jos yllä olevassa lauseessa luku-muuttuja
saisi arvon 5, niin ehto luku \&lt;= 0 saisi arvon false. Kuitenkin
ei-operaattori saa arvon true, kun lausekkeen arvo on false, joten koko
ehto onkin true ja perässä oleva tulostuslause tulostuisi. Lause olisi
siis sama kuin alla oleva:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
if (0 &lt; luku) Console.WriteLine("Luku on suurempi kuin nolla");</code></p>
<p>Ja-operaatiossa molempien lausekkeiden pitää olla tosia, että koko ehto
olisi tosi.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
if ((1 &lt;= luku) &amp;&amp; (luku &lt;= 99)) Console.WriteLine("Luku on välillä 1-99");</code></p>
<p>Yllä oleva ehto toteutuu, jos luku välillä 1-99. Vastaava asia
voitaisiin hoitaa myös sisäkkäisillä ehtolauseilla seuraavasti</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
if (1 &lt;= luku)
  if (luku &lt;= 99) Console.WriteLine("Luku on välillä 1-99");</code></p>
<p>Tällaisia sisäkkäisiä ehtolauseita pitäisi kuitenkin välttää, sillä ne
lisäävät virhealttiutta ja vaikeuttavat testaamista.</p>
<p>Epäyhtälöiden lukemista voi helpottaa, mikäli ne kirjoitetaan niin,
käytetään aina pienempi kuin -merkkiä (nuolen kärki vasemmalle). Tällöin
epäyhtälön operandit ovat samassa järjestyksessä, kuin miten ihmiset
mieltävät lukujen suuruusjärjestyksen.</p>
<h3>13.6.3 De Morganin lait</h3>
<p>Huomaa, että joukko-opista ja logiikasta tutut <em>De Morganin lait</em>
pätevät myös loogisissa operaatioissa. Olkoon p ja q bool-tyyppisiä
muuttujia. Tällöin:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
!(p || q) sama asia kuin !p &amp;&amp; !q
!(p &amp;&amp; q) sama asia kuin !p || !q</code></p>
<p>Lakeja voisi testata alla olevalla koodinpätkällä vaihtelemalla
muuttujien p ja q arvoja. Riippumatta muuttujien p ja q arvoista
tulostusten pitäisi aina olla true.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
public class DeMorgansLaws
{
    /// &lt;summary&gt;
    /// Testiohjelma DeMorganin laeille
    /// &lt;/summary&gt;
    public static void Main()
    {
        bool p = true;
        bool q = true;
        Console.WriteLine(!(p || q) == (!p &amp;&amp; !q));
        Console.WriteLine(!(p &amp;&amp; q) == (!p || !q));
    }
}</code></p>
<p>De Morganin lakia käyttämällä voidaan lausekkeita joskus saada
sievemmiksi. Tällaisinaan lauseet tuntuvat turhilta, mutta jos p ja q
ovat esimerkiksi epäyhtälöitä:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
if (!(a &lt; 5  &amp;&amp; b &lt; 3)) ...
if (!(a &lt; 5) || ! (b &lt; 3)) ...
if (a &gt;= 5 || b &gt;= 3) ...</code></p>
<p>niin ei-operaattorin siirto voikin olla mielekästä. Toinen tällainen
laki on osittelulaki.</p>
<h3>13.6.4 Osittelulaki</h3>
<p>Osittelulaki sanoo, että:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
p * (q + r) = (p * q) + (p * r)</code></p>
<p>Samaistamalla * \&lt;=> &amp;&amp; ja + \&lt;=> || todetaan loogisille
operaatioillekin osittelulaki:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
p &amp;&amp; (q || r) = (p &amp;&amp; q) || (p &amp;&amp; r)</code></p>
<p>Päinvastoin kuin normaalissa logiikassa, loogisille operaatioille
osittelulaista on myös toinen versio:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
p || (q &amp;&amp; r) = (p || q) &amp;&amp; (p || r)</code></p>
<h2>13.7 else if-rakenne</h2>
<p>Jos muuttujalle täytyy tehdä monia toisensa poissulkevia tarkistuksia,
voidaan käyttää erityistä else if -rakennetta. Siinä on kaksi tai
useampia ehtolauseita ja seuraavaan ehtoon mennään vain, jos mikään
aikaisemmista ehdoista ei ollut tosi. Rakenne on yleisessä muodossa
seuraava.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
if (ehto1) lause1;
else if (ehto2) lause2;
else if (ehto3) lause3;
else lause4;</code></p>
<p>Alimpana olevaan else-osaan mennään nyt vain siinä tapauksessa, että
mikään yllä olevista ehdoista ei ollut tosi. Tämä rakenne esitellään
usein omana rakenteenaan, vaikka oikeastaan tässä on vain useita
peräkkäisiä if-else -rakenteita, joiden sisennys on vain hieman
poikkeava.</p>
<p><img alt="\
 Kuva 15: else-if-rakenne
vuokaaviona." src="../src/luentomonistecsUusin_htm_m28552086.gif" /></p>
<p>\
 Seuraava vuokaavio kuvaisi rakennetta, jossa on yksi if-lause ja sen
jälkeen kaksi else if-lausetta.</p>
<p>\</p>
<h3>13.7.1 Esimerkki: Tenttiarvosanan laskeminen</h3>
<p>Tehdään laitoksen henkilökunnalle aliohjelma, joka laskee opiskelijan
tenttiarvosanan. Parametrinaan aliohjelma saa tentin maksimipistemäärän,
läpipääsyrajan sekä opiskelijan pisteet. Aliohjelma palauttaa arvosanan
0-5 niin, että arvosanan 1 saa läpipääsyrajalla ja muut arvosanat
skaalataan mahdollisimman tasaisesti.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
using System;
/// <summary>
/// Laskee opiskelijan tenttiarvosanan. 
/// </summary>
public class LaskeTenttiArvosana
{
    /// <summary>
    /// Laskee tenttiarvosanan pistevälien mukaan.
    /// </summary>
    /// <param name="maksimipisteet">Tentin maksimipisteet</param>
    /// <param name="lapipaasyraja">Tentin läpipääsyraja</param>
    /// <param name="tenttipisteet">Opiskelijan saamat tenttipisteet</param>
    /// <returns>tenttiarvosana välillä 0-5.</returns>
    public static int LaskeArvosana(int maksimipisteet, int lapipaasyraja,
      int tenttipisteet)
    {
        //Lasketaan eri arvosanoille tasaiset pistevälit
        int arvosanojenPisteErot = (maksimipisteet - lapipaasyraja) / 5;
        int arvosana = 0;</p>
<pre><code>    if      (lapipaasyraja + 4 * arvosanojenPisteErot &lt;= tenttipisteet) arvosana = 5;
    else if (lapipaasyraja + 3 * arvosanojenPisteErot &lt;= tenttipisteet) arvosana = 4;
    else if (lapipaasyraja + 2 * arvosanojenPisteErot &lt;= tenttipisteet) arvosana = 3;
    else if (lapipaasyraja + 1 * arvosanojenPisteErot &lt;= tenttipisteet) arvosana = 2;
    else if (lapipaasyraja + 0 * arvosanojenPisteErot &lt;= tenttipisteet) arvosana = 1;
    return arvosana;
}

/// &lt;summary&gt;
/// Pääohjelmassa tehdään testitulostuksia
/// &lt;/summary&gt;
public static void Main()
{
    //Tehdään muutama testitulostus
    Console.WriteLine(LaskeArvosana(100, 50, 75));
    Console.WriteLine(LaskeArvosana(24, 12, 12));
}
</code></pre>
<p>}
```</p>
<p>Aliohjelmassa lasketaan aluksi eri arvosanojen välinen piste-ero, jota
käytetään arvosanojen laskemiseen. Arvosanojen laskeminen aloitetaan
ylhäältä alaspäin. Ehto voi sisältää myös aritmeettisia operaatioita.
Lisäksi alustetaan muuttuja arvosana, johon talletetaan opiskelijan
saama arvosana. Muuttujaan arvosana talletetaan 5, jos tenttipisteet
ylittävät läpipääsyrajan johon lisätään arvosanojen välinen piste-ero
kerrottuna neljällä. Jos opiskelijan pisteet eivät riittäneet arvosanaan
5, mennään seuraavaan else-if -rakenteeseen ja tarkastetaan riittävätkö
pisteet arvosanaan 4. Näin jatketaan edelleen kunnes kaikki arvosanat on
käyty läpi. Lopuksi palautetaan muuttujan arvosana arvo. Pääohjelmassa
aliohjelmaa on testattu muutamalla testitulostuksella.</p>
<p>Tässäkin esimerkissä monet if-lauseet voitaisiin välttää <em>silmukalla</em>
ja/tai <em>taulukoinnilla</em>. Tästä puhutaan luvussa 15.</p>
<h3>13.7.2 Harjoitus</h3>
<p>Miten ohjelmaa pitäisi muuttaa, jos pisteiden tarkastus aloitettaisiin
arvosanasta 0?</p>
<h3>13.7.3 Harjoitus</h3>
<p>Lyhenisikö koodi ja tarvittaisiinko else-lauseita, jos lause arvosana =
5; korvattaisiin lauseella return 5; ?</p>
<h2>13.8 switch-rakenne</h2>
<p>switch-rakennetta voidaan käyttää silloin, kun meidän täytyy suorittaa
valintaa yksittäisten kokonaislukujen tai merkkien (char) perusteella.
Jokaista odotettua muuttujan arvoa kohtaan on switch-rakenteessa oma
case-osa, johon kirjoitetaan toimenpiteet, jotka tehdään tässä
tapauksessa. Yleinen muoto switch-rakenteelle on seuraava.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
switch (valitsin) //valitsin on useimmiten joku muuttuja
{
   case arvo1:
      lauseet;
      break;</p>
<p>case arvo2:
      lauseet;
      break;</p>
<p>case arvoX:
      lauseet;
      break;</p>
<p>default:
      lauseet;
       break;
}
```</p>
<p>Jokaisessa case-kohdassa sekä default-kohdassa on lauseiden jälkeen
oltava lause, jolla hypätään pois switch-lohkosta. Ylläolevassa
esimerkissä hyppylauseena toimi break-lause. Toisin kuin joissain
esimerkiksi C++:ssa, ei C#:ssa sallita suorituksen siirtymistä
tapauksesta (case) toiseen, mikäli tapauksessa on yksikin lause.
Esimerkiksi seuraava koodi aiheuttaisi virheen.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
switch (valitsin)
{
    // Seuraava koodi aiheuttaa virheen
    case 1:
        Console.WriteLine("Tapaus 1...");
        // Tähän kuuluisi break-lause tai muu hyppylause!
    case 2:
        Console.WriteLine("... ja/tai tapaus 2");
        break;
}</code></p>
<p>Kuitenkin, tapauksesta toiseen ”valuttaminen” on sallittu, mikäli tapaus
<em>ei</em> sisällä yhtään lausetta. Seuraavassa on esimerkki tästä.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
int luku = 3;
switch (luku)
{
    case 0:
    case 1:
        Console.WriteLine("Luku on 0 tai 1");
        break;
    case 2:
        Console.WriteLine("Luku on 2");
        break;
    default:
        Console.WriteLine("Oletustapaus");
        break;
}</code></p>
<h3>13.8.1 Esimerkki: Arvosana kirjalliseksi</h3>
<p>Tehdään aliohjelma, joka saa parametrina tenttiarvosanan numerona (0-5)
ja palauttaa kirjallisen arvosanan String-oliona.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
/// <summary>
/// Palauttaa parametrina saamansa numeroarvosanan kirjallisena.
/// </summary>
/// <param name="numero">tenttiarvosana numerona</param>
/// <returns>tenttiarvosana kirjallisena</returns>
public static String KirjallinenArvosana(int numero) 
{
   String arvosana = "";
   switch(numero) 
   {
      case 0:
         arvosana = "Hylätty";
         break;</p>
<pre><code>  case 1:
     arvosana = "Välttävä";
     break;

  case 2:
     arvosana = "Tyydyttävä";
     break;

  case 3:
     arvosana = "Hyvä";
     break;

  case 4:
     arvosana = "Kiitettävä";
     break;

  case 5:
     arvosana = "Erinomainen";
     break;

  default:
     arvosana = "Virheellinen arvosana";
      break;
</code></pre>
<p>}
   return arvosana;
}
```</p>
<p>Koska return-lause lopettaa metodin toiminnan, voitaisiin yllä olevaa
aliohjelmaa lyhentää palauttamalla jokaisessa case-osassa suoraan
kirjallinen arvosana. Tällöin break-lauseet voisi jättää pois, sillä
return-lauseen ansiosta tapauksesta toiseen valuttaminen ei ole
mahdollista.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
public static String KirjallinenArvosana(int numero) 
{
   switch(numero) 
   {
      case 0:
         return "Hylätty";</p>
<pre><code>  case 1:
     return "Välttävä";

  case 2:
     return "Tyydyttävä";

  case 3:
     return "Hyvä";

  case 4:
     return "Kiitettävä";

  case 5:
     return "Erinomainen";

  default:
     return "Virheellinen arvosana";
</code></pre>
<p>}
}
```</p>
<p>break-lauseen voi siis turvallisesti jättää pois case-osasta, <em>jos</em>
case-osassa palautetaan joku arvo return-lauseella (tai kyseinen
case-osa ei sisällä yhtään lausetta). Muulloin break-lauseen
poisjättäminen johtaa virheeseen.</p>
<p>Lähes aina switch-rakenteen voi korvata if ja else-if -rakenteilla,
niinpä sitä on pidettävä vain yhtenä if-lauseena. Myös switch-rakenteen
voi usein välttää käyttämällä taulukoita.</p>
<h1>14. Olioiden ja alkeistietotyyppien erot</h1>
<p>Tehdään ohjelma, jolla demonstroidaan olioiden ja alkeistietotyyppien
eroja.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
/// <summary>
/// Tutkitaan olioviitteiden käyttöä ja käyttäytymistä.
/// </summary>
public class Olioviitteet
{
  /// <summary>
  /// Alustetaan muuttujia ja tulostetaan. 
  /// Testaillaan olioiden ja alkeismuuttujien eroja.
  /// </summary>
  public static void Main()
  {
    StringBuilder s1 = new StringBuilder("eka");
    StringBuilder s2 = new StringBuilder("eka");</p>
<pre><code>Console.WriteLine(s1 == s2);       // false
Console.WriteLine(s1.Equals(s2));  // true

int i1 = 11;
int i2 = 10 + 1;

Console.WriteLine(i1 == i2);       // true

int[] it1 = new int[1]; it1[0] = 3;
int[] it2 = new int[1]; it2[0] = 3;

Console.WriteLine(it1 == it2);       // false
Console.WriteLine(it1.Equals(it2));  // true
Console.WriteLine(it1[0] == it2[0]); // true

s2 = s1;
Console.WriteLine(s1 == s2);         // true
</code></pre>
<p>}
}
```</p>
<p>Tarkastellaan ohjelmaa hieman tarkemmin:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
StringBuilder s1 = new StringBuilder("eka");
StringBuilder s2 = new StringBuilder("eka");</code></p>
<p>Yllä luodaan kaksi StringBuilder-luokan ilmentymää.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Console.WriteLine(s1 == s2); // false</code></p>
<p>Vertailu palauttaa false, koska siinä verrataan olioviitteitä, ei niitä
olioiden arvoja, joihin olioviitteet viittaavat.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Console.WriteLine(s1.Equals(s2)); // true</code></p>
<p>Arvoja, joihin muuttujat viittaavat, voidaan vertailla Equals-metodilla
kuten yllä.</p>
<p>C#:n primitiivityypit sen sijaan sijoittuvat suoraan arvoina
pinomuistiin (tai myöhemmin olioiden attribuuttien tapauksessa oliolle
varattuun muistialueeseen). Siksi vertailu</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
(i1 == i2)</code></p>
<p>on totta.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
int[] it1 = new int[1]; it1[0] = 3;
int[] it2 = new int[1]; it2[0] = 3;
Console.WriteLine(it1 == it2); // false</code></p>
<p>Vastaavasti kuten StringBuilder-olioilla yllä oleva tulostus palauttaa
false. Huomaa, että vaikka taulukko sisältää int-tyyppisiä
kokonaislukuja (jotka ovat primitiivityyppisiä), niin
<em>kokonaislukutaulukko</em> on olio. Jälleen verrataan taulukkomuuttujien
viitteitä, eikä arvoja joihin muuttujat viittaavat.</p>
<p>Ohjelman kaikki muuttujat ovat lokaaleja muuttujia, eli ne on esitelty
lokaalisti Main-metodin sisällä eivätkä "näy" näin ollen Main-metodin
ulkopuolelle. Tällaisille muuttujille varataan tilaa yleensä
kutsupinosta. Kutsupino on dynaaminen tietorakenne, johon tallennetaan
tietoa aktiivisista aliohjelmista. Siitä käytetään usein myös pelkästään
nimeä pino. Pinosta puhutaan lisää kurssilla ITKA203 Käyttöjärjestelmät.
Tässä vaiheessa pino voisi hieman yksinkertaistettuna olla lokaalien
muuttujien kohdalta suurin piirtein seuraavan näköinen:</p>
<p><img alt="\
 Kuva 16: Olioviitteet" src="../src/luentomonistecsUusin_htm_m7a47ee4d.gif" /></p>
<p>\
 \</p>
<p>Jos sijoitetaan "olio" toiseen "olioon", niin tosiasiassa sijoitetaan
viitemuuttujien arvoja, eli sijoituksen <code>s2 = s1</code>{.western} jälkeen
molemmat merkkijono-olioviitteet "osoittavat" samaan olioon. Nyt tilanne
muuttuisi seuraavasti:</p>
<p><img alt="\
 Kuva 17: Kaksi viitettä samaan
olioon" src="../src/luentomonistecsUusin_htm_2e6e1c93.gif" /></p>
<p>\
 \
 \</p>
<p>Sijoituksen jälkeen kuvassa muistipaikkaan 8040 ei osoita (viittaa)
enää kukaan ja tuo muistipaikka muuttuu "roskaksi". Kun roskienkeruu
(garbage-collection, gc) seuraavan kerran käynnistyy, "vapautetaan"
tällaiset käyttämättömät muistialueet. Tätä automaattista roskienkeruuta
on pidetty yhtenä syynä esimerkiksi Javan menestykseen. Samalla täytyy
kuitenkin varoittaa, että muisti on vain yksi resurssi ja automatiikka
on olemassa vain muistin hoitamiseksi. Muut resurssit kuten esimerkiksi
tiedostot ja tietokannat pitää edelleen hoitaa samalla huolellisuudella
kuin muissakin kielissä. [LAP]</p>
<p>Edellä muistipaikan 8040 olio muuttui roskaksi sijoituksessa s2 = s1.
Olio voidaan muuttaa roskaksi myös sijoittamalla sen viitemuuttujaan
null-viite. Tämän takia koodissa pitää usein testata onko olioviite null
ennen kuin oliota käytetään, jos ei olla varmoja onko viitteen päässä
oliota.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
s2 = null;
...
if (s2 != null) Console.WriteLine("s2:n pituus on " + s2.Length);</code></p>
<p>Ilman testiä esimerkissä tulisi NullPointerException-poikkeus.</p>
<p>\
 \</p>
<h1>15. Taulukot</h1>
<p>Muuttujaan pystytään tallentamaan yksi arvo kerrallaan. Usein
ohjelmoinnissa kuitenkin tulee tilanteita, joissa meidän tulisi
tallettaa useita samantyyppisiä yhteenkuuluvia arvoja. Jos haluaisimme
tallettaa esimerkiksi kaikkien kuukausien päivien lukumäärän, voisimme
tietenkin tehdä tämän kuten alla:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
int tammikuu  = 31;
int helmikuu  = 28;
int maaliskuu = 31;
int huhtikuu  = 30;
int toukokuu  = 31;
int kesakuu   = 30;
int heinakuu  = 31;
int elokuu    = 31;
int syyskuu   = 30;
int lokakuu   = 31;
int marraskuu = 30;
int joulukuu  = 31;</code></p>
<p>Kuukausien tapauksessa tämäkin tapa toimisi vielä jotenkin, mutta entäs
jos meidän täytyisi tallentaa vaikka Ohjelmointi 1 -kurssin
opiskelijoiden nimet tai vuoden jokaisen päivän keskilämpötila?</p>
<p>Kun käsitellään useita samaan asiaan liittyviä jossain mielessä
samankaltaisia tai yhteen liittyviä arvoja, on usein syytä ottaa
käyttöön <em>taulukko</em> (array). Taulukko on tietorakenne, johon voi
tallentaa useita samantyyppisiä muuttujia. Yksittäistä taulukon
muuttujaa sanotaan <em>alkioksi</em> (element). Jokaisella alkiolla on
taulukossa paikka, jota sanotaan <em>indeksiksi</em> (index). Taulukon
indeksointi alkaa C#:ssa aina nollasta, eli esimerkiksi 12-alkioisen
taulukon ensimmäisen alkion indeksi olisi 0 ja viimeisen 11.</p>
<p>Taulukon koko täytyy määrittää etukäteen, eikä sitä voi myöhemmin
muuttaa<a href="#sdfootnote1sym">^1^</a>.</p>
<h2>15.1 Taulukon luominen</h2>
<p>C#:ssa taulukon voi luoda sekä alkeistietotyypeille, että
oliotietotyypeille, mutta yhteen taulukkoon voi tallentaa aina vain yhtä
tietotyyppiä. Taulukon määritteleminen ja luominen tapahtuu yleisessä
muodossa seuraavasti:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Tietotyyppi[] taulukonNimi;
taulukonNimi = new Tietotyyppi[taulukonKoko];</code></p>
<p>Ensiksi määritellään taulukon tietotyyppi, jonka jälkeen luodaan
varsinainen taulukko. Tämän voisi tehdä myös samalla rivillä:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Tietotyyppi[] taulukonNimi = new Tietotyyppi[taulukonKoko]; //kaikki alkiot null-viitteitä</code></p>
<p>Kuukausien päivien lukumäärille voisimme määritellä nyt taulukon
seuraavasti:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
int[] kuukausienPaivienLkm = new int[12]; // kaikki alkiot 0</code></p>
<p>Taulukkoon voi myös sijoittaa arvot määrittelyn yhteydessä. Tällöin
sanotaan, että taulukko <em>alustetaan</em> (initialize). Tällöin varsinaista
luontilausetta ei tarvita, sillä taulukon koko määräytyy sijoitettujen
arvojen lukumäärän perusteella. Sijoitettavat arvot kirjoitetaan
aaltosulkeiden sisään.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Tietotyyppi[] = {arvo1, arvo2,...arvoX};</code></p>
<p>Esimerkiksi kuukausien päivien lukumäärille voisimme määritellä ja
alustaa taulukon seuraavasti:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
int[] kuukausienPaivienLkm = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};</code></p>
<p>Taulukko voitaisiin nyt kuvata nyt seuraavasti:</p>
<p><img alt="\
 Kuva 18:
kuukausienPaivienLkm-taulukko" src="../src/luentomonistecsUusin_htm_m6dff106.gif" /></p>
<p>\
 Huomaa, että jokaisella taulukon alkiolla on yksikäsitteinen indeksi.
Indeksi tarvitaan, jotta taulukon alkiot voitaisiin myöhemmin "löytää"
taulukosta. Jos taulukkoa ei alusteta määrittelyn yhteydessä, alustetaan
alkiot automaattisesti oletusarvoihin taulukon luomisen yhteydessä.
Tällöin numeeriset arvot alustetaan nollaksi, bool-tyyppi saa arvon
false ja oliotyypit (esim. String) null-viitteen. [MÄN][KOS]</p>
<h2>15.2 Taulukon alkioon viittaaminen</h2>
<p>Taulukon alkioihin pääsee käsiksi taulukon nimellä ja indeksillä.
Ensiksi kirjoitetaan taulukon nimi, jonka jälkeen hakasulkeiden sisään
halutun alkion indeksi. Yleisessä muodossa taulukon alkioihin viitataan
seuraavasti.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
taulukonNimi[indeksi];</code></p>
<p>Taulukkoon viittaamista voidaan käyttää nyt kuten mitä tahansa sen
tyyppistä arvoa. Esimerkiksi voisimme tulostaa tammikuun pituuden
kuukausienPaivienLkm-taulukosta.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Console.WriteLine(kuukausienPaivienLkm[0]); //tulostuu 31</code></p>
<p>Tai tallentaa tammikuun pituuden edelleen muuttujaan.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
int tammikuu = kuukausienPaivienLkm[0];</code></p>
<p>Taulukkoon viittaava indeksi voi olla myös int-tyyppinen lauseke,
jolloin kuukausienPaivienLkm-taulukkoon viittaaminen onnistuu yhtä hyvin
seuraavasti:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
int indeksi = 0;
Console.WriteLine(kuukausienPaivienLkm[indeksi]); // ensimmäinen alkio
Console.WriteLine(kuukausienPaivienLkm[indeksi + 3]); // neljäs alkio</code></p>
<p>Taulukon arvoja voi tietenkin myös muuttaa. Jos esimerkiksi olisi
kyseessä karkausvuosi, voisimme muuttaa helmikuun pituudeksi 29.
Helmikuuhan on taulukon indeksissä 1, sillä indeksointi alkoi nollasta.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
kuukausienPaivienLkm[1] = 29;</code></p>
<p>Jos viittaamme taulukon alkioon, jota ei ole olemassa, saamme
IndexOutOfRangeException-poikkeuksen. Tällöin kääntäjä tulostaa
seuraavan kaltaisen virheilmoituksen ja ohjelman suoritus päättyy.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Unhandled Exception: System.IndexOutOfRangeException: Index was outside the bounds of the array.</code></p>
<p>Myöhemmin opitaan kuinka poikkeuksista voidaan toipua ja ohjelman
suoritusta jatkaa.</p>
<h2>15.3 Esimerkki: lumiukon pallot taulukkoon</h2>
<p>Luvussa 4.3 teimme lumiukon kolmesta pallosta. Tehdään sama siten, että
laitetaan yksittäiset PhysicsObject-oliot taulukkoon.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
using Jypeli;
using System;
/// @author  Antti-Jussi Lakanen
/// @version 22.8.2012
///
/// <summary>
/// Lumiukko, jonka pallot ovat taulukossa.
/// </summary>
public class Lumiukko : PhysicsGame
{
    /// <summary>
    /// Pääohjelmassa laitetaan "peli" käyntiin Jypelille tyypilliseen tapaan  <br />
    /// </summary>
    public static void Main()
    {
        using (Lumiukko peli = new Lumiukko())
        {
            peli.Run();
        }
    }</p>
<pre><code>/// &lt;summary&gt;
/// Piirretään oliot ja zoomataan kamera niin että kenttä näkyy kokonaan.
/// &lt;/summary&gt;
public override void Begin()
{
    Camera.ZoomToLevel();
    Level.Background.Color = Color.Black;

    // Lisätään pallot taulukkoon, ja sitten lisätään kentälle
    PhysicsObject[] pallot = new PhysicsObject[3];
    pallot[0] = new PhysicsObject(2 * 100.0, 2 * 100.0, Shape.Circle);
    pallot[0].Y = Level.Bottom + 200.0;
    pallot[1] = new PhysicsObject(2 * 50.0, 2 * 50.0, Shape.Circle);
    pallot[1].Y = pallot[0].Y + 100 + 50;
    pallot[2] = new PhysicsObject(2 * 30.0, 2 * 30.0, Shape.Circle);
    pallot[2].Y = pallot[1].Y + 50 + 30;

    Add(pallot[0]); Add(pallot[1]); Add(pallot[2]);
}
</code></pre>
<p>}
```</p>
<p>Näkyvä lopputulos on sama lumiukko kuin aikaisemminkin. Nyt pallot ovat
kuitenkin taulukkorakenteessa.</p>
<p><img alt="\
 Kuva 19: Lumiukon pallot ovat pallot-taulukon
alkioita." src="../src/luentomonistecsUusin_htm_m5634df04.png" /></p>
<p>\
 Nyt taulukon avulla pääsemme käsiksi yksittäisiin pallo-olioihin.
Esimerkiksi keskimmäisen pallon värin muuttaminen onnistuisi seuraavasti</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
pallot[1].Color = Color.Yellow;</code></p>
<h2>15.4 Esimerkki: arvosana kirjalliseksi</h2>
<p>Ehtolauseiden yhteydessä teimme switch-rakennetta käyttämällä
aliohjelman, joka palautti parametrinaan saamaansa numeroarvosanaa
vastaavan kirjallisen arvosanan. Tehdään nyt sama aliohjelma taulukkoa
käyttämällä. Kirjalliset arvosanat voidaan nyt tallentaa
String-tyyppiseen taulukkoon.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
/// &lt;summary&gt;
/// Palauttaa parametrina saamansa numeroarvosanan kirjallisena.
/// &lt;/summary&gt;
/// &lt;param name="numero"&gt;tenttiarvosana numerona&lt;/param&gt;
/// &lt;returns&gt;tenttiarvosana kirjallisena&lt;/returns&gt;
public static String KirjallinenArvosana(int numero)
{
    String[] arvosanat = {"Hylätty", "Välttävä", "Tyydyttävä", 
                            "Hyvä", "Kiitettävä", "Erinomainen"};
    if (numero &lt; 0 || arvosanat.Length &lt;= numero) return "Virheellinen syöte!";
    return arvosanat[numero];
}</code></p>
<p>Ensimmäiseksi aliohjelmassa määritellään ja alustetaan taulukko, jossa
on kaikki kirjalliset arvosanat. Taulukko määritellään niin, että
taulukon indeksissä 0 on arvosanaa 0 vastaava kirjallinen arvosana,
taulukon indeksissä 1 on arvosanaa 1 vastaava kirjallinen arvosana ja
niin edelleen. Tällä tavalla tietty taulukon indeksi vastaa suoraan
vastaavaa kirjallista arvosanaa. Kirjallisten arvosanojen hakeminen on
näin todella nopeaa.</p>
<p>Jos vertaamme tätä tapaa switch-rakenteella toteutettuun tapaan
huomaamme, että koodin määrä väheni huomattavasti. Tämä tapa on lisäksi
nopeampi, sillä jos esimerkiksi hakisimme arvosanalle viisi kirjallista
arvosanaa, switch-rakenteessa tehtäisiin viisi vertailuoperaatiota.
Taulukkoa käyttämällä vertailuoperaatioita ei tehdä yhtään, vaan
ainoastaan yksi hakuoperaatio taulukosta.</p>
<h2>15.5 Moniulotteiset taulukot</h2>
<p>Taulukot voivat olla myös moniulotteisia. Kaksiulotteinen taulukko (eli
matriisi) on esimerkki moniulotteisesta taulukosta, joka koostuu
vähintään kahdesta samanpituisesta taulukosta. Kaksiulotteisella
taulukolla voidaan esittää esimerkiksi tason tai kappaleen pinnan
koordinaatteja.</p>
<p>Kaksiulotteinen taulukko määritellään seuraavasti:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
tyyppi[,] taulukonNimi;</code></p>
<p>Huomaa, että määrittelyssä [,] tarkoittaa, että esitelty taulukko on
kaksiulotteinen. Vastaavasti [,,] tarkoittaisi, että taulukko on
kolmiulotteinen ja niin edelleen.</p>
<p>Moniulotteisen taulukon alkioiden määrä tulee aina ilmoittaa ennen
taulukon käyttöä. Tämä tapahtuu new-operaattorilla seuraavasti:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
taulukonNimi = new tyyppi[rivienLukumaara, sarakkeidenLukumaara]</code></p>
<p>Esimerkiksi kaksiulotteisen String-tyyppisen taulukon kurssin
opiskelijoiden nimille voisi alustaa seuraavasti.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
String[,] kurssinOpiskelijat = new String[256, 2];</code></p>
<p>Taulukkoon voisi nyt asettaa kurssilaisten nimiä seuraavasti:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
//ensimmäinen kurssilainen
kurssinOpiskelijat[0, 0] = "Virtanen";
kurssinOpiskelijat[0, 1] = "Ville";
//toinen kurssilainen
kurssinOpiskelijat[1, 0] = "Korhonen";
kurssinOpiskelijat[1, 1] = "Kalle";</code></p>
<p>Taulukko näyttäisi nyt seuraavalta:</p>
<p><img alt="\
 Kuva 20:
kurssinOpiskelijat-taulukko" src="../src/luentomonistecsUusin_htm_m502edb07.gif" /></p>
<p>\
 Moniulotteiseen taulukkoon viittaaminen onnistuu vastaavasti kuin
yksiulotteiseen. Ulottuvuuksien kasvaessa joudutaan vain antamaan
enemmän indeksejä.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
// tulostaa Ville Virtanen
Console.WriteLine(kurssinOpiskelijat[0,1] + " " + kurssinOpiskelijat[0,0]);</code></p>
<p>Huomaa, että yllä olevassa esimerkissä ”+”- merkki ei toimi
aritmeettisena operaattorina, vaan sillä yhdistetään tulostettavia
merkkijonoja. C#:ssa ”+”-merkkiä käytetään siis myös merkkijonojen
yhdistelyyn.</p>
<p>Kun etunimi ja sukunimi on talletettu taulukkoon omille paikoilleen,
mahdollistaa se tietojen joustavamman käsittelyn. Nyt opiskelijoiden
nimet voidaan halutessa tulostaa muodossa: "etunimi sukunimi" tai
muodossa, "sukunimi, etunimi" kuten alla:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
// tulostaa Virtanen, Ville
Console.WriteLine(kurssinOpiskelijat[0,0] + ", " + kurssinOpiskelijat[0,1]);</code></p>
<p>Todellisuudessa henkilötietorekisteriä ei kuitenkaan tehdä tällä
tavalla. Järkevämpää olisi tehdä Henkilo-luokka, jossa olisi kentät
etunimelle ja sukunimelle ja mahdollisille muille tiedoille. Tästä
luokasta luotaisiin sitten jokaiselle opiskelijalle oma olio. Tällä
kurssilla ei kuitenkaan tehdä vielä omia olioluokkia.</p>
<p>Moniulotteinen taulukko voidaan määriteltäessä alustaa kuten
yksiulotteinenkin. Määritellään ja alustetaan seuraavaksi taulukko
elokuville:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
String[,] elokuvat =  { {"Pulp Fiction", "Toiminta", "Tarantino"     }, 
                        {"2001: Avaruusseikkailu", "Scifi", "Kubrick"},
                        {"Casablanca", "Draama", "Curtiz"            } };</code></p>
<p>Yllä oleva määrittely luo 3 x 3 kokoisen taulukon:</p>
<p><img alt="\
 Kuva 21: Taulukon elokuvat
sisältö." src="../src/luentomonistecsUusin_htm_45faf2c8.gif" /></p>
<p>\
 Kun taulukko on luotu, sen alkioihin viitataan seuraavalla tavalla.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
taulukonNimi[rivi-indeksi, sarakeindeksi]</code></p>
<p>Alla oleva esimerkki hahmottaa taulukon alkioihin viittaamista.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Console.WriteLine(elokuvat[0, 0]);  //tulostaa "Pulp Fiction"
Console.WriteLine("Tyyppi: " + elokuvat[0, 1]); //tulostaa "Tyyppi:  Toiminta"
Console.WriteLine("Ohjaaja: " + elokuvat[0, 2]); //tulostaa "Ohjaaja: Tarantino"</code></p>
<p>\</p>
<p>Tällä tavalla jokaiselle riville tulee yhtä monta saraketta eli alkiota.
Jos eri riveille halutaan eri määrä alkioita, voidaan käyttää niin
sanottuja jagged array -taulukkoja. Lue lisää jagged arrayn
MSDN-dokumentaatiosta osoitteesta
<a href="http://msdn.microsoft.com/en-us/library/2s05feca(v=vs.80).aspx">http://msdn.microsoft.com/en-us/library/2s05feca(v=vs.80).aspx</a>.</p>
<h3>15.5.1 Harjoitus</h3>
<p>Miten tulostat taulukosta Casablanca? Entä Kubrick?</p>
<h2>15.6 Taulukon kopioiminen</h2>
<p>Myös taulukot ovat olioita. Siispä taulukkomuuttujat ovat
viitemuuttujia. Tämän takia taulukon kopioiminen <em>ei</em> onnistu alla
olevalla tavalla kuten alkeistietotyypeillä:</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
int[] taulukko1 = {1, 2, 3, 4, 5};
int[] taulukko2 = taulukko1;</p>
<p>taulukko2[0] = 10;
Console.WriteLine(taulukko1[0]); //tulostaa 10
```</p>
<p>Yllä olevassa esimerkissä sekä taulukko1, että taulukko2 ovat
olioviitteitä ja viittaavat nyt samaan taulukkoon.</p>
<p>Taulukon kopioiminen onnistuu muun muassa Clone-metodilla.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
int[] taulukko = {1, 2, 3, 4, 5};
// Clone-metodi luo identtisen kopion taulukosta
int[] kopio_taulukosta = (int[])taulukko.Clone();</code></p>
<p>Huomaa, että sijoituksessa vaaditaan ns. tyyppimuunnos: ennen
taulukko.Clone-lausetta kirjoitetaan (int[]), sulkujen kanssa, joka
muuttaa Clone-metodin palauttaman ”yleisen” Object-olion
kokonaislukutaulukoksi.</p>
<p>Nyt meillä olisi identtinen kopio taulukosta, jonka muuttaminen ei siis
vaikuta alkuperäiseen taulukkoon.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
kopio_taulukosta[0] = 3;
Console.WriteLine(taulukko[0] + ", " + kopio_taulukosta[0]); // 1, 3</code></p>
<h2>15.7 Esimerkki: Moniulotteiset taulukot käytännössä</h2>
<p>Kaksiulotteisia taulukoita kutsutaan yleisesti matriiseiksi, ja ne ovat
käytössä erityisesti matemaattisissa sovelluksissa kuvaten
lineaarifunktioita. Muitakin käyttökohteita matriiseilla kuitenkin on.
Esimerkiksi laivanupotuspelin pelikenttä voidaan ajatella 2-ulotteiseksi
taulukoksi.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
using System;</p>
<p>/// @author  Antti-Jussi Lakanen
/// @version 22.8.2012
///
/// <summary>
/// Moniulotteiset taulukot käytännössä.
/// </summary>
public class Laivanupotus
{
  /// <summary>
  /// Taulukon alustus ja tulostus.
  /// </summary>
  public static void Main()
  {
    int[,] ruudut = { { 1, 0, 2 }, { 0, 0, 3 } };
    Console.WriteLine(ruudut[0, 0] + " " + ruudut[0, 1] + " " + ruudut[0, 2]);
    Console.WriteLine(ruudut[1, 0] + " " + ruudut[1, 1] + " " + ruudut[1, 2]);
    // Tulostaa:
    // 1 0 2
    // 0 0 3
  } 
}
```</p>
<p>Vaikka ruudukko oli vielä aika pieni (2 riviä x 3 saraketta), on
alkioiden tulostaminen melko työlästä. Esimerkiksi jo 20 x 20 kokoisen
taulukon tulostaminen pelkkiä tulostuslauseita peräkkäin laittamalla
olisi jo kohtuuttoman iso työ.</p>
<p>Edelleen, yhtenä toiveena voisi olla, että löytäisimme ”ruudukosta”
rivin, jolla tyhjiä paikkoja on eniten. Tämä tieto voisi auttaa meitä
asemoimaan uuden laivan oikein. Mielivaltaiselle ruudukolle tämä ei
vielä meidän tiedoillamme onnistu.</p>
<p>Näihin tehtäviin tarvitsemme toistorakenteita, jotka esitellään
seuraavassa luvussa.</p>
<h2>15.8 Taulukoiden täydennykset wikissä</h2>
<p>Lisätietoja 1- ja 2-ulotteisista taulukoista löydät kurssin
wiki-sivuston monisteen täydennyksistä. Lue myös nuo materiaalit läpi
(kuuluvat tenttialueeseen).</p>
<p><a href="https://trac.cc.jyu.fi/projects/ohj1/wiki/taulukot">https://trac.cc.jyu.fi/projects/ohj1/wiki/taulukot</a>
(1-ulotteiset taulukot)</p>
<p><a href="https://trac.cc.jyu.fi/projects/ohj1/wiki/taulukot2D">https://trac.cc.jyu.fi/projects/ohj1/wiki/taulukot2D</a>
(2-ulotteiset taulukot)</p>
<p>\
 \</p>
<h1>16. Toistorakenteet (silmukat)</h1>
<p>Ohjelmoinnissa tulee usein tilanteita, joissa samaa tai lähes samaa
asiaa täytyy toistaa ohjelmassa useampia kertoja. Varsinkin taulukoiden
käsittelyssä tällainen asia tulee usein eteen. Jos haluaisimme
esimerkiksi tulostaa kaikki edellisessä luvussa tekemämme
kuukausienPaivienLkm-taulukon luvut, onnistuisi se tietenkin
seuraavasti:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Console.WriteLine(kuukausienPaivienLkm[0]);
Console.WriteLine(kuukausienPaivienLkm[1]);
Console.WriteLine(kuukausienPaivienLkm[2]);
Console.WriteLine(kuukausienPaivienLkm[3]);
Console.WriteLine(kuukausienPaivienLkm[4]);
Console.WriteLine(kuukausienPaivienLkm[5]);
Console.WriteLine(kuukausienPaivienLkm[6]);
Console.WriteLine(kuukausienPaivienLkm[7]);
Console.WriteLine(kuukausienPaivienLkm[8]);
Console.WriteLine(kuukausienPaivienLkm[9]);
Console.WriteLine(kuukausienPaivienLkm[10]);
Console.WriteLine(kuukausienPaivienLkm[11]);</code></p>
<p>Tuntuu kuitenkin tyhmältä toistaa lähes samanlaista koodia useaan
kertaan. Tällöin on järkevämpää käyttää jotain toistorakennetta.
Toistorakenteet soveltuvat erinomaisesti taulukoiden käsittelyyn, mutta
niistä on myös moniin muihin tarkoituksiin. Toistorakenteista käytetään
usein myös nimitystä <em>silmukat</em> (loop).</p>
<p>Tämä luku on pitkä ja sisältää runsaasti esimerkkejä. Toistorakenteiden
hallinta on kuitenkin hyvin tärkeää ohjelmoinnin opettelun
alkuvaiheilla.</p>
<h2>16.1 ”Syö niin kauan, kuin puuroa on lautasella”</h2>
<p>Ideana toistorakenteissa on, että toistamme tiettyä asiaa niin kauan
kuin joku ehto on voimassa. Esimerkki ihmiselle suunnatusta
toistorakenteesta aamupuuron syöntiin.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Syö aamupuuroa niin kauan, kuin puuroa on lautasella.</code></p>
<p>Yllä olevassa esimerkissä on kaikki toistorakenteeseen vaadittavat
elementit. Toimenpiteet mitä tehdään: "Syö aamupuuroa.", sekä ehto
kuinka toistetaan: "niin kauan kuin puuroa on lautasella". Toinen
esimerkki toistorakenteesta voisi olla seuraava:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Tulosta kuukausienPaivienLkm-taulukon kaikki luvut.</code></p>
<p>Myös yllä oleva lause sisältää toistorakenteen elementit, vaikka ne
onkin hieman vaikeampi tunnistaa. Toimenpiteenä tulostetaan
kuukausienPaivienLkm-taulukon lukuja ja ehdoksi voisi muotoilla: "kunnes
kaikki luvut on tulostettu". Lauseen voisikin muuttaa muotoon:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Tulosta kuukausienPaivienLkm-taulukon lukuja, kunnes kaikki luvut on tulostettu.</code></p>
<p>C#:ssa on neljän tyyppisiä toistorakenteita:</p>
<ul>
<li>
<p>for</p>
</li>
<li>
<p>while</p>
</li>
<li>
<p>do-while</p>
</li>
<li>
<p>foreach</p>
</li>
</ul>
<p>On tilanteita, joissa voimme vapaasti valita näistä minkä tahansa, mutta
useimmiten toistorakenteen valinnan kanssa täytyy olla tarkkana.
Jokaisella näistä on tietyt ominaispiirteensä, eivätkä kaikki
toistorakenteet sovi kaikkiin mahdollisiin tilanteisiin.</p>
<h2>16.2 while-silmukka</h2>
<p>while-silmukka on yleisessä muodossa seuraava:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
while (ehto) lause;</code></p>
<p>Kuten ehtolauseissa, täytyy ehdon taas olla jokin lauseke, joka saa joko
arvon true tai false. Ehdon jälkeen voi yksittäisen lauseen sijaan olla
myös lohko.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
while (ehto) 
{
  lause1;
  lause2;
  lauseX;
}</code></p>
<p>Silmukan lauseita toistetaan niin kauan, kuin ehto on voimassa, eli sen
arvo on true. Ehto tarkastetaan aina ennen kuin siirrytään seuraavalle
kierrokselle. Jos ehto saa siis heti alussa arvon false, ei lauseita
suoriteta kertaakaan.</p>
<h3>16.2.1 Huomautus: ikuinen silmukka</h3>
<p>Huomaa, että jos while-silmukan ehto on aina true, on kyseessä <em>ikuinen
silmukka</em> (infinite loop). Ikuinen silmukka on nimensä mukaisesti
silmukka, joka ei pääty koskaan. Ikuinen silmukka johtuu siitä, että
silmukan ehto ei saa koskaan arvoa false. Useimmiten ikuinen silmukka on
ohjelmointivirhe, mutta joskus (hallitun) ikuisen silmukan tekeminen on
perusteltua. Tällöin silmukasta kuitenkin poistutaan (ennemmin tai
myöhemmin) break-lauseen avulla. Tällöinhän silmukka ei oikeastaan ole
ikuinen, vaikka tällaisesta silmukasta sitä nimitystä usein
käytetäänkin. break-lauseesta puhutaan tässä luvussa myöhemmin kohdassa
16.8.1.</p>
<p>Ikuinen silmukka on mahdollista tehdä myös muilla toistorakenteilla.</p>
<h3>16.2.2 While-silmukka vuokaaviona</h3>
<p><img alt="\
 Kuva 22: while-silmukka
vuokaaviona" src="../src/luentomonistecsUusin_htm_m38354e6c.gif" /></p>
<p>\
 \</p>
<h3>16.2.3 Esimerkki: Taulukon tulostaminen</h3>
<p>Tehdään aliohjelma joka tulostaa int-tyyppisen yksiulotteisen taulukon
sisällön.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
public class Silmukat 
{  <br />
/// <summary>
/// Tulostaa int-tyyppisen taulukon sisällön.
/// </summary>
/// <param name="taulukko">Tulostettava taulukko</param>
  public static void TulostaTaulukko(int[] taulukko) 
  {
    int i = 0;
    while (i &lt; taulukko.Length){
      Console.Write(taulukko[i] + " ");
      i++;
    }
  }</p>
<p>/// <summary>
  /// Pääohjelma.
  /// </summary>
  public static void Main() 
  {
    int[] kuukausienPaivienLkm = 
      {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
    TulostaTaulukko(kuukausienPaivienLkm);
    Console.ReadKey();
  }
}
```</p>
<p>Tarkastellaan TulostaTaulukko-aliohjelman sisältöä hieman tarkemmin.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
int i = 0;</code></p>
<p>Tässä luodaan uusi muuttuja, jolla kontrolloidaan, mitä taulukon
alkioita ollaan tulostamassa. Lisäksi sen avulla selvitetään, milloin
taulukon kaikki alkiot on tulostettu ruudulle. Muuttuja alustetaan
arvoon 0, sillä taulukon ensimmäinen alkio on aina indeksissä 0.
Muuttujalle annetaan nimeksi i. Useimmiten pelkät kirjaimet ovat huonoja
muuttujan nimiä, koska ne kuvaavat muuttujaa huonosti. Silmukoissa
kuitenkin nimi i on vakiinnuttanut asemansa kontrolloimassa silmukoiden
kierroksia, joten sitä voidaan hyvällä omallatunnolla käyttää.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
while (i &lt; taulukko.Length)</code></p>
<p>Aliohjelman toisella rivillä aloitetaan while-silmukka. Ehtona on, että
(silmukkaa suoritetaan niin kauan, kuin) muuttujan i arvo on <em>pienempi</em>
kuin taulukon pituus. Taulukon pituus saadaan aina selville
kirjoittamalla nimen perään .Length. Huomionarvoinen seikka on, että
Length-sanan perään ei tule sulkuja, sillä se ei ole metodi vaan
attribuutti.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Console.Write(taulukko[i] + " ");</code></p>
<p>Ensimmäisessä silmukan lauseessa tulostetaan taulukon alkio indeksissä
i. Perään tulostetaan välilyönti erottamaan eri alkiot toisistaan.
Console.WriteLine-metodin sijaan käytämme nyt toista Console-luokan
metodia. Console.Write-metodi ei tulosta perään rivinvaihtoa, joten
sillä voidaan tulostaa taulukon alkiot peräkkäin.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
i++;</code></p>
<p>Silmukan viimeinen lause kasvattaa muuttujan iarvoa yhdellä. Ilman tätä
lausetta saisimme aikaan ikuisen silmukan, sillä indeksin arvo olisi
koko ajan 0ja silmukan ehto olisi aina tosi. Lisäksi metodi tulostaisi
koko ajan taulukon ensimmäistä alkioita. Indeksimuuttujan hallintaan
liittyvät virheet ovat tyypillisiä aloittelevan (ja pidemmällekin
edistyneen) ohjelmoijan virheitä. Ongelmalliseksi virheen tekee se,
ettei se ole syntaksivirhe, jolloin esimerkiksi Visual Studio ei anna
tilanteesta virheilmoitusta.</p>
<p>while-silmukkaa tulisi käyttää silloin, kun meillä ei ole tarkkaa tietoa
silmukan suorituskierrosten lukumäärästä. Koska taulukon koko on
tarkalleen tiedossa taulukon luomisen jälkeen, olisi läpikäyminen
käytännössä järkevämpää ja tehdä for-silmukalla, missä vaara ikuisen
silmukan syntymiseen on pienempi. Myöhemmin löytyy järkevämpää käyttöä
while-silmukalle.</p>
<h3>16.2.4 Esimerkki: Monta palloa</h3>
<p>Tehdään aliohjelma, joka luo halutun kokoisen pallon haluttuun paikkaan,
ja vielä haluamalla värillä. Main-metodi on jätetty listauksesta pois.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
using System;
using Jypeli;</p>
<p>/// <summary>
/// Paljon palloja tippuu alaspäin.
/// </summary>
public class MontaPalloa : PhysicsGame
{
    /// <summary>
    /// Ruudulla näkyvä sisältö.
    /// </summary>
    public override void Begin()
    {
        Level.CreateBorders();
        Gravity = new Vector(0, -500); 
        Camera.ZoomToLevel();</p>
<pre><code>    int i = 0;
    while (i &lt; 100)
    {
        int sade = RandomGen.NextInt(5, 20);
        double x = RandomGen.NextDouble(Level.Left + sade, Level.Right - sade);
        double y = RandomGen.NextDouble(Level.Bottom + sade, Level.Top - sade);
        Color vari = RandomGen.NextColor();
        PhysicsObject pallo = LuoPallo(x, y, vari, sade);
        Add(pallo);
        i++;
    }
}

/// &lt;summary&gt;
/// Luo yksittäisen pallon ja palauttaa sen.
/// &lt;/summary&gt;
/// &lt;param name="x"&gt;Pallon kp x-koordinaatti&lt;/param&gt;
/// &lt;param name="y"&gt;Pallon kp y-koordinaatti&lt;/param&gt;
/// &lt;param name="vari"&gt;Pallon väri&lt;/param&gt;
/// &lt;param name="sade"&gt;Pallon säde&lt;/param&gt;
public static PhysicsObject LuoPallo(double x, double y, Color vari, double sade)
{
    PhysicsObject pallo = new PhysicsObject(2 * sade, 2 * sade, Shape.Circle);
    pallo.Color = vari;
    pallo.X = x;
    pallo.Y = y;
    return pallo;
}
</code></pre>
<p>}
```</p>
<p>Ajettaessa koodin tulisi piirtää ruudulle sata palloa, jotka putoavat
alaspäin kohti kentän reunaa. Katso seuraava kuva.</p>
<p><img alt="\
 Kuva 23: Pallot tippuu." src="../src/luentomonistecsUusin_htm_373fc74f.png" /></p>
<p>\
 Tutkitaan tarkemmin LuoPallo-aliohjelmaa Aliohjelma palauttaa
PhysicsObject-olion, siis paluuarvon tyyppinä on luonnnollisesti
PhysicsObject. Parametreja ovat</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
double x, double y, Color vari, double sade</code></p>
<p>siis pallon keskipisteen x- ja y-koordinaatit, väri ja säde.</p>
<p>Huomaa, että LuoPallo on tässä funktioaliohjelma, joka ei tee ohjelmassa
mitään ”näkyvää”. Se vain luo pallon, kuten nimikin kertoo, mutta ei
lisää sitä ruudulle. Tästä syystä ei tarvita myöskään Game-parametria,
joka Lumiukko-esimerkissä aikanaan tarvittiin. Sen sijaan lisääminen
tehdään Begin-aliohjelmassa. Yleisesti ottaen aliohjelmissa ei pidä
tehdä enempää kuin mitä dokumentaatiossa kerrotaan - jopa aliohjelman
nimestä pitäisi kaikkein tärkein selvitä.</p>
<p>Jos haluttaisiin, että tämä kyseinen aliohjelma myös lisää pallon
ruutuun, tulisi se nimetä jotenkin muuten, esimerkiksi LisaaPallo olisi
loogisempi vaihtoehto. Silloin palloa ei palautettaisi kysyjälle ja
paluuarvon tyypiksi tulisi void.</p>
<p>Siirrytään sitten takaisin Begin-aliohjelmaan.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
int i = 0;
while (i &lt; 100)</code></p>
<p>Tässä alustetaan int-tyyppinen indeksi i nollaksi ja määritetään
while-sanan jälkeen sulkujen sisään ehto jonka perusteella silmukassa
etenemistä jatketaan. Aaltosulut on jätetty listauksesta
tarkoituksellisesti pois.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
int sade = RandomGen.NextInt(5, 20);
double x = RandomGen.NextDouble(Level.Left + sade, Level.Right - sade);
double y = RandomGen.NextDouble(Level.Bottom + sade, Level.Top - sade);
Color vari = RandomGen.NextColor();
PhysicsObject pallo = LuoPallo(x, y, vari, sade);
Add(pallo);
i++;</code></p>
<p>Silmukassa arvotaan ensin kunkin pallon säde RandomGen-luokan
satunnaislukugeneraattorilla. Ensimmäisenä parametrina
NextInt-aliohjelmalle annetaan pienin mahdollinen arvottava luku,
toisena parametrina luku, jota pienempi arvottavan luvun tulee olla.
Toisin sanoen, luvut tulevat olemaan välillä 5-19. Samoin arvotaan
double-tyyppiset koordinaatit sekä Color-tyyppinen väri.</p>
<p>Tämän jälkeen luodaan normaalisti PhysicsObject-fysiikkaolio, ja
annetaan LuoPallo-aliohjelmalle parametrina juuri tekemämme muuttujat,
joka sitten palauttaa haluamamme pallon. Pallo lisätään kentälle
Add-metodin avulla.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
i++;</code></p>
<p>Silmukan jokaisen ”kierroksen” jälkeen indeksin arvoa on lisättävä
yhdellä, ettemme joutuisi ikuiseen silmukkaan.</p>
<h2>16.3 do-while-silmukka</h2>
<p>do-while-silmukka eroaa while-silmukasta siinä, että do-while-silmukassa
ilmoitetaan ensiksi lauseet (mitä tehdään) ja vasta sen jälkeen ehto
(kauanko tehdään). Tämän takia do-while -silmukka suoritetaankin joka
kerta <em>vähintään</em> yhden kerran. Yleisessä muodossa do-while -silmukka on
seuraavanlainen:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
do 
{
  lause1;
  lause2;
  (...)
  lauseN;
} while (ehto);</code></p>
<p>Vuokaaviona do-while -silmukan voisi esittää seuraavasti:</p>
<p><img alt="\
 Kuva 24: do-while-silmukka
vuokaaviona." src="../src/luentomonistecsUusin_htm_m6406473.gif" /></p>
<p>\
 \</p>
<h3>16.3.1 Esimerkki: nimen kysyminen käyttäjältä</h3>
<p>Seuraavassa esimerkissä käyttäjää pyydetään syöttämään merkkijono Jos
käyttäjä antaa tyhjän jonon, kysytään nimeä uudestaan. Tätä toistetaan
niin kauan, kunnes käyttäjä antaa jotain muuta kuin tyhjän jonon.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
using System;</p>
<p>/// <summary>
/// Harjoitellaan do-while-silmukan käyttöä.
/// </summary>
public class NimenTulostus
{
    /// <summary>
    /// Pyydetään käyttäjältä syöte ja tulostellaan.
    /// </summary>
    public static void Main()
    {
        String nimi;
        do
        {
            Console.Write("Anna nimi &gt; ");
            nimi = Console.ReadLine();
        } while (nimi.Length == 0);
        Console.WriteLine("Hei, " + nimi + "!");
        Console.ReadKey();
    }
}
```</p>
<p>Tämä kuvaa hyvin do-while-silmukan olemusta: nimi halutaan kysyä
varmasti ainakin kerran, mutta mahdollisesti useamminkin - emme
kuitenkaan voi olla varmoja kuinka monta kertaa useammin.</p>
<p>Todellisuudessa nimen oikeellisuuden tarkistaminen olisi tietenkin
monimutkaisempaa, mutta idea do-while-silmukan osalta olisi täsmälleen
vastaava.</p>
<h2>16.4 for-silmukka</h2>
<p>Kun silmukan suoritusten lukumäärä on ennalta tiedossa, on järkevintä
käyttää for-silmukkaa. Esimerkiksi taulukoiden käsittelyyn for-silmukka
on yleensä paras vaihtoehto. Syntaksiltaan for-silmukka eroaa selvästi
edellisistä. Perinteinen for-silmukka on yleisessä muodossa
seuraavanlainen:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
for (muuttujien alustukset; ehto; silmukan lopussa tehtävät toimenpiteet)
{ 
   lauseet; // silmukan runko-osa
}</code></p>
<p>Silmukan <em>kontrollilauseke</em> eli kaarisulkujen sisäpuoli sisältää kolme
operaatiota, jotka on erotettu toisistaan puolipisteellä.</p>
<ul>
<li>
<p>Muuttujien alustukset: Useimmiten alustetaan vain yksi muuttuja,
    mutta myös useampien muuttujien alustaminen on mahdollista.</p>
</li>
<li>
<p>Ehto: Kuten muissakin silmukoissa, lauseita toistetaan niin kauan
    kuin ehto on voimassa.</p>
</li>
<li>
<p>Silmukan lopussa tehtävät toimenpiteet: Useimmiten muuttujan tai
    muuttujien arvoa kasvatetaan yhdellä, mutta myös suuremmalla
    määrällä kasvattaminen on mahdollista.</p>
</li>
</ul>
<p>Alla for-silmukan syntaksi graafisessa ”junarataformaatissa” (ks. luku
28.2) - tosin tarkoitusta varten hieman yksinkertaistettuna.</p>
<p><img alt="\
 Kuva 25: for-silmukan syntaksi graafisessa
&quot;junaratamuodossa&quot;." src="../src/luentomonistecsUusin_htm_207ef69a.gif" /></p>
<p>\
 Alla esimerkki yksinkertaisesta for-silmukasta. Siinä tulostetaan 10
kertaa ”Hello World!” ja perääni-muuttujan sen hetkinen arvo.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
for (int i = 0; i &lt; 10; i++) 
{
   Console.WriteLine("Hello World " + i);
}</code></p>
<p>Kontrollilausekkeessa alustetaan aluksi muuttujan i arvoksi 0.
Seuraavaksi ehtona on, että silmukan suoritusta jatketaan niin kauan
kuin muuttujan i arvo on pienempää kuin luku 10. Lopuksi
kontrollilausekkeessa todetaan, että muuttujan i arvoa kasvatetaan joka
kierroksella yhdellä.</p>
<p>Vuokaaviona ylläolevan for-silmukan voisi kuvata alla olevalla tavalla.</p>
<p><img alt="\
 Kuva 26: Vuokaavio
for-silmukalle" src="../src/luentomonistecsUusin_htm_821ea7c.png" /></p>
<p>\
 \
 \</p>
<p>\</p>
<p>Huomaa, että i-muuttujan arvo alkaa nollasta, joka tulostetaan
ensimmäisenä Hello World! -tekstin jälkeen. Silmukan runko-osan
suorittamisen ehtona on, että i-muuttujan arvon on oltava alle 10, joten
kun i saavuttaa arvon 10 (10:n kierroksen päätteeksi), poistutaan
silmukasta.</p>
<p>Silmukan runko-osassa ei suinkaan aina tarvitse tulostaa mitään. Otetaan
esimerkki, missä luodun taulukon alkioihin sijoitetaan aina kahden
edellisen alkion sisältämien lukujen summa. Ensimmäisiksi alkioden
arvoksi asetetaan ”manuaalisesti” luku 1.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
int[] luvut = new int[10];
luvut[0] = 1;
luvut[1] = 1;</p>
<p>for (int i = 2; i &lt; luvut.Length; i++)
{<br />
  luvut[i] = luvut[i - 1] + luvut[i - 2];
}
```</p>
<p>Huomaa että silmukka on aloitettu indeksistä 2, jotta i-2 >= 0 ja näin
jokainen indeksi on laillinen lauseessa</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
luvut[i] = luvut[i - 1] + luvut[i - 2];</code></p>
<p>Silmukan jälkeen taulukon sisältö näyttää seuraavalta.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
      [0] [1] [2] [3] [4] [5] [6] [7] [8] [9]
luvut  1   1   2   3   5   8   13  21  34  55</code></p>
<p>\
 \</p>
<p>Myös for-silmukalla voidaan tehdä "ikuinen" silmukka:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
for(;;)
{
  // ”ikuisesti” suoritettavat asiat
}</code></p>
<h3>16.4.1 Huomautus: while ja for -silmukoiden yhtäläisyydet ja erot</h3>
<p>for- ja while-silmukkarakenteilla voidaan periaatteessa tehdä täsmälleen
samat asiat. for-silmukan yleisen muodon</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
for (muuttujien alustukset; ehto; silmukan lopussa tehtävät toimenpiteet)
{ 
   lauseet; // silmukan runko-osa
}</code></p>
<p>voisi tehdä while-rakenteella seuraavasti.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
muuttujien alustukset;
while (ehto) 
{
  lauseet; // silmukan runko-osa
  silmukan lopussa tehtävät toimenpiteet;
}</code></p>
<p><em>Mihin for-silmukkaa sitten tarvitaan?</em></p>
<p>Nuo kolme osaa - muuttujien alustus, ehto, lopussa tehtävät toimenpiteet
- ovat osia, jotka jokaisessa silmukkarakenteessa tarvítaan.
for-silmukassa ne kirjoitetaan selvästi perättäin yhdelle riville,
jolloin ne on helpompi saada kirjoitettua kerralla oikein. Silmukan
suoritusta ohjaavat tekijät on myös helpompi lukea yhdeltä riviltä, kuin
yrittää selvittää sitä eri riveiltä (silmukkahan voi olla hyvinkin monta
koodiriviä pitkä). continue-lauseen osalta for- ja while-silmukat
toimivat eri tavalla. while-silmukassa lisäykset voivat jäädä tekemättä
continue-lauseen kanssa.</p>
<h3>16.4.2 Esimerkki: lumiukon pallot keltaiseksi</h3>
<p>Palataan esimerkkiin 15.3. Koska pallo-oliot ovat taulukossa, voimme
käydä taulukon alkiot läpi silmukan avulla, ja muuttaa kaikkien pallojen
värin toiseksi. Main-metodi on jätetty listauksesta pois. Pallojen
luomiseen käytämme esimerkissä 16.2.4 esiteltyä LuoPallo-aliohjelmaa.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
using System;
using Jypeli;</p>
<p>/// @author  Antti-Jussi Lakanen
/// @version 22.12.2011
///
/// <summary>
/// Lumiukko, jonka pallot väritetään.
/// </summary>
public class LumiukkoKeltaisetPallot : PhysicsGame
{
    /// <summary>
    /// Piirretään oliot ja zoomataan kamera niin että kenttä näkyy kokonaan.
    /// </summary>
    public override void Begin()
    {
        Camera.ZoomToLevel();
        Level.Background.Color = Color.Black;
        // Lisätään pallot taulukkoon, ja sitten lisätään kentälle
        PhysicsObject[] pallot = new PhysicsObject[3];
        pallot[0] = LuoPallo(0, Level.Bottom + 200, Color.White, 100);
        pallot[1] = LuoPallo(0, pallot[0].Y + 100 + 50, Color.White, 50);
        pallot[2] = LuoPallo(0, pallot[1].Y + 50 + 30, Color.White, 30);
        Add(pallot[0]); Add(pallot[1]); Add(pallot[2]);</p>
<pre><code>    // Muutetaan pallojen väri
    for (int i = 0; i &lt; pallot.Length; i++)
    {
        pallot[i].Color = Color.Yellow;
    }
}

/// &lt;summary&gt;Luo yksittäisen pallon ja palauttaa sen.&lt;/summary&gt;
/// &lt;param name="x"&gt;Pallon kp x-koordinaatti&lt;/param&gt;
/// &lt;param name="y"&gt;Pallon kp y-koordinaatti&lt;/param&gt;
/// &lt;param name="vari"&gt;Pallon väri&lt;/param&gt;
/// &lt;param name="sade"&gt;Pallon säde&lt;/param&gt;
public static PhysicsObject LuoPallo(double x, double y, Color vari, double sade)
{
    PhysicsObject pallo = new PhysicsObject(2 * sade, 2 * sade, Shape.Circle);
    pallo.Color = vari;
    pallo.X = x;
    pallo.Y = y;
    return pallo;
}
</code></pre>
<p>}
```</p>
<h3>16.4.3 Harjoitus</h3>
<p>Tee aliohjelma, joka muuttaa PhysicsObject-taulukossa olevien olioiden
värin halutuksi.</p>
<h3>16.4.4 Esimerkki: Keskiarvo-aliohjelma</h3>
<p>Muuttujien yhteydessä teimme aliohjelman, joka laski kahden luvun
keskiarvon. Tällainen aliohjelma ei ole kovin hyödyllinen, sillä jos
haluaisimme laskea kolmen tai neljän luvun keskiarvon, täytyisi meidän
tehdä niillä omat aliohjelmat. Sen sijaan jos annamme luvut taulukossa,
pärjäämme yhdellä aliohjelmalla. Tehdään siis nyt aliohjelma Keskiarvo,
joka laskee taulukossa olevien kokonaislukujen keskiarvon. Kirjoitetaan
sille myös ComTest-testit.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
/// &lt;summary&gt;
/// Palauttaa parametrina saamansa int-taulukon
/// alkoiden keskiarvon.
/// &lt;/summary&gt;
/// &lt;param name="luvut"&gt;summattavat luvut&lt;/param&gt;
/// &lt;returns&gt;summa&lt;/returns&gt;
/// &lt;example&gt;
/// &lt;pre name="test"&gt;
///   int[] luvut1 = {0};
///   Laskuja.Keskiarvo(luvut1) ~~~ 0;
///   int[] luvut2 = {3, 3, 3};
///   Laskuja.Keskiarvo(luvut2) ~~~ 3;
///   int[] luvut3 = {3, -3, 3};
///   Laskuja.Keskiarvo(luvut3) ~~~ 1;
///   int[] luvut4 = {-3, -6};
///   Laskuja.Keskiarvo(luvut4) ~~~ -4.5;
/// &lt;/pre&gt;
/// &lt;/example&gt;
public static double Keskiarvo(int[] luvut)
{
    double summa = 0;
    for (int i = 0; i &lt; luvut.Length; i++)
    {
        summa += luvut[i];
    }
    return summa / luvut.Length;
}</code></p>
<p>Ohjelmassa lasketaan ensiksi kaikkien taulukoiden lukujen summa
muuttujaan summa. Koska taulukoiden indeksointi alkaa nollasta, on
ehdottoman kätevää asettaa myös laskurimuuttuja i aluksi arvoon 0.
Ehtona on, että silmukkaa suoritetaan niin kauan kuin muuttuja i on
pienempi kuin taulukon pituus. Jos tuntuu, että ehdossa pitäisi olla
yhtä suuri tai pienempi kuin -merkki (\&lt;=), niin pohdi seuraavaa. Jos
taulukon koko olisi vaikka 7, niin tällöin viimeinen alkio olisi
alkiossa luvut[6], koska indeksointi alkaa nollasta. Tästä johtuen jos
ehdossa olisi "\&lt;="-merkki, viitattaisiin viimeisenä taulukon alkioon
luvut[7], joka ei enää kuulu taulukon muistialueeseen. Tällöin ohjelma
kaatuisi ja saisimme "IndexOutOfRangeException"-poikkeuksen.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
return summa / luvut.Length;</code></p>
<p>Aliohjelman lopussa palautetaan lukujen summa jaettuna lukujen määrällä,
eli taulukon pituudella.</p>
<h3>16.4.5 Harjoitus</h3>
<p>Pohdi, mikä tärkeä tapaus jää huomiotta taulukon keskiarvon
laskemisessa.</p>
<h3>16.4.6 Esimerkki: Taulukon kääntäminen käänteiseen järjestykseen</h3>
<p>Kontrollirakenteen ensimmäisessä osassa voidaan siis alustaa myös useita
muuttujia. Klassinen esimerkki tällaisesta tapauksesta on taulukon
alkioiden kääntäminen päinvastaiseen järjestykseen.</p>
<p>Tehdään aliohjelma joka saa parametrina int-tyyppisen taulukon ja
palauttaa taulukon käänteisessä järjestyksessä.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
/// &lt;summary&gt;
/// Aliohjelma kääntää kokonaisluku-taulukon alkiot päinvastaiseen
/// järjestykseen.
/// &lt;/summary&gt;
/// &lt;param name="taulukko"&gt;Käännettävä taulukko.&lt;/param&gt;
public static void KaannaTaulukko(int[] taulukko) 
{
   int temp;
   for (int vasen = 0, oikea = taulukko.Length-1; vasen &lt; oikea; vasen++, oikea--) 
   {
      temp = taulukko[vasen];
      taulukko[vasen] = taulukko[oikea];
      taulukko[oikea] = temp;
   }
}</code></p>
<p>Ideana yllä olevassa aliohjelmassa on, että meillä on kaksi muuttujaa.
Muuttujia voisi kuvata kuvainnollisesti osoittimiksi. Osoittimista
toinen osoittaa aluksi taulukon alkuun ja toinen taulukon loppuun.
Oikeasti osoittimet ovat int-tyyppisiä muuttujia, jotka saavat arvokseen
taulukon indeksejä. Taulukon alkuun osoittavan muuttujan nimi on vasen
ja taulukon loppuun osoittavan muuttujan nimi on oikea. Vasenta
osoitinta liikutetaan taulukon alusta loppuun päin ja oikeaa taulukon
lopusta alkuun päin. Jokaisella kierroksella vaihdetaan niiden taulukon
alkioiden paikat keskenään, joihin osoittimet osoittavat. Silmukan
suoritus lopetetaan juuri ennen kuin osoittimet kohtaavat toisensa.</p>
<p>Tarkastellaan aliohjelmaa nyt hieman tarkemmin.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
int temp;</code></p>
<p>Ensimmäiseksi metodissa on alustettu temp-niminen muuttuja. Muuttuja
tarvitaan, jotta taulukon alkioiden paikkojen vaihtaminen onnistuisi.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
for (int vasen = 0, oikea = taulukko.Length-1; vasen &lt; oikea; vasen++, oikea--)</code></p>
<p>Kontrollirakenteessa alustetaan ja päivitetään nyt kahta eri muuttujaa.
Muuttujat erotetaan toisistaan pilkulla. Huomaa, että muuttujan tyyppi
kirjoitetaan vain yhden kerran! Ehtona on, että suoritusta jatketaan
niin kauan kuin muuttuja vasen on pienempää kuin muuttuja oikea. Lopuksi
päivitetään vielä muuttujien arvoja. Eri muuttujien päivitykset
erotetaan toisistaan jälleen pilkulla. Muuttujaa vasen kasvatetaan joka
kierroksella yhdellä kun taas muuttujaa oikea sen sijaa vähennetään.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
temp = taulukko[vasen];</code></p>
<p>Seuraavaksi laitetaan vasemman osoittimen osoittama alkio väliaikaiseen
säilytykseen temp-muuttujaan.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
taulukko[vasen] = taulukko[oikea];</code></p>
<p>Nyt voimme tallentaa oikean osoittimen osoittaman alkion vasemman
osoittimen osoittaman alkion paikalle.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
taulukko[oikea] = temp;</code></p>
<p>Yllä olevalla lauseella asetetaan vielä temp-muuttujaan talletettu arvo
oikean osoittimen osoittamaan alkioon. Nyt vaihto on suoritettu
onnistuneesti.</p>
<p>Tässä funktiolla oli sivuvaikutus, eli se muutti parametrina vietyä
taulukkoa. Jos haluttaisiin alkuperäisen taulukon säilyvän, pitäisi
funktion alussa luoda uusi taulukko tulosta varten, sijoittaa arvot
käänteisessä järjestyksessä ja lopuksi <em>palauttaa</em> viite uuteen
taulukkoon.</p>
<h3>16.4.7 Harjoitus</h3>
<p>Tee funktiosta KaannaTaulukko sivuvaikutukseton versio.</p>
<h3>16.4.8 Esimerkki: arvosanan laskeminen taulukoilla</h3>
<p>Ehtolauseita käsiteltäessä tehtiin aliohjelma, joka laski
tenttiarvosanan. Aliohjelma sai parametreina tentin maksimipisteet,
läpipääsyrajan ja opiskelijan tenttipisteet ja palautti opiskelijan
arvosanan. Tehdään nyt vastaava ohjelma käyttämällä taulukoita.
Kirjoitetaan samalla ComTest-testit.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
using System;
using System.Collections.Generic;</p>
<p>/// @author  Antti-Jussi Lakanen, Martti Hyvönen
/// @version 22.12.2011
///
/// <summary>
/// Harjoitellaan vielä taulukoiden käyttöä.
/// </summary>
public class Arvosana
{
    /// <summary>
    /// Laskee opiskelijan tenttiarvosanan asteikoilla 0-5.
    /// </summary>
    /// <param name="maksimipisteet">Tentin maksimipisteet.</param>
    /// <param name="lapaisyraja">Tentin läpipääsyraja.</param>
    /// <param name="tenttipisteet">Opiskelijan tenttipisteet.</param>
    /// <returns>Opiskelijan tenttiarvosana.</returns>
    /// <example>
    /// <pre name="test">
    /// Arvosana.LaskeArvosana(24, 12, 11) === 0; 
    /// Arvosana.LaskeArvosana(24, 12, 12) === 1; 
    /// Arvosana.LaskeArvosana(24, 12, 13) === 1; 
    /// Arvosana.LaskeArvosana(24, 12, 14) === 1; 
    /// Arvosana.LaskeArvosana(24, 12, 15) === 2; 
    /// Arvosana.LaskeArvosana(24, 12, 19) === 3; 
    /// Arvosana.LaskeArvosana(24, 12, 20) === 4; 
    /// Arvosana.LaskeArvosana(24, 12, 22) === 5; 
    /// Arvosana.LaskeArvosana(24, 12, 28) === 5; 
    /// </pre>
    /// </example>
    public static int LaskeArvosana(int maksimipisteet, int lapaisyraja, 
                                    int tenttipisteet)
    {
        double[] arvosanaRajat = new double[6];
        double arvosanojenPisteErot = (maksimipisteet - lapaisyraja) / 5.0;</p>
<pre><code>    //Arvosanan 1 rajaksi tentin läpipääsyraja
    arvosanaRajat[1] = lapaisyraja;

    //Asetetaan taulukkoon jokaisen arvosanan raja
    for (int i = 2; i &lt;= 5; i++)
    {
        arvosanaRajat[i] = arvosanaRajat[i - 1] + arvosanojenPisteErot;
    }

    //Katsotaan mihin arvosanaan tenttipisteet riittävät
    for (int i = 5; 1 &lt;= i; i--)
    {
        if (arvosanaRajat[i] &lt;= tenttipisteet) return i;
    }
    return 0;
}

/// &lt;summary&gt;
/// Pääohjelma
/// &lt;/summary&gt;
public static void Main()
{
    Console.WriteLine(LaskeArvosana(24, 12, 19)); // tulostaa 3
    Console.WriteLine(LaskeArvosana(24, 12, 11)); // tulostaa 0
    Console.ReadKey();
}
</code></pre>
<p>}
```</p>
<p>Aliohjelman idea on, että jokaisen arvosanan raja tallennetaan
taulukkoon. Kun taulukkoa sitten käydään läpi lopusta alkuun päin,
voidaan kokeilla mihin arvosanaan opiskelijan pisteet riittävät.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
double[] arvosanaRajat = new double[6];</code></p>
<p>Aliohjelman alussa alustetaan tenttiarvosanojen pisterajoille taulukko.
Taulukko alustetaan kuuden kokoiseksi, jotta voisimme tallentaa jokaisen
arvosanan pisterajan vastaavan taulukon indeksin kohdalle. Arvosanan 1
pisteraja on taulukon indeksissä 1 ja arvosanan 2 indeksissä 2 ja niin
edelleen. Näin taulukon ensimmäinen indeksi jää käyttämättä, mutta
taulukkoon viittaaminen on selkeämpää. Koska pisterajat voivat olla
desimaalilukuja, on taulukon oltava tyypiltään double[].</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
double arvosanojenPisteErot = (maksimipisteet - lapaisyraja) / 5.0;</code></p>
<p>Yllä oleva rivi laskee arvosanojen välisen piste-eron. Mieti, miksi
jakoviivan alapuolelle on luku laitettava muodossa 5.0, eikä 5.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
arvosanaRajat[1] = lapaisyraja;</code></p>
<p>Tällä rivillä asetetaan arvosanan 1 rajaksi tentin läpipääsyraja.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
for (int i = 2; i &lt;= 5; i++) 
{
   arvosanaRajat[i] = arvosanaRajat[i-1] + arvosanojenPisteErot;
}</code></p>
<p>Yllä oleva silmukka laskee arvosanojen 2-5 pisterajat. Seuraava
pisteraja saadaan lisäämällä edelliseen arvosanojen välinen piste-ero.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
for (int i = 5; 1 &lt;= i; i--) 
{
   if (arvosanaRajat[i] &lt;= tenttipisteet) return i;
}</code></p>
<p>Tällä silmukalla sen sijaan katsotaan mihin arvosanaan opiskelijan
tenttipisteet riittävät. Arvosanoja aletaan käydä läpi lopusta alkuun
päin. Tämän takia muuttujan i arvo asetetaan aluksi arvoon 5 ja joka
kierroksella sitä pienennetään yhdellä. Kun oikea arvosana on löytynyt,
palautetaan tenttiarvosana (eli taulukon indeksi) välittömästi, ettei
käydä taulukon alkioita turhaan läpi.</p>
<p>Pääohjelmassa ohjelmaa on kokeiltu muutamilla testitulostuksilla.
Tarkemmat testit on tehty kuitenkin ComTest-testeinä, joita voidaan
testata automaattisesti.</p>
<p>Jos laskisimme useiden oppilaiden tenttiarvosanoja, niin aliohjelmamme
laskisi myös arvosanaRajat-taulukon arvot jokaisella kerralla erikseen.
Tämä on melko typerää tietokoneen resurssien tuhlausta. Meidän
kannattaakin tehdä oma aliohjelma siitä osasta, joka laskee
tenttiarvosanojen rajat. Tämä aliohjelma voisi palauttaa arvosanojen
rajat suoraan taulukossa. Nyt voisimme muuttaa LaskeArvosana-aliohjelmaa
niin, että se saa parametrikseen arvosanojen rajat taulukossa ja
opiskelijan tenttipisteet. Molempien aliohjelmien ComTest-testit ovat
myös näkyvillä.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
using System;
using System.Collections.Generic;
using System.Text;
using System.Globalization;</p>
<p>/// @author  Antti-Jussi Lakanen, Martti Hyvönen
/// @version 22.12.2011
///
/// <summary>
/// Harjoitellaan taulukoiden käyttöä ja lasketaan tenttiarvosanoja.
/// </summary>
public class Arvosanat
{
    /// <summary>
    /// Laskee tenttiarvosanojen pisterajat taulukkoon.
    /// </summary>
    /// <param name="maksimipisteet">tentin maksimipisteet</param>
    /// <param name="lapaisyraja">tentin läpipääsyraja</param>
    /// <returns>arvosanojen pisterajat taulukossa</returns>
    /// <example>
    /// <pre name="test">
    /// double[] rajat1 = Arvosanat.LaskeRajat(24, 12);
    /// String rajat1Jono = Arvosanat.TaulukkoJonoksi(rajat1);
    /// rajat1Jono === "0, 12, 14.4, 16.8, 19.2, 21.6";
    /// double[] rajat2 = Arvosanat.LaskeRajat(30, 15);
    /// String rajat2Jono = Arvosanat.TaulukkoJonoksi(rajat2);
    /// rajat2Jono === "0, 15, 18, 21, 24, 27";
    /// </pre>
    /// </example>
    public static double[] LaskeRajat(int maksimipisteet, int lapaisyraja)
    {
        double[] arvosanaRajat = new double[6];
        double arvosanojenPisteErot = Math.Round((maksimipisteet - lapaisyraja) / 5.0, 1);</p>
<pre><code>    arvosanaRajat[1] = lapaisyraja;

    // Asetetaan taulukkoon jokaisen arvosanan raja
    for (int i = 2; i &lt;= 5; i++)
        arvosanaRajat[i] = arvosanaRajat[i - 1] + arvosanojenPisteErot;
    return arvosanaRajat;
}

/// &lt;summary&gt;
/// Muuttaa annetun taulukon merkkijonoksi.
/// Erotinmerkkinä toimii pilkku + välilyönti (", ").
/// &lt;/summary&gt;
/// &lt;param name="taulukko"&gt;Jonoksi muutettava taulukko.&lt;/param&gt;
/// &lt;returns&gt;Taulukko merkkijonona.&lt;/returns&gt;
/// &lt;example&gt;
/// &lt;pre name="test"&gt;
/// double[] taulukko1 = new double[] {15.4, 20, 1.0, 5.9, -2.4};
/// Arvosanat.TaulukkoJonoksi(taulukko1) === "15.4, 20, 1, 5.9, -2.4";
/// &lt;/pre&gt;
/// &lt;/example&gt;
public static String TaulukkoJonoksi(double[] taulukko)
{
    String erotin = "";
    StringBuilder jono = new StringBuilder();
    foreach (double luku in taulukko)
    {
        jono.Append(erotin);
        jono.Append(luku.ToString(CultureInfo.InvariantCulture));
        erotin = ", ";
    }
    return jono.ToString();
}

/// &lt;summary&gt;
/// Laskee opiskelijan tenttiarvosanan asteikoilla 0-5.
/// &lt;/summary&gt;
/// &lt;param name="pisterajat"&gt;Arvosanojen rajat taulukossa. 
/// Arvosanan 1 raja taulukon indeksissä 1 jne. &lt;/param&gt;
/// &lt;param name="tenttiPisteet"&gt;Saadut tenttipisteet.&lt;/param&gt;
/// &lt;returns&gt;Tenttiarvosana välillä [0..5].&lt;/returns&gt;
/// &lt;example&gt;
/// &lt;pre name="test"&gt;
/// double[] rajat = {0, 12, 14, 16, 18, 20};
/// Arvosanat.LaskeArvosana(rajat, 11) === 0;
/// Arvosanat.LaskeArvosana(rajat, 12) === 1;
/// Arvosanat.LaskeArvosana(rajat, 14) === 2;
/// Arvosanat.LaskeArvosana(rajat, 22) === 5;
/// Arvosanat.LaskeArvosana(rajat, 28) === 5;
/// &lt;/pre&gt;
/// &lt;/example&gt;
public static int LaskeArvosana(double[] pisterajat, int tenttiPisteet)
{
    for (int i = pisterajat.Length - 1; 1 &lt;= i; i--)
    {
        if (pisterajat[i] &lt;= tenttiPisteet) return i;
    }
    return 0;
}

/// &lt;summary&gt;
/// Pääohjelmassa pari esimerkkiä.
/// &lt;/summary&gt;
public static void Main()
{
    double[] pisterajat = LaskeRajat(24, 12);
    Console.WriteLine(LaskeArvosana(pisterajat, 12)); // tulostaa 1
    Console.WriteLine(LaskeArvosana(pisterajat, 20)); // tulostaa 4
    Console.WriteLine(LaskeArvosana(pisterajat, 11)); // tulostaa 0
    Console.ReadKey();
}
</code></pre>
<p>}
```</p>
<p>Yllä olevassa esimerkissä lasketaan nyt arvosanarajat vain kertaalleen
taulukkoon ja samaa taulukkoa käytetään nyt eri arvosanojen laskemiseen.
Yhden aliohjelman kuuluisikin aina suorittaa vain yksi tehtävä tai
toimenpide. Näin aliohjelman koko ei kasva mielettömyyksiin. Lisäksi
mahdollisuus, että pystymme hyödyntämään aliohjelmaa joskus myöhemmin
toisessa ohjelmassa lisääntyy.</p>
<p>Ohjelmassa on testejä varten tehty yksi apufunktio, TaulukkoJonoksi,
jonka tehtävä on palauttaa taulukon alkiot yhtenä merkkijonona perättäin
lueteltuna pilkulla erotettuna.</p>
<h2>16.5 foreach-silmukka</h2>
<p>Taulukoita käsiteltäessä voidaan käyttää myös foreach-silmukkaa. Se on
eräänlainen paranneltu versio for-silmukasta. Nimensä mukaan se käy läpi
kaikki taulukon alkiot. Se on syntaksiltaan selkeämpi silloin, kun
haluamme tehdä jotain jokaiselle taulukon alkiolle. Sen syntaksi on
yleisessä muodossa seuraava.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
foreach (taulukonAlkionTyyppi alkio in taulukko) 
{
   lauseet;
}</code></p>
<p>Nyt foreach-silmukan kontrollilausekkeessa ilmoitetaan vain kaksi asiaa.
Ensiksi annetaan tyyppi ja nimi muuttujalle, joka viittaa yksittäiseen
taulukon alkioon. Tyypin täytyy olla sama kuin käsiteltävän taulukon
alkiotyyppi, mutta nimen saa itse keksiä. Tälle muuttujalle tehdään ne
toimenpiteet, mitä jokaiselle taulukon alkiolle halutaan tehdä. Toisena
tietona foreach-silmukalle pitää antaa sen taulukon nimi, mitä halutaan
käsitellä. Huomaa, että tiedot erotetaan foreach-silmukassa
kaksoispisteellä. Tämä erottaa foreach-silmukan for-silmukasta.
Esimerkiksi kuukausienPaivienLkm-taulukon alkioita voisi nyt tulostaa
seuraavasti.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
foreach (int kuukaudenPituus in kuukausienPaivienLkm) 
{
   Console.Write(kuukaudenPituus + " ");
}</code></p>
<p>Vapaasti suomennettuna:</p>
<p>"Jokaiselle kuukaudelle kuukausienPaivienLkm-taulukossa (tee)...".</p>
<h3>16.5.1 Esimerkki: taulukon pallot keltaisiksi</h3>
<p>Esimerkissä 16.4.2 värjäsimme lumiukon pallot keltaisiksi. Koska
halusimme muuttaa <em>kaikkien</em> taulukossa olevien olioiden värin, on
luontevampaa käyttää tehtävään foreach-silmukkaa. LuoPallo-aliohjelma on
sama kuin esimerkissä 16.4.2 ja jätetty listauksesta pois.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
using Jypeli;
using System;</p>
<p>/// <summary>
/// Lumiukko, jonka pallot ovat taulukossa.
/// </summary>
public class Lumiukko : PhysicsGame
{
    /// <summary>
    /// Piirretään oliot ja zoomataan kamera niin että kenttä näkyy kokonaan.
    /// </summary>
    public override void Begin()
    {
        Camera.ZoomToLevel();
        Level.Background.Color = Color.Black;</p>
<pre><code>    PhysicsObject[] pallot = new PhysicsObject[3];
    pallot[0] = LuoPallo(0, Level.Bottom + 200, Color.White, 100);
    pallot[1] = LuoPallo(0, pallot[0].Y + 100 + 50, Color.White, 50);
    pallot[2] = LuoPallo(0, pallot[1].Y + 50 + 30, Color.White, 30);
    Add(pallot[0]); Add(pallot[1]); Add(pallot[2]);

    foreach (PhysicsObject pallo in pallot)
    {
        pallo.Color = Color.Yellow;
    }
}

/// &lt;summary&gt; ... 
public static PhysicsObject LuoPallo(double x, double y, Color vari, double sade) ...
</code></pre>
<p>}
```</p>
<h2>16.6 Sisäkkäiset silmukat</h2>
<p>Kaikkia silmukoita voi kirjoittaa myös toisten silmukoiden sisälle.
Sisäkkäisiä silmukoita tarvitaan ainakin silloin, kun halutaan tehdä
jotain moniulotteisille taulukoille. Luvussa 15.5 määrittelimme
kaksiulotteisen taulukon elokuvien tallentamista varten. Tulostetaan nyt
sen sisältö käyttämällä kahta for-silmukkaa.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
String[,] elokuvat = new String[3,3] 
{ 
   {"Pulp Fiction", "Toiminta", "Tarantino"}, 
   {"2001: Avaruusseikkailu", "Scifi", "Kubrick"},
   {"Casablanca", "Draama", "Curtiz"} 
};</p>
<p>for (int r = 0; r &lt; 3; r++) // rivit
{ 
   for (int s = 0; s &lt; 3; s++) // sarakkeet
   { 
      Console.Write(elokuvat[r, s] + " | ");
   }
   Console.WriteLine();
}
```</p>
<p>Ulommassa for-silmukassa käydään läpi taulukon jokainen rivi, eli eri
elokuvat. Kun elokuva on ”valittu”, käydään elokuvan tiedot läpi.
Sisemmässä for-silmukassa käydään läpi aina kaikki yhden elokuvan
tiedot. Tietyn elokuvan eri tiedot tai kentät on tässä päätetty erottaa
"|"-merkillä. Sisemmän for-silmukan jälkeen tulostetaan vielä
rivinvaihto Console.WriteLine()-metodilla. Näin eri elokuvat saadaan eri
riveille.</p>
<p>Tässä täytyy ottaa huomioon, että ulomman taulukon indeksejä käydään
läpi eri muuttujalla kuin sisempää taulukkoa. Usein on tapana kirjoittaa
ensimmäisen (ulomman) indeksimuuttujan nimeksi i, ja seuraavan nimeksi
j. Samannimisiä muuttujia ei voi käyttää, sillä ne ovat nyt samalla
näkyvyysalueella. Tässä kyseisessä esimerkissä on loogisempaa käyttää
riveihin ja sarakkeisiin viittaavia indeksien nimiä - siis r ja s.</p>
<h2>16.7 Esimerkki: rivi, jolla eniten vapaata tilaa</h2>
<p>Palataan vielä aikaisempaan laivanupotusesimerkkiin. Tehdään aliohjelma,
joka etsii 2-ulotteisen taulukon riveistä sen, jolla on eniten tyhjää
(eli missä on eniten tilaa laittaa uusi laiva).</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
/// <summary>
/// Aliohjelma palauttaa sen rivin (indeksin),
/// millä on eniten vapaata (eli rivi, jolla 
/// eniten 0-alkioita). Jos näitä rivejä on useita, niin
/// palautetaan niistä ensimmäinen.
/// </summary>
/// <param name="ruudut">taulukko ruuduista, kussakin 
/// ruudussa 0 = vapaa, muu numero = laivan numero.</param>
/// <returns>Rivi, jolla eniten vapaata.</returns>
/// <example>
/// <pre name="test">
/// int[,] ruudut1 = {{1, 0, 2}, {0, 0, 2}, {3, 3, 3}};
/// Laivanupotus.RiviJollaEnitenVapaata(ruudut1) === 1;
/// int[,] ruudut2 = {{1, 1, 1}, {2, 2, 2}, {3, 3, 3}};
/// Laivanupotus.RiviJollaEnitenVapaata(ruudut2) === 0;
/// int[,] ruudut3 = {{1, 1, 0}, {0, 0, 2}, {0, 3, 0}};
/// Laivanupotus.RiviJollaEnitenVapaata(ruudut3) === 1;
/// </pre>
/// </example>
public static int RiviJollaEnitenVapaata(int[,] ruudut)
{
  int riviJollaEnitenVapaata = 0;
  int enitenVapaitaMaara = 0;</p>
<p>for (int r = 0; r &lt; ruudut.GetLength(0); r++)
  {
    int vapaata = 0;
    for (int s = 0; s &lt; ruudut.GetLength(1); s++)
    {
      if (ruudut[r, s] == 0) vapaata++;
    }
    if (vapaata &gt; enitenVapaitaMaara)
    {
      riviJollaEnitenVapaata = r;
      enitenVapaitaMaara = vapaata;
    }
  }
  return riviJollaEnitenVapaata;
}
```</p>
<h2>16.8 Silmukan suorituksen kontrollointi break- ja continue-lauseilla</h2>
<p>Silmukoiden normaalia toimintaa voidaan muuttaa break- ja
continue-lauseilla. Niiden käyttäminen <em>ei</em> ole tavallisesti
suositeltavaa, vaan silmukat pitäisi ensisijaisesti suunnitella niin,
ettei niitä tarvittaisi.</p>
<h3>16.8.1 break</h3>
<p>break-lauseella hypätään välittömästi pois silmukasta ja ohjelman
suoritus jatkuu silmukan jälkeen.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
int laskuri = 0;
while (true) 
{
   if (laskuri = 10) break;
   Console.WriteLine("Hello world!");
   laskuri++;
}</code></p>
<p>Yllä olevassa ohjelmassa muodostetaan ikuinen silmukka asettamalla
while-silmukan ehdoksi true. Tällöin ohjelman suoritus jatkuisi
loputtomiin ilman break-lausetta. Nyt break-lause suoritetaan, kun
laskuri saa arvon 10. Tämä rakennehan on täysin järjetön, sillä
if-lauseen ehdon voisi asettaa käänteisenä while-lauseen ehdoksi ja
ohjelma toimisi täysin samanlailla. Useimmiten break-lauseen käytön
voikin välttää.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
int laskuri = 0;
while (laskuri != 10) 
{
   Console.WriteLine("Hello world!");
   laskuri++;
}</code></p>
<p>break-lauseen käyttö voi kuitenkin olla järkevää, jos kesken silmukan
todetaan, että silmukan jatkaminen on syytä lopettaa.</p>
<h3>16.8.2 continue</h3>
<p>continue-lauseella hypätään silmukan alkuun ja silmukan suoritus jatkuu
siitä normaalisti. Sillä voidaan siis ohittaa lohkon loppuosa.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
// Tulostetaan luku vain jos se on pariton
for (int i = 0; i &lt; 100; i++) 
{
   if (i % 2 == 0) continue;
   Console.WriteLine(i);
}</code></p>
<p>Yllä oleva ohjelmanpätkä siirtyy silmukan alkuun kun muuttujan i ja
luvun 2 jakojäännös on 0. Muussa tapauksessa ohjelma tulostaa muuttujan
i arvon. Toisin sanoen ohjelma tulostaa vain parittomat luvut. Myös
continue-rakennetta voi ja kannattaa pyrkiä välttämään, samoin kuin
turhia if-rakenteita. Yllä olevan ohjelmanpätkän voisi kirjoittaa vaikka
seuraavasti.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
for (int i = 0; i &lt; 100; i++) 
{
   if (i % 2 != 0) Console.WriteLine(i);
}</code></p>
<p>Tai vielä yksinkertaisemmin seuraavasti.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
for (int i = 1; i &lt; 100; i += 2) 
{  
   Console.WriteLine(i);
}</code></p>
<p>Tyypillisesti continue-lausetta käytetään tilanteessa, jossa todetaan
joidenkin arvojen olevan sellaisia, että tämä silmukan kierros on syytä
lopettaa, mutta silmukan suoritusta täytyy vielä jatkaa.</p>
<h2>16.9 Ohjelmointikielistä puuttuva silmukkarakenne</h2>
<p>Silloin tällöin ohjelmoinnissa tarvitsisimme rakennetta, jossa silmukan
sisäosa on jaettu kahteen osaan. Ensimmäinen osa suoritetaan vaikka ehto
ei enää olisikaan voimassa, mutta jälkimmäinen osa jätetään
suorittamatta. Tällaista rakennetta ei C#-kielestä löydy valmiina.
Tämän rakenteen voi kuitenkin tehdä itse, jolloin on perusteltua käyttää
hallittua ikuista silmukkaa, joka lopetetaan break-lauseella. Rakenne
voisi olla suunnilleen seuraavanlainen:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
while (true) 
{ //ikuinen silmukka
   Silmukan ensimmäinen osa //suoritetaan, vaikka ehto ei pädekkään
   if (ehto) break;
   Silmukan toinen osa //ei suoriteta enää, kun ehto ei ole voimassa
}</code></p>
<p>Jos silmukan ehdoksi asetetaan true, täytyy jossain kohtaa ohjelmassa
olla break-lause, ettei silmukasta tulisi ikuista. Tällainen rakenne on
näppärä juuri silloin, kun haluamme, että silmukan lopettamista
tarkastellaan keskellä silmukkaa.</p>
<h2>16.10 Yhteenveto</h2>
<p>Silmukan valinta:</p>
<ul>
<li>
<p>for: Jos silmukan kierrosten määrä on ennalta tiedossa</p>
</li>
<li>
<p>foreach: Jos haluamme tehdä jotain jonkun Collection-tietorakenteen
    tai taulukon kaikille alkioille.</p>
</li>
<li>
<p>while: Jos silmukan kierrosten määrä ei ole tiedossa
    (erikoistapauksena hallittu ikuinen silmukka, josta poistutaan
    break-lauseella), emmekä välttämättä halua suorittaa silmukkaa
    kertaakaan.</p>
</li>
<li>
<p>do-while: Jos silmukan kierrosten määrä ei ole tiedossa, mutta
    haluamme suorittaa silmukan vähintään yhden kerran.</p>
</li>
</ul>
<p>Seuraava kuva kertaa vielä kaikki C#:n valmiit silmukat:</p>
<p><img alt="\
 Kuva 27: C#:n silmukat" src="../src/luentomonistecsUusin_htm_m6ff43568.gif" /></p>
<p>\
 \
 \</p>
<h1>17. Merkkijonojen pilkkominen ja muokkaaminen</h1>
<h2>17.1 String.Split()</h2>
<p>Merkkijonoja voidaan pilkkoa String-olion Split-metodilla. Metodi
palauttaa palaset merkkijono-tyyppisessä taulukossa String[]. Nyt
Split()-metodille annetaan parametrina taulukko niistä merkeistä (Char),
joiden halutaan toimivan erotinmerkkeinä. Pyydetään käyttäjältä syöte,
ja oletetaan, että haluamme, että välilyönti, puolipiste ja pilkku
toimivat erotinmerkkeinä.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
char[] erottimet = new char[] { ' ', ';', ',' };
Console.Write("Kirjoita &gt; ");
String jono1 = Console.ReadLine();
String[] pilkottu = jono1.Split(erottimet);</code></p>
<p>Merkkijono katkaistaan, jos merkki on välilyönti, pilkku tai puolipiste.
Jos käyttäjä antaa useamman erotinmerkin peräkkäin (vaikkapa kaksi
välilyöntiä), niin voi olla toivottavaa, ettei kuitenkaan taulukkoon
luoda tyhjää alkiota. Tämä voidaan hoitaa antamalla Split()-metodille
lisäparametri StringSplitOptions.RemoveEmptyEntries.</p>
<p>Jättämällä tyhjät alkiot huomiotta esimerkiksi merkkijono ”kissa,,,;
koira” palauttaisi vain kaksialkioisen taulukon:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
String jono2 = "kissa,,,; koira";
char[] erottimet = new char[] { ' ', ';', ',' };
String[] pilkottu = jono2.Split(erottimet,
  StringSplitOptions.RemoveEmptyEntries);
// Palat olisivat nyt ["kissa", "koira"]</code></p>
<p>Huomaa, että erotinmerkit eivät tule mukaan taulukkoon, vaan ne
"häviävät".</p>
<h2>17.2 String.Trim()</h2>
<p>String-olion Trim()-metodi palauttaa merkkijonon, josta on poistettu
välilyönnit parametrina annetun merkkijonon alusta ja lopusta.
Esimerkiksi merkkijono</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
String jono3 = "  kalle   ja kille    ";</code></p>
<p>muuttuisi muotoon</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Console.WriteLine(jono3 .Trim()); // "kalle   ja kille".</code></p>
<p>Huomaa, että merkkijonon keskellä olevia "ylimääräisiä" välilyöntejä
Trim-metodi ei kuitenkaan poista.</p>
<h2>17.3 Esimerkki: Merkkijonon pilkkominen ja muuttaminen kokonaisluvuiksi</h2>
<p>Tehdään ohjelma, joka kysyy käyttäjältä positiivisia kokonaislukuja,
laskee ne yhteen ja tulostaa tuloksen näytölle. Käyttäjä antaa luvut
siten, että välilyönti ja pilkku toimivat erotinmerkkeinä. Mikäli
käyttäjä antaa jotain muita merkkejä kuin positiivisia kokonaislukuja
(ja erotinmerkkejä), ohjelma antaa virheilmoituksen ja suoritus päättyy.
Ohjelmassa tehdään seuraavat aliohjelmat.</p>
<p>int[] MerkkijonoLuvuiksi(String, char[])\
 Aliohjelma muuttaa annetun merkkijonon kokonaislukutaulukoksi siten,
etta luvut erotellaan annetun merkkitaulukon (erotinmerkkien)
perusteella. Syötteen tulee sisältää vain lukuja ja erotinmerkkejä.</p>
<p>int LaskeYhteen(int[])\
 Palauttaa annetun kokonaislukutaulukon alkioiden summan.</p>
<p>bool OnkoVainLukuja(String, char[])\
 Tutkii sisältääkö annettu merkkijono vain lukuja (positiivisia
kokonaislukuja) ja erotinmerkkejä. Jos annettu merkkijono on tyhjä
(pituus on 0), palautetaan false.</p>
<p>void TulostaTaulukko(int[])\
 Tulostaa annetun kokonaislukutaulukon kaikki alkiot.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
using System;</p>
<p>/// @author  Antti-Jussi Lakanen
/// @version 22.12.2011
///
/// <summary>
/// Harjoitellaan merkkijonojen pilkkomista.
/// </summary>
public class MjLuvuiksi
{
    /// <summary>
    /// Kysellaan kayttajalta merkkijonoja ja 
    /// tehdaan niista taulukkoja, lasketaan lukuja yhteen ja tulostellaan.
    /// </summary>
    public static void Main()
    {
        char[] erottimet = new char[] { ' ', ',' };
        Console.Write("Anna positiivisia kokonaislukuja &gt; ");
        String lukusyote = Console.ReadLine();</p>
<pre><code>    // Jos kayttaja antanut jotain muuta kuin positiivisia 
    // kokonaislukuja, ei yritetakaan laskea lukuja yhteen
    if (OnkoVainLukuja(lukusyote, erottimet))
    {
        int[] luvut = MerkkijonoLuvuiksi(lukusyote, erottimet);
        Console.WriteLine("Tulkittiin luvut:");
        TulostaTaulukko(luvut);
        Console.WriteLine("Antamiesi lukujen summa on : " + LaskeYhteen(luvut));
    }
    else
        Console.WriteLine("Annoit muuta kuin lukuja, tai tyhjan jonon");
    Console.ReadKey();
}

/// &lt;summary&gt;
/// Aliohjelma muuttaa annetun merkkijonon 
/// kokonaislukutaulukoksi siten, etta luvut 
/// erotellaan annetun merkkitaulukon (erotinmerkkien)
/// perusteella. Syötteen tulee sisältää vain
/// lukuja ja erotinmerkkejä.
/// &lt;/summary&gt;
/// &lt;param name="lukusyote"&gt;Muunnettava merkkijono&lt;/param&gt;
/// &lt;param name="erottimet"&gt;Sallitut erotinmerkit merkkitaulukossa&lt;/param&gt;
/// &lt;returns&gt;Merkkijonosta selvitetty kokonaislukutaulukko.&lt;/returns&gt;
/// &lt;example&gt;
/// &lt;pre name="test"&gt;
/// int[] luvut1 = MjLuvuiksi.MerkkijonoLuvuiksi("1 2 3", new char[] {' '});
/// String.Join(",", luvut1) === "1,2,3";
/// int[] luvut2 = MjLuvuiksi.MerkkijonoLuvuiksi(",,1,, 2 ,3", new char[] {' ', ','});
/// String.Join(",", luvut2) === "1,2,3";
/// int[] luvut3 = MjLuvuiksi.MerkkijonoLuvuiksi("", new char[] {' '});
/// String.Join(",", luvut3) === "";
/// &lt;/pre&gt;
/// &lt;/example&gt;
public static int[] MerkkijonoLuvuiksi(string lukusyote, char[] erottimet)
{
    // Tyhjat pois edesta ja lopusta (Trim) 
    // Jos on annettu ylimaaraisia valilyonteja, ei lisata niita taulukkoon.            
    String[] pilkottu = 
       lukusyote.Trim().Split(erottimet, StringSplitOptions.RemoveEmptyEntries);
    int[] luvut = new int[pilkottu.Length]; // luvut[] saa kookseen saman kuin
                                            //  pilkottu[]
    for (int i = 0; i &lt; pilkottu.Length; i++)
        luvut[i] = int.Parse(pilkottu[i]);
    return luvut;
}

/// &lt;summary&gt;
/// Laskee kokonaislukutaulukon alkiot yhteen ja palauttaa alkioiden summan.
/// &lt;/summary&gt;
/// &lt;param name="luvut"&gt;Tutkittava kokonaislukutaulukko&lt;/param&gt;
/// &lt;returns&gt;Taulukon alkioiden summa&lt;/returns&gt;
/// &lt;example&gt;
/// &lt;pre name="test"&gt;
/// int[] luvut1 = {5, 7, 9, 10};
/// MjLuvuiksi.LaskeYhteen(luvut1) === 31;
/// int[] luvut2 = {-5, 5, -10, 10};
/// MjLuvuiksi.LaskeYhteen(luvut2) === 0;
/// int[] luvut3 = {};
/// MjLuvuiksi.LaskeYhteen(luvut3) === 0;
/// &lt;/pre&gt;
/// &lt;/example&gt;
public static int LaskeYhteen(int[] luvut)
{
    int summa = 0;
    for (int i = 0; i &lt; luvut.Length; i++)
        summa += luvut[i];
    return summa;
}

/// &lt;summary&gt;
/// Aliohjelmassa tutkitaan sisaltaako merkkijono muitakin
/// merkkeja kuin positiivisia kokonaislukuja ja erotinmerkkeja.
/// &lt;/summary&gt;
/// &lt;param name="lukusyote"&gt;Tutkittava merkkinojo, 
/// josta etsitaan vieraita merkkeja&lt;/param&gt;
/// &lt;param name="erottimet"&gt;Sallitut erotinmerkit 
/// merkkitaulukossa&lt;/param&gt;
/// &lt;returns&gt;Onko pelkkiä lukuja&lt;/returns&gt;
/// &lt;example&gt;
/// &lt;pre name="test"&gt;
/// MjLuvuiksi.OnkoVainLukuja("1,2,3", new char[]{','}) === true;
/// MjLuvuiksi.OnkoVainLukuja("1, 2, 3", new char[]{','}) === false;
/// MjLuvuiksi.OnkoVainLukuja("1, 2, 3", new char[]{',', ' '}) === true;
/// MjLuvuiksi.OnkoVainLukuja("", new char[]{' '}) === false;
/// &lt;/pre&gt;
/// &lt;/example&gt;
public static bool OnkoVainLukuja(string lukusyote, char[] erottimet)
{
    // Jos yhtaan merkkia ei ole annettu, 
    // palautetaan automaattisesti kielteinen vastaus.
    if (lukusyote.Length == 0) return false; 
    for (int i = 0; i &lt; erottimet.Length; i++)
        // Korvataan erotinmerkit tyhjalla merkkijonolla, 
        // silla olemme kiinnostuneita vain "varsinaisesta sisallosta"
        lukusyote = lukusyote.Replace(erottimet[i].ToString(), "");

    foreach (char merkki in lukusyote)
        // Jos yksikin merkki on jokin muu kuin numero, 
        // palautetaan kielteinen vastaus.
        if (!Char.IsDigit(merkki)) return false; 
    return true;
}

/// &lt;summary&gt;
/// Tulostetaan kokonaislukutaulukon osat foreach-silmukassa
/// &lt;/summary&gt;
/// &lt;param name="t"&gt;Tulostettava taulukko&lt;/param&gt;
public static void TulostaTaulukko(int[] t)
{
    foreach (int pala in t)
        Console.WriteLine(pala);
    Console.WriteLine("-------------------------------");
}
</code></pre>
<p>}
```</p>
<h1>18. Järjestäminen</h1>
<p>Kuinka järjestät satunnaisessa järjestyksessä olevan korttipakan kortit
järjestykseen pienimmästä suurimpaan?</p>
<p>Yksi tutkituimmista ohjelmointiongelmista ja algoritmeista on
järjestämisalgoritmi. Siis kuinka saamme esimerkiksi korttipakan kortit
numerojärjestykseen. Tai ohjelmointiin soveltuvammin kuinka saamme
järjestettyä taulukon luvut? Vaikka aluksi tuntuu, ettei erilaisia
tapoja järjestämiseen ole kovin montaa, on niitä todellisuudessa
kymmeniä, ellei satoja.</p>
<p>Järjestämisalgoritmeja käsitellään enemmän muilla kursseilla (esim.
<a href="https://korppi.jyu.fi/kotka/r.jsp?course=132236">ITKA201 Algoritmit 1</a>
ja <a href="https://korppi.jyu.fi/kotka/r.jsp?course=132249">TIEP111 Ohjelmointi
2</a>). Tässä vaiheessa
meille riittää, että osaamme käyttää C#:sta valmiina löytyvää
järjestämisaliohjelmaa Sort. Tämä on siitäkin syystä järkevää, että
kielestä valmiina löytyvä järjestämisalgoritmi on <em>lähes aina nopeampi</em>,
kuin itse tehty.</p>
<p>Taulukot voidaan järjestää käyttämällä Array-luokasta löytyvää
Sort-aliohjelmaa. Parametrina Sort-aliohjelma saa järjestettävän
taulukon. Aliohjelman tyyppi on static void, eli se <em>ei</em> palauta mitään,
vaan ainoastaan järjestää taulukon.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
int[] taulukko = {-4, 5, -2, 4, 5, 12, 9};
Array.Sort(taulukko);</p>
<p>// Tulostetaan alkiot, että nähdää onnistuiko järjestäminen.
foreach (int alkio in taulukko) 
{
   Console.WriteLine(alkio);
}
```</p>
<p>Alkioiden pitäisi nyt tulostua numerojärjestyksessä. Taulukko voitaisiin
myös järjestää vain osittain antamalla Sort-aliohjelmalle lisäksi
parametreina aloitusindeksi, sekä järjestettävien alkioiden määrä.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
int[] taulukko2 = {-4, 5, -2, 4, 5, 12, 9};</p>
<p>// järjestetään nyt vain kolme ensimmäistä alkiota
Array.Sort(taulukko2, 0, 3);</p>
<p>foreach (int alkio in taulukko) 
{
   Console.WriteLine(alkio);
}</p>
<p>// Tulostuu -4 -2 5 4 5 12 9
```</p>
<p>Kaikkia alkeistietotyyppisiä taulukoita voidaan järjestää
Sort-aliohjelmalla. Lisäksi voidaan järjestää taulukoita, joiden
alkioiden tietotyyppi <em>toteuttaa</em> (implements) IComparable-rajapinnan.
Esimerkiksi String-luokka toteuttaa tuon rajapinnan. Rajapinnoista
puhutaan lisää kohdassa 23.1.</p>
<h1>19. Olion ulkonäön muuttaminen (Jypeli)</h1>
<p>Olemme tähän mennessä käyttäneet jo monia Jypeli-kirjastoon
kirjoitettuja luokkia ja aliohjelmia. Tässä luvussa esitellään muutamia
yksittäisiä tärkeitä luokkia, aliohjelmia ja ominaisuuksia.</p>
<p>Luodaan ensin olio, jonka ulkonäköä esimerkeissä muutetaan.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
PhysicsObject palikka = new PhysicsObject(100, 50);</code></p>
<p>Olion on suorakulmio, jonka leveys on 100 ja korkeus 50. Jos haluat
olion näkyviin pelikentälle, muista aina lisätä se seuraavalla
lauseella.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Add(palikka);</code></p>
<h2>19.1 Väri</h2>
<p>Vaihdetaan seuraavaksi luomamme olion väri. Värin voi vaihtaa
seuraavalla tavalla:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
palikka.Color = Color.Gray;</code></p>
<p>Esimerkissä siis oliosta tehtiin harmaa. Värejä on valmiina paljon ja
niistä voi valita haluamansa. Voit esikatsella valmiita värejä
osoitteesta</p>
<p><a href="https://trac.cc.jyu.fi/projects/npo/wiki/OlionUlkonako#a2.V%C3%A4ri">https://trac.cc.jyu.fi/projects/npo/wiki/OlionUlkonako#a2.Väri</a>.</p>
<p>Omia värejä voi myös tehdä seuraavasti:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
palikka.Color = new Color( 0, 0, 0 );</code></p>
<p>Oliosta tuli musta. Ensimmäinen arvo kertoo punaisen värin määrän,
toinen arvo vihreän värin määrän ja kolmas sinisen värin määrän.
"Värimaailman" lyhenne RGB (<strong>R</strong>ed, <strong>G</strong>reen, <strong>B</strong>lue) tulee tästä.
Lyhenteestä on helppo muistaa missä järjestyksessä värit tulevat. Määrät
ovat kokonaislukuja välillä 0-255 (byte). Muitakin tapoja värien
asettamiseen on olemassa, mutta näillä kahdella pärjää jo hyvin.</p>
<h2>19.2 Koko</h2>
<p>Kokoa voi vaihtaa seuraavasti.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
palikka.Width = leveys;
palikka.Height = korkeus;</code></p>
<p>Leveys ja korkeus annetaan double-tyyppisinä lukuina. Saman asian voi
tehdä myös vektorin avulla yhdellä rivillä.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
palikka.Size = new Vector(leveys, korkeus);</code></p>
<h2>19.3 Tekstuuri</h2>
<p>Tekstuurikuvat kannattaa tallentaa png-muodossa, jolloin kuvaan voidaan
tallentaa myös alpha-kanavan tieto (läpinäkyvyys). Tallenna png-kuva
projektin Content-kansioon. Klikkaa sitten Visual Studion Solution
Explorerissa projektin nimen päällä hiiren oikealla napilla ja Add ->
Existing item. Hae kansiorakenteesta juuri tallentamasi kuva.</p>
<p>Tämän jälkeen tekstuuri asetetaan kuvalle seuraavasti.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Image olionKuva = LoadImage("kuvanNimi");
olio.Image = olionKuva;</code></p>
<p>Huomaa, että png-tunnistetta ei tarvitse laittaa kuvan nimen perään.</p>
<p>Saman voi tehdä myös lyhyemmin:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
palikka.Image = LoadImage("kuvanNimi");</code></p>
<p>Kohta kuvanNimi on Contentiin siirretyn kuvan nimi. Esimerkiksi, jos
kuva on kissa.png, niin kuvan nimi on silloin pelkkä kissa.</p>
<h2>19.4 Olion muoto</h2>
<p>Joskus olion muodon voi antaa jo oliota luotaessa. Muotoa voi kuitenkin
myös jälkikäteen muuttaa. Esimerkiksi:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
olio.Shape = Shape.Circle;</code></p>
<p>Tämä tekee oliostamme ympyrän muotoisen. Muita mahdollisia muotoja on
esimerkiksi nelikulmio, Shape.Rectangle.</p>
<h1>20. Ohjainten lisääminen peliin (Jypeli)</h1>
<p>Peli voi ottaa vastaan näppäimistön, Xbox 360 -ohjaimen, hiiren ja
Windows Phone 7 -puhelimen ohjausta. Ohjainten liikettä "kuunnellaan" ja
jokaiselle ohjaimelle voidaan määrittää erikseen, mitä mistäkin
tapahtuu. Kullekin ohjaimelle (näppäimistö, hiiri, Xbox-ohjain,
WP7-kosketusnäyttö, WP7-kiihtyvyysanturi) on tehty oma Listen-aliohjelma
jolla kuuntelun asettaminen onnistuu.</p>
<p>Jokainen Listen-kutsu on muodoltaan samanlainen riippumatta siitä mitä
ohjainta kuunnellaan. Ensimmäinen parametri kertoo mitä näppäintä
kuunnellaan, esimerkiksi:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Näppäimistö: Key.Up
Xbox360-ohjain: Button.DPadLeft
Hiiri: MouseButton.Left</code></p>
<p>Visual Studion kirjoitusapu auttaa löytämään mitä erilaisia
näppäinvaihtoehtoja kullakin ohjaimella on.</p>
<p>Toinen parametri määrittää minkälaisia näppäinten tapahtumia halutaan
kuunnella ja sillä on neljä mahdollista arvoa:</p>
<ul>
<li>
<p>ButtonState.Released: Näppäin on juuri vapautettu</p>
</li>
<li>
<p>ButtonState.Pressed: Näppäin on juuri painettu alas</p>
</li>
<li>
<p>ButtonState.Up: Näppäin on ylhäällä (vapautettuna)</p>
</li>
<li>
<p>ButtonState.Down: Näppäin on alaspainettuna</p>
</li>
</ul>
<p>Kolmas parametri kertoo mitä tehdään, kun näppäin sitten on painettuna.
Tähän tulee tapahtuman käsittelijä, eli sen aliohjelman nimi, jonka
suoritukseen haluamme siirtyä näppäimen tapahtuman sattuessa.</p>
<p>Neljäs parametri on ohjeteksti, joka voidaan näyttää pelaajalle pelin
alussa. Tässä tarvitsee vain kertoa mitä tapahtuu kun näppäintä
painetaan. Ohjetekstin tyyppi on String eli merkkijono. Merkkijono on
jono kirjoitusmerkkejä tietokoneen muistissa. Merkkijonoilla voimme
esittää mm. sanoja ja lauseita. Jos ohjetta ei halua tai tarvitse
laittaa, neljännen parametrin arvoksi voi antaa null jolloin se jää
tyhjäksi.</p>
<p>Parametrejä voi antaa enemmänkin sen mukaan mitä pelissä tarvitsee. Omat
(eli valinnaiset) parametrit laitetaan edellä mainittujen pakollisten
parametrien jälkeen ja ne viedään automaattisesti Listen-kutsussa
annetulle käsittelijälle. Tästä esimerkki hetken kuluttua.</p>
<p>Esimerkki näppäimistön kuuntelusta:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Keyboard.Listen(Key.Left, ButtonState.Down, 
  LiikutaPelaajaaVasemmalle, "Liikuta pelaajaa vasemmalle");</code></p>
<p>Kun vasen (Key.Left) näppäin on alhaalla (ButtonState.Down), niin
liikutetaan pelaajaa suorittamalla metodi LiikutaPelaajaaVasemmalle.
Viimeisenä parametrina on pelissä näkyvä näppäinohjeteksti.</p>
<p>Vastaava esimerkki Xbox 360 -ohjaimen kuuntelusta:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
ControllerOne.Listen(Button.DPadLeft, ButtonState.Down, LiikutaPelaajaaVasemmalle,
                    "Liikuta pelaajaa vasemmalle");</code></p>
<p>Yhtäaikaisesti voidaan kuunnella jopa neljää XBox-ohjainta. Tässä
kuunnellaan ohjaimista ensimmäistä (ControllerOne). Muut ohjaimet ovat
ControllerTwo ja niin edelleen. Kunkin ohjaimen järjestysluku näkyy
ohjaimen keskellä olevassa Xbox-kuvakkeessa, missä erityinen valo
indikoi, mikä ohjain on kysymyksessä.</p>
<h2>20.1 Näppäimistö</h2>
<p>Tässä esimerkissä asetetaan näppäimistön nuolinäppäimet liikuttamaan
pelaajaa.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
using System;
using Jypeli;</p>
<p>/// <summary>
/// Peli, jossa liikutellaan palloa.
/// </summary>
public class Peli : PhysicsGame
{
    /// <summary>
    /// Luodaan pelaaja ja asetetaan näppäintenkuuntelijat
    /// </summary>
    public override void Begin()
    {
        PhysicsObject pelaaja = new PhysicsObject(50, 50, Shape.Circle);
        Add(pelaaja);
        Keyboard.Listen(Key.Left, ButtonState.Down,
            LiikutaPelaajaa, "Liikuta vasemmalle", 
            pelaaja, new Vector(-1000, 0));
        Keyboard.Listen(Key.Right, ButtonState.Down,
            LiikutaPelaajaa, "Liikuta oikealle", pelaaja, new Vector(1000, 0));
        Keyboard.Listen(Key.Up, ButtonState.Down, 
            LiikutaPelaajaa, "Liikuta ylös", pelaaja, new Vector(0, 1000));
        Keyboard.Listen(Key.Down, ButtonState.Down,
            LiikutaPelaajaa, "Liikuta alas", pelaaja, new Vector(0, -1000));
    }</p>
<pre><code>/// &lt;summary&gt;
/// Aliohjelmassa liikutetaan 
/// oliota "työntämällä".
/// &lt;/summary&gt;
/// &lt;param name="suunta"&gt;Mihin suuntaan&lt;/param&gt;
private void LiikutaPelaajaa(PhysicsObject olio, Vector suunta)
{
    olio.Push(suunta);
}
</code></pre>
<p>}
```</p>
<p><em>Tapahtumankäsittelijän</em> LiikutaPelaajaa parametreista Physicsobject
olio ja Vector suunta saadaan tiedot, <em>mitä oliota</em> halutaan liikuttaa
ja <em>mihin suuntaan</em>. Huomaa, että nämä tiedot annetaan kutsuvaiheessa
”ylimääräisinä parametreina”, eli Keyboard.Listen-riveillä.</p>
<h2>20.2 Lopetuspainike ja näppäinohjepainike</h2>
<p>Pelin lopettamiselle ja näppäinohjeen näyttämiselle ruudulla on
Jypelissä olemassa valmiit aliohjelmat. Ne voidaan asettaa näppäimiin
seuraavasti:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Keyboard.Listen(Key.Escape, ButtonState.Pressed, Exit, "Poistu");
Keyboard.Listen(Key.F1, ButtonState.Pressed, ShowControlHelp, "Näytä ohjeet");</code></p>
<p>Tässä näppäimistön Esc-painike lopettaa pelin ja F1-painike näyttää
ohjeet.</p>
<p>ShowControlHelp näyttää peliruudulla pelissä käytetyt näppäimet ja
niille asetetut ohjetekstit. Ohjeteksti on Listen-kutsun neljäntenä
parametrina annettu merkkijono.</p>
<h2>20.3 Peliohjain</h2>
<p>Sama esimerkki XBox-peliohjainta käyttäen voidaan tehdä korvaamalla
rivit</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Keyboard.Listen(...);</code></p>
<p>riveillä</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
ControllerOne.Listen(...);</code></p>
<p>esimerkiksi näin</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
ControllerOne.Listen(Button.DPadLeft, ButtonState.Down, LiikutaPelaajaa, "Liikuta vasemmalle", pelaaja, new Vector(-1000, 0));</code></p>
<p>LiikutaPelaajaa-aliohjelmaan sen sijaan ei tarvitse tehdä muutoksia,
joten sama aliohjelma kelpaa sekä näppäimen että Xbox-ohjaimen
"digipad"-napin kuunteluun.</p>
<h3>20.3.1 Analoginen ”tatti”</h3>
<p>Jos halutaan kuunnella ohjaimen tattien liikettä, käytetään
ListenAnalog-kutsua.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
ControllerOne.ListenAnalog(AnalogControl.LeftStick, 0.1, 
                           LiikutaPelaajaa, "Liikuta pelaajaa tattia pyörittämällä.");</code></p>
<p>Kuunnellaan vasenta tattia (AnalogControl.LeftStick). Luku 0.1 kuvaa
sitä, miten herkästä liikkeestä tattia kuunteleva aliohjelma
suoritetaan. Kuuntelua käsittelee aliohjelma LiikutaPelaajaa.</p>
<p>LiikutaPelaajaa-aliohjelman tulee ottaa vastaan seuraavanlainen
parametri:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
private void LiikutaPelaajaa(AnalogState tatinTila)
{
  // Liikutellaan
}</code></p>
<p>Tatin asento saadaan selville parametrina vastaan otettavasta
AnalogState-tyyppisestä muuttujasta:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
private void LiikutaPelaajaa(AnalogState tatinTila)
{
  Vector tatinAsento = tatinTila.StateVector;
  // Tehdään jotain tatin asennolla, esim liikutetaan pelaajaa...
}</code></p>
<p>StateVector antaa siis vektorin, joka kertoo mihin suuntaan tatti
osoittaa. Vektorin X ja Y -koordinaattien arvot ovat molemmat väliltä
miinus yhdestä yhteen (-1 - 1) tatin suunnasta riippuen. Tämän vektorin
avulla voidaan esimerkiksi kertoa pelaajalle mihin suuntaan sen kuuluu
liikkua.</p>
<p><img alt="\
 Kuva 28: Yksikköympyrä." src="../src/luentomonistecsUusin_htm_54d2cbea.png" /></p>
<p>\
 \
 \</p>
<p>Tatin asennon tietyllä hetkellä saa selville myös ilman jatkuvaa tatin
kuuntelua kirjoittamalla:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Vector tatinAsento = ControllerOne.LeftThumbDirection;</code></p>
<p>Tämä palauttaa samoin vektorin tatin sen hetkisestä asennosta (X ja Y
väliltä -1, 1).</p>
<p>Myös Xbox-ohjaimen liipaisimia voidaan kuunnella. Lue lisää ohjewikistä:
<a href="https://trac.cc.jyu.fi/projects/npo/wiki/OhjaintenLisays">https://trac.cc.jyu.fi/projects/npo/wiki/OhjaintenLisays</a>.</p>
<h2>20.4 Hiiri</h2>
<h3>20.4.1 Näppäimet</h3>
<p>Hiiren näppäimiä voi kuunnella aivan samaan tapaan kuin näppäimistön ja
Xbox-ohjaimenkin.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Mouse.Listen(MouseButton.Left, ButtonState.Pressed, Ammu, "Ammu aseella.");</code></p>
<p>Tässä esimerkissä painettaessa hiiren vasenta näppäintä kutsutaan
Ammu-nimistä aliohjelmaa. Tuo aliohjelma pitää tietenkin erikseen tehdä:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
private void Ammu()
{
  /// Kirjoita tähän Ammu()-aliohjelman koodi.
}</code></p>
<h3>20.4.2 Hiiren liike</h3>
<p>Hiirellä ohjauksessa on kuitenkin usein oleellista tietää jotain
kursorin sijainnista. Hiiren kursori ei ole oletuksenä näkyvä
peliruudulla, mutta sen saa halutessaan helposti näkyviin, kun
kirjoittaa koodiin seuraavan rivin vaikkapa kentän luomisen yhteydessä:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Mouse.IsCursorVisible = true;</code></p>
<p>Hiiren paikan ruudulla saadaan vektorina kirjoittamalla:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Vector paikkaRuudulla = Mouse.PositionOnScreen;</code></p>
<p>Tämä kertoo kursorin paikan näyttökoordinaateissa, ts. origo keskellä.
Y-akseli kasvaa ylöspäin.</p>
<p>Hiiren paikan pelimaailmassa (peli- ja fysiikkaolioiden
koordinaatistossa) voi saada kirjoittamalla</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Vector paikkaKentalla = Mouse.PositionOnWorld;</code></p>
<p>Tämä kertoo kursorin paikan maailmankoordinaateissa. Origo on keskellä
ja Y-akseli kasvaa ylöspäin.</p>
<p>Hiiren liikettä voidaan kuunnella aliohjelmalla Mouse.ListenMovement.
Sille annetaan parametrina kuuntelun herkkyyttä kuvaava double,
käsittelijä sekä ohjeteksti. Näiden lisäksi voidaan antaa myös omia
parametreja. Käsittelijällä on yksi pakollinen parametri. Esimerkki
hiiren kuuntelusta:</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
private PhysicsObject pallo;</p>
<p>public override void Begin()
{
    pallo = new PhysicsObject(30.0, 30.0, Shape.Circle);
    Add(pallo);
    Mouse.IsCursorVisible = true;
    Mouse.ListenMovement(0.1, KuunteleLiiketta, null);
}</p>
<p>private void KuunteleLiiketta(AnalogState hiirenTila)
{      <br />
    pallo.X = Mouse.PositionOnWorld.X;
    pallo.Y = Mouse.PositionOnWorld.Y;</p>
<pre><code>Vector hiirenLiike = hiirenTila.MouseMovement;
</code></pre>
<p>}
```</p>
<p>Tässä esimerkissä luomamme fysiikkaolio nimeltä pallo seuraa hiiren
kursoria. Käsittelijää kutsutaan aina kun hiirtä liikuttaa.
ListenMovement:in parametreissa herkkyys (tässä 0.1) tarkoittaa sitä,
miten pieni hiiren liike aiheuttaa tapahtuman.</p>
<p>Tapahtumankäsittelijällä on pakollinen AnalogState-luokan olio
parametrina. Siitä saa myös irti tietoa hiiren liikkeistä. Tässä
esimerkissä hiirenTila.MouseMovement antaa hiiren liikevektorin, joka
kertoo mihin suuntaan ja miten voimakkaasti kursori on liikkunut (hiiren
ollessa paikoillaan se on nollavektori).</p>
<h3>20.4.3 Hiiren kuunteleminen vain tietyille peliolioille</h3>
<p>Jos hiiren painalluksia halutaan kuunnella vain tietyn peliolion (tai
fysiikkaolion) kohdalla, voidaan käyttää apuna
Mouse.ListenOn-aliohjelmaa:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Mouse.ListenOn(pallo, MouseButton.Left, ButtonState.Down, PoimiPallo, null);</code></p>
<p>Parametrina annetaan se olio, jonka päällä hiiren painalluksia halutaan
kuunnella. Muut parametrit ovat kuin normaalissa Listen-kutsussa.
Käsittelijää PoimiPallo kutsutaan tässä esimerkissä silloin, kun hiiren
kursori on pallo-nimisen olion päällä ja hiiren vasen nappi on
painettuna pohjaan.</p>
<p>Hiirellä on olemassa myös esimerkiksi seuraavanlainen metodi:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
PhysicsObject kappale = new PhysicsObject(50.0, 50.0);
bool onkoPaalla = Mouse.IsCursorOn(kappale);</code></p>
<p>Mouse.IsCursorOn palauttaa totuusarvon true tai false riippuen siitä,
onko kursori sille annetun olion (peli-, fysiikka- tai näyttöolion)
päällä.</p>
<h1>21. Piirtoalusta (Jypeli)</h1>
<p>Piirtoalustalla voidaan peliin piirtää kuvioita. Nämä kuviot ovat siis
pelissä näkyviä elementtejä, jotka eivät ole PhysicsObject- tai
GameObject-olioita, vaan ne piirretään ”erillään” peliolioista. Ne eivät
noudata fysiikan lakeja. Tällä hetkellä piirtoalustalle voi piirtää
janoja.</p>
<p>Piirtämistä varten peliluokkaan lisätään Paint-aliohjelma, joka
ylikirjoittaa (<em>override</em>) kantaluokan vastaavan aliohjelman.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
protected override void Paint(Canvas canvas)
{
  // Tässä välissä piirretään kuviot
  base.Paint(canvas);
}</code></p>
<p>Jypeli-kirjasto kutsuu Paint-aliohjelmaa tasaisin väliajoin (kymmeniä
kertoja sekunnissa) pelin ollessa käynnissä. Siinä voi siis toteuttaa
animaatioita muuttamalla koordinaatteja sen mukaan millä ajanhetkellä
piirretään.</p>
<p>Itse piirtäminen tapahtuu parametrina saatavan Canvas-olion metodeilla.
Nykyisellään niitä on yksi:</p>
<ul>
<li>DrawLine: Piirtää janan. Parametrina alku- ja loppupisteen
    koordinaatit joko vektoreina tai luettelemalla molempien pisteiden
    x- ja y-koordinaatit.</li>
</ul>
<p>Väri voidaan asettaa BrushColor-ominaisuuden kautta.</p>
<p>Piirtoalueen reunojen koordinaatteja voi lukea samaan tapaan kuin
kentänkin reunoja:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
canvas.Left               Vasemman reunan x-koordinaatti
canvas.Right       Oikean reunan x-koordinaatti
canvas.Bottom      Alareunan y-koordinaatti
canvas.Top                 Yläreunan y-koordinaatti
canvas.TopLeft     Vasen ylänurkka
canvas.TopRight    Oikea ylänurkka
canvas.BottomLeft  Vasen alanurkka
canvas.BottomRight Oikea alanurkka</code></p>
<p>Seuraavaksi esimerkkejä.</p>
<h2>21.1 Esimerkki: Punainen rasti</h2>
<p>Alla oleva esimerkki piirtää punaisen rastin Canvas-olion vasempaan
ylänurkkaan ja mustan rastin oikeaan ylänurkkaan.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
protected override void Paint(Canvas canvas)
{
  canvas.BrushColor = Color.Red;
  double x = canvas.Left + 100, y = canvas.Top - 100;
  canvas.DrawLine(new Vector(x - 50, y + 50), new Vector(x + 50, y - 50));
  canvas.DrawLine(new Vector(x + 50, y + 50), new Vector(x - 50, y - 50));</p>
<p>canvas.BrushColor = Color.Black;
  x = canvas.Right - 100;
  y = canvas.Top - 100;
  canvas.DrawLine(new Vector(x - 50, y + 50), new Vector(x + 50, y - 50));
  canvas.DrawLine(new Vector(x + 50, y + 50), new Vector(x - 50, y - 50));</p>
<p>base.Paint(canvas);
}
```</p>
<p>Alla kuva lopputuloksesta.</p>
<p><img alt="\
 Kuva 29: Punainen ja musta rasti Paint-aliohjelmalla ja Canvas-oliolla
piirrettynä." src="../src/luentomonistecsUusin_htm_m692570a4.png" /></p>
<p>\
 \</p>
<h2>21.2 Esimerkki: Pyörivä jana</h2>
<p>Seuraavassa esimerkissä tehdään satunnaisesti väriään vaihtava jana,
joka pyörii alkupisteensä ympäri.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
protected override void Paint(Canvas canvas)
{
  canvas.BrushColor = RandomGen.NextColor();
  double ajanhetki = Game.Time.SinceStartOfGame.TotalSeconds;
  Vector keskipiste = new Vector(0, 0);
  Vector reunapiste = new Vector(100 * Math.Cos(ajanhetki), 100 * Math.Sin(ajanhetki));
  canvas.DrawLine(keskipiste, keskipiste + reunapiste);
  base.Paint(canvas);
}</code></p>
<p>\
 \</p>
<p>\
 \</p>
<h1>22. Rekursio</h1>
<blockquote>
<p>“To iterate is human, to recurse divine.” -L. Peter Deutsch</p>
</blockquote>
<p>Rekursiolla tarkoitetaan algoritmia joka tarvitsee itseään
ratkaistakseen ongelman. Ohjelmoinnissa esimerkiksi aliohjelmaa, joka
kutsuu itseään, sanotaan rekursiiviseksi. Rekursiolla voidaan ratkaista
näppärästi ja pienemmällä määrällä koodia monia ongelmia, joiden
ratkaiseminen olisi muuten (esim. silmukoilla) melko työlästä.
Rakenteeltaan rekursiivinen algoritmi muistuttaa jotain seuraavaa.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
public static void Rekursio(parametrit) 
{
   if (lopetusehto) return;
   // toimenpiteitä ... 
   Rekursio(uudet parametrit);  // Itsensä kutsuminen
}</code></p>
<p>Oleellista on, että rekursiivisessa aliohjelmassa on joku <em>lopetusehto</em>.
Muutoin aliohjelma kutsuu itseään loputtomasti. Toinen oleellinen seikka
on, että seuraavan kutsun, tässä Rekursio(uudet parametrit), parametreja
jotenkin muutetaan, muutoin rekursiolla ei saada mitään järkevää
aikaiseksi.</p>
<p>Yksinkertainen esimerkki rekursioista voisi olla <em>kertoman</em> laskeminen.
Muistutuksena viiden kertoma on siis tulo 5*4*3*2*1. Tämä ei
välttämättä ole paras tapa laskea kertomaa, mutta havainnollistaa
rekursiota hyvin. Luonnollisesti laitamme mukaan myös ComTest-testit.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
/// <summary>
/// Lasketaan luvun kertoma kaavasta
/// <code>
/// 0! = 1
/// 1! = 1
/// n! = n*(n-1)!
/// </code>
/// </summary>
/// <param name="n">Minkä luvun kertoma lasketaan</param>
/// <returns>n!</returns>
/// <example>
/// <pre name="test">
/// Rekursio.Kertoma(0) === 1;
/// Rekursio.Kertoma(1) === 1;
/// Rekursio.Kertoma(5) === 120;
/// </pre>
/// </example>
public static long Kertoma(int n)
{
    if (n &lt;= 1) return 1;
    return n * Kertoma(n - 1);
}</p>
<p>/// <summary>
/// Pääohjelma
/// </summary>
public static void Main()
{
    long k = Kertoma(5);
    Console.WriteLine(k);
    Console.ReadLine();
}
```</p>
<p>Aliohjelma <code>Kertoma</code> saa parametrikseen luvun, jonka kertoma halutaan
laskea. Tutustutaan aliohjelmaan tarkemmin.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
if (n &lt;= 1) return 1;</code></p>
<p>Yllä oleva rivi on ikään kuin rekursion lopetusehto. Jos n on pienempää
tai yhtä suurta kuin 1, niin palautetaan luku 1. Oleellista on, että
lopetusehto on ennen uutta rekursiivista aliohjelmakutsua.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
return n * Kertoma(n-1);</code></p>
<p>Tällä rivillä tehdään nyt tuo rekursiivinen kutsu eli aliohjelma kutsuu
itseään. Yllä oleva rivi onkin oikeastaan tuttu matematiikasta:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
n! = n * (n-1)!</code></p>
<p>Siinä palautetaan siis n kerrottuna n-1 kertomalla. Esimerkiksi luvun
viisi kertoman laskemista yllä olevalla aliohjelmalla voisi
havainnollistaa seuraavasti.</p>
<p><img alt="\
 Kuva 30: Kertoman laskeminen rekursiivisesti. Vaiheet
numeroitu." src="../src/luentomonistecsUusin_htm_7fd89c6a.gif" /></p>
<p>\
 Tulosta voidaan lähteä ”kasaamaan” lopusta alkuun päin. Nyt Kertoma(1)
palauttaa siis luvun 1 ja samalla lopettaa rekursiivisten kutsujen
tekemisen. Kertoma(2) taas palauttaa 2 * Kertoma(1) eli 2 * 1 eli
luvun 2. Nyt taas Kertoma(3) palauttaa 3 * Kertoma(2) eli 3 * 2 ja
niin edelleen. Lopulta Kertoma(5) palauttaa 5 * Kertoma(4) eli 5 * 24
= 120. Näin on saatu laskettua viiden kertoma rekursiivisesti. [LIA]</p>
<h2>22.1 Sierpinskin kolmio</h2>
<p>Sierpinskin kolmio on puolalaisen matemaatikko <em>Wacław Sierpińskin</em>
vuonna 1915 esittelemä <em>fraktaali</em>. Se on tasasivuinen kolmio, jonka
ympärille piirretään kolme uutta tasasivuista kolmiota niin, että kunkin
uuden kolmion jokin kärki on edellisen (suuremman) kolmion sivun
keskipisteessä. Kunkin uuden kolmion koko on puolet suuremmasta
kolmiosta. Uudet kolmiot muodostuvat siis ”ison” kolmion yläosaan,
vasempaan alakulmaan ja oikeaan alakulmaan. Tilanne selviää paremmin
kuvasta. Sierpinskin kolmion toinen vaihe on alla. Kolmion viivojen
piirtämiseen käytämme Canvas-oliota (ks. luku 21).</p>
<p><img alt="\
 Kuva 31: Sierpinskin kolmion toisessa vaiheessa ensimmäisen kolmion
ympärille on piirretty kolme uutta
kolmiota." src="../src/luentomonistecsUusin_htm_m2a659b.png" /></p>
<p>\
 sekä ”lopputulos”, missä pienimpiä kolmioita on jo hyvin vaikea erottaa
toisistaan.</p>
<p><img alt="\
 Kuva 32: Valmis Sierpinskin
kolmio." src="../src/luentomonistecsUusin_htm_m5638d5c8.png" /></p>
<p>\
 Sierpinskin kolmion piirtäminen onnistuu loistavasti rekursiolla, mutta
ilman rekursiota kolmion piirtäminen olisi melko työlästä. Sierpinskin
kolmiosta voi lukea lisää esim. Wikipediasta:
<a href="http://en.wikipedia.org/wiki/Sierpinski_triangle">http://en.wikipedia.org/wiki/Sierpinski_triangle</a>.</p>
<p>Kirjoitetaan algoritmi pseudokoodiksi:</p>
<p><em>Pseudokoodi</em> = Ohjelmointikieltä muistuttavaa koodia, jonka tarkoitus
on piilottaa eri ohjelmointikielten syntaksierot ja jättää jäljelle
algoritmin perusrakenne. Algoritmia suunniteltaessa voi olla helpompaa
hahmotella ongelmaa ensiksi pseudokielisenä, ennen kuin kirjoittaa
varsinaisen ohjelman. Pseudokoodille ei ole mitään standardia, vaan
jokainen voi kirjoittaa sitä omalla tavallaan. Järkevintä kuitenkin
kirjoittaa niin, että mahdollisimman moni ymmärtäisi sitä.</p>
<p>\</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
PiirraSierpinskinKolmio(korkeus, x, y) // x ja y tarkoittavat kärjellään
                                       // seisovan kolmion alakulman koordinaatteja
{
   jos (korkeus &lt; PIENIN_SALLITTU_KORKEUS) poistu</p>
<p>sivunPituus2 = korkeus / sqrt(3) // Sivun pituus jaettuna kahdella
   alakulma = (x, y) // Pistepari
   vasenYlakulma = (x - sivunPituus2, y + korkeus)
   oikeaYlakulma = (x + sivunpituus2, y + korkeus)</p>
<p>PiirraViiva(alakulma, vasenYlakulma) // Viiva alakulmasta vasempaan yläkulmaan
   PiirraViiva(vasenYlakulma, oikeaYlakulma)  // Vastaavasti ...
   PiirraViiva(oikeaYlakulma, alakulma)</p>
<p>PiirraSierpinskinKolmio(korkeus / 2, x - sivunPituus2, y)
   PiirraSierpinskinKolmio(korkeus / 2, x + sivunPituus2, y)
   PiirraSierpinskinKolmio(korkeus / 2, x, y + korkeus)
}
```</p>
<p>Tämä muistuttaa jo paljon oikeaa koodia. Käytetään seuraavaksi oikeaa
koodia.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
using System;
using Jypeli;</p>
<p>/// <summary>
/// Sierpinskin kolmio
/// </summary>
public class Sierpinski : Game
{
    private static double pieninKorkeus = 10.0;</p>
<pre><code>public override void Begin()
{
    Level.Background.Color = Color.White;
}

protected override void Paint(Canvas canvas)
{
    base.Paint(canvas);
    double korkeus = 300;
    SierpinskinKolmio(canvas, 0, -korkeus, korkeus);
}

/// &lt;summary&gt;
/// Piirtää Sierpinskin kolmion.
/// &lt;/summary&gt;
/// &lt;param name="canvas"&gt;Piirtoalusta&lt;/param&gt;
/// &lt;param name="x"&gt;Alareunan x&lt;/param&gt;
/// &lt;param name="y"&gt;Alareunan y&lt;/param&gt;
/// &lt;param name="h"&gt;Korkeus&lt;/param&gt;
public static void SierpinskinKolmio(Canvas canvas, double x, double y, double h)
{
    if (h &lt; pieninKorkeus) return;

    double s2 = h / (Math.Sqrt(3)); // sivun pituus s/2
    Vector p1 = new Vector(x, y);
    Vector p2 = new Vector(x - s2, y + h);
    Vector p3 = new Vector(x + s2, y + h);
    canvas.DrawLine(p1, p2);
    canvas.DrawLine(p2, p3);
    canvas.DrawLine(p3, p1);

    SierpinskinKolmio(canvas, x - s2, y, h / 2);
    SierpinskinKolmio(canvas, x + s2, y, h / 2);
    SierpinskinKolmio(canvas, x, y + h, h / 2);
}
</code></pre>
<p>}
```</p>
<p>Tarkastellaan ohjelman tiettyjä osia hieman tarkemmin.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
private static double pieninKorkeus = 10.0;</code></p>
<p>Attribuuttina määritellään muuttuja, jolla kontrolloidaan kuinka kauan
rekursiota jatketaan. Muuttuja pieninKorkeus näkyy siis kaikkialla
luokassa Sierpinski. pieninKorkeus on määritelty ”globaaliksi” sillä
perusteella, ettei muuttujan alustus toistuisi loputtomasti. Tässä
ohjelmassa voidaan nimittäin suorittaa aliohjelma SierpinskinKolmio
todella monta kertaa, riippuen muuttujan pieninKorkeus arvosta.</p>
<p>Yllä oleva muuttuja voisi olla myös vakio. Tämän kyseisen ohjelman
tapauksessa se voisi olla jopa perusteltua. Kuitenkin, on myöskin
perusteltua olettaa, että ohjelmamme kehittyessä pienimmän kolmion
korkeutta olisi mahdollista muuttaa vaikkapa käyttäjän toimesta, ja
silloin pieninKorkeus ei olisikaan enää vakio, vaan ohjelman ajon aikana
muuttuva luku.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
protected override void Paint(Canvas canvas)
{
  base.Paint(canvas);
  double korkeus = 300;
  SierpinskinKolmio(canvas, 0, -korkeus, korkeus);
}</code></p>
<p>Paint-aliohjelmassa määrittelemme ensimmäisenä piirrettävän, eli
suurimman, kolmion korkeuden. Sen jälkeen kutsumme
SierpinskinKolmio-aliohjelmaa, jolle välitämme parametrina canvas-olion,
johon kolmioita piirretään, ja kolmion paikan (0, -korkeus) sekä
tietenkin korkeuden.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
public static void SierpinskinKolmio(Canvas canvas, double x, double y, double h)</code></p>
<p>Aliohjelma SierpinskinKolmio on staattinen, sillä sen suorittamiseksi
riittää parametrina tulevat tiedot. Se on myös void-tyyppinen, koska
emme odota sen palauttavan mitään. Aliohjelma saa neljä parametria:
piirtoalusta, johon kolmio piirretään, kolmion alimman pisteen x- ja
y-koordinaatit, sekä kolmion korkeuden. Nämä parametrit riittävät
tasasivuisen kolmion piirtämiseen Canvas-olion avulla.</p>
<p>Sivuutetaan hetkeksi if-rakenne ja tarkastellaan if-lauseen jälkeen
tulevia lauseita.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
double s2 = h / (Math.Sqrt(3)); // sivun pituus s/2</code></p>
<p>Ennen kuin voimme piirtää kolmiot, meidän on selvitettävä, mitkä ovat
kolmion sivujen pituudet. Tasasivuisen kolmion kaikki sivut ovat yhtä
pitkiä, joten yhden sivun pituuden laskeminen riittää meille! Käytämme
vanhaa kunnon Pythagoraan lausetta. Olkoon h kolmion korkeus ja s sivun
pituus.</p>
<p><img alt="" src="../src/luentomonistecsUusin_htm_51840fcf.png" />\
 Koska x-akselilla siirrymme kolmion alimmasta kärjestä puolen sivun
mitan verran joko vasemmalle tai oikealle, on mielekästä jakaa sivun
pituus s vielä kahdella, jotta laskut hieman helpottuvat jatkossa.</p>
<p><img alt="" src="../src/luentomonistecsUusin_htm_453c7491.png" />\
 Tämä tulos on tallennetaan s2-muuttujaan.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Vector p1 = new Vector(x, y);
Vector p2 = new Vector(x - s2, y + h);
Vector p3 = new Vector(x + s2, y + h);</code></p>
<p>Yllä lasketaan kolmion kärkipisteiden paikat edellä laskettua sivun
pituutta hyväksi käyttäen. Allaoleva kuva selventää vielä pisteiden
laskemista.</p>
<p><img alt="\
 Kuva 33: Kolmion pisteiden
laskeminen." src="../src/luentomonistecsUusin_htm_m12b38605.png" /></p>
<p>\
 Piirretään sitten yksi kolmio.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
canvas.DrawLine(p1, p2);
canvas.DrawLine(p2, p3);
canvas.DrawLine(p3, p1);</code></p>
<p>Yllä olevat rivit piirtävät yhden kolmion hyödyntäen laskettuja
kärkipisteiden koordinaatteja.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
SierpinskinKolmio(canvas, x - s2, y, h / 2); // Vasen alakolmio
SierpinskinKolmio(canvas, x + s2, y, h / 2); // Oikea alakolmio
SierpinskinKolmio(canvas, x, y + h, h / 2); // Yläkolmio</code></p>
<p>Kutsutaan tehtyä aliohjelmaa kolmesti, jolloin alkuperäisen kolmion
koordinaattien ja koon perusteella piirretään kolme pienempää kolmiota:
alkuperäisen kolmion vasemmalle, oikealle ja yläpuolelle.</p>
<p>Otetaan hetkeksi askel taaksepäin, ja tarkastellaan, milloin rekursiosta
poistutaan.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
if (h &lt; pieninKorkeus) return;</code></p>
<p>Aliohjelmaan tultaessa saatiin parametrina korkeus, h-muuttuja. Mikäli
h:n arvo alittaa annetun pienimmän korkeuden, poistutaan välittömästi
return-lauseella. Tällöin h:ta pienempiä kolmioita ei enää piirretä.
Toisaalta, mikäli kolmion korkeus h <em>ei alita</em> annettua minimiä, niin
silloin piirrellään, kuten aiemmin käytiin läpi.</p>
<p>Olennaista tässä on huomata, että niin kauan, kuin korkeus h on
<em>enemmän</em> kuin annettu kolmion minimikorkeus, emme pääse ensimmäistä
SierpinskinKolmio-aliohjelmakutsua ”pidemmälle”. Kullakin kutsukerralla
näet korkeus h puolittuu, joten vasta h:n ollessa riittävän pieni
lopetusehto toteutuu. Rekursion idean mukaisesti vasta sitten etenemme
seuraaviin SierpinskinKolmio-kutsuihin (kaksi jälkimmäistä).</p>
<p>\</p>
<h2>22.2 Harjoitus</h2>
<p>Montako kertaa tässä esimerkissä lopulta suoritetaan aliohjelma
SierpinskinKolmio?</p>
<h1>23. Dynaamiset tietorakenteet</h1>
<p>Taulukot tarjoavat meille vielä hyvin rajalliset puitteet ohjelmointiin.
Mietitäänpä vaikka tilanne, jossa meidän tarvitsisi laskea käyttäjän
syöttämiä lukuja yhteen Käyttäjä saisi syöttää niin monta lukua kuin
haluaa ja lopuksi painaa enter, jolloin meidän täytyisi laskea ja
tulostaa näytölle käyttäjän syöttämien lukujen summan. Minne talletamme
käyttäjän syöttämät luvut? Taulukkoon? Minkä kokoisen taulukon luomme?
10 alkiota? 100? vai jopa 1000? Vaikka tekisimme kuinka ison taulukon,
aina käyttäjä voi teoriassa syöttää enemmän lukuja ja luvut eivät mahdu
taulukkoon. Toisaalta jos teemme 1000 kokoisen taulukon ja käyttäjä
syöttääkin vain muutaman luvun, varaamme kohtuuttomasti koneen muistia.
Tällaisia tilanteita varten C#:ssä on dynaamisia tietorakenteita, eli
kokoelmia. Niiden koko kasvaa sitä mukaan kun alkioita lisätään.
Dynaamisia tietorakenteita ovat muun muassa listat, puut, vektorit,
pinot ym. Niiden käyttäminen ja rakenne eroaa huomattavasti toisistaan.</p>
<h2>23.1 Rajapinnat</h2>
<p>C#:ssa on olemassa <em>rajapintoja</em> (interface), joissa määritellään
tietyt metodit, ja kaikkien luokkien, jotka toteuttavat (implement)
tämän rajapinnan, täytyy sisältää samat metodit. Rajapintojen hienous on
siinä, että voimme käyttää samoja metodeja kaikkiin niihin olioihin,
jotka toteuttavat saman rajapinnan. Meillä voisi olla, vaikka rajapinta
Muodot. Nyt voisimme tehdä luokat Ympyra, Kolmio ja Suorakulmio, jotka
kaikki toteuttaisivat Muodot-rajapinnan. Voisimme nyt luoda esimerkiksi
Muodot-tyyppisen taulukon, johon voisi nyt tallentaa kaikkia
Muodot-rajapinnan toteutettavien luokkien olioita. Jos
Muodot-rajapinnassa olisi määritelty metodi Varita(), voisimme värittää
silmukassa kerralla taulukollisen ympyröitä, kolmioita ja suorakulmioita
samalla metodilla.</p>
<p>Kokoelmat ovat olio-ohjelmoinnin taulukoita. Generics-kokoelmaluokat
(System.Collections.Generics-nimiavaruus) ovat tyyppiturvallisia, toisin
sanoen kokoelman jäsenten (ja mahdollisen avaimen) tyyppi voidaan
määritellä. System.Collections.ObjectModel-nimiavaruudessa on geneerisiä
kantaluokkia omien kokoelmien toteuttamiseen sekä ”wrappereitä” (ns.
kääreluokkia), joilla voidaan esimerkiksi tehdä read-only-kokoelmia.</p>
<p>Valmiita tietorakenteita on C#:ssa melko paljon, joten ennen oman
tietorakenteen tekemistä kannattaa tutustua niihin. Tässä luvussa
tutustumme lähinnä geneeriseen listaan (List\&lt;T>). Oman tietorakenteen
tekeminen onkin jo sitten Ohjelmointi 2-kurssin asiaa.</p>
<h2>23.2 Listat (List\&lt;T>)</h2>
<p>Tutustutaan seuraavaksi yhteen C#:n dynaamisista tietorakenteista,
List\&lt;T>-luokkaan, joka on geneerinen tietorakenne. List\&lt;T>
muistuttaa jonkin verran taulukkoa; taulukoilla ja listoilla on paljon
yhteistä:</p>
<ul>
<li>
<p>Niissä voi olla vain yhden tyyppisiä alkioita</p>
</li>
<li>
<p>Yksittäiseen alkioon päästään käsiksi laittamalla alkion
    paikkaindeksi hakasulkujen sisään, esimerkiksi luvut[15, 14], tai
    pallot[4].</p>
</li>
<li>
<p>Molemmilla on metodeja (funktioita, aliohjelmia) sekä ominaisuuksia</p>
</li>
</ul>
<p>List\&lt;T>-olioon ja muihin dynaamisiin tietorakenteisiin voi tallentaa
niin alkeistietotyyppejä kuin oliotietotyyppejäkin. Käsittelemämme
<em>geneerinen lista</em> vaatii <em>aina</em> tiedon siitä, minkä tyyppisiä alkioita
tietorakenteeseen laitetaan. Muun tyyppisiä alkioita listaan ei voi
laittaa.</p>
<p>Tietotyyppi laitetaan tietorakenneluokan jälkeen kulmasulkujen sisään -
tästä esimerkki seuraavaksi.</p>
<h3>23.2.1 Tietorakenteen määrittäminen</h3>
<p>Dynaamisen tietorakenteen määrittämisen syntaksi poikkeaa hieman
tavallisen olion määrittelystä. Ehdit jo varmaan ihmetellä, mikä on
kulmasulkeissa oleva T List-sanan jälkeen. Kyseinen T tarkoittaa listaan
talletettavien alkioiden tyyppiä. Tyyppi voi olla alkeistietotyyppi tai
oliotyyppi. Yleisessä muodossa uuden listan määrittely menee
seuraavasti:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
TietorakenneLuokanNimi&lt;TallettavienOlioidenTyyppi&gt; rakenteenNimi =
  new TietorakenneLuokanNimi&lt;TallettavienOlioidenTyyppi&gt;();</code></p>
<p>Voisimme esimerkiksi tallettaa elokuvien nimiä seuraavaan
List\&lt;String>-rakenteeseen. Määritellään uusi (tyhjä) lista
seuraavasti.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
List&lt;String&gt; elokuvat = new List&lt;String&gt;();</code></p>
<h3>23.2.2 Alkioiden lisääminen ja poistaminen</h3>
<p>Alkioiden lisääminen List\&lt;T>-olioon, ja itse asiassa kaikkiin
Collections.Generic-nimiavaruuden luokkien olioihin, onnistuu
Add-metodilla. Add-metodi lisää alkion aina tietorakenteen ”loppuun”,
eli loogisessa mielessä viimeiseksi. Kun indeksointi alkaa jälleen
nollasta, niin ensimmäinen lisätty alkio löytyy siis indeksistä 0,
seuraava 1 jne. Elokuvia voitaisiin nyt lisätä seuraavasti:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
elokuvat.Add("Casablanca");
elokuvat.Add("Star Wars");
elokuvat.Add("Toy Story");</code></p>
<p>Alkion poistaminen halutusta paikasta (indeksistä) tehdään
RemoveAt-metodilla. Parametriksi annetaan sen alkion indeksi, joka
halutaan poistaa. Alkion "Casablanca" poistaminen onnistuisi
seuraavasti.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
elokuvat.RemoveAt(0);</code></p>
<p>Koska rakenne on dynaaminen, muuttuu listan alkioiden järjestys
lennosta. Nyt "Star Wars"-merkkijono löytyisi indeksistä 0. Poistaa voi
myös suoraan alkion sisällöllä.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
elokuvat.Remove("Star Wars");</code></p>
<p>Remove-metodi toimii siten, että se poistaa listasta ensimmäisen
esiintymän, joka vastaa annettua parametria. Metodi palauttaa true,
mikäli listasta poistettiin alkio. Vastaavasti palautetaan false, mikäli
annettua parametria vastaavaa alkiota ei löytynyt, jolloin listasta ei
poistettu mitään.</p>
<p>Tietorakenteen koon, tai oikeammin sanottuna tietorakenteen sisältämien
alkioiden lukumäärän, tietää olion Count-ominaisuus.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Console.WriteLine(elokuvat.Count); //tulostaa 1
elokuvat.Add("Full Metal Jacket");
Console.WriteLine(elokuvat.Count); //tulostaa 2</code></p>
<p>Tiettyyn alkioon pääsee käsiksi taulukon tapaan, eli laittamalla haluttu
paikkaindeksi hakasulkeiden sisään. Ensimmäisen alkion voisi tulostaa
esimerkiksi seuraavaksi:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Console.WriteLine(elokuvat[0]); // tulostaa "Toy Story"</code></p>
<p>Näillä metodeilla pärjää jo melko hyvin. Muista metodeista voi lukea
List\&lt;T>-luokan dokumentaatiosta:
<a href="http://msdn.microsoft.com/en-us/library/6sh2ey19.aspx">http://msdn.microsoft.com/en-us/library/6sh2ey19.aspx</a>.</p>
<p>Tehdään toinen esimerkki int-tyyppisillä luvuilla. Annetaan listalle
sisältö heti listaa alustettaessa.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
List&lt;int&gt; luvut = new List&lt;int&gt;() { 3, 3, 1, 7, 3, 5, 7 };</code></p>
<p>Huomaa, että edellä olevaan listaan ei voi tallentaa muita kuin
int-tyyppisiä kokonaislukuja.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
luvut.Add(5.3); // Kääntäjä ilmoittaa virheestä!</code></p>
<p>Yllä oleva esimerkki osoittaa, että näiden <em>vahvasti tyypitettyjen</em>
tietorakenteiden käyttö on myös turvallista - tietorakenteeseen ei voi
”vahingossa” laittaa väärän tyyppisiä alkioita, joka saattaisi sitten
myöhemmin aiheuttaa vakavia ongelmia.</p>
<p>Tarkistetaan vielä listan sisältämien alkioiden lukumäärä.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Console.WriteLine(luvut.Count); // Tulostaa 7</code></p>
<p>Poistetaan sitten kaikki ne alkiot, joiden arvo on 3. Tässä voimme
käyttää tehokkaasti hyväksemme while-silmukkaa ja Remove-funktiota.
Remove-funktion totuusarvotyyppinen paluuarvo käy hyvin while-ehdoksi.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
While (luvut.Remove(3));</code></p>
<p>Listan alkiot näyttävät tämän jälkeen seuraavalta.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
1, 7, 5, 7</code></p>
<p>\
 \</p>
<p>\
 \</p>
<h1>24. Poikkeukset</h1>
<blockquote>
<p>“If you don’t handle [exceptions], we shut your application down. 
That dramatically increases the reliability of the system.”\
 - Anders Hejlsberg</p>
</blockquote>
<p>Poikkeus (exception) on ohjelman suorituksen aikana ilmenevä ongelma.
Jos poikkeusta ei käsitellä, ohjelman suoritus yleensä kaatuu ja
konsoliin tulostetaan jokin virheilmoitus. Tässä vaiheessa kurssia näin
on varmasti käynyt jo monta kertaa. Poikkeus voi tapahtua jos
esimerkiksi yritämme viitata taulukon alkioon, jota ei ole olemassa.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
int[] taulukko = new int[5];
taulukko[5] = 5;</code></p>
<p>Esimerkiksi yllä oleva koodinpätkä aiheuttaisi
IndexOutOfRangeException-nimisen poikkeuksen. Näitä poikkeuksia tulee
aluksi usein silloin, kun taulukoita käsitellään silmukoiden avulla ja
silmukan lopetusehto on väärin. Poikkeuksia aiheuttavat myös esimerkiksi
jonkun luvun jakaminen nollalla, sekä yritys muuttaa tekstiä sisältävä
merkkijono joksikin numeeriseksi tietotyypiksi.</p>
<p>Poikkeuksia voidaan kuitenkin käsitellä hallitusti poikkeustenhallinnan
(exception handling) avulla. Tällöin poikkeukseen varaudutaan ja
ohjelman suoritusta voidaan jatkaa poikkeuksen sattuessa. Poikkeusten
hallinta sisältää aina try- ja catch-lohkon. Lisäksi voidaan käyttää
myös finally-lohkoa.</p>
<p>C#:n poikkeukset ovat olioita. [VES][KOS][DEI]</p>
<h2>24.1 try-catch</h2>
<p>Ideana try-catch -rakenteessa on, että poikkeusalttiit lauseet
sijoitetaan try-lohkon sisään. Tämän jälkeen catch-lohkossa kerrotaan
mitä poikkeustilanteessa tehdään. Ennen catch-lohkoa täytyy kuitenkin
kertoa mitä poikkeuksia yritetään ottaa kiinni (catch). Tämä ilmoitetaan
sulkeissa catch-sanan jälkeen, ennen catch-lohkoa aloittavaa
aaltosulkua. Yleisessä muodossa try-catch rakenne olisi seuraava:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
try 
{
   //jotain lauseita mitä koitetaan suorittaa
} 
catch (PoikkeusLuokanNimi poikkeukselleAnnettavaNimi) 
{
   //jotain toimenpiteitä mitä tehdään kun poikkeus ilmenee
}</code></p>
<p>catch-lohkoon mennään vain siinä tapauksessa, että try-lohko aiheuttaa
sen tietyn poikkeuksen, jota catch-osassa ilmoitetaan otettavan kiinni.
Muissa tapauksissa catch-lohko ohitetaan. Jos try-lohkossa on useita
lauseita, catch-lohkoon mennään heti ensimmäisen poikkeuksen sattuessa,
eikä loppuja lauseita enää suoriteta. Otetaan esimerkiksi nollalla
jakaminen. Nollalla jako aiheuttaisi DivideByZeroException-poikkeuksen.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
int n1 = 7, n2 = 0, n3 = 4;</p>
<p>try 
{
   Console.WriteLine("{0}", 10 / n1);
   Console.WriteLine("{0}", 10 / n2);
   Console.WriteLine("{0}", 10 / n3);
} 
catch (DivideByZeroException e) 
{
   Console.WriteLine("Nollalla jako: " + e.Message);
}
```</p>
<p>Yllä olevassa esimerkissä keskimmäinen tulostus aiheuttaisi
DivideByZeroException-poikkeuksen ja tällöin siirryttäisiin välittömästi
catch-lohkoon. Kolmesta try-lohkossa olevasta tulostusrivistä tulostuisi
siis vain ensimmäinen. Jos haluaisimme, että kaikki lauseet, jotka eivät
heitä poikkeusta suoritettaisiin, täytyisi meidän tehdä jokaiselle
lauseelle oma try-catch -rakenteensa. Tällöin saisimme aikaan melkoisen
try-catch -viidakon. Useimmiten tällaisissa tilanteissa olisikin
järkevää tehdä suoritettavasta toimenpiteestä aliohjelma, joka
sisältäisi try-catch -rakenteen. Tällöin koodi siistiytyisi ja lyhenisi
huomattavasti.</p>
<p>Esimerkissämme catch-lohkossa tulostetaan nyt virheilmoitus.
Poikkeusolio on nimetty "e":ksi, joka on hyvin yleinen poikkeusolion
viitemuuttujalle annettava nimi. Koska C#:n poikkeukset olivat olioita,
on niillä myös joukko metodeja ja ominaisuuksia. catch-lohkossa on
kutsuttu DivideByZeroException-luokan Message-ominaisuutta, joka
sisältää poikkeukselle määritellyn virheilmoituksen, jonka siis
tulostamme tässä konsoli-ikkunaan.</p>
<p>Voidaan määritellä myös useita catch-lohkoja, jolloin voimme ottaa
kiinni monia erityyppisiä poikkeuksia.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
try 
{
   //jotain lauseita mitä koitetaan suorittaa
} 
catch (PoikkeusTyyppiA e) 
{
   //jotain toimenpiteitä mitä tehdään kun poikkeus ilmenee
} 
catch (PoikkeusTyyppiB e) 
{
   //jotain toimenpiteitä mitä tehdään kun poikkeus ilmenee
} 
catch (PoikkeusTyyppiC e) 
{
   //jotain toimenpiteitä mitä tehdään kun poikkeus ilmenee
}</code></p>
<p>Jos poikkeustapauksessa tehtävät toimenpiteet eivät vaihtele riippuen
poikkeuksen tyypistä, voimme ottaa kiinni yksinkertaisesti
Exception-luokan olioita. Kaikki C#:n poikkeusluokat perivät
Exception-luokan, joten sitä käyttämällä saamme kiinni kaikki
mahdolliset poikkeukset. Joskus voi olla järkevää laittaa viimeinen
catch-lohko nappaamaan Exception-poikkeuksia, jolloin saamme kaikki
loputkin mahdolliset poikkeukset kiinni. Monesti kuitenkin tiedämme
hyvin tarkkaan, mitä poikkeuksia toimenpiteemme voivat aiheuttaa, joten
tämä olisi turhaa. Ja jos emme tiedä mitään poikkeuksesta, emme sitä
osaa käsitelläkään ja siksi Exception-luokan poikkeuksen
kiinniottamisessa on oltava todella varovainen. [VES][KOS][DEI]</p>
<h2>24.2 finally-lohko</h2>
<p>finally-lohkon käyttäminen ei ole pakollista, mutta kun sitä käytetään,
kirjoitetaan se catch-lohkojen jälkeen. Mikäli finally-lohko
kirjoitetaan mukaan, suoritetaan se joka tapauksessa riippumatta siitä
aiheuttiko try-lohko poikkeuksia.</p>
<p>finally-lohko on hyödyllinen muun muassa käsiteltäessä tiedostoja,
jolloin tiedosto on suljettava aina käsittelyn jälkeen poikkeuksista
riippumatta. finally-lohkon sisältävä try-catch -rakenne olisi yleisessä
muodossa seuraava:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
try 
{
   //jotain lauseita mitä koitetaan suorittaa
} 
catch (PoikkeusLuokanNimi poikkeukselleAnnettavaNimi) 
{
   //jotain toimenpiteitä mitä tehdään kun poikkeus ilmenee
} 
finally 
{
   //joka tapauksessa suoritettavat lauseet
}</code></p>
<h2>24.3 Yleistä</h2>
<p>Poikkeukset ovat nimensä mukaan säännöstä poikkeavia tapahtumia. Niitä
ei tulisikaan käyttää periaatteella: "En ole varma toimiiko tämä, joten
laitan try-catch-rakenteen sisään." Poikkeukset ovat sitä varten, että
hyvinkin suunnitellussa ja mietityssä koodissa voi joskus tapahtua
jotain odottamatonta, johon varautuminen voi parhaimmillaan pitää
lentokoneen kurssissa tai hätäkeskuspäivystyksen tietojärjestelmän
pystyssä.</p>
<h1>25. Tietojen lukeminen ulkoisesta lähteestä</h1>
<p>Muuttujat toimivat tiedon talletuksessa niin kauan, kun ohjelma on
käynnissä. Ohjelman suorituksen loputtua muuttujien muistipaikat
luovutetaan kuitenkin muiden prosessien käyttöön. Tämän takia muuttujat
eivät sovellu sellaisen tiedon talletukseen, jonka pitäisi säilyä kun
ohjelma suljetaan. Pitkäaikaiseen tiedon talletukseen soveltuvat hyvin
tiedostot ja tietokannat. Tiedostot ovat yksinkertaisempia ja ehkä
helpompia käyttää, kun taas tietokannat tarjoavat paljon monipuolisempia
ominaisuuksia. Tiedostoihin voidaan tallentaa myös esimerkiksi jotain
ohjelman tarvitsemia alkuasetuksia. Tässä luvussa selvitetään
yksinkertaisten esimerkkien avulla tiedon lukeminen tiedostosta, sekä
tiedon hakeminen WWW:stä.</p>
<p>Tutkitaan seuraavaksi esimerkkiä tekstin lukemisesta tiedostosta
Windows-ympäristössä. Muuntyyppisten tiedostojen lukeminen, tiedostoon
kirjoittaminen, sekä toiminta Windows Phone 7- ja Xbox-ympäristöissä
jätetään tämän kurssin osaamistavoitteiden ulkopuolelle.</p>
<h2>25.1 Tekstin lukeminen tiedostosta</h2>
<p>System.IO-nimiavaruus sisältää muun muassa tiedostojen käsittelyyn
tarvittavia aliohjelmia. Seuraavassa esimerkissä luetaan
tekstitiedostosta tietoa, sekä kirjoitetaan tiedostoon.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Kalle, 5
Pekka, 10
Janne, 0
Irmeli, 15</code></p>
<p>Näiden voidaan ajatella olevan vaikka topten-listan pisteitä. Annetaan
tiedoston nimeksi data.txt.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;</p>
<p>/// <summary>
/// Harjoitellaan tiedostoon kirjoittamista
/// sekä tiedostosta lukua.
/// </summary>
public class TiedostostaLuku
{
    /// <summary>
    /// Luetaan ja kirjoitetaan tekstitiedostosta.
    /// </summary>
    public static void Main()
    {
        // Määritellään tiedostopolku vakioksi
        const string POLKU = @"C:\MyTemp\data.txt";</p>
<pre><code>    // Jos tiedostoa ei ole olemassa, luodaan se ja kirjoitetaan sinne
    if (!File.Exists(POLKU))
    {
        // Taulukon alkiot ovat tiedoston rivejä
        string[] uudetRivit = { "A-J, 0", "Pekka, 0", "Kalle, 0" };
        File.WriteAllLines(POLKU, uudetRivit);
    }

    // Tämä teksti lisätään jokaisella ajokerralla,
    // jolloin tiedosto pitenee aina yhdellä rivillä
    string appendText = "Tämä on ylimääräinen rivi" + Environment.NewLine;
    File.AppendAllText(POLKU, appendText);

    // Avataan tiedosto ja kirjoitetaan sen sisältö ruudulle
    string[] luetutRivit = File.ReadAllLines(POLKU);
    foreach (string s in luetutRivit)
    {
        Console.WriteLine(s);
    }
}
</code></pre>
<p>}
```</p>
<p>\
 \</p>
<p>Tutkitaan tärkeimpiä kohtia hieman tarkemmin</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
if (!File.Exists(POLKU))</code></p>
<p>Tarkistetaan, onko tiedostoa olemassa. Mikäli ei ole, kirjoitetaan polun
osoittamaan tiedostoon muutama nimi, ja nimien perään pilkku, sekä
”pistemäärä”.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
File.WriteAllLines(POLKU, uudetRivit);</code></p>
<p>File-luokka sisältää WriteAllLines-metodin, joka kirjoittaa kaikki
String-taulukon alkiot annettuun tiedostoon.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
File.AppendAllText(POLKU, appendText);</code></p>
<p>AppendAllText-metodi lisää annettuun tiedostoon String-olion sisältämän
tekstin siten, että teksti tulee tiedoston loppuun.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
string[] luetutRivit = File.ReadAllLines(POLKU);</code></p>
<p>ReadAllLines-metodi lukee annetusta polusta kaikki rivit
String-taulukkoon. Yksi tiedoston rivi vastaa yhtä taulukon alkiota.</p>
<p>Huomaa, että mikäli tiedostolle ei ole annettu absoluuttista polkua,
ohjelma hakee tiedostoa suhteessa siihen kansioon, missä ajettava
exe-tiedosto on. Tässä esimerkissä annettiin tiedoston absoluuttinen
polku kokonaisuudessaan.</p>
<h2>25.2 Tekstin lukeminen netistä</h2>
<p>Luetaan seuraavaksi tietoja netistä. Tässä koko HTML-sivun data
tallennetaan rivi kerrallaan List\&lt;String>-tietorakenteeseen ilman sen
kummempaa jatkokäsittelyä. Lopuksi listan sisältö tulostetaan ruudulle
rivi kerrallaan.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
using System;
using System.Collections.Generic;
using System.IO;
using System.Net;
using System.Text;</p>
<p>/// @author  Antti-Jussi Lakanen
/// @version 22.8.2012
///
/// <summary>
/// Tulostetaan kayttajan antaman www-osoitteen
/// sisältö (GET-pyynnön palauttama HTML-koodi).
/// </summary>
public class TiedotNetista
{
    /// <summary>
    /// Paaohjelmassa haetaan tiedot netistä listaan ja tulostetaan listan sisältö.
    /// </summary>
    public static void Main()
    {
        List<String> lista = new List<string>();
        LueNetistaListaan("https://www.jyu.fi", lista);</p>
<pre><code>    foreach (String rivi in lista)
    {
        Console.WriteLine(rivi);
    }
    Console.ReadKey();
}

/// &lt;summary&gt;
/// Luetaan annetun url-osoitteen koko html-koodi
/// ja laitetaan rivi kerrallaan String-listaan.
/// &lt;/summary&gt;
/// &lt;param name="url"&gt;URL-osoite, mika halutaan lukea.&lt;/param&gt;
/// &lt;param name="lista"&gt;Lista, johon riveja kirjoitetaan.&lt;/param&gt;
public static void LueNetistaListaan(String url, List&lt;String&gt; lista)
{
    HttpWebResponse response = null;
    StreamReader reader = null;
    try
    {
        // Lahetetaan HTTP-pyynto palvelimelle
        HttpWebRequest request = (HttpWebRequest)WebRequest.Create(url); 
        request.Method = "GET"; // Maaritellaan pyynnon tyypiksi GET
        response = (HttpWebResponse)request.GetResponse();
        reader = new StreamReader(response.GetResponseStream(), Encoding.UTF8); 
        while (!reader.EndOfStream)
        {
            String rivi = reader.ReadLine();
            lista.Add(rivi);
        }
    }
    catch (WebException we) // Ellei saada luotua verkkoyhteytta sopivalla protokollalla
    {
        Console.WriteLine(we.Message);
    }
    finally
    {
        if (reader != null)
            reader.Close();
        if (response != null)
        // Tietovirta taytyy sulkea, jottei sovelluksessa 
        // jouduttaisi tilaan, missa yhteydet "loppuvat kesken".
            response.Close(); 
    }
}
</code></pre>
<p>}
```</p>
<p>Mukana on yksi try-catch-finally-rakenne. Verkkoyhteydet ovat alttiita
kaikenlaisille virheille, joten poikkeusten ”kiinniottaminen” on
perusteltua ja suorastaan välttämätöntä.</p>
<h2>25.3 Satunnaisluvut</h2>
<p>Random-luokka sijaitsee System-nimiavaruudessa. Random-luokasta löytyy
metodeja joilla voimme arpoa erityyppisiä satunnaislukuja. Arpomista
varten meidän täytyy luoda Random-olio, jotta metodeja voitaisiin
kutsua. Random-oliolla on metodi Next, joka saa parametrikseen
kokonaisluvun ja arpoo sitten satunnaisen luvun 0 ja parametrinaan
saamansa luvun väliltä niin, että parametrina annettava luku ei enää
kuulu arvottaviin lukuihin. Arvottava luku on siis aina puoliavoimella
välillä [0, parametri[. Jos haluaisimme arpoa luvun suljetulta väliltä
[0, 10] täytyisi meidän siis muuttaa parametria vastaavasti, sillä kun
käsitellään kokonaislukuja, suljettu väli [0, 10] on sama asia kuin
puoliavoin väli [0, 11[. Alla oleva koodinpätkä arpoisi nyt siis luvun
0:n ja 10:n väliltä niin, että luvut 0 ja 10 kuuluvat arvottaviin
lukuihin.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Random rand = new Random();
int satunnaisluku = rand.Next(11);</code></p>
<p>Jos haluaisimme arpoa luvun esimerkiksi suljetulta väliltä [50, 99],
sanoisimme</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Random rand = new Random();
int satunnaisluku = rand.Next(50, 100);</code></p>
<p>Liukuluku arvotaan NextDouble-metodilla.</p>
<p>Jypelissä olevasta RandomGen-luokasta löytyy useita staattisia metodeja,
joilla satunnaislukujen (ja -värien, totuusarvojen, jne.) luominen on
helpompaa. Lue RandomGen-luokan dokumentaatio osoitteesta
<a href="http://kurssit.it.jyu.fi/npo/material/latest/documentation/html/">http://kurssit.it.jyu.fi/npo/material/latest/documentation/html/</a>
.</p>
<h1>26. Lukujen esitys tietokoneessa</h1>
<h2>26.1 Lukujärjestelmät</h2>
<p>Meille tutuin lukujärjestelmä on 10-järjestelmä. Siinä on 10 eri
symbolia lukujen esittämiseen (0...9). Lukua 10 sanotaan 10-järjestelmän
<em>kantaluvuksi</em>. Tietotekniikassa käytetään kuitenkin myös muita
lukujärjestelmiä. Yleisimpiä ovat 2-järjestelmä (binäärijärjestelmä),
8-järjestelmä (oktaalijärjestelmä) ja 16-järjestelmä (heksajärjestelmä).
Binäärijärjestelmässä luvut esitetään kahdella symbolilla (0 ja 1) ja
oktaalijärjestelmässä vastaavasti kahdeksalla symbolilla (0..7). Samalla
periaatteella heksajärjestelmässä käytetään 16 symbolia, mutta koska
numerot loppuvat kesken, otetaan avuksi aakkoset. Symbolin 9 jälkeen
tulee siis symboli A, jonka jälkeen B ja näin jatketaan edelleen F:n
asti, joka vastaa siis 10-järjestelmän lukua 15. Heksajärjestelmä
sisältää siis symbolit 0..9 ja (jatkuen) A..F.</p>
<p>Koska lukujärjestelmät sisältävät samoja symboleja, täytyy ne osata
jotenkin erottaa toisistaan. Tämä tehdään usein alaindekseillä.
Esimerkiksi binääriluku 11 voitaisiin kirjoittaa muodossa 11~2~.Tällöin
sen erottaa 10-järjestelmän luvusta 11, joka voitaisiin vastaavasti
kirjoittaa muodossa 11~10~. Koska alaindeksien kirjoittaminen koneella
on hieman haastavaa, käytetään usein myös merkintää, jossa binääriluvun
perään lisätään B-kirjain. Esimerkiksi 11B tarkoittaisi samaa kuin
11~2~.</p>
<p>Kaikissa yllä mainituissa lukujärjestelmissä symbolin paikalla on
oleellinen merkitys. Kun symboleja laitetaan peräkkäin, ei siis ole
yhdentekevää millä paikalla luvussa tietty symboli on. [MÄN]</p>
<h2>26.2 Paikkajärjestelmät</h2>
<p>Käyttämämme lukujärjestelmät ovat paikkajärjestelmiä, eli jokaisen
numeron paikka luvussa on merkitsevä. Jos numeroiden paikkaa luvussa
vaihdetaan, muuttuu luvun arvokin. Luvun</p>
<p>``` {.esimerkki-western .c90 lang="zxx" xml:lang="zxx"}</p>
<p>n3n2n1n0
```</p>
<p>arvo on</p>
<p>``` {.esimerkki-western .c90 lang="zxx" xml:lang="zxx"}</p>
<p>n3<em>k3 + n2</em>k2 + n1<em>k1 + n0</em>k0
```</p>
<p>missä k on käytetyn järjestelmän kantaluku. Esimerkiksi
10-järjestelmässä:</p>
<p>``` {.esimerkki-western .c90 lang="zxx" xml:lang="zxx"}</p>
<p>2536 = 2<em>103 + 5</em>102 + 3<em>101 + 6</em>100 = 2<em>1000 + 5</em>100 + 3<em>10 + 6</em>1
```</p>
<p>Sanomme siis, että luvussa 2536 on 2 kappaletta tuhansia, 5 kappaletta
satoja, 3 kappaletta kymmeniä ja 6 kappaletta ykkösiä.</p>
<p>Jos luvussa olevat symbolien paikat siis numeroidaan oikealta vasemmalle
alkaen nollasta, saadaan luvun arvo selville summaamalla kussakin
paikassa oleva arvo kerrottuna kantaluku potenssiin paikan numero. Tämä
toimii myös desimaaliluvuille kun numeroidaan desimaalimerkin oikealla
puolella olevat paikat -1, -2, -3 jne. Esimerkiksi</p>
<p>``` {.esimerkki-western .c90 lang="zxx" xml:lang="zxx"}</p>
<p>25.36 = 2<em>101 + 5</em>100 + 3<em>10-1 + 6</em>10-2 = 2<em>10 + 5</em>1 + 3<em>0.1 + 6</em>0.01
```</p>
<h2>26.3 Binääriluvut</h2>
<p>Binäärijärjestelmässä kantalukuna on 2 ja siten on käytössä kaksi
symbolia: 0 ja 1. Binäärijärjestelmä on tietotekniikassa oleellisin
järjestelmä, sillä lopulta laskenta suurimmassa osassa nykyprosessoreita
tapahtuu binäärilukuina. Tarkemmin sanottuna binääriluvut esitetään
prosessorissa jännitteinä. Tietty jänniteväli vastaa arvoa 0 ja tietty
jänniteväli arvoa 1.</p>
<h3>26.3.1 Binääriluku 10-järjestelmän luvuksi</h3>
<p>Esimerkiksi binääriluku 10110 voidaan muuttaa 10-järjestelmän luvuksi
seuraavasti.</p>
<p>``` {.esimerkki-western .c90 lang="zxx" xml:lang="zxx"}</p>
<p>101102 = 1<em>24 + 0</em>23 + 1<em>22 + 1</em>21 + 0*20 = 0 + 2 + 4 + 0 + 16 = 2210
```</p>
<p>Binäärimuodossa oleva desimaaliluku 101.1011 saadaan muutettua
10-järjestelmän luvuksi seuraavasti. Muuttaminen tehdään samalla
periaatteella kun yllä. Nyt desimaaliosaan mentäessä potenssien
vähentämistä edelleen jatketaan, jolloin potenssit muuttuvat
negatiivisiksi:</p>
<p>``` {.esimerkki-western .c90 lang="zxx" xml:lang="zxx"}</p>
<p>101.10112 = 1<em>22 + 0</em>21 + 1<em>20 + 1</em>2-1 + 0<em>2-2 + 1</em>2-3 + 1*2-4 = 4 + 0 + 1 + 0.5 + 0 + 0.125 + 0.0625 = 5.687510
```</p>
<p>Binääriluku 101.1011 on siis 10-järjestelmän lukuna 5.6875.</p>
<h3>26.3.2 10-järjestelmän luku binääriluvuksi</h3>
<p>10-järjestelmän luku saadaan muutettua binääriluvuksi jakamalla sen
kokonaisosaa toistuvasti kahdella ja merkkaamalla paperin syrjään 0, jos
jako meni tasan ja muuten 1. Kun lukua ei voi enää jakaa, saa
binääriluvun selville kun lukee jakojäännökset päinvastaisesta
suunnasta, kuin mistä aloitimme laskemisen. Esimerkiksi luku 19~10~
voidaan muuttaa binääriluvuksi seuraavasti:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
19/2 = 9, jakojäännös 1
 9/2 = 4, jakojäännös 1
 4/2 = 2, jakojäännös 0
 2/2 = 1, jakojäännös 0
 1/2 = 0, jakojäännös 1</code></p>
<p>Kun jakojäännökset luetaan nyt alhaalta ylöspäin, saamme binääriluvun
10011. Vastaavasti laskenta voitaisiin hahmotella kuten alla, josta
jakojäännös selviää paremmin. Idea molemmissa on kuitenkin sama.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
19 = 2*9+1
 9 = 2*4+1
 4 = 2*2+0
 2 = 2*1+0
 1 = 2*0+1</code></p>
<p>Muutetaan vielä luku 126~10~ binääriluvuksi.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
126 = 2*63+0
 63 = 2*31+1
 31 = 2*15+1
 15 =  2*7+1
  7 =  2*3+1
  3 =  2*1+1
  1 =  2*0+1</code></p>
<p>Valmis binääriluku on siis 1111110</p>
<p>Desimaaliluvuissa täytyy kokonaisosa ja desimaaliosa muuttaa
binääriluvuiksi erikseen. Kokonaisosa muutetaan binääriluvuksi kuten
yllä. Desimaaliosa muutetaan kertomalla desimaaliosaa toistuvasti
kahdella ja merkkaamalla paperin syrjään nyt 1, jos tulo oli suurempaa
tai yhtä suurta kuin 1 ja 0 jos tulo jäi alle yhden. Muutetaan luku
0.8125~10~ binääriluvuksi.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
0.8125 * 2 = 1.625
 0.625 * 2 = 1.25
  0.25 * 2 = 0.5
   0.5 * 2 = 1.0</code></p>
<p>Luku meni tasan, eli luku 0.8125~10~ = 0.1101~2~. Binääriluku voidaan
siis lukea kuten alla olevassa kuvassa.</p>
<p><img alt="\
 Kuva 34: Luvun 0.8125 muuttaminen
binääriluvuksi" src="../src/luentomonistecsUusin_htm_m373c2e01.png" /></p>
<p>\
 Muutetaan vielä luku 0.675~10~ binääriluvuksi.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
0.675 * 2 = 1.35
 0.35 * 2 = 0.7
  0.7 * 2 = 1.4
  0.4 * 2 = 0.8
  0.8 * 2 = 1.6
  0.6 * 2 = 1.2
  0.2 * 2 = 0.4
  0.4 * 2 = 0.8</code></p>
<p>Kun kerromme uudelleen samaa desimaaliosaa kahdella, voidaan laskeminen
lopettaa. Tällöin kyseessä on päättymätön luku. Luvussa rupeaisi siis
toistumaan jakso 11001100. Nyt luku luetaan samasta suunnasta, josta
laskeminenkin aloitettiin. Enää meidän tarvitsee päättää millä
tarkkuudella luku esitetään. Mitä enemmän bittejä käytämme, sitä
tarkempi luvusta tulee.</p>
<p>``` {.esimerkki-western .c90 lang="zxx" xml:lang="zxx"}</p>
<p>0.67510 = 0.1010110011001100112
```</p>
<p>Jaksoa voitaisiin siis jatkaa loputtomiin, mutta oleellista on, että
lukua 0.675 ei pystytä esittämään tarkasti binääriluvuilla.</p>
<p>Koitetaan muuttaa luku 23.375~10~ binääriluvuksi. Ensiksi muutetaan
kokonaisosa.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
23 = 2*11+1
11 = 2 *5+1
 5 = 2 *2+1
 2 = 2* 1+0
 1 = 2* 0+1</code></p>
<p>Kokonaisosa on siis 10111~2~. Muutetaan vielä desimaaliosa.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
0.375 * 2 = 0.75
 0.75 * 2 = 1.5
  0.5 * 2 = 1.0</code></p>
<p>Eli 23.375~10~ = 10111.011~2~.</p>
<h2>26.4 Negatiiviset binääriluvut</h2>
<p>Negatiivinen luku voidaan esittää joko suorana, 1-komplementtina tai
2-komplementtina.</p>
<h3>26.4.1 Suora tulkinta</h3>
<p>Suorassa tulkinnassa varataan yksi bitti ilmoittamaan luvun etumerkkiä
(+/-). Jos meillä on käytössä 4 bittiä, niin tällöin luku +3~10~ = 0011
ja -3~10~ = 1011. Suoran esityksen mukana tulee ongelmia
laskutoimituksia suoritettaessa; mm. luvulla nolla on tällöin kaksi
esitystä, 0000 ja 1000, mikä ei ole toivottava ominaisuus.</p>
<h3>26.4.2 1-komplementti</h3>
<p>Jos luku on positiivinen, kirjoitetaan se normaalisti, ja jos luku on
negatiivinen, niin käännetään kaikki bitit päinvastaisiksi. Esimerkiksi
luku +3~10~ = 0011 ja -3~10~ = 1100. Tässäkin systeemissä luvulla nolla
on kaksi esitystä, 0000 ja 1111.</p>
<h3>26.4.3 2-komplementti</h3>
<p>Yleisimmin käytetty tapa ilmoittaa negatiiviset luvut on 2-komplementti.
Tällöin positiivisesta luvusta otetaan ensin 1-komplementti, eli
muutetaan nollat ykkösiksi ja ykköset nolliksi ("käännetään" kaikki
bitit vastakkaisiksi), minkä jälkeen tulokseen lisätään 1. Tämän
esitystavan etuna on se, että yhteenlasku toimii totuttuun tapaan myös
negatiivisilla luvuilla. Vähennyslasku suoritetaan summaamalla luvun
vastaluku:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
2-3 = 2+(-3)</code></p>
<p>Muodostetaan luvusta 1 negatiivinen luku:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
luku 1:           0001
käännetään bitit: 1110
lisätään 1:       1111</code></p>
<p>Luku -1 on siis kahden komplementtina 1111. Kokeillaan tehdä samaa
luvulle 2.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
luku 2:           0010
käännetään bitit: 1101
lisätään 1:       1110</code></p>
<p>Saatiin siis, että -2 on kahden komplementtina 1110. Kokeillaan vielä
muuttaa 3 vastaavaksi negatiiviseksi luvuksi.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
luku 3:           0011
käännetään bitit: 1100
lisätään 1:       1101</code></p>
<p>Saatiin, että -3 on kahden komplementtina 1101.</p>
<p>Voidaanko luvut muuttaa samalla menetelmällä takaisin positiivisiksi
luvuiksi? Kokeile!</p>
<h3>26.4.4 2-komplementin yhteenlasku</h3>
<p>Jos vastauksen merkitsevin bitti (vasemman puoleisin) on 1, on vastaus
negatiivinen ja 2-komplementtimuodossa. Tällöin vastauksen
tulkitsemiseksi sille suoritetaan muunnos edellä esitetyllä tavalla
(ensin käännetään bitit, sitten lisätään 1). Muunnoksen tuloksena
saadaan luvun itseisarvo, itse luku on siis tällöin aina negatiivinen.
Jos merkitsevin bitti on 0, on vastaus positiivinen, eikä mitään
muunnosta tarvitse suorittaa.</p>
<p>Lasketaan esimerkiksi 2+1</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
 00 <br />
  0010
+ 0001</p>
<hr />
<p>0011
```</p>
<p>Merkitsevin bitti on 0, joten vastaus on 0011~2~ = 3~10~. Lasketaan
seuraavaksi 1-2.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
 00 <br />
  0001
+ 1110</p>
<hr />
<p>1111
```</p>
<p>Merkitsevin bitti on nyt 1 eli luku on kahden komplementti. Kun
käännetään bitit ja lisätään 1 saadaan luku 0001. Koska merkitsevin
bitti oli 1 on luku siis negatiivinen, joten saatiin vastaukseksi -1.</p>
<p>Lasketaan vielä -2-3.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
 11 <br />
  1110
+ 1101</p>
<hr />
<p>1011
```</p>
<p>Luku on jälleen negatiivinen. Kun käännetään bitit ja lisätään 1,
saadaan 0101~2~ = 5~10~. Vastaus on siis -5~10~.</p>
<p>Lopuksi vielä pari laskua joiden tulos ei mahdu 4:ään bittiin. Aluksi 6
+ 7</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
  11 <br />
  0110
+ 0111</p>
<hr />
<p>1101   =&gt; 0010 + 1  =&gt; -3 (siis neg. luku kahden pos. luvun yhteenlaskusta)
```</p>
<p>Vastaavasti -7-6</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
 10 <br />
  1001
+ 1010 </p>
<hr />
<p>0011  =&gt; +3 (positiivinen luku kahden negatiivisen yhteenlaskusta)
```</p>
<p>Kahdessa viimeisessä laskussa päädyttiin väärään tulokseen! Tämä on
luonnollista, sillä tietenkään rajallisella bittimäärällä ei voida
esittää rajaansa isompia lukuja. Meidän esimerkkimme 4 bitin
lukualueella saadaan vain lukualue [-8, 7]. Vertaa alkeistietotyyppien
lukualueisiin, jotka esiteltiin kohdassa 7.2. 2-komplementin yksi
lisäetu on se, että siinä mainitunkaltainen <em>ylivuoto</em> (overflow), eli
lukualueen ylitys, on helppo todeta: viimeiseen bittiin (merkkibittiin)
tuleva ja sieltä lähtevä muistinumero on erisuuri. Edellisissäkin
esimerkeissä oikeaan tulokseen päätyneissä laskuissa ne olivat samat ja
väärän tulokseen päätyneissä laskuissa eri suuret. <em>Alivuoto</em>
(underflow) tulee vastaavasti liukuluvuilla silloin kun laskutoimituksen
tulos tuottaa nollan, vaikka oikeassa maailmassa tulos ei vielä
olisikaan nolla.</p>
<h2>26.5 Lukujärjestelmien suhde toisiinsa</h2>
<p>Koska binääriluvuista muodostuu usein hyvin pitkiä, ilmoitetaan ne usein
ihmiselle helpommin luettavassa muodossa joko 8- tai 16-järjestelmän
lukuina. Tutustutaan nyt jälkimmäiseen, eli heksajärjestelmään.
Heksajärjestelmässä on käytössä merkit 0...9A...F, eli yhteensä 16
symbolia. Näin yhdellä symbolilla voidaan esittää jopa luku 15~10~ =
1111~2~. Heksalukuja A...F vastaavat 10-järjestelmän luvut näet alla
olevasta taulukosta.</p>
<hr />
<p>A~16~   10~10~
  B~16~   11~10~
  C~16~   12~10~
  D~16~   13~10~
  E~16~   14~10~
  F~16~   15~10~</p>
<hr />
<p>Yhdellä 16-järjestelmä symbolilla voidaan siis esittää 4-bittinen
binääriluku. Binääriluku voidaankin muuttaa heksajärjestelmän luvuksi
järjestelemällä bitit oikealta alkaen neljän bitin ryhmiin ja
käyttämällä kunkin 4-bitin yhdistelmän heksavastinetta. Muutetaan luku
11101101~2~ heksajärjestelmään.</p>
<p>``` {.esimerkki-western .c90 lang="zxx" xml:lang="zxx"}</p>
<p>111011012 =1110 11012
11102 = E16
11012 = D16
111011012 =1110 11012 = ED16
```</p>
<p>Vastaavasti voitaisiin muuttaa binääriluku 8-järjestelmän luvuksi, mutta
nyt vain järjesteltäisiin bitit oikealta alkaen kolmen bitin ryhmiin.</p>
<p>Alla olevassa taulukossa on esitetty 10-, 2-, 8- ja 16-järjestelmän
luvut 0~10~..15~10~. Lisäksi on esitetty mikä olisi vastaavan
binääriluvun 2-komplementti -tulkinta.</p>
<p>Taulukko 9: Lukujen vastaavuus eri lukujärjestelmissä.</p>
<hr />
<p>10-järj.   2-järj.   8-järj.   16-järj.   2-komplementti
  0          0000      00        0          0
  1          0001      01        1          1
  2          0010      02        2          2
  3          0011      03        3          3
  4          0100      04        4          4
  5          0101      05        5          5
  6          0110      06        6          6
  7          0111      07        7          7
  8          1000      10        8          -8
  9          1001      11        9          -7
  10         1010      12        A          -6
  11         1011      13        B          -5
  12         1100      14        C          -4
  13         1101      15        D          -3
  14         1110      16        E          -2
  15         1111      17        F          -1</p>
<hr />
<h2>26.6 Liukuluku (floating-point)</h2>
<p>Liukulukua käytetään siis reaalilukujen esitykseen tietokoneissa.
Liukulukuesitykseen kuuluu neljä osaa: etumerkki (s), mantissa (m),
kantaluku (k) ja eksponentti (c). Kantaluvulla ja eksponentilla
määritellään luvun suuruusluokka ja mantissa kuvaa luvun merkitseviä
numeroita. Luku x saadaan laskettua kaavalla:</p>
<p>``` {.esimerkki-western .c90 lang="zxx" xml:lang="zxx"}</p>
<p>x = ( - 1)smkc
```</p>
<p>Tietotekniikassa yleisimmin käytetyssä standardissa IEE 754 kantaluku on
2, jolloin kaava saadaan muotoon:</p>
<p>``` {.esimerkki-western .c90 lang="zxx" xml:lang="zxx"}</p>
<p>x = ( - 1)sm2c
```</p>
<p>IEE 754-standardissa luvun etumerkki (s) ilmoitetaan bittimuodossa
ensimmäisellä bitillä, jolloin s voi saada joko arvon 0, joka tarkoittaa
positiivista lukua tai arvon 1, joka tarkoittaa siis negatiivista lukua.</p>
<p>Tutustutaan seuraavaksi kuinka float ja double esitetään bittimuodossa.</p>
<p>float on kooltaan 32 bittiä. Siinä ensimmäinen bitti siis tarkoittaa
etumerkkiä, seuraavat 8 bittiä eksponenttia ja jäljelle jäävät 23 bittiä
mantissaa.</p>
<p><img alt="\
 Kuva 35: Float 0.875 liukulukuna
bittimuodossa" src="../src/luentomonistecsUusin_htm_7deb6236.png" /></p>
<p>\
 double on kooltaan 64 bittiä. Siinäkin ensimmäinen bitti tarkoittaa
etumerkkiä, seuraavat 11 bittiä eksponenttia ja jäljelle jäävät 52
bittiä kuvaavat mantissaa.</p>
<p><img alt="\
 Kuva 36: Double 0.800 liukulukuna
bittiesityksenä" src="../src/luentomonistecsUusin_htm_4ce95e8f.png" /></p>
<p>\
 Eksponentti esitetään niin, että siitä vähennetään ns. BIAS arvo. BIAS
arvo floatissa on 127 ja doublessa se on 1023. Näin samalla
binääriluvulla saadaan esitettyä sekä positiiviset että negatiiviset
eksponentit. Jos floatin eksponenttia kuvaavat bitit olisivat
esimerkiksi 01111110, eli desimaalimuodossa 126, niin eksponentti olisi
126 - 127 = -1.</p>
<p>Mantissa puolestaan esitetään niin, että se on aina vähintään 1.
Mantissaa kuvaavat bitit esittävätkin ainoastaan mantissan
desimaaliosaa. Jos floatin mantissaa kuvaavat bitit olisivat esimerkiksi
10100000000000000000000, olisi mantissa tällöin binäärimuodossa 1.101
eli desimaalimuodossa 1.625.</p>
<h3>26.6.1 Liukuluvun binääriesityksen muuttaminen 10-järjestelmään</h3>
<p>Kokeillaan nyt muuttaa muutama binäärimuodossa oleva float
kokonaisuudessaan 10-järjestelmän luvuksi. Esimerkkinä liukuluku:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
00111111 10000000 00000000 00000000</code></p>
<p>Bitit on järjestetty nyt tavuittain. Voisimme järjestellä bitit niin,
että liukuluvun eri osat näkyvät selkeämmin:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
0 01111111 00000000000000000000000</code></p>
<p>Ensimmäinen bitti on nolla, eli luku on positiivinen. Seuraavat 8 bittiä
ovat 01111111, joka on 10-järjestelemän lukuna 127 eli eksponentti on
127-127 = 0. Mantissaa esittäviksi biteiksi jää pelkkiä nollia, eli
mantissa on 1.0, koska mantissahan oli aina vähintään 1. Nyt liukuluvun
kaavalla voidaan laskea mikä luku on kyseessä:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
x = ( - 1)0*1.0*20 = 1.0</code></p>
<p>Kyseessä olisi siis reaaliluku 1.0. Kunhan muistetaan ottaa huomioon
ensimmäinen bitti etumerkkinä, voidaan liukuluvun laskemiseen käyttää
vielä yksinkertaisempaa kaavaa:</p>
<p>``` {.esimerkki-western .c90 lang="zxx" xml:lang="zxx"}</p>
<p>x = m2c
```</p>
<p>Muutetaan vielä toinen liukuluvun binääriesitys 10-järjestelmän luvuksi.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
 00111111 01100000 00000000 00000000</code></p>
<p>Ensimmäinen bitti on jälleen 0, eli luku on positiivinen. Seuraavat 8
bittiä ovat 01111110, joka on desimaalilukuna 126. Eksponentti on siis
126-127 = -1. Mantissaan jää nyt bitit 11000000000000000000000 eli
mantissa on binääriluku 1.11, joka on 10-järjestelmässä luku 1.75.
Liukuluvun esittämäksi reaaliluvuksi saadaan siis:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
1.75*2-1 = 0.875</code></p>
<h3>26.6.2 10-järjestelmän luku liukuluvun binääriesitykseksi</h3>
<p>Kun muutetaan 10-järjestelmän luku liukuluvun binääriesitykseksi, täytyy
ensiksi selvittää liukuluvun eksponentti. Tämä saadaan selville
skaalaamalla luku välille [1,2[ kertomalla tai jakamalla lukua
toistuvasti luvulla 2, niin, että luku x on aluksi muodossa:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
x*20</code></p>
<p>Nyt jos jaamme luvun kahdella, niin samalla eksponentti kasvaa yhdellä.
Jos taas kerromme luvun kahdella, niin eksponentti vähenee yhdellä. Näin
luvun arvo ei muutu ja saamme luvun muotoon</p>
<p>``` {.esimerkki-western .c90 lang="zxx" xml:lang="zxx"}</p>
<p>m*2c
```</p>
<p>jossa m on välillä [1,2[. Tämä onkin jo liukuluvun esitysmuoto. Enää
meidän ei tarvitsisi kuin muuttaa se tietokoneen ymmärtämäksi
binääriesitykseksi.</p>
<p>Muutetaan esimerkkinä 10-järjestelmän luku -0.1 liukuluvun
binääriesitykseksi. Etumerkki huomioidaan sitten ensimmäisessä bitissä,
joten nyt voidaan käsitellä lukua 0.1. Luku voidaan nyt kirjoittaa
muodossa :</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
0.1 = 0.1*2</code></p>
<p>Nyt kerrotaan lukua kahdella kunnes se on välillä [1,2[ ja muistetaan
vähentää jokaisen kertomisen jälkeen eksponenttia yhdellä, jotta luvun
arvo ei muutu.</p>
<p>``` {.esimerkki-western .c90 lang="zxx" xml:lang="zxx"}</p>
<p>0.1 = 0.1<em>20 = 0.2</em>2-1 = 0.4<em>2-2 = 0.8</em>2-3 = 1.6*2-4
```</p>
<p>Eksponentiksi saatiin -4, eli liukuluvun binääriesitykseen siihen
lisätään BIAS, eli saadaan 10-järjestelmän luku -4 + 127 = 123, joka on
binäärilukuna 1111011. Muutetaan nyt mantissa binääriluvuksi. Muista,
että mantissan kokonaisosaa ei merkitty liukuluvun binääriesitykseen.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Ensimmäinen bitti  =&gt; 1  (jota ei merkitä)
0.6 * 2  = 1.2     =&gt; 1
0.2 * 2  = 0.4     =&gt; 0
0.4 * 2  = 0.8     =&gt; 0
0.8 * 2  = 1.6     =&gt; 1
0.6 * 2  = 1.2     =&gt; 1</code></p>
<p>Tästä nähdään jo, että kyseessä on päättymätön luku, koska meidän täytyy
jälleen kertoa lukua 0.6 kahdella. Laskeminen voidaan siis lopettaa,
sillä jakso on jo nähtävillä. Kun jaksoa jatketaan 23 bitin mittaiseksi,
saadaan mantissaksi binääriluku 10011001100110011001100. Seuraavat kaksi
bittiä olisivat 11, joten luku pyöristyy vielä muotoon
10011001100110011001101. Nyt kaikki liukuluvun osat ovat selvillä:</p>
<ul>
<li>
<p>Etumerkkibitti: 1, sillä alkuperäinen luku oli -0.1</p>
</li>
<li>
<p>Eksponentti: 1111011</p>
</li>
<li>
<p>Mantissa: 10011001100110011001101</p>
</li>
</ul>
<p>Eli yhdistämällä saadaan:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
1 1111011 10011001100110011001101</code></p>
<p>Binääriluku voidaan vielä järjestellä tavuittain:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
1111101 11001100 11001100 11001101</code></p>
<p>Lukua 0.1 ei siis voi esittää liukulukuna tarkasti, vaan pientä heittoa
tulee aina.</p>
<h3>26.6.3 Huomio: doublen lukualue</h3>
<p>Liukuluku esitys on siitä näppärä, että eksponentin ansiosta sillä
saadaan todella suuri lukualue käyttöön. double:n eksponenttiin oli
käytössä 11 bittiä. Tällöin suurin mahdollinen eksponentti on
binääriluku 11111111111 vähennettynä double:n BIAS arvolla. Tästä
saadaan desimaalilukuna 2047 - 1023 = 1024. Kun mantissa voi olla
välillä [1, 2[, saadaan double:n maksimiarvoksi 2*2^1024^, joka on
likimain 3.59 * 10^308^. double:n lukualue on siis suunnilleen [-3.59
* 10^308^, 3.59 * 10^308^], kun long-tyypin lukualue oli [-2^63^,
2^63^[. double-tyypillä pystytään siis esittämään paljon suurempia
lukuja kuin long-tyypillä.</p>
<h3>26.6.4 Liukulukujen tarkkuus</h3>
<p>Liukuluvut ovat tarkkoja, jos niillä esitettävä luku on esitettävissä
mantissan bittien määrän mukaisena kahden potenssien kombinaatioina.
Esimerkiksi luvut 0.5, 0.25 jne. ovat tarkkoja. Harmittavasti kuitenkin
edellä todettiin että 10-järjestelmän luku 0.1 ei ole tarkka. Siksi
esimerkiksi rahalaskuissa on käytettävä joko senttejä tai esimerkiksi
C#:n Decimal-luokkaa (Javan BigDecimal). Laskuissa kuitenkin nämä
erikoistyypit ovat hitaampia, tilanteesta riippuen eivät kuitenkaan
välttämättä merkitsevästi.</p>
<p>Toisaalta liukuluvulla voi esittää tarkasti kokonaislukuja aina arvoon
2^mantissan\ bittien\ lukumäärä^ saakka. Eli doublella (52 bittiä
mantissalle) voi tarkasti käsitellä suurempia kokonaislukuja kuin
int-tyypillä (32 bittiä luvun esittämiseen). long-tyypin 64-bitillä
päästään vielä doublea suurempiin tarkkoihin kokonaislukuihin. Valmiit
kokonaislukutyypit ovat yleensä laskennassa liukulukutyyppejä nopeampia,
joten siksi kokonaislukutyyppejä kannattaa suosia. Nykyprosessoreissa
sen sijaan double ja float tyyppien laskut eivät merkittävästi poikkea
suoritusnopeudeltaan, joten siksi doublea on pidettävä ensisijaisena
valintana kun tarvitaan reaalilukua. Kaikissa mobiilialustoissa ei
välttämättä ole käytössä liukulukutyyppejä ja tämä on otettava
erikoistapauksissa huomioon. Joissakin tapauksissa kieli (esimerkiksi
Java) voi tukea liukulukuja, mutta kohdealustassa ei ole niille
prosessoritason tukea. Tällöin liukulukujen käyttö voi olla hidasta.
Tarvittaessa laskuja voi suorittaa niin, että skaalaa lukualueen
kuvitteellisesti niin, että vaikka sisäisesti luku 1000 on loogisesti 1
ja 1 on loogisesti 0.001 (fixed point arithmetic).</p>
<h3>26.6.5 Intelin prosessorikaan ei ole aina osannut laskea liukulukuja oikein</h3>
<p>Wired-lehden 10 pahimman ohjelmistobugin listalle on päässyt Intelin
prosessorit, joissa ilmeni vuonna 1993 virheitä, kun suoritettiin
jakolaskuja tietyllä välillä olevilla liukuluvuilla. Prosessorien
korvaaminen aiheutti Intelille arviolta 475 miljoonan dollarin kulut.
Tosin virhe esiintyi käytännössä vain muutamissa harvoissa erittäin
matemaattisissa ongelmissa, eikä oikeasta häirinnyt tavallista
toimistokäyttäjää millään tavalla. Tästä ja muista listan bugeista voi
lukea lisää alla olevasta linkistä.</p>
<p><a href="http://www.wired.com/software/coolapps/news/2005/11/69355">http://www.wired.com/software/coolapps/news/2005/11/69355</a></p>
<h1>27. ASCII-koodi</h1>
<p>ASCII (American Standard Code for Information Interchange) on merkistö,
joka käyttää seitsemän-bittistä koodausta. Sillä voidaan siis esittää
ainoastaan 128 merkkiä. Nimestäkin voi päätellä, että skandinaaviset
merkit eivät ole mukana, mistä seuraa ongelmia tietotekniikassa vielä
tänäkin päivänä, kun siirrytään ”skandeja” tukevasta koodistosta
ASCII-koodistoon.</p>
<p>ASCII-koodistossa siis jokaista merkkiä vastaa yksi 7-bittinen
binääriluku. Vastaavuudet näkyvät alla olevasta taulukosta, jossa
selkeyden vuoksi binääriluku on esitetty 10-järjestelmän lukuna, sekä
heksalukuna.</p>
<p>Taulukko 10: ASCII-merkistö.</p>
<hr />
<p>Des   Hex   Merkki                       \    \    \       \    \    \    \     \    \</p>
<p>0     0     NUL (null)                   32   20   Space   64   40   @    96    60   `</p>
<p>1     1     SOH (otsikon alku)           33   21   !       65   41   A    97    61   a</p>
<p>2     2     STX (tekstin alku)           34   22   "       66   42   B    98    62   b</p>
<p>3     3     ETX (tekstin loppu)          35   23   #      67   43   C    99    63   c</p>
<p>4     4     EOT (end of transmission)    36   24   \$      68   44   D    100   64   d</p>
<p>5     5     ENQ (enquiry)                37   25   %       69   45   E    101   65   e</p>
<p>6     6     ACK (acknowledge)            38   26   &amp;       70   46   F    102   66   f</p>
<p>7     7     BEL (bell)                   39   27   '       71   47   G    103   67   g</p>
<p>8     8     BS (backspace)               40   28   (       72   48   H    104   68   h</p>
<p>9     9     TAB (tabulaattori)           41   29   )       73   49   I    105   69   i</p>
<p>10    A     LF (uusi rivi)               42   2A   *      74   4A   J    106   6A   j</p>
<p>11    B     VT (vertical tab)            43   2B   +       75   4B   K    107   6B   k</p>
<p>12    C     FF (uusi sivu)               44   2C   ,       76   4C   L    108   6C   l</p>
<p>13    D     CR (carriage return)         45   2D   -       77   4D   M    109   6D   m</p>
<p>14    E     SO (shift out)               46   2E   ,       78   4E   N    110   6E   n</p>
<p>15    F     SI (shift in)                47   2F   /       79   4F   O    111   6F   o</p>
<p>16    10    DLE (data link escape)       48   30   0       80   50   P    112   70   p</p>
<p>17    11    DC1 (device control 1)       49   31   1       81   51   Q    113   71   q</p>
<p>18    12    DC2 (device control 2)       50   32   2       82   52   R    114   72   r</p>
<p>19    13    DC3 (device control 3)       51   33   3       83   53   S    115   73   s</p>
<p>20    14    DC4 (device control 4)       52   34   4       84   54   T    116   74   t</p>
<p>21    15    NAK (negative acknowledge)   53   35   5       85   55   U    117   75   u</p>
<p>22    16    SYN (synchronous table)      54   36   6       86   56   V    118   76   v</p>
<p>23    17    ETB (end of trans. block)    55   37   7       87   57   W    119   77   w</p>
<p>24    18    CAN (cancel)                 56   38   8       88   58   X    120   78   x</p>
<p>25    19    EM (end of medium)           57   39   9       89   59   Y    121   79   y</p>
<p>26    1A    SUB (substitute)             58   3A   :       90   5A   Z    122   7A   z</p>
<p>27    1B    ESC (escape)                 59   3B   ;       91   5B   [    123   7B   {</p>
<p>28    1C    FS (file separator)          60   3C   \&lt;      92   5C   \   124   7C   |</p>
<p>29    1D    GS (group separator)         61   3D   =       93   5D   ]    125   7D   }</p>
<p>30    1E    RS (record separator)        62   3E   >      94   5E   \^   126   7E   \~</p>
<p>31    1F    US (unit separator)          63   3F   ?       95   5F   _   127   7F   DEL</p>
<hr />
<p>Monissa ohjelmointikielissä, kuten myös Javassa, ASCII-merkkien
desimaaliarvoja voidaan sijoittaa suoraan char-tyyppisiin muuttujiin.
Esimerkiksi pikku-a:n (a) voisi sijoittaa muuttujaan c seuraavasti:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
char c = 97;</code></p>
<p>Esimerkiksi tiedosto, jonka sisältö olisi loogisesti</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
Kissa istuu 
puussa</code></p>
<p>koostuisi oikeasti Windows-käyttöjärjestelmässä biteistä (joiden arvot
on lukemisen helpottamiseksi seuraavassa kuvattu heksana):</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
4B 69 73 73 61 20 69 73 74 75 75 0D 0A 70 75 75 73 73 61</code></p>
<p>Erona eri käyttöjärjestelmissä on se, miten rivinvaihto kuvataan.
Windowsissa rivinvaihto on CR LF (0D 0A) ja Unix-pohjaisissa
järjestelmissä pelkkä LF (0A).</p>
<p>Tiedoston sisältöä voit katsoa esimerkiksi antamalla komentoriviltä
komennot (jos tiedosto on kirjoitettu tiedostoon kissa.txt)</p>
<p><code>{lang="zxx" xml:lang="zxx"}
C:\MyTemp&gt;debug kissa.txt
-d
0D2F:0100  4B 69 73 73 61 20 69 73-74 75 75 0D 0A 70 75 75   Kissa istuu..puu
0D2F:0110  73 73 61 61 61 6D 65 74-65 72 73 20 34 00 1E 0D   ssaaameters 4...
...
-q</code></p>
<p>\
 \</p>
<h1>28. Syntaksin kuvaaminen</h1>
<h2>28.1 BNF</h2>
<p>Tässä luvussa kuvataan Java-kielen syntaksia. Syntaksia eli kielioppia
voidaan kuvata ns. BNF:llä (Backus-Naur Form). Kielen peruselementit on
käyty läpi alla olevassa taulukossa:</p>
<hr />
<p>\&lt;>   BNF-kaavio koostuu <em>non-terminaaleista</em> (välikesymbolit) ja <em>terminaaleista</em> (päätesymbolit)<em>.</em> Non-terminaalit kirjoitetaan pienempi kuin (\&lt;)- ja suurempi kuin (>)-merkkien väliin. Jokaiselle non-terminaalille on oltava jossain määrittely. Terminaali sen sijaan kirjoitetaan koodin sellaisenaan.
  ::=    Aloittaa non-terminaalin määrittelyn. Määrittely voi sisältää uusia non-terminaaleja ja terminaaleja.
  |      ”|”-merkki kuvaa sanaa ”tai”. Tällöin ”|”-merkin vasemmalla puolella olevan osan sijasta voidaan kirjoittaa oikealla puolella oleva osa.</p>
<hr />
<p>Määrittely on yleisessä muodossa seuraava:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
&lt;nonterminaali&gt; ::= _lause_</code></p>
<p>Jossa _lause_ voi sisältää uusia non-terminaaleja ja terminaaleja,
sekä "|"-merkkejä.</p>
<p>Kielen syntaksin kuvaaminen aloitetaan käännösyksikön (complitatonunit)
määrittelystä. Tämä on Javassa .java-päätteinen tiedosto. Tämä on siis
ensimmäinen non-terminaali, joka määritellään. Tämä määrittely sisältää
sitten toisia non-terminaaleja, joille kaikille on olemassa omat
määrittelyt. Näin jatketaan, kunnes lopulta on jäljellä pelkkiä
terminaaleja ja kielen syntaksi on yksiselitteisesti määritelty.</p>
<p>Esimerkiksi muuttujan määrittelyn syntaksin voisi kuvata seuraavasti.
Esimerkissä on lihavoituna kaikki terminaalit.</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
<local variable declaration statement> ::= <local variable declaration>;
<local variable declaration> ::= <type> <variable declarators></p>
<p><type> ::= <primitive type> | <reference type> 
<primitive type> ::= <numeric type> | boolean
<numeric type> ::= <integral type> | <floating-point type>
<integral type> ::= byte | short | int | long | char
<floating-point type> ::= float | double
<reference type> ::= <class or interface type> | <array type>
<class or interface type> ::= <class type> | <interface type>
<class type> ::= <type name>
<interface type> ::= <type name>
<array type> ::= <type> []</p>
<p><variable declarators> ::= <variable declarator> | <variable declarators> , 
                               <variable declarator>
<variable declarator> ::= <variable declarator id> | <variable declarator id>= <variable initializer>
<variable declarator id> ::= <identifier> | <variable declarator id> []
<variable initializer> ::= <expression> | <array initializer>
```</p>
<p>Lopetetaan muuttujan määrittelyn kuvaaminen tähän. Kokonaisuudessaan
siitä tulisi todella pitkä. Koko Javan syntaksin BNF:nä löytää
seuraavasta linkistä.</p>
<p><a href="http://www.daimi.au.dk/dRegAut/JavaBNF.html">http://www.daimi.au.dk/dRegAut/JavaBNF.html</a>.</p>
<h2>28.2 Laajennettu BNF (EBNF)</h2>
<p>Alkuperäisellä BNF:llä syntaksin kuvaaminen on melko työlästä. Tämän
takia on otettu käyttöön laajennettu BNF (extended BNF). Siinä
terminaalit kirjoitetaan lainausmerkkien sisään ja non-terminaalit nyt
ilman ”\&lt;>”-merkkejä. Lisäksi tulee kaksi uutta ominaisuutta.</p>
<hr />
<p>{}   Aaltosulkeiden sisään kirjoitetut osat voidaan jättää joko kokonaan pois tai toistaa yhden tai useamman kerran.
  []   Hakasulkeiden sisään kirjoitetut osat voidaan suorittaa joko kerran tai ei ollenkaan.</p>
<hr />
<p>Nyt muuttujan määrittelyn syntaksi saadaan kuvattua hieman helpommin:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
variable_declaration ::= { modifier } type variable_declarator 
                               { "," variable_declarator } ";"
modifier ::= "public" | "private" | "protected" | "static" | "final" | "native" |
             "synchronized" | "abstract" | "threadsafe" | "transient"
type ::= type_specifier { "[" "]" }
type_specifier  ::= "boolean" | "byte" | "char" | "short" | "int" | "float" | "long" 
                               | "double" | class_name | interface_name
variable_declarator ::= identifier { "[" "]" } [ "="variable_initializer ]
identifier ::= "a..z,$,_" { "a..z,$,_,0..9,unicode character over 00C0" }
variable_initializer ::= expression | ( "{" [ variable_initializer 
                               { "," variable_initializer } [ "," ] ] "}" )</code></p>
<p>Lausekkeen (expression) avaamisesta aukeaisi jälleen uusia ja uusia
non-terminaaleja, joten muuttujan määrittelyn kuvaaminen kannattaa
lopettaa tähän. Voit katsoa loput seuraavasta linkistä:</p>
<p><a href="http://cui.unige.ch/db-research/Enseignement/analyseinfo/JAVA/BNFindex.html">http://cui.unige.ch/db-research/Enseignement/analyseinfo/JAVA/BNFindex.html</a></p>
<p><img alt="\
 Kuva 37: Muuttujan määrittelyn syntaksia &quot;junaradoilla&quot;
esitettynä" src="../src/luentomonistecsUusin_htm_6974859.gif" /></p>
<p>\
 Vastaavasti syntaksia voidaan kuvata ”junaradoilla”. Tämä on eräs
graafinen tapa kuvata syntaksia. Kuvataan seuraavaksi muuttujaan
määrittelyä ”junaratojen” avulla. Junaradoissa non-terminaalit on
kuvattu suorakulmiolla ja terminaalit vähän pyöreämmillä suorakulmiolla.
Vaihtoehdot kuvataan taas niin, että risteyskohdassa voidaan valita vain
yksi vaihtoehtoisista raiteista. Lisäksi raiteissa on ”silmukoita”,
joissa voidaan tehdä useampi kierros. Silmukoilla kuvataan siis
”{}”-merkkien välissä olevia lauseita. Lisäksi on ”ohitusraiteita”,
joilla voidaan ohittaa joku osa kokonaan. Tällä kuvataan ”[]”-merkkien
välissä olevia lauseita.</p>
<p>Kuvasta puuttuu vielä tekstiesimerkissä olevien identifier ja
variable_intializer non-terminaalien junarataesitys. Piirrä niiden
”junaradat” samaan tapaan.</p>
<p>Lisätietoa:</p>
<ul>
<li>
<p><a href="https://korppi.jyu.fi/kotka/r.jsp?course=69997">TIEA241 Automaatit ja
    kieliopit</a> .</p>
</li>
<li>
<p>Paavo Niemisen 2007 pitämän Ohjelmointi 1-kurssin luentokalvot:
    <a href="http://users.jyu.fi/~nieminen/ohj1/materiaalia/luento02.pdf">http://users.jyu.fi/\~nieminen/ohj1/materiaalia/luento02.pdf</a>.
    Junaratoja löytyy myös muista Paavon kalvoista.</p>
</li>
<li>
<p>Javan syntaksi EBNF:nä kuvattuna. Sisältää myös graafiset junaradat.
    <a href="http://cui.unige.ch/db-research/Enseignement/analyseinfo/JAVA/BNFindex.html">http://cui.unige.ch/db-research/Enseignement/analyseinfo/JAVA/BNFindex.html</a></p>
</li>
<li>
<p>Wikipedia:
    <a href="http://en.wikipedia.org/wiki/Backus%E2%80%93Naur_Form">http://en.wikipedia.org/wiki/Backus%E2%80%93Naur_Form</a></p>
</li>
<li>
<p>Googlella löytyy lisää</p>
</li>
</ul>
<p>\
 \</p>
<p>C#:n syntaksia on kuvattu MSDN-dokumentaatiossa muun muassa seuraavilla
sivuilla.</p>
<p><a href="http://msdn.microsoft.com/en-us/library/Aa664812">http://msdn.microsoft.com/en-us/library/Aa664812</a>.</p>
<h1>29. Jälkisanat</h1>
<p>Joskus ohjelmoidessa tulee vaan tämmöinen olo:</p>
<p><a href="http://www.youtube.com/watch?v=K21fuhDo5Bo&amp;eurl=http%3A%2F%2Fwww.devtopics.com%2Fbest-computer-programming-videos%2F&amp;feature=player_embedded">http://www.youtube.com/watch?v=K21fuhDo5Bo</a></p>
<p>Totu siihen ja keitä lisää kahvia.</p>
<h1>Liite: Sanasto</h1>
<p>Internetistä löytyy ohjelmoinnista paremmin tietoa englanniksi. Tässä
tiedonhakua auttava sanasto ohjelmoinnin perustermeistä.</p>
<hr />
<p>aliohjelma                subprogram, subroutine, procedure   konstruktori          constructor          rajapinta                      interface</p>
<p>alirajapinta              subinterface                        koodauskäytänteet     code conventions     roskienkeruu                   garbage collection</p>
<p>alivuoto                  underflow                           kääntäjä              compiler             roskienkerääjä                 garbage collector</p>
<p>alkeistietotyyppi         primitive types                     kääriä                wrap                 sijoituslause                  assignment statement</p>
<p>alkio                     element                             lause                 statement            sijoitusoperaattori            assignment operator</p>
<p>alustaa                   intialize                           lippu                 flag                 silmukka                       loop</p>
<p>aritmeettinen operaatio   arithmetic operation                lohko                 block                sovelluskehitin                Integrated Development Environment</p>
<p>aritmeettinen lauseke     arithmetic expression               luokka                class                staattinen                     static</p>
<p>bugi                      bug                                 metodi                method               standardi syöttövirta          standard input stream</p>
<p>destruktori               destructor                          muuttuja              variable             standardi tulostusvirta        standard output stream</p>
<p>dokumentaatio             documentation                       määritellä            declare              standardi virhetulostusvirta   standard error output stream</p>
<p>funktio                   function                            olio                  object               syntaksi                       syntax</p>
<p>globaali vakio            global constant                     ottaa kiinni          catch                taulukko                       array</p>
<p>globaali muuttuja         global variable                     paketti               package              testaus                        testing</p>
<p>indeksi                   index                               parametri             parameter            toteuttaa                      implement</p>
<p>julkinen                  public                              periytyminen          inheritance          tuoda                          import</p>
<p>keskeytyskohta            breakpoint                          poikkeus              exception            vakio                          constant</p>
<p>komentorivi               Command Prompt                      poikkeustenhallinta   exception handling   yksikkötestaus-rajapinta       unit testing framework</p>
<p>\                         \                                   \                     \                    ylivuoto                       overflow</p>
<hr />
<p>\
 \</p>
<h1>Liite: Yleisimmät virheilmoitukset ja niiden syyt</h1>
<p>Aloittavan ohjelmoijan voi joskus olla vaikeaa saada selvää kääntäjän
virheilmoituksista. Kootaan tänne muutamia yleisimpiä C#-kääntäjän
virheilmoituksia. Osa virheilmoituksista on Jypeli-spesifisiä.</p>
<h2>Tyyppiä tai nimiavaruutta ei löydy</h2>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
The type or namespace name 'PhysisObject' could not be found (are you missing a using directive or an assembly reference?)</code></p>
<p>Syitä</p>
<p>\</p>
<ul>
<li>
<p>Oletko kirjoittanut esim. jonkun aliohjelman tai tyypin nimen
    väärin? Katso sanoja, jotka on väritetty punakynällä. Äskeisessä
    virheviestissä PhysisObject pitäisi kirjoittaa PhysicsObject. Käytä
    Visual Studion koodin täydennystä kirjoitusvirheiden välttämiseksi.</p>
</li>
<li>
<p>Jokin kirjasto puuttuu (kts Kirjastojen liittäminen projektiin:
    <a href="https://trac.cc.jyu.fi/projects/npo/wiki/KirjastojenLiittaminenKasin">wiki</a>,
    <a href="http://www.youtube.com/watch?v=vlqaRwu1ScY">video</a>)</p>
</li>
<li>
<p>Jokin using-lause puuttuu. Jypeli-pelien projektimalleissa ovat
    vakiona seuraavat using-lauseet:</p>
</li>
</ul>
<p><code>{.esimerkkisisennetty-western lang="zxx" xml:lang="zxx"}
 using System;
 using Jypeli;
 using Jypeli.Widgets;
 using Jypeli.Assets;</code></p>
<h2>Peli.Aliohjelma(): not all code paths return a value</h2>
<p>Aliohjelmalle on määritelty paluuarvo, mutta se ei palauta mitään (eli
return-lause puuttuu).</p>
<p>Seuraavassa aliohjelmassa paluuarvoksi on määritelty PhysicsObject,
mutta aliohjelma ei palauta mitään arvoa.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
PhysicsObject LuoPallo()
{
    PhysicsObject pallo = new PhysicsObject(50.0, 50.0, Shape.Circle);
}</code></p>
<p>Tällöin Visual Studio antaa virheilmoituksen.</p>
<p><img alt="" src="../src/luentomonistecsUusin_htm_77f7f118.png" />\
 Jos pallo halutaan palauttaa aliohjelmasta, niin aliohjelmaa pitää
korjata seuraavasti.</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
PhysicsObject LuoPallo()
{
    PhysicsObject pallo = new PhysicsObject(50.0, 50.0, Shape.Circle);
    return pallo;
}</code></p>
<h2>Muuttujaa ei ole olemassa nykyisessä kontekstissa</h2>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
The name 'massa' does not exist in the current context</code></p>
<p>Seuraavassa koodinpätkässä käytetään muuttujaa nimeltä massa, mutta
tuota muuttujaa ei ole esitelty missään. Jokainen muuttuja, jota
ohjelmassa käytetään, tulee esitellä jossakin. Esittely tarkoittaa, että
jollakin rivillä kirjoitetaan muuttujan tyyppi sekä nimi seuraavasti:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
double massa;</code></p>
<p>Samalla rivillä esittelyn kanssa voi myös sijoittaa muuttujalle
alkuarvon:</p>
<p><code>{.esimerkki-western lang="zxx" xml:lang="zxx"}
double massa = 100.0;</code></p>
<p>Niinpä äskeisen koodin virhe voidaan korjata kertomalla muuttujan massa
tyyppi (tyyppi on double) siinä missä tuo muuttuja ensimmäisen kerran
otetaan käyttöön:</p>
<p>Jos muuttuja esitellään aliohjelmassa lokaalina muuttujana, se pitää
alustaa ennen sen käyttöä. Jos muuttujaa tarvitaan useammassa metodissa,
voi sen esitellä attribuuttina luokan sisällä:</p>
<p>``` {.esimerkki-western lang="zxx" xml:lang="zxx"}
public class Peli : PhysicsGame
{
    private double massa; // Attribuutti, joka näkyy kaikille luokan metodeille</p>
<pre><code>public override void Begin()
{
    massa = 100.0;
    PhysicsObject pallo = new PhysicsObject(50.0, 50.0, Shape.Circle);
    pallo.Mass = massa;
    TulostaMassa();
}

public void TulostaMassa() // HUOM! Ei ole static
{
    MessageDisplay.Add("Massa on " + massa);
}
</code></pre>
<p>}
```</p>
<p>Lähdeluettelo</p>
<p>DOC: Sun, , ,
http://java.sun.com/j2se/javadoc/writingdoccomments/index.html</p>
<p>HYV: Hyvönen Martti, Lappalainen Vesa, Ohjelmointi 1, 2009</p>
<p>KOSK: Jussi Koskinen, Ohjelmistotuotanto-kurssin luentokalvot(Osa:
Ohjelmistojen ylläpito),</p>
<p>KOS: Kosonen, Pekka; Peltomäki, Juha; Silander, Simo, Java 2
Ohjelmoinnin peruskirja, 2005</p>
<p>VES: Vesterholm, Mika; Kyppö, Jorma, Java-ohjelmointi, 2003</p>
<p>LAP: Vesa Lappalainen, Ohjelmointi 2, ,
http://users.jyu.fi/\~vesal/kurssit/ohj2/moniste/html/m-Title.htm</p>
<p>MÄN: Männikkö, Timo, Johdatus ohjelmointiin- moniste, 2002</p>
<p>LIA: Y. Daniel Liang, Introduction to Java programming, 2003</p>
<p>DEI: Deitel, H.M; Deitel, P.J, Java How to Program, 2003</p>
<p>\</p>
<p>\</p>
<hr />
<p>Jyväskylän yliopisto    \   University of Jyväskylä</p>
<p>Tietotekniikan laitos       Department of Mathematical</p>
<p>\                           Information Technology</p>
<p>Luentomoniste 17            Lecture Notes 17</p>
<hr />
<p>\</p>
<p>1.</p>
<p>MÄKINEN, RAINO A. E., Numeeriset menetelmät. 1999 (107 s.)</p>
<p>2.</p>
<p>LAPPALAINEN, VESA ja RISTO LAHDELMA, Olio-ohjelmointi ja C++. 1999 (107
s.)</p>
<p>3.</p>
<p>LAPPALAINEN, VESA, Windows-ohjelmointi C-kielellä. 1999 (150 s.)</p>
<p>4.</p>
<p>ORPONEN, PEKKA, Tietorakenteet ja algoritmit 2. 2.p., 2000 (50 s.)</p>
<p>5.</p>
<p>LAPPALAINEN, VESA, Ohjelmointi++. 1999 (315 s.)</p>
<p>6.</p>
<p>MÄNNIKKÖ, TIMO, Johdatus ohjelmointiin. 2000 (155 s.)</p>
<p>7.</p>
<p>KOIKKALAINEN, PASI ja PEKKA ORPONEN, Tietotekniikan perusteet. 2001 (150
s.)</p>
<p>8.</p>
<p>ARNĂUTU, VIOREL, Numerical methods for variational problems. 2001 (100
s.)</p>
<p>9.</p>
<p>KRAVCHUK, ALEXANDER, Mathematical modelling of the biomedical
tomography: The 12^th^ Jyväskylä Summer School. 2003 (83 s.)</p>
<p>10.</p>
<p>MIETTINEN, KAISA, Epälineaarinen optimointi. 2003 (146 s.)</p>
<p>11.</p>
<p>LAPPALAINEN VESA &amp; VIITANEN SANTTU, Ohjelmointi 2. 2012 (240 s.)</p>
<p>12.</p>
<p>KAIJANAHO, ANTTI-JUHANI &amp; KÄRKKÄINEN TOMMI, Formaalit menetelmät. 2005
(171 s.)</p>
<p>13.</p>
<p>HOPPE, RONALD H. W., Numerical solution of optimization problems with
PDE constraints: Lecture notes of a course given in the 14^th^ Jyväskylä
Summer School, August 9-27, 2004. 2006 (65 s.)</p>
<p>14.</p>
<p>JYRKI JOUTSENSALO, TIMO HÄMÄLÄINEN &amp; ALEXANDER SAYENKO, QoS Supported
Networks, Scheduling, and Pricing; Theory and Applications (214 s.)</p>
<p>15.</p>
<p>MARTTI HYVÖNEN, VESA LAPPALAINEN, Ohjelmointi 1. 2009. (132 s.)</p>
<p>16.</p>
<p>ANTTI-JUHANI KAIJANAHO, Ohjelmointikielten periaatteet. 2010. (153 s.)</p>
<p>17.</p>
<p>MARTTI HYVÖNEN, VESA LAPPALAINEN, ANTTI-JUSSI LAKANEN,\
 Ohjelmointi 1 C#. 3. korjattu painos, 2013. (158 s.)</p>
<p>\
 \</p>
<p>ISBN 978-951-39-4859-7 (3. korjattu painos)</p>
<p>ISSN 1456-9787</p>
<p><a href="#sdfootnote1anc">1</a>Array.Resize-metodi ei muuta alkuperäistä taulukkoa,
vaan luo uuden taulukon, kopioi alkuperäisen taulukon kaikki alkiot
uuteen taulukkoon, ja sen jälkeen korvaa alkuperäisen taulukon
(viitteen) uudella taulukolla (viitteellä). Ks.
<a href="http://msdn.microsoft.com/en-us/library/bb348051.aspx">http://msdn.microsoft.com/en-us/library/bb348051.aspx</a>.</p>
<p>Jyväskylä 2013</p>
