5.2 Suorittaminen
-----------------

C\#:n tuottaa siis lähdekoodista suoritettavan (tai ”ajettavan”)
tiedoston. Tämä tiedosto on käyttöjärjestelmäriippuvainen, ja
suoritettavissa vain sillä alustalla, johon käännös on tehty. Toisin
sanoen, Windows-ympäristössä käännetyt ohjelmat eivät ole ajettavissa OS
X -käyttöjärjestelmässä, ja toisin päin.

Toisin kuin C\#, eräät toiset ohjelmointikielet tuottavat
käyttöjärjestelmäriippumatonta koodia. Esimerkiksi *Java*-kielessä
kääntäjän tuottama tiedosto on niin sanottua *tavukoodia*, joka on
käyttöjärjestelmäriippumatonta koodia. Tavukoodin suorittamiseen
tarvitaan Java-virtuaalikone (Java Virtual Machine). Java-virtuaalikone
on oikeaa tietokonetta matkiva ohjelma, joka tulkkaa tavukoodia ja
suorittaa sitä sitten kohdekoneen prosessorilla. Tässä on merkittävä ero
perinteisiin käännettäviin kieliin (esimerkiksi C ja C++), joissa
käännös on tehtävä erikseen jokaiselle eri laitealustalle. [VES][KOS]

\
 \

6. Aliohjelmat
==============

> “Copy and paste is a design error.” - David Parnas

Pääohjelman lisäksi ohjelma voi sisältää muitakin aliohjelmia.
Aliohjelmaa *kutsutaan* pääohjelmasta, metodista tai toisesta
aliohjelmasta suorittamaan tiettyä tehtävää. Aliohjelmat voivat saada
parametreja ja palauttaa arvon, kuten metoditkin. Pohditaan seuraavaksi
mihin aliohjelmia tarvitaan.

Jos tehtävänämme olisi piirtää useampi lumiukko, niin tämänhetkisellä
tietämyksellämme tekisimme todennäköisesti jonkin alla olevan kaltaisen
ratkaisun.