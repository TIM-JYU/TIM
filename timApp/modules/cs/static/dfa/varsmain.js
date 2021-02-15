import {setData, varsStringToJson, VariableRelations, compareValsAndRefs, compareWithUserCode} from './vars.js';
/*
Ref lista
Ref aku
Ref ankka
List $1 R 3
New $2 "Aku"
New $3 "Ankka"
lista -> $1
aku -> $2
aku -> $3
ankka -> $3
$1[0] -> $2
$1[1] -> $3
$1[-1] -> $3
$2[1] -> $3
$1[2] -> $2
$1[2] -> $3
value a = 3
V a = 4
kettu
a = 2
a -> $2
$1[0] -> aku
aku[0] -> aku

v a 5
r b
n $1 kissa
l $2 v 5
a $3 r 3
l $4 r 6
l $5 v a,b,c,d
l $6 r $1, $2

s $j1 V 1 "Ankka Aku" Ankkalinna 12345 ...
a $2 v 1 2 3

s $j2 V 2 "Susi Sepe" Takametsä 12555 ...
s $j3 V 4 "Ponteva Veli" Takametsä 12355 ...
a $alkiot R $j1 $j2 $j3
s st V 1 1 2 3 4 5 6 7 8 9

s jasenet V 1 "Kelmien kerho" nimet.dat 8 3 $1
s $j1 V 1 "Ankka Aku" Ankkalinna 12345 ...
a $2 v 1 2 3
s $j2 V 2 "Susi Sepe" Takametsä 12555 ...
s $j3 V 4 "Ponteva Veli" Takametsä 12355 ...
a $alkiot R $j1 $j2 $j3
s st V 0 1 2 3 4 5 6 7 8 9
v susi 20
v kettu 900
g w: 1, rank: 1, rankdir: 1
s $1 a 12 null
s $2 a 10 null
$1[1] -> $2
a $3 r 5
g rank: -1
v a 3
v b 4
r e
g rank: 2, y: 200
s $4@2 a 1,2,3
$2[1] -> $4
r d@4 -> $4
e -> $4
r f@2->$4
a $1 v5
a $2 v4
a $3 v3
g r 0
r t
a $4 rv $1 $2 $3
t -> $4
*/
const code = `
gn item5: r 1
rank 1: rd 1, ax=120, ay=100, rd: 1, dx: 100
rank 0, rd: 1
g w:1, snap: 0 1
class Item: value V, next SR
ref root
g r 0
s.Item item1 1
s.Item item2 2
s.Item item3 3
s.Item item4 4

root -> item1
item1.next -> item2
item2.next -> item3
item3.next -> item4
g gotoStep
g firstStep, r 1, sx: 100
// BYCODEBEGIN
CODE: // Uusi apuviite
ref p
CODE: /// Alkuun
p = root
CODE: /// Eteenpäin, etsitään 3
p = p.next
CODE: // Eteenpäin kunnes p.value == 3
p = p.next
style1 item3.value fill=lime
CODE: /// Luodaan uusi alkio
ref uusi -> s.Item item5 5
CODE: /// Ja tämän perään 3:n seuraava
uusi.next = p.next
CODE: /// Tämän 3:n perään
p.next = uusi
move item5 item4
// BYCODEEND
`;
const code22=`
# Ohjelmointi 2 kurssin malliharjoitustyön rakenne
    g w:3, h:2, dir:1
class Naytto: kerho R
    g w:2, h:2, dir:1
class Kerho: jasenet R, harrastukset R
    g w:3
class Jasenet: kokonimi S, tiedostonNimi S, maxLkm V, lkm V, alkiot R
    g w:3
class Jasen: tunnusNro V, nimi S, osoite S, postinumero V, "": S
    g w:3
class Harrastukset: "_": S, tiedostonNimi S, maxLkm V, lkm V, alkiot R
    g w:3
class Harrastus: tunnusNro V, jasenNro V, ala S, aloitusvuosi V, tuntiaViikossa V

g rank KERHO, ax: 250, ay: 45
g rx: 380, ry: -4
g debug c
s.Naytto +naytto
t {sx: -130, sy: -100} Naytto naytto

g rx: 0, ry: 0
s.Kerho +kerho
t {"align": middle, sy: -105} Kerho
t {"align": end, sx: -70, sy: -20} jasenet
t {"align": start, sx: 70, sy: -20} harrastukset
SVG <path d="M 730 130 Q 450 125 450 0" fill="none" stroke="#000" stroke-dasharray="3 3"></path>

naytto.kerho -> kerho

g rank JASENET, ax: 100, ay: 200
g rx: 30, ry: 0
s.Jasenet +jasenet "Kelmien kerho" nimet.dat 8 0
g rx: 0, ry: 160, w:2, dir: 0
a +alkiot R8
g rx: 150, ry: 120
s.Jasen j1 1 "Ankka Aku" Ankkalinna 12345 ...
s.Jasen j2 2 "Susi Sepe" Takametsä 12555 ...
s.Jasen j3 4 "Ponteva Veli" Takametsä 12355 ...
t {"align": middle} Jasen

jasenet.alkiot -> alkiot

alkiot[0] -> j1
alkiot[1] -> j2
alkiot[2] -> j3
jasenet.lkm = 3


g rank HARRASTUKSET, ax: 360, ay: 200
g rx: 0, ry: 0
s.Harrastukset +harrastukset "" harrastukset.dat 10
g rx: 20, ry: 160, w : 2, dir: 0
a alkioth R10
g rx: 240, ry: 110, snap: 0 2
s.Harrastus h1  1 1 kalastus 1955 20
s.Harrastus h2  2 1 "laiskottelu" 1950 20
s.Harrastus h3  3 2 "kelmien kerho" 1962 2
s.Harrastus h4  4 1 "työn pakoilu" 1962 40
s.Harrastus h5  5 2 "possujen jah." 1954 20
s.Harrastus h6  7 4 "susiansojen..." 1956 15
t {"align": middle} Harrastus

harrastukset.alkiot -> alkioth
alkioth[0] -> h1
alkioth[1] -> h2
alkioth[2] -> h3
alkioth[3] -> h4
alkioth[4] -> h5
alkioth[5] -> h6
harrastukset.lkm = 6


kerho.jasenet -> jasenet
kerho.harrastukset -> harrastukset
g gotoStep
g firstStep

/// Etsitään Aku Ankka
g x: 520, y:30
n guinimi:nimi Ankka Aku
g rank ETSI, ax: 760, ay: 100
g rx 0, ry 0
ref jasen
g tx 390, ty: 30
ref kerhonimi:nimi -> guinimi
g tx 270, ty: 100
ref jasen2:jasen
g tx 210, ty: 160
ref jasenetnimi:nimi -> guinimi
g tx 155, ty: 300
ref jasen3:jasen
style guinimi fill=yellow
jasen3 -> j1
style j1.nimi fill=lime
// Palautetaan viite löytyneeseen
style guinimi 
style j1.nimi 
jasen2 -> j1
jasen3 -> null
gn jasen3: tsx -500
jasenetnimi -> null
gn jasenetnimi: tsx -500
g rank ETSI
jasen -> j1
jasen2 -> null
gn jasen2: tsx -700
kerhonimi -> null
gn kerhonimi: tsx -700
// Nyt on käyttöliittymällä viite Aku Ankkaan
/// Etsitään Aku Ankan harrastuksia
ref guiharrastukset: harrastukset
gn jasen2: tsx 0
// Viedään jasen-viite kerholle
jasen2 -> j1
g rank HARRASTUKSET
g ry -75, rx 120
// Viedään jäsenen id harrastuksille
val id=1
// Luodaan löytyneiden lista
ref loydetyt
ref har
g rank ETSI
l loyt RV5
loydetyt -> loyt
// Aletaan etsiä niitä harrastuksia, joilla 1 jäsen id:ssä
style id fill=yellow
har -> h1
style h1.jasenNro fill=lime
// Lisätään viitteet löytyneiden listaan
loyt[0] -> h1
loyt.count = 1
style h1.jasenNro
har -> h2
style h2.jasenNro fill=lime
loyt[1] -> h2
loyt.count = 2
style h2.jasenNro
har -> h3
style h3.jasenNro fill=red
style h3.jasenNro
har -> h4
style h4.jasenNro fill=lime
loyt[2] -> h4
loyt.count = 3
style h4.jasenNro
har -> h5
style h5.jasenNro fill=red
style h5.jasenNro
har -> h6
style h6.jasenNro fill=red
style h6.jasenNro
gn id: tsx -600
// Palautetaan viite löytyneiden listaan
har -> null
gn har: tsx -600
guiharrastukset -> loyt
loydetyt -> null
gn loydetyt: tsx -600
jasen2 -> null
gn jasen2: tsx -600
// Nyt on viite jäseneen ja sen harrastuksiin
`;
setData({
    code: code, args: "1001", params: {
         mode: "step,code",
         errorlevel: 3,
         // animate: "commands",
         animate: "code",
         allowLazy: false,
         justone: true,
         }
});

if (false) {
    const code2 = `
Ref susi
New $1 Susi
susi -> $1
#Ref isosusi
`;

// let vars1 =  new VariableRelations(code, {mode: "static", errorlevel: 3, xanimate: "commands", allowLazy: true});
// let vars2 =  new VariableRelations(code2, {mode: "static", errorlevel: 3, xanimate: "commands", allowLazy: true});
// vars1.runUntil();
// vars2.runUntil();
    let diff = compareValsAndRefs(code, code2);
    console.log(diff);
// BYCODEBEGIN
// BYCODEEND

    let data = {
        answer_call_data: {
            markup: {
                fullprogram:
                    `
jarray t1 v5x4 6 3 2 9 | 0 44 0 22 | 0  0 55 | 33 | 0 0 81
ref tp1:t1 -> t1
jarray t v5x4
ref tp:t -> t
// BYCODEBEGIN
t[0][3] = 9
// BYCODEEND
`
            }
        }, save_object: {
            usercode:
                `
t[0][3] = 91
t[0][2] = 2
t[0][1] = 3
t[0][0] = 6

t[1][3] = 22
t[1][1] = 44

t[2][2] = 55

t[3][0] = 33

t[4][2] = 81
`
        }
    };
    let tcode =
        `
t[0][0] = 6
t[0][1] = 3
t[0][2] = 2
t[0][3] = 9

t[1][0] = 0
t[1][1] = 44
t[1][2] = 0
t[1][3] = 22

t[2][0] = 0
t[2][1] = 0
t[2][2] = 55
t[2][3] = 0

t[3][0] = 33
t[3][1] = 0
t[3][2] = 0
t[3][3] = 0

t[4][0] = 0
t[4][1] = 0
t[4][2] = 81
t[4][3] = 0
`;
    let diff2 = compareWithUserCode(data, tcode);
    console.log(diff2);
}